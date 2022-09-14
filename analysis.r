# load libraries
library(tidyverse)
library(sf)
library(raster)
library(rgdal)
library(runjags)
library(rjags)
library(coda)
library(snowfall)
library(parallel)
library(modeest)
library(sjlabelled)
library(factoextra)
library(HDInterval)
library(PerformanceAnalytics)
library(tidybayes)
library(mice)

# load functions
source("functions.r")

# load raw survey response data
Pure <- read_csv("input/survey_responses/pure_responses_20211210.csv")
BCT <- read_csv("input/survey_responses/bct_responses_20211210.csv")
Mail <- read_csv("input/survey_responses/mail_responses_20211210.csv")

# load geocodes response locations - create new field NewPropID
Geocodes <- read_csv("input/geocodes/geocode_points_final_final_table.csv") %>% mutate(NewPropID = if_else(is.na(propid), CADID, propid))

# load property level predictor data for responses
Preds_Responses <- read_csv("input/predictors/props_responses_predictors_table.csv")

# load property level predictor data for all properties - so we can make predictions
Preds_Properties <- read_csv("input/predictors/props_properties_predictors_table.csv")

# compile survey responses and remove responses not from property owner or manager, not complete, or not consented
# also remove known test responses and responses known to be outside of NSW and ACT

# compile pure

Compiled_Pure <- Pure %>%
                filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                         Finished == "TRUE",
                         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                         Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
                         Status != "Survey Preview",          # [COLUMN: Status] remove preview items
                         !rid %in% c(3657683231,7851483144,7393760845,6887891501,5410774572,1370281511), # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
                         !rid %in% c(1473371087,9197412646,7012337383,4666826207,3758237201,2711428891,1877608265,4903070120,8963111258), # [COLUMN: rid] remove other tests: 1473371087, 9197412646, 7012337383, 4666826207, 3758237201, 2711428891, 1877608265, 4903070120, 8963111258
                         !ResponseId %in% c("R_1gGR9du45NBxZ8R","R_2qazcFdKqVkwWm8","R_3FQfz9x5dlG9wsk","R_2v0pQY8Rt3QQidj","R_SBmhZPkdUQBn2Ex","R_2v7CVYBE0fOPfWu",
                                        "R_6rnbTpeqDfSQJBD","R_1dMSdc29URnPMDr","R_21FykEy7kg1Ej5H","R_4Sp7PggnX8qhq2R","R_eyyz22mPeHHxpIt","R_2sax0wr9wrNxApT",
                                        "R_3hrn4WXTGp8gg7Y","R_2bK4iv8qKMQNA4J","R_2too9cnr5g2jcCM","R_Rn9yZKCGInAM1iN","R_3rOqczF7y6wNhLj","R_1rpOP8hEQ6O4oNc",
                                        "R_1hXwRWiRPFHA1Ng","R_2sandcSYmsTFtNw")) # [COLUMN: ResponseId] remove response IDs outside of NSW: R_1gGR9du45NBxZ8R, R_2qazcFdKqVkwWm8, R_3FQfz9x5dlG9wsk, R_2v0pQY8Rt3QQidj, R_SBmhZPkdUQBn2Ex, R_2v7CVYBE0fOPfWu
                                                                             # R_6rnbTpeqDfSQJBD, R_1dMSdc29URnPMDr, R_21FykEy7kg1Ej5H, R_4Sp7PggnX8qhq2R, R_eyyz22mPeHHxpIt, R_2sax0wr9wrNxApT, R_3hrn4WXTGp8gg7Y, R_2bK4iv8qKMQNA4J
                                                                             # R_2too9cnr5g2jcCM, R_Rn9yZKCGInAM1iN, R_3rOqczF7y6wNhLj, R_1rpOP8hEQ6O4oNc, R_1hXwRWiRPFHA1Ng, R_2sandcSYmsTFtNw
write.csv(Compiled_Pure, file="output/compiled_survey_data/pure_responses_compiled.csv", row.names = FALSE)

# compile bct

Compiled_BCT <- BCT %>%
                filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                         Finished == "TRUE",
                         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                         Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
                         Status != "Survey Preview",          # [COLUMN: Status] remove preview items
                         !ResponseId %in% c("R_1hZ19EtGQ8is0qe","R_3EQmDdj6NdFrjiL")) # [COLUMN: ResponseId] remove response IDs outside NSW or used for testing: R_1hZ19EtGQ8is0qe, R_3EQmDdj6NdFrjiL
write.csv(Compiled_BCT, file="output/compiled_survey_data/bct_responses_compiled.csv", row.names = FALSE)

# compile mail

Compiled_Mail <- Mail %>%
                filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                         Finished == "TRUE",
                         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                         Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
                         Status != "Survey Preview",          # [COLUMN: Status] remove preview items
                         !Q83 %in% c("123456","345678","234567","456789","222","333","777","AAAA","BBBB","CCCC")) # [COLUMN: Q83] remove tests: 123456 (Test), 345678 (QLD), 234567 (Test), 456789 (Test), 222, 333, 777, AAAA, BBBB, CCCC
write.csv(Compiled_Mail, file="output/compiled_survey_data/mail_responses_compiled.csv", row.names = FALSE)

# join response geocoded locations and remove cases that are in urban or intensive use areas. Here we remove cases where the associated
# property (closest property) has a secondary land use (ALUM code) that is: Manufacturing and Industrial (5.3.0 - Based on [LU_Sec]) or
# Services (5.5.0 - Based on [LU_Sec]) or has a tertiary land use (ALUM code) that is: Urban Residential (5.4.1 - Based on [LU_Tert])
# this aims to remove responses that may be unreliable due to a mismatch between reported spatial location and the target property types

Compiled_Pure_NotIntensive <- Compiled_Pure %>% left_join(Geocodes, by = c("ResponseId" = "ResponseID")) %>% filter(!(!is.na(OID_) & (LU_Sec == "5.3.0 Manufacturing and industrial" |
                                                    LU_Sec == "5.5.0 Services" | LU_Tert == "5.4.1 Urban residential"))) %>% mutate(CADID = ifelse(LocationType == "Road" | InsideStudyArea == "No", NA, CADID)) %>% mutate(propid = ifelse(LocationType == "Road" | InsideStudyArea == "No", NA, propid)) %>% mutate(NewPropID = ifelse(LocationType == "Road" | InsideStudyArea == "No", NA, NewPropID)) %>% dplyr::select(-DistanceLot, -LocationType, -MERGE_SRC, -ha, -MosType, -MosGrp, -LU_Sec, -LU_Tert, -PostCode.y)
write.csv(Compiled_Pure_NotIntensive %>% dplyr::select(-OID_, -InsideStudyArea), file="output/compiled_survey_data/pure_responses_compiled_notintensive.csv", row.names = FALSE)

Compiled_BCT_NotIntensive <- Compiled_BCT %>% left_join(Geocodes, by = c("ResponseId" = "ResponseID")) %>% filter(!(!is.na(OID_) & (LU_Sec == "5.3.0 Manufacturing and industrial" |
                                                    LU_Sec == "5.5.0 Services" | LU_Tert == "5.4.1 Urban residential"))) %>% mutate(CADID = ifelse(LocationType == "Road" | InsideStudyArea == "No", NA, CADID)) %>% mutate(propid = ifelse(LocationType == "Road" | InsideStudyArea == "No", NA, propid)) %>% mutate(NewPropID = ifelse(LocationType == "Road" | InsideStudyArea == "No", NA, NewPropID)) %>% dplyr::select(-DistanceLot, -LocationType, -MERGE_SRC, -ha, -MosType, -MosGrp, -LU_Sec, -LU_Tert, -PostCode.y)
write.csv(Compiled_BCT_NotIntensive %>% dplyr::select(-OID_, -InsideStudyArea), file="output/compiled_survey_data/bct_responses_compiled_notintensive.csv", row.names = FALSE)

Compiled_Mail_NotIntensive <- Compiled_Mail %>% left_join(Geocodes, by = c("ResponseId" = "ResponseID")) %>% filter(!(!is.na(OID_) & (LU_Sec == "5.3.0 Manufacturing and industrial" |
                                                      LU_Sec == "5.5.0 Services" | LU_Tert == "5.4.1 Urban residential"))) %>% mutate(CADID = ifelse(LocationType == "Road" | InsideStudyArea == "No", NA, CADID)) %>% mutate(propid = ifelse(LocationType == "Road" | InsideStudyArea == "No", NA, propid)) %>% mutate(NewPropID = ifelse(LocationType == "Road" | InsideStudyArea == "No", NA, NewPropID)) %>% dplyr::select(-DistanceLot, -LocationType, -MERGE_SRC, -ha, -MosType, -MosGrp, -LU_Sec, -LU_Tert, -PostCode.y)
write.csv(Compiled_Mail_NotIntensive %>% dplyr::select(-OID_, -InsideStudyArea), file="output/compiled_survey_data/mail_responses_compiled_notintensive.csv", row.names = FALSE)

# remove responses that we don't have a spatial location and are not inside the study area
Compiled_Pure_NotIntensive_Location_Only <- Compiled_Pure_NotIntensive %>% filter(!is.na(OID_) & InsideStudyArea == "Yes") %>% dplyr::select(-OID_, -InsideStudyArea)
write.csv(Compiled_Pure_NotIntensive_Location_Only, file="output/compiled_survey_data/pure_responses_compiled_notintensive_locationonly.csv", row.names = FALSE)
Compiled_BCT_NotIntensive_Location_Only <- Compiled_BCT_NotIntensive %>% filter(!is.na(OID_) & InsideStudyArea == "Yes") %>% dplyr::select(-OID_, -InsideStudyArea)
write.csv(Compiled_BCT_NotIntensive_Location_Only, file="output/compiled_survey_data/bct_responses_compiled_notintensive_locationonly.csv", row.names = FALSE)
Compiled_Mail_NotIntensive_Location_Only <- Compiled_Mail_NotIntensive %>% filter(!is.na(OID_) & InsideStudyArea == "Yes") %>% dplyr::select(-OID_, -InsideStudyArea)
write.csv(Compiled_Mail_NotIntensive_Location_Only, file="output/compiled_survey_data/mail_responses_compiled_notintensive_locationonly.csv", row.names = FALSE)

# select data for WTA analysis

# select pure
Selected_Pure <- Compiled_Pure_NotIntensive_Location_Only %>% dplyr::select(ResponseId, RecordedDate, Lat, Long, Address, NewPropID, SA1_7DIG16, KMR, Q3.4, Q3.5, Q8.4, Q8.5, Q8.6, Q8.7) %>%
                    arrange(RecordedDate) %>% mutate(AreaHa = Q3.4, AreaAc = Q3.5, WTAInf = trimws(as.character(Q8.4)), WTATen = trimws(as.character(Q8.6)),
                    PropInf = ifelse(Q8.5 >= 0 & Q8.5 <= 100, Q8.5 / 100, NA), PropTen = ifelse(Q8.7 >= 0 & Q8.7 <= 100, Q8.7 / 100, NA)) %>%
                    dplyr::select(ResponseId, RecordedDate, Lat, Long, Address, NewPropID, SA1_7DIG16, KMR, AreaHa, AreaAc, WTAInf, WTATen, PropInf, PropTen)

# select bct
Selected_BCT <- Compiled_BCT_NotIntensive_Location_Only %>% dplyr::select(ResponseId, RecordedDate, Lat, Long, Address, NewPropID, SA1_7DIG16, KMR, Q3.4, Q3.5, Q8.4, Q8.5, Q8.6, Q8.7) %>%
                    arrange(RecordedDate) %>% mutate(AreaHa = Q3.4, AreaAc = Q3.5, WTAInf = trimws(as.character(Q8.4)), WTATen = trimws(as.character(Q8.6)),
                    PropInf = ifelse(Q8.5 >= 0 & Q8.5 <= 100, Q8.5 / 100, NA), PropTen = ifelse(Q8.7 >= 0 & Q8.7 <= 100, Q8.7 / 100, NA)) %>%
                    dplyr::select(ResponseId, RecordedDate, Lat, Long, Address, NewPropID, SA1_7DIG16, KMR, AreaHa, AreaAc, WTAInf, WTATen, PropInf, PropTen)

# select mail
Selected_Mail <- Compiled_Mail_NotIntensive_Location_Only %>% dplyr::select(ResponseId, RecordedDate, Address, Lat, Long, NewPropID, SA1_7DIG16, KMR, Q3.4, Q3.5, Q8.4, Q8.5, Q8.6, Q8.7) %>%
                    arrange(RecordedDate) %>% mutate(AreaHa = Q3.4, AreaAc = Q3.5, WTAInf = trimws(as.character(Q8.4)), WTATen = trimws(as.character(Q8.6)),
                    PropInf = ifelse(Q8.5 >= 0 & Q8.5 <= 100, Q8.5 / 100, NA), PropTen = ifelse(Q8.7 >= 0 & Q8.7 <= 100, Q8.7 / 100, NA)) %>%
                    dplyr::select(ResponseId, RecordedDate, Lat, Long, Address, NewPropID, SA1_7DIG16, KMR, AreaHa, AreaAc, WTAInf, WTATen, PropInf, PropTen)
# fix non-numeric area data on mailout survey
Selected_Mail <- Selected_Mail %>% mutate(AreaHa = as_numeric(if_else(AreaHa == "2.2ha", "2.2", AreaHa)))

# compile all selected
Selected_All <- rbind(Selected_Pure, Selected_BCT, Selected_Mail) %>% arrange(RecordedDate)

# remove duplicates based on address
Selected_All <- Selected_All[!duplicated(Selected_All$Address), ]
write.csv(Selected_All, file="output/compiled_survey_data/all_selected.csv", row.names = FALSE)

# join to response data
Selected_All <- Selected_All %>% left_join(Preds_Responses, "NewPropID") %>% dplyr::select(-OID_, -Shape_Length, -Shape_Area)

# remove duplicates based NewPropID
Selected_All <- Selected_All[!duplicated(Selected_All[,"NewPropID"]) | is.na(Selected_All[,"NewPropID"]), ]

# set land values that are 0 to NA
Selected_All[which(Selected_All[,"LValHa"] == 0), "LValHa"] <- NA

# where respondents say they wouldn't consider a covenant make sure that the proportion the would covenant is set to NA
Selected_All <- Selected_All %>% mutate(PropInf = if_else(WTAInf == "I would not participate", NA_real_, PropInf), PropTen = if_else(WTATen == "I would not participate", NA_real_, PropTen))

# import and compile census data
# import census data at SA1 level
G01 <- read_csv("input/census/2016Census_G01_NSW_SA1.csv") %>% dplyr::select(SA1_7DIGITCODE_2016, Birthplace_Australia_P, Birthplace_Elsewhere_P, Lang_spoken_home_Eng_only_P, Lang_spoken_home_Oth_Lang_P, High_yr_schl_comp_Yr_12_eq_P, High_yr_schl_comp_Yr_12_eq_P, High_yr_schl_comp_Yr_11_eq_P, High_yr_schl_comp_Yr_10_eq_P, High_yr_schl_comp_Yr_9_eq_P, High_yr_schl_comp_Yr_8_belw_P, High_yr_schl_comp_D_n_g_sch_P, Tot_P_P) %>%
          mutate(PBirthAus = Birthplace_Australia_P / (Birthplace_Australia_P + Birthplace_Elsewhere_P), PEngLang = Lang_spoken_home_Eng_only_P / (Lang_spoken_home_Eng_only_P + Lang_spoken_home_Oth_Lang_P), PYear12Ed = High_yr_schl_comp_Yr_12_eq_P / (High_yr_schl_comp_Yr_12_eq_P + High_yr_schl_comp_Yr_11_eq_P + High_yr_schl_comp_Yr_10_eq_P + High_yr_schl_comp_Yr_9_eq_P + High_yr_schl_comp_Yr_8_belw_P +	High_yr_schl_comp_D_n_g_sch_P))
G02 <- read_csv("input/census/2016Census_G02_NSW_SA1.csv") %>% dplyr::select(SA1_7DIGITCODE_2016, Median_age_persons, Average_household_size, Median_tot_hhd_inc_weekly, Median_mortgage_repay_monthly) %>%
          mutate(Age = Median_age_persons, HSize = Average_household_size, HInc = Median_tot_hhd_inc_weekly, MortPay = Median_mortgage_repay_monthly) %>% dplyr::select(-SA1_7DIGITCODE_2016)
G08 <- read_csv("input/census/2016Census_G08_NSW_SA1.csv") %>% dplyr::select(SA1_7DIGITCODE_2016, Tot_P_BP_B_OS, Tot_P_Tot_Resp, Tot_P_BP_NS) %>%
          mutate(PBornOS = Tot_P_BP_B_OS / (Tot_P_Tot_Resp - Tot_P_BP_NS)) %>% dplyr::select(-SA1_7DIGITCODE_2016)
G25 <- read_csv("input/census/2016Census_G25_NSW_SA1.csv") %>% dplyr::select(SA1_7DIGITCODE_2016, CF_ChU15_a_Total_P, CF_no_ChU15_a_Total_P, OPF_ChU15_a_Total_P, OPF_no_ChU15_a_Total_P, Total_P) %>%
          mutate(PHComp1 = CF_ChU15_a_Total_P / Total_P, PHComp2 = CF_no_ChU15_a_Total_P / Total_P, PHComp3 = OPF_ChU15_a_Total_P / Total_P, PHComp4 = OPF_no_ChU15_a_Total_P / Total_P) %>%
          dplyr::select(-SA1_7DIGITCODE_2016)
G46 <- bind_cols(read_csv("input/census/2016Census_G46A_NSW_SA1.csv"), read_csv("input/census/2016Census_G46B_NSW_SA1.csv") %>% dplyr::select(-SA1_7DIGITCODE_2016)) %>%
          dplyr::select(SA1_7DIGITCODE_2016, P_PGrad_Deg_Total, P_GradDip_and_GradCert_Total, P_BachDeg_Total, P_Tot_Total, P_Lev_Edu_NS_Total, P_Lev_Edu_IDes_Total) %>%
          mutate(PBachEd = (P_PGrad_Deg_Total + P_GradDip_and_GradCert_Total + P_BachDeg_Total) / (P_Tot_Total - P_Lev_Edu_NS_Total - P_Lev_Edu_IDes_Total)) %>% dplyr::select(-SA1_7DIGITCODE_2016)
G53 <- bind_cols(read_csv("input/census/2016Census_G53A_NSW_SA1.csv"), read_csv("input/census/2016Census_G53B_NSW_SA1.csv") %>% dplyr::select(-SA1_7DIGITCODE_2016)) %>%
          dplyr::select(SA1_7DIGITCODE_2016, Agri_for_fish_Tot, Tot_Tot, ID_NS_Tot) %>%
          mutate(PAgEmploy = Agri_for_fish_Tot / (Tot_Tot - ID_NS_Tot)) %>% dplyr::select(-SA1_7DIGITCODE_2016)
# combine census data
Census <- bind_cols(G01, G02, G08, G25, G46, G53) %>% dplyr::select(SA1_7DIGITCODE_2016, Age, PYear12Ed, PBachEd, PBirthAus, PEngLang, PBornOS, HSize, PHComp1, PHComp2, PHComp3, PHComp4, HInc, MortPay, PAgEmploy)
# save census data
write.csv(Census, file="output/census/census.csv", row.names = FALSE)

# summarise data by the SA1 7 digit code, generate sequential IDs for SA1s, and join SA1 level census predictors
SA1_IDLookUp <- Selected_All %>% group_by(SA1_7DIG16) %>% summarise(ObsCount = n()) %>% mutate(SA1_7DIG16 = as.numeric(SA1_7DIG16)) %>% arrange(SA1_7DIG16) %>% mutate(SA1ID = 1:n())
SA1_Var <- SA1_IDLookUp %>% left_join(Census, by = c("SA1_7DIG16" = "SA1_7DIGITCODE_2016"))

# undertake dimension reduction for the census data predictors using PCA
SA1_PCA <- prcomp(~Age + PYear12Ed + PBachEd + PBirthAus + PEngLang + PBornOS + HSize + PHComp1 + PHComp2 + PHComp3 + PHComp4 + HInc + MortPay + PAgEmploy, data = SA1_Var, scale = TRUE)

# inspect scree plots
fviz_eig(SA1_PCA)
fviz_eig(SA1_PCA, choice = "eigenvalue")

# inspect bi-plot of variables
fviz_pca_var(SA1_PCA, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_var(SA1_PCA, axes = c(3, 4), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_var(SA1_PCA, axes = c(5, 6), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# create component values for each individual sample and choose number of components to include - here we select the first 5 PC that explain > 80% of the variation in the data
# MODIFY HERE BASED ON INSPECTION OF SCREE PLOT / VARIANCE EXPLAINED TO CHOOSE THE NUMBER OF COMPONENTS TO INCLUDE
SA1_PCA_Ind <- get_pca_ind(SA1_PCA)$coord[, 1:5] %>% as_tibble()

# join to SA1 variables
SA1_Var <- bind_cols(SA1_Var, SA1_PCA_Ind)

# join IDs for SA1s to survey data - to use for random-effects if needed
Selected_All <- Selected_All %>% mutate(SA1_7DIG16 = as.numeric(SA1_7DIG16)) %>% left_join(SA1_IDLookUp, by = c("SA1_7DIG16")) %>% dplyr::select(-ObsCount)

# create IDs for KMRs
KMR_IDLookUp <- Selected_All %>% group_by(KMR) %>% summarise(ObsCount = n()) %>%  arrange(KMR) %>% mutate(KMRID = 1:n())

# join IDs for KMRs to survey data - to use for random-effects if needed
Selected_All <- Selected_All %>% left_join(KMR_IDLookUp, by = c("KMR")) %>% dplyr::select(-ObsCount)

# save property level data
write.csv(Selected_All, file="output/compiled_survey_data/selected_all.csv", row.names = FALSE)

# extract data we need to fit the models
Data_Model_Fit <- Selected_All %>% dplyr::select(WTAInf, PropInf, WTATen, PropTen, Area, LValHa, MosType, LUSec, DistMU, DistOU, PropNatTree, PropNatGrass, Condition, Connectivity, Elevation, Slope, TRIndex, SoilCap, SA1ID, KMR, KMRID)

# compile data at the individual property level

# whether the landholder would even consider adopting an agreement or not for a perpetual covenant
ACCEPT.Inf.df <- Data_Model_Fit %>%
                  mutate(ACCEPT = recode(WTAInf,
                                   "$50"="1",
                                   "$100"="1",
                                   "$1,500"="1",
                                   "$2,500"="1",
                                   ">$2,500"="1",
                                   "$2,000"="1",
                                   "I would not participate"="0",
                                   "I would pay"="1",
                                   "$500"="1",
                                   "$750"="1",
                                   "$25"="1",
                                   "$1,000"="1",
                                   "$250"="1",
                                   "$0"="1",
                                   " "=NA_character_, .default=NA_character_)) %>%
                  dplyr::select(ACCEPT)

ACCEPT.Inf <- as.numeric(unlist(ACCEPT.Inf.df))

# whether the landholder would even consider adopting an agreement or not for a 10 year covenant
ACCEPT.10yr.df <- Data_Model_Fit %>%
                   mutate(ACCEPT = recode(WTATen,
                                   "$50"="1",
                                   "$100"="1",
                                   "$1,500"="1",
                                   "$2,500"="1",
                                   ">$2,500"="1",
                                   "$2,000"="1",
                                   "I would not participate"="0",
                                   "I would pay"="1",
                                   "$500"="1",
                                   "$750"="1",
                                   "$25"="1",
                                   "$1,000"="1",
                                   "$250"="1",
                                   "$0"="1",
                                   " "=NA_character_, .default=NA_character_)) %>%
                   dplyr::select(ACCEPT)

ACCEPT.10yr <- as.numeric(unlist(ACCEPT.10yr.df))

# wta for perpetual covenant
WTA.Inf <- Data_Model_Fit %>%
            mutate(WTA = recode(WTAInf,
                          "$50"="50",
                          "$100"="100",
                          "$1,500"="1500",
                          "$2,500"="2500",
                          ">$2,500"=NA_character_,
                          "$2,000"="2000",
                          "I would not participate"=NA_character_,
                          "I would pay"=NA_character_,
                          "$500"="500",
                          "$750"="750",
                          "$25"="25",
                          "$1,000"="1000",
                          "$250"="250",
                          "$0"="0",
                          " "=NA_character_, .default=NA_character_)) %>%
            dplyr::select(WTA)
WTA.Inf <- as.numeric(unlist(WTA.Inf)) / 1000 # to convert to 1000s of dollars

# wta for 10 year covenant
WTA.10yr <- Data_Model_Fit %>%
            mutate(WTA = recode(WTATen,
                          "$50"="50",
                          "$100"="100",
                          "$1,500"="1500",
                          "$2,500"="2500",
                          ">$2,500"=NA_character_,
                          "$2,000"="2000",
                          "I would not participate"=NA_character_,
                          "I would pay"=NA_character_,
                          "$500"="500",
                          "$750"="750",
                          "$25"="25",
                          "$1,000"="1000",
                          "$250"="250",
                          "$0"="0",
                          " "=NA_character_, .default=NA_character_)) %>%
            dplyr::select(WTA)
WTA.10yr <- as.numeric(unlist(WTA.10yr)) / 1000 # to convert to 1000s of dollars

# censoring for perpetual covenant
CENS.Inf <- Data_Model_Fit %>%
              mutate(CENS = recode(WTAInf,
                         "$50"="1",
                         "$100"="1",
                         "$1,500"="1",
                         "$2,500"="1",
                         ">$2,500"="2",
                         "$2,000"="1",
                         "I would not participate"=NA_character_,
                         "I would pay"="0",
                         "$500"="1",
                         "$750"="1",
                         "$25"="1",
                         "$1,000"="1",
                         "$250"="1",
                         "$0"="1",
                         " "=NA_character_, .default=NA_character_)) %>%
              dplyr::select(CENS)
CENS.Inf <- as.numeric(unlist(CENS.Inf))

# censoring for 10 year covenant
CENS.10yr <- Data_Model_Fit %>%
              mutate(CENS = recode(WTATen,
                         "$50"="1",
                         "$100"="1",
                         "$1,500"="1",
                         "$2,500"="1",
                         ">$2,500"="2",
                         "$2,000"="1",
                         "I would not participate"=NA_character_,
                         "I would pay"="0",
                         "$500"="1",
                         "$750"="1",
                         "$25"="1",
                         "$1,000"="1",
                         "$250"="1",
                         "$0"="1",
                         " "=NA_character_, .default=NA_character_)) %>%
              dplyr::select(CENS)
CENS.10yr <- as.numeric(unlist(CENS.10yr))

# proportion of property for perpetual covenant
PROP.Inf <- Data_Model_Fit %>% dplyr::select(PropInf)
PROP.Inf <- as.numeric(unlist(PROP.Inf))

# proportion of property for 10 year covenant
PROP.10yr <- Data_Model_Fit %>% dplyr::select(PropTen)
PROP.10yr <- as.numeric(unlist(PROP.10yr))

# KMR
KMR <- Data_Model_Fit %>% dplyr::select(KMR) %>% droplevels() %>% as_factor()
KMR$KMR <- relevel(KMR$KMR, "Central Coast")

# Mosaic type
MOS <- Data_Model_Fit %>% dplyr::select(MosType) %>% droplevels()

# visualise the frequency of Mosaic types in the responses and target properties and save
fct_count(MOS$MosType)
write.csv(fct_count(MOS$MosType), file="output/compiled_survey_data/mosaic_freq_responses.csv", row.names = FALSE)
# compare to the Mosaic type frequencies in private properties in the study area (the target population for land holders)
fct_count(Preds_Responses$MosType)
write.csv(fct_count(Preds_Responses$MosType), file="output/compiled_survey_data/mosaic_freq_properties.csv", row.names = FALSE)

# group factors
# here we group Mosaic Types as follows based on what the most frequent types are in the response data and using sensible groupings
# each of these groups has at least 5 responses
# D13 - Retired, traditional couples living in coastal and scenic areas, with average pensionable income levels
# E16 - Working in trades, middle-aged families owning acreages of land with large properties just outside the metro fringe
# N48N49 - N48 (Rural farmers and farm owners with below average income, living 10-40km away from the nearest town) & N49 (Very rural farmers and farm owners with below average income, living 40km+ from the nearest town) grouped together
# N50N51 - N50 (Single farm workers in very small rural towns. with low income and low value properties) & N51 (Low education, traditional, singles in far inland remote towns, with low income and low value properties) grouped together
# OTHER - All other types (these are represented at low frequencies in the data)
# CHANGE HERE TO GROUP DIFFERENTLY
MOS$MosType <- fct_other(MOS$MosType, keep = c("D13", "E16", "N48", "N49", "N51", "N50"), other_level = "OTHER")
levels(MOS$MosType) <- c("D13", "E16", "N48N49","N48N49", "N50N51", "N50N51", "OTHER")
MOS$MosType <- relevel(MOS$MosType, "OTHER")

# land use
LU <- Data_Model_Fit %>% dplyr::select(LUSec) %>% droplevels()
# visualise the frequency of land-uses in the responses and target properties and save
fct_count(LU$LUSec)
write.csv(fct_count(LU$LUSec), file="output/compiled_survey_data/landuse_freq_responses.csv", row.names = FALSE)
# compare to the land-use frequencies in private properties in the study area (the target population for landholders)
fct_count(Preds_Responses$LUSec)
write.csv(fct_count(Preds_Responses$LUSec), file="output/compiled_survey_data/landuse_freq_properties.csv", row.names = FALSE)

# group factors
# here we take the five most common land-uses (with at least 5 responses) in the response data and group all other land-uses under OTHER
# CHANGE HERE TO GROUP DIFFERENTLY
LU$LUSec <- fct_other(LU$LUSec, keep = c("1.3.0 Other minimal use", "2.1.0 Grazing native vegetation", "3.2.0 Grazing modified pastures", "5.4.0 Residential and farm infrastructure", "3.3.0 Cropping"),
                                            other_level = "OTHER")
LU$LUSec <- relevel(LU$LUSec, "OTHER")

# property size
AREA <- Data_Model_Fit %>%
                  dplyr::select(Area)
AREA <- as.numeric(unlist(AREA))

# land value
LVAL <- Data_Model_Fit %>%
                  dplyr::select(LValHa)
LVAL <- as.numeric(unlist(LVAL))

# distance to major urban centres
DMU <- Data_Model_Fit %>%
                  dplyr::select(DistMU)
DMU <- as.numeric(unlist(DMU))

# distance to other urban centres
DOU <- Data_Model_Fit %>%
                  dplyr::select(DistOU)
DOU <- as.numeric(unlist(DOU))

# proportion of property that is native trees
PTREE <- Data_Model_Fit %>%
                  dplyr::select(PropNatTree)
PTREE <- as.numeric(unlist(PTREE))

# proportion of property that is native grass
PGRASS <- Data_Model_Fit %>%
                  dplyr::select(PropNatGrass)
PGRASS <- as.numeric(unlist(PGRASS))

# ecological condition
COND <- Data_Model_Fit %>%
                  dplyr::select(Condition)
COND <- as.numeric(unlist(COND))

# ecological connectivity
CONN <- Data_Model_Fit %>%
                  dplyr::select(Connectivity)
CONN <- as.numeric(unlist(CONN))

# elevation
ELEV <- Data_Model_Fit %>%
                  dplyr::select(Elevation)
ELEV <- as.numeric(unlist(ELEV))

# slope
SLOPE <- Data_Model_Fit %>%
                  dplyr::select(Slope)
SLOPE <- as.numeric(unlist(SLOPE))

# terrain ruggedness index
TRI <- Data_Model_Fit %>%
                  dplyr::select(TRIndex)
TRI <- as.numeric(unlist(TRI))

# soil capability
SCAP <- Data_Model_Fit %>%
                  dplyr::select(SoilCap)
SCAP <- ordered(as.factor(unlist(SCAP)), levels =(c("2", "3", "4", "5", "6", "7", "8")))

# SA1 ID
SA1ID <- Data_Model_Fit %>%
                  dplyr::select(SA1ID)
SA1ID <- as.numeric(unlist(SA1ID))

# KMR ID
KMRID <- Data_Model_Fit %>%
                  dplyr::select(KMRID)
KMRID <- as.numeric(unlist(KMRID))

# compile data at the SA1 level

# extract data we need for the models at the SA1 level
Data_Model_Fit_SA1 <- SA1_Var %>% dplyr::select(Dim.1, Dim.2, Dim.3, Dim.4, Dim.5)

# DIM 1
DIM1 <- Data_Model_Fit_SA1 %>%
                  dplyr::select(Dim.1)
DIM1 <- as.numeric(unlist(DIM1))

# DIM 2
DIM2 <- Data_Model_Fit_SA1 %>%
                  dplyr::select(Dim.2)
DIM2 <- as.numeric(unlist(DIM2))

# DIM 3
DIM3 <- Data_Model_Fit_SA1 %>%
                  dplyr::select(Dim.3)
DIM3 <- as.numeric(unlist(DIM3))

# DIM 4
DIM4 <- Data_Model_Fit_SA1 %>%
                  dplyr::select(Dim.4)
DIM4 <- as.numeric(unlist(DIM4))

# DIM 5
DIM5 <- Data_Model_Fit_SA1 %>%
                  dplyr::select(Dim.5)
DIM5 <- as.numeric(unlist(DIM5))

# compile predictor variables and impute missing values

IndPredVars <- tibble(KMR, MOS, LU, AREA, LVAL, DMU, DOU, PTREE, PGRASS, COND, CONN, ELEV, SLOPE, TRI, SCAP)
SA1PredVars <- tibble(DIM1, DIM2, DIM3, DIM4, DIM5)

m <- 10
Imp <- mice(IndPredVars, m = m)
IndPredVars.Imp <- list()
# loop through imputations
for (i in 1:m) {
  IndPredVars.Imp[[i]] <- as_tibble(complete(Imp, i)) %>% mutate(SCAP = as.numeric(SCAP))
}

# format required data for the JAGS model

# probability landholder would consider covenant model (X)
ACCEPT.Inf.X <- ACCEPT.Inf[which(!is.na(ACCEPT.Inf))]
ACCEPT.10yr.X <- ACCEPT.10yr[which(!is.na(ACCEPT.10yr))]
IndPreds.Inf.X <- list()
# loop through imputations
for (i in 1:m) {
  IndPreds.Inf.X[[i]] <- IndPredVars.Imp[[i]][which(!is.na(ACCEPT.Inf)),] %>% dplyr::select(-PGRASS) %>% mutate(AREA = scale(log(AREA)), LVAL = scale(log(LVAL)), DMU = scale(log(DMU + 1)),  DOU = scale(log(DOU + 1)), PTREE = scale(PTREE), COND = scale(COND), CONN = scale(CONN), ELEV = scale(log(ELEV)), SLOPE = scale(SLOPE), TRI = scale(log(TRI)), SCAP = scale(SCAP))
}
IndPreds.10yr.X <- list()
# loop through imputations
for (i in 1:m) {
  IndPreds.10yr.X[[i]] <- IndPredVars.Imp[[i]][which(!is.na(ACCEPT.10yr)),] %>% dplyr::select(-PGRASS) %>% mutate(AREA = scale(log(AREA)), LVAL = scale(log(LVAL)), DMU = scale(log(DMU + 1)),  DOU = scale(log(DOU + 1)), PTREE = scale(PTREE), COND = scale(COND), CONN = scale(CONN), ELEV = scale(log(ELEV)), SLOPE = scale(SLOPE), TRI = scale(log(TRI)), SCAP = scale(SCAP))
}
SA1ID.Inf.X <- (as_tibble(SA1ID[which(!is.na(ACCEPT.Inf))]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(SA1ID[which(!is.na(ACCEPT.Inf))]), NEWID = 1:length(unique(SA1ID[which(!is.na(ACCEPT.Inf))])))), by = c("value" = "OLDID")))$NEWID
SA1ID.10yr.X <- (as_tibble(SA1ID[which(!is.na(ACCEPT.10yr))]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(SA1ID[which(!is.na(ACCEPT.10yr))]), NEWID = 1:length(unique(SA1ID[which(!is.na(ACCEPT.10yr))])))), by = c("value" = "OLDID")))$NEWID
SA1Preds.Inf.X <- SA1PredVars[unique(SA1ID[which(!is.na(ACCEPT.Inf))]),]
SA1Preds.10yr.X <- SA1PredVars[unique(SA1ID[which(!is.na(ACCEPT.10yr))]),]
KMRID.Inf.X <- (as_tibble(KMRID[which(!is.na(ACCEPT.Inf))]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(KMRID[which(!is.na(ACCEPT.Inf))]), NEWID = 1:length(unique(KMRID[which(!is.na(ACCEPT.Inf))])))), by = c("value" = "OLDID")))$NEWID
KMRID.10yr.X <- (as_tibble(KMRID[which(!is.na(ACCEPT.10yr))]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(KMRID[which(!is.na(ACCEPT.10yr))]), NEWID = 1:length(unique(KMRID[which(!is.na(ACCEPT.10yr))])))), by = c("value" = "OLDID")))$NEWID

# willingness to accept bid model (Y)
CENS.Inf.Y <- CENS.Inf[which(ACCEPT.Inf == 1)]
CENS.10yr.Y <- CENS.10yr[which(ACCEPT.10yr == 1)]
WTA.Inf.Y <- WTA.Inf[which(ACCEPT.Inf == 1)]
WTA.10yr.Y <- WTA.10yr[which(ACCEPT.10yr == 1)]
IndPreds.Inf.Y <- list()
# loop through imputations
for (i in 1:m) {
  IndPreds.Inf.Y[[i]] <- IndPredVars.Imp[[i]][which(ACCEPT.Inf == 1),] %>% dplyr::select(-PGRASS) %>% mutate(AREA = scale(log(AREA)), LVAL = scale(log(LVAL)), DMU = scale(log(DMU + 1)),  DOU = scale(log(DOU + 1)), PTREE = scale(PTREE), COND = scale(COND), CONN = scale(CONN), ELEV = scale(log(ELEV)), SLOPE = scale(SLOPE), TRI = scale(log(TRI)), SCAP = scale(SCAP))
}
IndPreds.10yr.Y <- list()
# loop through imputations
for (i in 1:m) {
  IndPreds.10yr.Y[[i]] <- IndPredVars.Imp[[i]][which(ACCEPT.10yr == 1),] %>% dplyr::select(-PGRASS) %>% mutate(AREA = scale(log(AREA)), LVAL = scale(log(LVAL)), DMU = scale(log(DMU + 1)),  DOU = scale(log(DOU + 1)), PTREE = scale(PTREE), COND = scale(COND), CONN = scale(CONN), ELEV = scale(log(ELEV)), SLOPE = scale(SLOPE), TRI = scale(log(TRI)), SCAP = scale(SCAP))
}
SA1ID.Inf.Y <- (as_tibble(SA1ID[which(ACCEPT.Inf == 1)]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(SA1ID[which(ACCEPT.Inf == 1)]), NEWID = 1:length(unique(SA1ID[which(ACCEPT.Inf == 1)])))), by = c("value" = "OLDID")))$NEWID
SA1ID.10yr.Y <- (as_tibble(SA1ID[which(ACCEPT.10yr == 1)]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(SA1ID[which(ACCEPT.10yr == 1)]), NEWID = 1:length(unique(SA1ID[which(ACCEPT.10yr == 1)])))), by = c("value" = "OLDID")))$NEWID
SA1Preds.Inf.Y <- SA1PredVars[unique(SA1ID[which(ACCEPT.Inf == 1)]),]
SA1Preds.10yr.Y <- SA1PredVars[unique(SA1ID[which(ACCEPT.10yr == 1)]),]
KMRID.Inf.Y <- (as_tibble(KMRID[which(ACCEPT.Inf == 1)]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(KMRID[which(ACCEPT.Inf == 1)]), NEWID = 1:length(unique(KMRID[which(ACCEPT.Inf == 1)])))), by = c("value" = "OLDID")))$NEWID
KMRID.10yr.Y <- (as_tibble(KMRID[which(ACCEPT.10yr == 1)]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(KMRID[which(ACCEPT.10yr == 1)]), NEWID = 1:length(unique(KMRID[which(ACCEPT.10yr == 1)])))), by = c("value" = "OLDID")))$NEWID

# proportion of property would covenant model (Z)

# transform proportions using (y * (n−1) + 0.5) / n where n is the sample size so as to deal with 0 and 1 values - the assumption being that these are not actually 0 or 1 but close to 0 or 1
# see Smithson M, Verkuilen J (2006). "A Better Lemon Squeezer? Maximum-Likelihood Regression with Beta-Distributed Dependent Variables." Psychological Methods, 11 (1), 54–71
PROP.Inf.Z <- (PROP.Inf[which(ACCEPT.Inf == 1)] * (length(PROP.Inf[which(ACCEPT.Inf == 1)]) - 1) + 0.5) / length(PROP.Inf[which(ACCEPT.Inf == 1)])
PROP.10yr.Z <- (PROP.10yr[which(ACCEPT.10yr == 1)] * (length(PROP.10yr[which(ACCEPT.10yr == 1)]) - 1) + 0.5) / length(PROP.10yr[which(ACCEPT.10yr == 1)])
IndPreds.Inf.Z <- list()
# loop through imputations
for (i in 1:m) {
  IndPreds.Inf.Z[[i]] <- IndPredVars.Imp[[i]][which(ACCEPT.Inf == 1),] %>% dplyr::select(-PGRASS) %>% mutate(AREA = scale(log(AREA)), LVAL = scale(log(LVAL)), DMU = scale(log(DMU + 1)),  DOU = scale(log(DOU + 1)), PTREE = scale(PTREE), COND = scale(COND), CONN = scale(CONN), ELEV = scale(log(ELEV)), SLOPE = scale(SLOPE), TRI = scale(log(TRI)), SCAP = scale(SCAP))
}
IndPreds.10yr.Z <- list()
# loop through imputations
for (i in 1:m) {
  IndPreds.10yr.Z[[i]] <- IndPredVars.Imp[[i]][which(ACCEPT.10yr == 1),] %>% dplyr::select(-PGRASS) %>% mutate(AREA = scale(log(AREA)), LVAL = scale(log(LVAL)), DMU = scale(log(DMU + 1)),  DOU = scale(log(DOU + 1)), PTREE = scale(PTREE), COND = scale(COND), CONN = scale(CONN), ELEV = scale(log(ELEV)), SLOPE = scale(SLOPE), TRI = scale(log(TRI)), SCAP = scale(SCAP))
}
SA1ID.Inf.Z <- (as_tibble(SA1ID[which(ACCEPT.Inf == 1)]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(SA1ID[which(ACCEPT.Inf == 1)]), NEWID = 1:length(unique(SA1ID[which(ACCEPT.Inf == 1)])))), by = c("value" = "OLDID")))$NEWID
SA1ID.10yr.Z <- (as_tibble(SA1ID[which(ACCEPT.10yr == 1)]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(SA1ID[which(ACCEPT.10yr == 1)]), NEWID = 1:length(unique(SA1ID[which(ACCEPT.10yr == 1)])))), by = c("value" = "OLDID")))$NEWID
SA1Preds.Inf.Z <- SA1PredVars[unique(SA1ID[which(ACCEPT.Inf == 1)]),]
SA1Preds.10yr.Z <- SA1PredVars[unique(SA1ID[which(ACCEPT.10yr == 1)]),]
KMRID.Inf.Z <- (as_tibble(KMRID[which(ACCEPT.Inf == 1)]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(KMRID[which(ACCEPT.Inf == 1)]), NEWID = 1:length(unique(KMRID[which(ACCEPT.Inf == 1)])))), by = c("value" = "OLDID")))$NEWID
KMRID.10yr.Z <- (as_tibble(KMRID[which(ACCEPT.10yr == 1)]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(KMRID[which(ACCEPT.10yr == 1)]), NEWID = 1:length(unique(KMRID[which(ACCEPT.10yr == 1)])))), by = c("value" = "OLDID")))$NEWID

# Check for collinearity

TestCorData <- IndPredVars %>% mutate(AREA = scale(log(AREA)), LVAL = scale(log(LVAL)), DMU = scale(log(DMU + 1)),  DOU = scale(log(DOU + 1)), PTREE = scale(PTREE), COND = scale(COND), CONN = scale(CONN), ELEV = scale(log(ELEV)),
                  SLOPE = scale(SLOPE), TRI = scale(log(TRI)), SCAP = scale(as.numeric(SCAP)))

# check collinearity among continuous variables - considering removing one of the variables for pairs of variables with correlation coefficients > 0.6
# outcomes: removed AREA kept LVAL, removed DMU kept LVAL, removed PTREE kept COND, removed CONN kept COND, removed TRI kelt SLOPE
# did not test against census PCs as these operate at a different resolution to the property and so wanted to keep those
Corr_Cont <- cor(TestCorData %>% dplyr::select(-KMR, -MosType, -LUSec), use = "complete.obs")
write.csv(Corr_Cont, file="output/collinearity/cor_cont.csv")

# check collinearity among categorical variables
# all are significantly correlated but have left in for now given each categorical variable represents a different characteristic
# KMR represents region, LUSec represents how land holders use the land, and MOSType represents landholder demographics
# KMR vs MOSType
Cases1 <- TestCorData %>% dplyr::select(KMR, MosType)
CTable1 <- table(Cases1$KMR, Cases1$MosType)
ChiTest1 <- chisq.test(CTable1)
# KMR vs LUSec
Cases2 <- TestCorData %>% dplyr::select(KMR, LUSec)
CTable2 <- table(Cases2$KMR, Cases2$LUSec)
ChiTest2 <- chisq.test(CTable2)
# MOSType vs LUSec
Cases3 <- TestCorData %>% dplyr::select(MosType, LUSec)
CTable3 <- table(Cases3$MosType, Cases2$LUSec)
ChiTest3 <- chisq.test(CTable3)

# compile predictors for in-perpetuity covenant model

# predictors - x
XSA1.Inf <- model.matrix(~ DIM1 + DIM2 + DIM3 + DIM4 + DIM5, SA1Preds.Inf.X)
# put categorical variables for the property level at the start
XInd.Inf <- list()
for (i in 1:m) {
  XInd.Inf[[i]] <- model.matrix(~ KMR + MosType + LUSec + LVAL + DOU + COND + ELEV + SLOPE + SCAP, IndPreds.Inf.X[[i]])
}

# predictors - y
YSA1.Inf <- model.matrix(~ DIM1 + DIM2 + DIM3 + DIM4 + DIM5, SA1Preds.Inf.Y)
# put categorical variables for the property level at the start
YInd.Inf <- list()
for (i in 1:m) {
  YInd.Inf[[i]] <- model.matrix(~ KMR + MosType + LUSec + LVAL + DOU + COND + ELEV + SLOPE + SCAP, IndPreds.Inf.Y[[i]])
}

# predictors - z
ZSA1.Inf <- model.matrix(~ DIM1 + DIM2 + DIM3 + DIM4 + DIM5, SA1Preds.Inf.Z)
# put categorical variables for the property level at the start
ZInd.Inf <- list()
for (i in 1:m) {
  ZInd.Inf[[i]] <- model.matrix(~ KMR + MosType + LUSec + LVAL + DOU + COND + ELEV + SLOPE + SCAP, IndPreds.Inf.Z[[i]])
}

# compile predictors for 10 year covenant model

# predictors - x
XSA1.10yr <- model.matrix(~ DIM1 + DIM2 + DIM3 + DIM4 + DIM5, SA1Preds.10yr.X)
# put categorical variables for the property level at the start
XInd.10yr <- list()
for (i in 1:m) {
  XInd.10yr[[i]] <- model.matrix(~ KMR + MosType + LUSec + LVAL + DOU + COND + ELEV + SLOPE + SCAP, IndPreds.10yr.X[[i]])
}

# predictors - y
YSA1.10yr <- model.matrix(~ DIM1 + DIM2 + DIM3 + DIM4 + DIM5, SA1Preds.10yr.Y)
# put categorical variables for the property level at the start
YInd.10yr <- list()
for (i in 1:m) {
  YInd.10yr[[i]] <- model.matrix(~ KMR + MosType + LUSec + LVAL + DOU + COND + ELEV + SLOPE + SCAP, IndPreds.10yr.Y[[i]])
}

# predictors - z
ZSA1.10yr <- model.matrix(~ DIM1 + DIM2 + DIM3 + DIM4 + DIM5, SA1Preds.10yr.Z)
# put categorical variables for the property level at the start
ZInd.10yr <- list()
for (i in 1:m) {
  ZInd.10yr[[i]] <- model.matrix(~ KMR + MosType + LUSec + LVAL + DOU + COND + ELEV + SLOPE + SCAP, IndPreds.10yr.Z[[i]])
}

# FITTING MODELS WITH VARIABLE SELECTION

# set up JAGS data for in-perpetuity covenant

# NOTE THAT THIS SECTION OF CODE ONLY WORKS IF THE CATEGORICAL VARIALBES ARE LISTED BEFORE THE CONTINUOUS VARIABLES
# CATEGORICAL VARIABLES ALSO NEED TO BE LISTED BELOW IN SAME ORDER AS THEY ARE LISTED ABOVE
# NEED TO EDIT HERE IF CATEGORICAL VARIABLES ARE CHANGED

# number of categorical variables - note that we assume that the predictor variables for each model are identical
NCAT <- 3

# get the total number of levels for the categorical variables (including the reference category)
NCOVCAT <- length(levels(IndPreds.Inf.X[[1]]$KMR)) - 1 + length(levels(IndPreds.Inf.X[[1]]$MosType)) - 1 + length(levels(IndPreds.Inf.X[[1]]$LUSec)) - 1

# get vectors of the variable indices for each categorical variable
CATIND = c(rep(1, length(levels(IndPreds.Inf.X[[1]]$KMR)) - 1), rep(2, length(levels(IndPreds.Inf.X[[1]]$MosType)) - 1), rep(3, length(levels(IndPreds.Inf.X[[1]]$LUSec)) - 1))

# get index of the location where each categorical variable starts
CATFROM = c(1, 1 + length(levels(IndPreds.Inf.X[[1]]$KMR)) - 1, 1 + length(levels(IndPreds.Inf.X[[1]]$MosType)) - 1 + length(levels(IndPreds.Inf.X[[1]]$LUSec)) - 1)

# get index of the location where each categorical variable ends
CATTO = c(length(levels(IndPreds.Inf.X[[1]]$KMR)) - 1, length(levels(IndPreds.Inf.X[[1]]$KMR)) - 1 + length(levels(IndPreds.Inf.X[[1]]$MosType)) - 1, length(levels(IndPreds.Inf.X[[1]]$KMR)) - 1 + length(levels(IndPreds.Inf.X[[1]]$MosType)) - 1 + length(levels(IndPreds.Inf.X[[1]]$LUSec)) - 1)

# set up JAGS data for in-perpetuity covenant
data.Inf.Sel <- list()
for(i in 1:m) {
  data.Inf.Sel[[i]] <- list(N = length(ACCEPT.Inf.X), M = length(WTA.Inf.Y), O = length(PROP.Inf.Z), NSA = nrow(XSA1.Inf), MSA = nrow(YSA1.Inf), OSA = nrow(ZSA1.Inf),
                   ACCEPT = ACCEPT.Inf.X, WTA = WTA.Inf.Y, CENS = CENS.Inf.Y, PROP = PROP.Inf.Z, LIM = c(0, 2.5), X = XInd.Inf[[i]][, 2:ncol(XInd.Inf[[i]])], XSA = XSA1.Inf, Y = YInd.Inf[[i]][, 2:ncol(YInd.Inf[[i]])],
                   YSA = YSA1.Inf, Z = ZInd.Inf[[i]][, 2:ncol(ZInd.Inf[[i]])], ZSA = ZSA1.Inf, SAIDx = SA1ID.Inf.X, SAIDy = SA1ID.Inf.Y, SAIDz = SA1ID.Inf.Z, NCAT = NCAT, NCOVCAT = NCOVCAT,
                   NCOV = ncol(XInd.Inf[[i]][, 2:ncol(XInd.Inf[[i]])]), NCOVSA = ncol(XSA1.Inf), CATIND = CATIND, CATFROM = CATFROM, CATTO = CATTO)
}

# set up JAGS data for 10 year covenant
data.10yr.Sel <- list()
for(i in 1:m) {
  data.10yr.Sel[[i]] <- list(N = length(ACCEPT.10yr.X), M = length(WTA.10yr.Y), O = length(PROP.10yr.Z), NSA = nrow(XSA1.10yr), MSA = nrow(YSA1.10yr), OSA = nrow(ZSA1.10yr),
                   ACCEPT = ACCEPT.10yr.X, WTA = WTA.10yr.Y, CENS = CENS.10yr.Y, PROP = PROP.10yr.Z, LIM = c(0, 2.5), X = XInd.10yr[[i]][, 2:ncol(XInd.10yr[[i]])], XSA = XSA1.10yr, Y = YInd.10yr[[i]][, 2:ncol(YInd.10yr[[i]])],
                   YSA = YSA1.10yr, Z = ZInd.10yr[[i]][, 2:ncol(ZInd.10yr[[i]])], ZSA = ZSA1.10yr, SAIDx = SA1ID.10yr.X, SAIDy = SA1ID.10yr.Y, SAIDz = SA1ID.10yr.Z, NCAT = NCAT, NCOVCAT = NCOVCAT,
                   NCOV = ncol(XInd.10yr[[i]][, 2:ncol(XInd.10yr[[i]])]), NCOVSA = ncol(XSA1.10yr), CATIND = CATIND, CATFROM = CATFROM, CATTO = CATTO)
}

# run JAGS models

# load functions
source("functions.r")

# In-perpetuity covenant model

# parallel processing - comment out if not using parallel processing

# initialise cluster
sfInit( parallel = TRUE, cpus = 2)

# export data, functions and libraries to workers
sfExportAll()
sfClusterEval(library(runjags))
sfClusterEval(library(coda))
sfClusterEval(library(rjags))
sfClusterEval(library(parallel))
sfClusterEval(library(rjags))
sfClusterEval(library(modeest))

# run JAGS model
Jags.Fits.Sel.Inf <- sfLapply(data.Inf.Sel, get.jags.sel)

# stop cluster
sfStop()

# non-parallel processing - comment out if using parallel processing
#Jags.Fits.Sel.Inf <- get.jags.sel(data.Inf.Sel[[1]])

# export models fits to the output folder
saveRDS(Jags.Fits.Sel.Inf, file = "output/jags/Jags_Fits_Sel_Inf.rds")

# 10 year covenant model

# parallel processing - comment out if not using parallel processing

# initialise cluster
sfInit( parallel = TRUE, cpus = 2)

# export data, functions and libraries to workers
sfExportAll()
sfClusterEval(library(runjags))
sfClusterEval(library(coda))
sfClusterEval(library(rjags))
sfClusterEval(library(parallel))
sfClusterEval(library(rjags))
sfClusterEval(library(modeest))

# run JAGS model
Jags.Fits.Sel.10yr <- sfLapply(data.10yr.Sel, get.jags.sel)

# stop cluster
sfStop()

# non-parallel processing - comment out if using parallel processing
#Jags.Fits.Sel.10yr <- get.jags.sel(data.10yr.Sel[[1]])

# export models fits to the output folder
saveRDS(Jags.Fits.Sel.10yr, file = "output/jags/Jags_Fits_Sel.10yr.rds")

# PREDICTIONS

# load functions
source("functions.r")

# load models if needed
#Jags.Fits.Sel.Inf <- readRDS("output/jags/Jags_Fits_Sel_Inf.rds")
#Jags.Fits.Sel.10yr <- readRDS("output/jags/Jags_Fits_Sel_10yr.rds")

# get the properties data to make predictions and remove the following land uses:
# 1.1.0 Nature conservation
# 1.2.0 Managed resource protection
# 5.3.0 Manufacturing and industrial (5.3.0 - Based on [LU_Sec])
# 5.5.0 Services (5.5.0 - Based on [LU_Sec])
# 5.6.0 Utilities (5.6.0 - Based on [LU_Sec])
# 5.7.0 Transport and communication (5.7.0 - Based on [LU_Sec])
# 5.8.0 Mining (5.8.0 - Based on [LU_Sec])
# 5.9.0 Waste treatment and disposal (5.9.0 - Based on [LU_Sec]).
# 6.1.0 Lake (6.1.0 - Based on [LU_Sec])
# 6.2.0 Reservoir/dam (6.2.0 - Based on [LU_Sec])
# 6.3.0 River (6.3.0 - Based on [LU_Sec])
# 6.4.0 Channel/aqueduct (6.4.0 - Based on [LU_Sec])
# 6.6.0 Estuary/coastal waters (6.6.0 - Based on [LU_Sec]).
Preds_Properties_New <- Preds_Properties %>% filter(!grepl('1.1.0|1.2.0|5.3.0|5.5.0|5.6.0|5.7.0|5.8.0|5.9.0|6.1.0|6.2.0|6.3.0|6.4.0|6.6.0', LUSec))

# select required fields
Preds_Properties_New <- Preds_Properties_New %>% dplyr::select(NewPropID = NewPropID, KMR = KMR, MosType = MosType, LUSec = LUSec, LVAL = LValHa, AREA = Area, DMU = DistMU, DOU = DistOU, PTREE = PropNatTree, PGRASS = PropNatGrass,
                  COND = Condition, CONN = Connectivity, ELEV = Elevation, SLOPE = Slope, TRI = TRIndex, SCAP = SoilCap, SA1_7DIG16)

# get census data and join
#Census_Char <- Census %>% mutate(SA1_7DIGITCODE_2016 = as.character(SA1_7DIGITCODE_2016))
Preds_Properties_New_Join <- Preds_Properties_New %>% left_join(Census, by = c("SA1_7DIG16" = "SA1_7DIGITCODE_2016"))
PredPCAVars <- Preds_Properties_New_Join %>% dplyr::select(Age, PYear12Ed, PBachEd, PBirthAus, PEngLang, PBornOS, HSize, PHComp1, PHComp2, PHComp3, PHComp4, HInc, MortPay, PAgEmploy)
PredPCA <- as_tibble(predict(SA1_PCA, newdata = PredPCAVars)) %>% dplyr::select(DIM1 = PC1, DIM2 = PC2, DIM3 = PC3, DIM4 = PC4, DIM5 = PC5)
Preds_Properties_New <- bind_cols(Preds_Properties_New, PredPCA) %>% dplyr::select(-SA1_7DIG16)

# make sure data types are correct and relevel factors
Preds_Properties_New <- Preds_Properties_New %>% mutate(KMR = as_factor(KMR), MosType = as_factor(MosType), LUSec = as_factor(LUSec)) %>% mutate(KMR = relevel(KMR, "Central Coast"))
Preds_Properties_New$MosType <- fct_other(Preds_Properties_New$MosType, keep = c("D13", "E16", "N48", "N49", "N51", "N50"), other_level = "OTHER")
levels(Preds_Properties_New$MosType) <- c("D13", "E16", "N48N49","N48N49", "N50N51", "N50N51", "OTHER")
Preds_Properties_New$MosType <- relevel(Preds_Properties_New$MosType, "OTHER")
Preds_Properties_New$LUSec <- fct_other(Preds_Properties_New$LUSec, keep = c("1.3.0 Other minimal use", "2.1.0 Grazing native vegetation", "3.2.0 Grazing modified pastures", "5.4.0 Residential and farm infrastructure", "3.3.0 Cropping"), other_level = "OTHER")
Preds_Properties_New$LUSec <- relevel(Preds_Properties_New$LUSec, "OTHER")

# remove any properties where we have NAs
Preds_Properties_New <- na.omit(Preds_Properties_New)

# make sure elevations are greater than zero so we can log them - set anything <= 0 to 0.01 m as an approximation (note that this is only affects a small proportion of the properties)
Preds_Properties_New$ELEV[which(Preds_Properties_New$ELEV <= 0)] <- 0.01

# set the number of samples to draw from the full MCMC chains for each imputed data set
NumSamples <- 10000

# set up lists to store data and coefficients for predictions

CoefsX.Inf <- list()
CoefsY.Inf <- list()
CoefsZ.Inf <- list()
Model_MatrixX.Inf <- list()
Model_MatrixY.Inf <- list()
Model_MatrixZ.Inf <- list()
CoefsX.10yr <- list()
CoefsY.10yr <- list()
CoefsZ.10yr <- list()
Model_MatrixX.10yr <- list()
Model_MatrixY.10yr <- list()
Model_MatrixZ.10yr <- list()

# loop through each data imputation replicate and make predictions
  for (i in 1:m) {

    # In-perpetuity model

    # get expected coefficient values
    SummaryVals <- summary(Jags.Fits.Sel.Inf[[i]])
    CoefIDsX <- c(which(dimnames(SummaryVals)[[1]] == "betasa_x[1]"), which(dimnames(SummaryVals)[[1]] == "beta_x[1]"):which(dimnames(SummaryVals)[[1]] == "beta_x[20]"),
                    which(dimnames(SummaryVals)[[1]] == "betasa_x[2]"):which(dimnames(SummaryVals)[[1]] == "betasa_x[6]"))
    CoefIDsY <- c(which(dimnames(SummaryVals)[[1]] == "betasa_y[1]"), which(dimnames(SummaryVals)[[1]] == "beta_y[1]"):which(dimnames(SummaryVals)[[1]] == "beta_y[20]"),
                    which(dimnames(SummaryVals)[[1]] == "betasa_y[2]"):which(dimnames(SummaryVals)[[1]] == "betasa_y[6]"))
    CoefIDsZ <- c(which(dimnames(SummaryVals)[[1]] == "betasa_z[1]"), which(dimnames(SummaryVals)[[1]] == "beta_z[1]"):which(dimnames(SummaryVals)[[1]] == "beta_z[20]"),
                    which(dimnames(SummaryVals)[[1]] == "betasa_z[2]"):which(dimnames(SummaryVals)[[1]] == "betasa_z[6]"))
    CoefsExpX <- SummaryVals[CoefIDsX, "Mean"]
    CoefsExpY <- SummaryVals[CoefIDsY, "Mean"]
    CoefsExpZ <- SummaryVals[CoefIDsZ, "Mean"]

    # get coefficient values for individual MCMC draws
    MCMC_Draws <- as_tibble(as.mcmc(Jags.Fits.Sel.Inf[[i]]))
    CoefsX.Inf[[i]] <- MCMC_Draws[sample(nrow(MCMC_Draws), NumSamples), CoefIDsX]
    CoefsY.Inf[[i]] <- MCMC_Draws[sample(nrow(MCMC_Draws), NumSamples), CoefIDsY]
    CoefsZ.Inf[[i]] <- MCMC_Draws[sample(nrow(MCMC_Draws), NumSamples), CoefIDsZ]

    # compile data for predictions
    Predictions_DataX <- Preds_Properties_New %>% dplyr::select(-NewPropID, - PGRASS) %>% mutate(
                          AREA = as.vector(scale(log(AREA), center = attr(IndPreds.Inf.X[[i]]$AREA,"scaled:center"), scale = attr(IndPreds.Inf.X[[i]]$AREA,"scaled:scale"))),
                          LVAL = as.vector(scale(log(LVAL), center = attr(IndPreds.Inf.X[[i]]$LVAL,"scaled:center"), scale = attr(IndPreds.Inf.X[[i]]$LVAL,"scaled:scale"))),
                          DMU = as.vector(scale(log(DMU + 1), center = attr(IndPreds.Inf.X[[i]]$DMU,"scaled:center"), scale = attr(IndPreds.Inf.X[[i]]$DMU,"scaled:scale"))),
                          DOU = as.vector(scale(log(DOU + 1), center = attr(IndPreds.Inf.X[[i]]$DOU,"scaled:center"), scale = attr(IndPreds.Inf.X[[i]]$DOU,"scaled:scale"))),
                          PTREE = as.vector(scale(PTREE, center = attr(IndPreds.Inf.X[[i]]$PTREE,"scaled:center"), scale = attr(IndPreds.Inf.X[[i]]$PTREE,"scaled:scale"))),
                          COND = as.vector(scale(COND, center = attr(IndPreds.Inf.X[[i]]$COND,"scaled:center"), scale = attr(IndPreds.Inf.X[[i]]$COND,"scaled:scale"))),
                          CONN = as.vector(scale(CONN, center = attr(IndPreds.Inf.X[[i]]$CONN,"scaled:center"), scale = attr(IndPreds.Inf.X[[i]]$CONN,"scaled:scale"))),
                          ELEV = as.vector(scale(log(ELEV), center = attr(IndPreds.Inf.X[[i]]$ELEV,"scaled:center"), scale = attr(IndPreds.Inf.X[[i]]$ELEV,"scaled:scale"))),
                          SLOPE = as.vector(scale(SLOPE, center = attr(IndPreds.Inf.X[[i]]$SLOPE,"scaled:center"), scale = attr(IndPreds.Inf.X[[i]]$SLOPE,"scaled:scale"))),
                          TRI = as.vector(scale(log(TRI), center = attr(IndPreds.Inf.X[[i]]$TRI,"scaled:center"), scale = attr(IndPreds.Inf.X[[i]]$TRI,"scaled:scale"))),
                          SCAP = as.vector(scale(SCAP, center = attr(IndPreds.Inf.X[[i]]$SCAP,"scaled:center"), scale = attr(IndPreds.Inf.X[[i]]$SCAP,"scaled:scale"))))
    Predictions_DataY <- Preds_Properties_New %>% dplyr::select(-NewPropID, - PGRASS) %>% mutate(
                          AREA = as.vector(scale(log(AREA), center = attr(IndPreds.Inf.Y[[i]]$AREA,"scaled:center"), scale = attr(IndPreds.Inf.Y[[i]]$AREA,"scaled:scale"))),
                          LVAL = as.vector(scale(log(LVAL), center = attr(IndPreds.Inf.Y[[i]]$LVAL,"scaled:center"), scale = attr(IndPreds.Inf.Y[[i]]$LVAL,"scaled:scale"))),
                          DMU = as.vector(scale(log(DMU + 1), center = attr(IndPreds.Inf.Y[[i]]$DMU,"scaled:center"), scale = attr(IndPreds.Inf.Y[[i]]$DMU,"scaled:scale"))),
                          DOU = as.vector(scale(log(DOU + 1), center = attr(IndPreds.Inf.Y[[i]]$DOU,"scaled:center"), scale = attr(IndPreds.Inf.Y[[i]]$DOU,"scaled:scale"))),
                          PTREE = as.vector(scale(PTREE, center = attr(IndPreds.Inf.Y[[i]]$PTREE,"scaled:center"), scale = attr(IndPreds.Inf.Y[[i]]$PTREE,"scaled:scale"))),
                          COND = as.vector(scale(COND, center = attr(IndPreds.Inf.Y[[i]]$COND,"scaled:center"), scale = attr(IndPreds.Inf.Y[[i]]$COND,"scaled:scale"))),
                          CONN = as.vector(scale(CONN, center = attr(IndPreds.Inf.Y[[i]]$CONN,"scaled:center"), scale = attr(IndPreds.Inf.Y[[i]]$CONN,"scaled:scale"))),
                          ELEV = as.vector(scale(log(ELEV), center = attr(IndPreds.Inf.Y[[i]]$ELEV,"scaled:center"), scale = attr(IndPreds.Inf.Y[[i]]$ELEV,"scaled:scale"))),
                          SLOPE = as.vector(scale(SLOPE, center = attr(IndPreds.Inf.Y[[i]]$SLOPE,"scaled:center"), scale = attr(IndPreds.Inf.Y[[i]]$SLOPE,"scaled:scale"))),
                          TRI = as.vector(scale(log(TRI), center = attr(IndPreds.Inf.Y[[i]]$TRI,"scaled:center"), scale = attr(IndPreds.Inf.Y[[i]]$TRI,"scaled:scale"))),
                          SCAP = as.vector(scale(SCAP, center = attr(IndPreds.Inf.Y[[i]]$SCAP,"scaled:center"), scale = attr(IndPreds.Inf.Y[[i]]$SCAP,"scaled:scale"))))
    Predictions_DataZ <- Preds_Properties_New %>% dplyr::select(-NewPropID, - PGRASS) %>% mutate(
                          AREA = as.vector(scale(log(AREA), center = attr(IndPreds.Inf.Z[[i]]$AREA,"scaled:center"), scale = attr(IndPreds.Inf.Z[[i]]$AREA,"scaled:scale"))),
                          LVAL = as.vector(scale(log(LVAL), center = attr(IndPreds.Inf.Z[[i]]$LVAL,"scaled:center"), scale = attr(IndPreds.Inf.Z[[i]]$LVAL,"scaled:scale"))),
                          DMU = as.vector(scale(log(DMU + 1), center = attr(IndPreds.Inf.Z[[i]]$DMU,"scaled:center"), scale = attr(IndPreds.Inf.Z[[i]]$DMU,"scaled:scale"))),
                          DOU = as.vector(scale(log(DOU + 1), center = attr(IndPreds.Inf.Z[[i]]$DOU,"scaled:center"), scale = attr(IndPreds.Inf.Z[[i]]$DOU,"scaled:scale"))),
                          PTREE = as.vector(scale(PTREE, center = attr(IndPreds.Inf.Z[[i]]$PTREE,"scaled:center"), scale = attr(IndPreds.Inf.Z[[i]]$PTREE,"scaled:scale"))),
                          COND = as.vector(scale(COND, center = attr(IndPreds.Inf.Z[[i]]$COND,"scaled:center"), scale = attr(IndPreds.Inf.Z[[i]]$COND,"scaled:scale"))),
                          CONN = as.vector(scale(CONN, center = attr(IndPreds.Inf.Z[[i]]$CONN,"scaled:center"), scale = attr(IndPreds.Inf.Z[[i]]$CONN,"scaled:scale"))),
                          ELEV = as.vector(scale(log(ELEV), center = attr(IndPreds.Inf.Z[[i]]$ELEV,"scaled:center"), scale = attr(IndPreds.Inf.Z[[i]]$ELEV,"scaled:scale"))),
                          SLOPE = as.vector(scale(SLOPE, center = attr(IndPreds.Inf.Z[[i]]$SLOPE,"scaled:center"), scale = attr(IndPreds.Inf.Z[[i]]$SLOPE,"scaled:scale"))),
                          TRI = as.vector(scale(log(TRI), center = attr(IndPreds.Inf.Z[[i]]$TRI,"scaled:center"), scale = attr(IndPreds.Inf.Z[[i]]$TRI,"scaled:scale"))),
                          SCAP = as.vector(scale(SCAP, center = attr(IndPreds.Inf.Z[[i]]$SCAP,"scaled:center"), scale = attr(IndPreds.Inf.Z[[i]]$SCAP,"scaled:scale"))))

    # get model matrices

    # adoption
    Model_MatrixX.Inf[[i]] <- model.matrix(~ KMR + MosType + LUSec + LVAL + DOU + COND + ELEV + SLOPE + SCAP + DIM1 + DIM2 + DIM3 + DIM4 + DIM5, data = Predictions_DataX)
    # wta
    Model_MatrixY.Inf[[i]] <- model.matrix(~ KMR + MosType + LUSec + LVAL + DOU + COND + ELEV + SLOPE + SCAP + DIM1 + DIM2 + DIM3 + DIM4 + DIM5, data = Predictions_DataY)
    # proportion
    Model_MatrixZ.Inf[[i]] <- model.matrix(~ KMR + MosType + LUSec + LVAL + DOU + COND + ELEV + SLOPE + SCAP + DIM1 + DIM2 + DIM3 + DIM4 + DIM5, data = Predictions_DataZ)

    # 10 year model

    # get expected coefficient values
    SummaryVals <- summary(Jags.Fits.Sel.10yr[[i]])
    CoefIDsX <- c(which(dimnames(SummaryVals)[[1]] == "betasa_x[1]"), which(dimnames(SummaryVals)[[1]] == "beta_x[1]"):which(dimnames(SummaryVals)[[1]] == "beta_x[20]"),
                    which(dimnames(SummaryVals)[[1]] == "betasa_x[2]"):which(dimnames(SummaryVals)[[1]] == "betasa_x[6]"))
    CoefIDsY <- c(which(dimnames(SummaryVals)[[1]] == "betasa_y[1]"), which(dimnames(SummaryVals)[[1]] == "beta_y[1]"):which(dimnames(SummaryVals)[[1]] == "beta_y[20]"),
                    which(dimnames(SummaryVals)[[1]] == "betasa_y[2]"):which(dimnames(SummaryVals)[[1]] == "betasa_y[6]"))
    CoefIDsZ <- c(which(dimnames(SummaryVals)[[1]] == "betasa_z[1]"), which(dimnames(SummaryVals)[[1]] == "beta_z[1]"):which(dimnames(SummaryVals)[[1]] == "beta_z[20]"),
                    which(dimnames(SummaryVals)[[1]] == "betasa_z[2]"):which(dimnames(SummaryVals)[[1]] == "betasa_z[6]"))
    CoefsExpX <- SummaryVals[CoefIDsX, "Mean"]
    CoefsExpY <- SummaryVals[CoefIDsY, "Mean"]
    CoefsExpZ <- SummaryVals[CoefIDsZ, "Mean"]

    # get coefficient values for individual MCMC draws
    MCMC_Draws <- as_tibble(as.mcmc(Jags.Fits.Sel.10yr[[i]]))
    CoefsX.10yr[[i]] <- MCMC_Draws[sample(nrow(MCMC_Draws), NumSamples), CoefIDsX]
    CoefsY.10yr[[i]] <- MCMC_Draws[sample(nrow(MCMC_Draws), NumSamples), CoefIDsY]
    CoefsZ.10yr[[i]] <- MCMC_Draws[sample(nrow(MCMC_Draws), NumSamples), CoefIDsZ]

    # compile data for predictions
    Predictions_DataX <- Preds_Properties_New %>% dplyr::select(-NewPropID, - PGRASS) %>% mutate(
                          AREA = as.vector(scale(log(AREA), center = attr(IndPreds.10yr.X[[i]]$AREA,"scaled:center"), scale = attr(IndPreds.10yr.X[[i]]$AREA,"scaled:scale"))),
                          LVAL = as.vector(scale(log(LVAL), center = attr(IndPreds.10yr.X[[i]]$LVAL,"scaled:center"), scale = attr(IndPreds.10yr.X[[i]]$LVAL,"scaled:scale"))),
                          DMU = as.vector(scale(log(DMU + 1), center = attr(IndPreds.10yr.X[[i]]$DMU,"scaled:center"), scale = attr(IndPreds.10yr.X[[i]]$DMU,"scaled:scale"))),
                          DOU = as.vector(scale(log(DOU + 1), center = attr(IndPreds.10yr.X[[i]]$DOU,"scaled:center"), scale = attr(IndPreds.10yr.X[[i]]$DOU,"scaled:scale"))),
                          PTREE = as.vector(scale(PTREE, center = attr(IndPreds.10yr.X[[i]]$PTREE,"scaled:center"), scale = attr(IndPreds.10yr.X[[i]]$PTREE,"scaled:scale"))),
                          COND = as.vector(scale(COND, center = attr(IndPreds.10yr.X[[i]]$COND,"scaled:center"), scale = attr(IndPreds.10yr.X[[i]]$COND,"scaled:scale"))),
                          CONN = as.vector(scale(CONN, center = attr(IndPreds.10yr.X[[i]]$CONN,"scaled:center"), scale = attr(IndPreds.10yr.X[[i]]$CONN,"scaled:scale"))),
                          ELEV = as.vector(scale(log(ELEV), center = attr(IndPreds.10yr.X[[i]]$ELEV,"scaled:center"), scale = attr(IndPreds.10yr.X[[i]]$ELEV,"scaled:scale"))),
                          SLOPE = as.vector(scale(SLOPE, center = attr(IndPreds.10yr.X[[i]]$SLOPE,"scaled:center"), scale = attr(IndPreds.10yr.X[[i]]$SLOPE,"scaled:scale"))),
                          TRI = as.vector(scale(log(TRI), center = attr(IndPreds.10yr.X[[i]]$TRI,"scaled:center"), scale = attr(IndPreds.10yr.X[[i]]$TRI,"scaled:scale"))),
                          SCAP = as.vector(scale(SCAP, center = attr(IndPreds.10yr.X[[i]]$SCAP,"scaled:center"), scale = attr(IndPreds.10yr.X[[i]]$SCAP,"scaled:scale"))))
    Predictions_DataY <- Preds_Properties_New %>% dplyr::select(-NewPropID, - PGRASS) %>% mutate(
                          AREA = as.vector(scale(log(AREA), center = attr(IndPreds.10yr.Y[[i]]$AREA,"scaled:center"), scale = attr(IndPreds.10yr.Y[[i]]$AREA,"scaled:scale"))),
                          LVAL = as.vector(scale(log(LVAL), center = attr(IndPreds.10yr.Y[[i]]$LVAL,"scaled:center"), scale = attr(IndPreds.10yr.Y[[i]]$LVAL,"scaled:scale"))),
                          DMU = as.vector(scale(log(DMU + 1), center = attr(IndPreds.10yr.Y[[i]]$DMU,"scaled:center"), scale = attr(IndPreds.10yr.Y[[i]]$DMU,"scaled:scale"))),
                          DOU = as.vector(scale(log(DOU + 1), center = attr(IndPreds.10yr.Y[[i]]$DOU,"scaled:center"), scale = attr(IndPreds.10yr.Y[[i]]$DOU,"scaled:scale"))),
                          PTREE = as.vector(scale(PTREE, center = attr(IndPreds.10yr.Y[[i]]$PTREE,"scaled:center"), scale = attr(IndPreds.10yr.Y[[i]]$PTREE,"scaled:scale"))),
                          COND = as.vector(scale(COND, center = attr(IndPreds.10yr.Y[[i]]$COND,"scaled:center"), scale = attr(IndPreds.10yr.Y[[i]]$COND,"scaled:scale"))),
                          CONN = as.vector(scale(CONN, center = attr(IndPreds.10yr.Y[[i]]$CONN,"scaled:center"), scale = attr(IndPreds.10yr.Y[[i]]$CONN,"scaled:scale"))),
                          ELEV = as.vector(scale(log(ELEV), center = attr(IndPreds.10yr.Y[[i]]$ELEV,"scaled:center"), scale = attr(IndPreds.10yr.Y[[i]]$ELEV,"scaled:scale"))),
                          SLOPE = as.vector(scale(SLOPE, center = attr(IndPreds.10yr.Y[[i]]$SLOPE,"scaled:center"), scale = attr(IndPreds.10yr.Y[[i]]$SLOPE,"scaled:scale"))),
                          TRI = as.vector(scale(log(TRI), center = attr(IndPreds.10yr.Y[[i]]$TRI,"scaled:center"), scale = attr(IndPreds.10yr.Y[[i]]$TRI,"scaled:scale"))),
                          SCAP = as.vector(scale(SCAP, center = attr(IndPreds.10yr.Y[[i]]$SCAP,"scaled:center"), scale = attr(IndPreds.10yr.Y[[i]]$SCAP,"scaled:scale"))))
    Predictions_DataZ <- Preds_Properties_New %>% dplyr::select(-NewPropID, - PGRASS) %>% mutate(
                          AREA = as.vector(scale(log(AREA), center = attr(IndPreds.10yr.Z[[i]]$AREA,"scaled:center"), scale = attr(IndPreds.10yr.Z[[i]]$AREA,"scaled:scale"))),
                          LVAL = as.vector(scale(log(LVAL), center = attr(IndPreds.10yr.Z[[i]]$LVAL,"scaled:center"), scale = attr(IndPreds.10yr.Z[[i]]$LVAL,"scaled:scale"))),
                          DMU = as.vector(scale(log(DMU + 1), center = attr(IndPreds.10yr.Z[[i]]$DMU,"scaled:center"), scale = attr(IndPreds.10yr.Z[[i]]$DMU,"scaled:scale"))),
                          DOU = as.vector(scale(log(DOU + 1), center = attr(IndPreds.10yr.Z[[i]]$DOU,"scaled:center"), scale = attr(IndPreds.10yr.Z[[i]]$DOU,"scaled:scale"))),
                          PTREE = as.vector(scale(PTREE, center = attr(IndPreds.10yr.Z[[i]]$PTREE,"scaled:center"), scale = attr(IndPreds.10yr.Z[[i]]$PTREE,"scaled:scale"))),
                          COND = as.vector(scale(COND, center = attr(IndPreds.10yr.Z[[i]]$COND,"scaled:center"), scale = attr(IndPreds.10yr.Z[[i]]$COND,"scaled:scale"))),
                          CONN = as.vector(scale(CONN, center = attr(IndPreds.10yr.Z[[i]]$CONN,"scaled:center"), scale = attr(IndPreds.10yr.Z[[i]]$CONN,"scaled:scale"))),
                          ELEV = as.vector(scale(log(ELEV), center = attr(IndPreds.10yr.Z[[i]]$ELEV,"scaled:center"), scale = attr(IndPreds.10yr.Z[[i]]$ELEV,"scaled:scale"))),
                          SLOPE = as.vector(scale(SLOPE, center = attr(IndPreds.10yr.Z[[i]]$SLOPE,"scaled:center"), scale = attr(IndPreds.10yr.Z[[i]]$SLOPE,"scaled:scale"))),
                          TRI = as.vector(scale(log(TRI), center = attr(IndPreds.10yr.Z[[i]]$TRI,"scaled:center"), scale = attr(IndPreds.10yr.Z[[i]]$TRI,"scaled:scale"))),
                          SCAP = as.vector(scale(SCAP, center = attr(IndPreds.10yr.Z[[i]]$SCAP,"scaled:center"), scale = attr(IndPreds.10yr.Z[[i]]$SCAP,"scaled:scale"))))

    # get model matrices

    # adoption
    Model_MatrixX.10yr[[i]] <- model.matrix(~ KMR + MosType + LUSec + LVAL + DOU + COND + ELEV + SLOPE + SCAP + DIM1 + DIM2 + DIM3 + DIM4 + DIM5, data = Predictions_DataX)
    # wta
    Model_MatrixY.10yr[[i]] <- model.matrix(~ KMR + MosType + LUSec + LVAL + DOU + COND + ELEV + SLOPE + SCAP + DIM1 + DIM2 + DIM3 + DIM4 + DIM5, data = Predictions_DataY)
    # proportion
    Model_MatrixZ.10yr[[i]] <- model.matrix(~ KMR + MosType + LUSec + LVAL + DOU + COND + ELEV + SLOPE + SCAP + DIM1 + DIM2 + DIM3 + DIM4 + DIM5, data = Predictions_DataZ)
}

# load functions
source("functions.r")

# get predictions
PredictionsX.Inf <- get.predxz.list(Model_MatrixX.Inf, CoefsX.Inf, 6)
PredictionsY.Inf <- get.predy.list(Model_MatrixY.Inf, CoefsY.Inf, 6)
PredictionsZ.Inf <- get.predxz.list(Model_MatrixZ.Inf, CoefsZ.Inf, 6)
PredictionsX.10yr <- get.predxz.list(Model_MatrixX.10yr, CoefsX.10yr, 6)
PredictionsY.10yr <- get.predy.list(Model_MatrixY.10yr, CoefsY.10yr, 6)
PredictionsZ.10yr <- get.predxz.list(Model_MatrixZ.10yr, CoefsZ.10yr, 6)

# compile predictions and write to csv
Compiled_Predictions_Inf <- as_tibble(cbind(Preds_Properties_New, MeanAdopt = PredictionsX.Inf$Mean, MeanWTA = PredictionsY.Inf$Mean, MeanProp = PredictionsZ.Inf$Mean))
write.csv(Compiled_Predictions_Inf, file = "output/predictions/spatial_predictions_inf.csv", row.names = FALSE)
Compiled_Predictions_10yr <- as_tibble(cbind(Preds_Properties_New, MeanAdopt = PredictionsX.10yr$Mean, MeanWTA = PredictionsY.10yr$Mean, MeanProp = PredictionsZ.10yr$Mean))
write.csv(Compiled_Predictions_10yr, file = "output/predictions/spatial_predictions_10yr.csv", row.names = FALSE)

-------------------- FROM HERE ON OLD STUFF AND MODEL VISUALISATION - NEEDS TO BE WORKED ON - 17th August 2022 -----------------------

# COEFFICIENT PLOTS

# load models
Jags.Fits <- readRDS("output/jags/Jags_Fits.rds")
Jags.Fits.Sel <- readRDS("output/jags/Jags_Fits_Sel.rds")

# MODELS WITHOUT VARIABLE SELECTION

# PERPETUAL COVENANT

# property level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits[[1]], beta_x[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = beta_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_ind_inf.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits[[1]], beta_y[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfWTA %>% ggplot(aes(y = Param, x = beta_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_ind_inf.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits[[1]], beta_z[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfProp %>% ggplot(aes(y = Param, x = beta_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_ind_inf.jpg")

# SA1 level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits[[1]], betar_x[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = betar_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_sa1_inf.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits[[1]], betar_y[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfWTA %>% ggplot(aes(y = Param, x = betar_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_sa1_inf.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits[[1]], betar_z[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfProp %>% ggplot(aes(y = Param, x = betar_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_sa1_inf.jpg")

# 10 YEAR COVENANT

# property level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits[[2]], beta_x[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = beta_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_ind_10yr.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits[[2]], beta_y[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfWTA %>% ggplot(aes(y = Param, x = beta_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_ind_10yr.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits[[2]], beta_z[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfProp %>% ggplot(aes(y = Param, x = beta_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_ind_10yr.jpg")

# SA1 level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits[[2]], betar_x[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = betar_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_sa1_10yr.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits[[2]], betar_y[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfWTA %>% ggplot(aes(y = Param, x = betar_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_sa1_10yr.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits[[2]], betar_z[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfProp %>% ggplot(aes(y = Param, x = betar_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_sa1_10yr.jpg")


# MODEL FITTING WITH VARIABLE SELECTION

# IN-PERPETUITY COVENANT

# property level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits.Sel[[1]], beta_x[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = beta_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_ind_inf_sel.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits.Sel[[1]], beta_y[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfWTA %>% ggplot(aes(y = Param, x = beta_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_ind_inf_sel.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits.Sel[[1]], beta_z[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfProp %>% ggplot(aes(y = Param, x = beta_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_ind_inf_sel.jpg")

# SA1 level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits.Sel[[1]], betar_x[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = betar_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_sa1_inf_sel.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits.Sel[[1]], betar_y[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfWTA %>% ggplot(aes(y = Param, x = betar_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_sa1_inf_sel.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits.Sel[[1]], betar_z[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfProp %>% ggplot(aes(y = Param, x = betar_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_sa1_inf_sel.jpg")

# 10 YEAR COVENANT

# property level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits.Sel[[2]], beta_x[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = beta_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_ind_10yr_sel.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits.Sel[[2]], beta_y[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfWTA %>% ggplot(aes(y = Param, x = beta_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_ind_10yr_sel.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits.Sel[[2]], beta_z[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfProp %>% ggplot(aes(y = Param, x = beta_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_ind_10yr_sel.jpg")

# SA1 level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits.Sel[[2]], betar_x[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = betar_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_sa1_10yr_sel.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits.Sel[[2]], betar_y[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfWTA %>% ggplot(aes(y = Param, x = betar_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_sa1_10yr_sel.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits.Sel[[2]], betar_z[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfProp %>% ggplot(aes(y = Param, x = betar_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_sa1_10yr_sel.jpg")
