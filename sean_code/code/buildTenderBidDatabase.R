minorVersion <- strsplit(R.version[["minor"]], "[.]")[[1]][1]
userLibrary <- file.path(
  Sys.getenv("LOCALAPPDATA"),
  "R",
  "win-library",
  paste(R.version[["major"]], minorVersion, sep = ".")
)

if (dir.exists(userLibrary)) {
  .libPaths(c(userLibrary, .libPaths()))
}

library(sf)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

sf_use_s2(FALSE)

getScriptPath <- function() {
  fileArgument <- grep("^--file=", commandArgs(FALSE), value = TRUE)

  if (length(fileArgument) == 0) {
    return(file.path(getwd(), "code", "buildTenderBidDatabase.R"))
  }

  normalizePath(
    sub("^--file=", "", fileArgument[[1]]),
    winslash = "/",
    mustWork = TRUE
  )
}

standardiseGeometryName <- function(spatialData) {
  geometryColumn <- attr(spatialData, "sf_column")

  if (!identical(geometryColumn, "geometry")) {
    names(spatialData)[names(spatialData) == geometryColumn] <- "geometry"
    attr(spatialData, "sf_column") <- "geometry"
  }

  spatialData
}

cleanCampaignName <- function(campaignName) {
  campaignName %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[[:punct:]]+", " ") %>%
    str_replace_all("\\bconservation tender no\\b", " ") %>%
    str_replace_all("\\bno\\b", " ") %>%
    str_replace_all("\\bphase\\b", " ") %>%
    str_replace_all("\\bhabitat\\b", " ") %>%
    str_replace_all("\\bin\\b", " ") %>%
    str_squish()
}

classifyAgreementType <- function(campaignName, mechanismName) {
  cleanCampaign <- campaignName %>%
    coalesce("") %>%
    str_to_lower() %>%
    str_squish()

  cleanMechanism <- mechanismName %>%
    coalesce("") %>%
    str_to_lower() %>%
    str_squish()

  case_when(
    str_detect(cleanCampaign, "biodiversity offsets program") ~ "exclude",
    str_detect(cleanCampaign, "revolving fund") ~ "exclude",
    str_detect(cleanCampaign, "wildlife refuge") ~ "exclude",
    str_detect(cleanCampaign, "fixed price offer") ~ "fixed",
    str_detect(cleanCampaign, "conservation tender") ~ "tender",
    cleanMechanism %in% c("ct", "conservation tender") ~ "tender",
    str_detect(cleanCampaign, "unfunded") ~ "unfunded",
    str_detect(cleanCampaign, "koala strategy") ~ "unfunded",
    TRUE ~ NA_character_
  )
}

matchCovenantCampaign <- function(covenantCampaigns, tenderCampaigns) {
  tenderLookup <- tenderCampaigns %>%
    st_drop_geometry() %>%
    mutate(cleanTenderName = cleanCampaignName(campaignName)) %>%
    select(
      tendID,
      campaignNumber,
      campaignCode,
      campaignName,
      cleanTenderName
    )

  covenantLookup <- tibble(campaign = unique(covenantCampaigns)) %>%
    mutate(
      cleanCovenantName = cleanCampaignName(campaign),
      matchedCleanTenderName = case_when(
        str_detect(cleanCovenantName, "central west rivers") ~ "central west rivers",
        str_detect(cleanCovenantName, "southern highlands koala") ~ "southern highlands koala",
        str_detect(cleanCovenantName, "port macquarie") ~ "port macquarie kempsey koala",
        str_detect(cleanCovenantName, "lismore ballina") ~ "lismore ballina koala",
        str_detect(cleanCovenantName, "monaro grasslands") ~ "monaro grasslands",
        str_detect(cleanCovenantName, "south west slopes") ~ "south west slopes",
        str_detect(cleanCovenantName, "north west plains") ~ "north west plains",
        str_detect(cleanCovenantName, "northern tablelands") ~ "northern tablelands",
        str_detect(cleanCovenantName, "murray riverina") ~ "murray riverina",
        str_detect(cleanCovenantName, "central tablelands") ~ "central tablelands",
        str_detect(cleanCovenantName, "lachlan corridor") ~ "lachlan corridor",
        str_detect(cleanCovenantName, "plains wanderer") ~ "plains wanderer",
        str_detect(cleanCovenantName, "northern inland floodplain") ~ "northern inland floodplains",
        str_detect(cleanCovenantName, "snowgum.*north") ~ "snow gum grassy woodland and grasslands north",
        str_detect(cleanCovenantName, "snowgum.*north") ~ "snow gum grassy woodland and grasslands north",
        str_detect(cleanCovenantName, "snowgum.*south") ~ "snow gum grassy woodland and grasslands south",
        str_detect(cleanCovenantName, "snowgum.*south") ~ "snow gum grassy woodland and grasslands south",
        str_detect(cleanCovenantName, "lower clarence valley") ~ "lower clarence valley",
        str_detect(cleanCovenantName, "paroo warrego") ~ "paroo warrego catchments",
        str_detect(cleanCovenantName, "northern inland koala") ~ "northern inland koala",
        str_detect(cleanCovenantName, "darling baaka") ~ "darling baaka",
        str_detect(cleanCovenantName, "restoring murray woodlands") ~ "restoring murray woodlands",
        str_detect(cleanCovenantName, "upper hunter") ~ "upper hunter",
        TRUE ~ cleanCovenantName
      )
    ) %>%
    left_join(
      tenderLookup,
      by = c("matchedCleanTenderName" = "cleanTenderName")
    ) %>%
    select(
      campaign,
      tendID,
      campaignNumber,
      campaignCode,
      campaignName
    )

  covenantLookup
}

writeLayer <- function(spatialData, outputPath, layerName) {
  st_write(
    spatialData,
    outputPath,
    layer = layerName,
    append = FALSE,
    quiet = TRUE
  )
}

scriptPath <- getScriptPath()
projectPath <- normalizePath(file.path(dirname(scriptPath), ".."), winslash = "/")
inputPath <- file.path(projectPath, "inputs")
outputPath <- file.path(projectPath, "outputs", "tenderBidDatabase.gpkg")

if (file.exists(outputPath)) {
  stop(
    "Output already exists: ",
    outputPath,
    "\nMove or rename this file before rerunning the script."
  )
}

if (!dir.exists(dirname(outputPath))) {
  dir.create(dirname(outputPath), recursive = TRUE)
}

tenderBoundaryPath <- file.path(
  inputPath,
  "tender-boundaries",
  "BCT_tender_boundaries_incomplete.shp"
)

propertyPath <- paste0(
  "/vsizip/",
  file.path(
    inputPath,
    "properties",
    "NSW_Property_Conservation_Availability_260128.gdb.zip"
  )
)

propertyLayer <- "NSW_Property_Conservation_Availability_260128"

covenantPath <- file.path(
  inputPath,
  "covenant-boundaries",
  "BCT_agreements_110325.shp"
)

message("Reading tender-bid campaigns")
tenderBidCampaigns <- st_read(tenderBoundaryPath, quiet = TRUE) %>%
  standardiseGeometryName() %>%
  filter(mechanism == "CT") %>%
  transmute(
    tendID = sprintf("CT%02d", number),
    campaignNumber = as.integer(number),
    campaignCode = code,
    campaignName = name_long,
    dateOpen = as.Date(date_opene),
    dateClose = as.Date(date_close),
    bctRegion = bct_region,
    mechanism = mechanism,
    program = program,
    geometry = geometry
  ) %>%
  arrange(campaignNumber)

message("Reading properties")
properties <- st_read(propertyPath, layer = propertyLayer, quiet = TRUE) %>%
  standardiseGeometryName() %>%
  transmute(
    PropID = as.character(GURAS_PROPID),
    dcdbLotDp = DCDB_Lot_DP,
    totalPropertyAreaHa = Total_PROPID_AreaHa,
    conservationAvailability = Conservation_Availability,
    availableAreaHa = ConAvail_AreaHa,
    geometry = geometry
  )

message("Building tender-bid available properties")
tenderBidCampaignsProjected <- tenderBidCampaigns %>%
  st_transform(st_crs(properties)) %>%
  st_make_valid()

propertiesForTenderBid <- properties %>%
  filter(conservationAvailability == "Available for Conservation Agreements") %>%
  st_make_valid()

tenderBidAvailableProperties <- propertiesForTenderBid %>%
  st_filter(tenderBidCampaignsProjected) %>%
  st_intersection(tenderBidCampaignsProjected) %>%
  mutate(
    overlapAreaHa = as.numeric(st_area(geometry)) / 10000,
    eligibleForTenderBid = TRUE
  ) %>%
  transmute(
    tendID,
    PropID,
    campaignNumber,
    campaignCode,
    campaignName,
    availableAreaHa,
    overlapAreaHa,
    eligibleForTenderBid,
    geometry = geometry
  )

# Placeholder for future eligibility logic:
# - Join campaign-specific eligibility rules.
# - Filter or flag records by minimum hectares, region rules, and other criteria.
# - Keep the final field names unchanged.

message("Reading and matching tender-bid covenants")
covenantCampaignMatches <- st_read(covenantPath, quiet = TRUE) %>%
  mutate(
    agreementType = classifyAgreementType(campaign, mechanism)
  ) %>%
  filter(
    program == "CMP",
    funded == "Yes",
    agreementType == "tender"
  ) %>%
  st_drop_geometry() %>%
  pull(campaign) %>%
  matchCovenantCampaign(tenderBidCampaigns)

tenderBidCovenantsRaw <- st_read(covenantPath, quiet = TRUE) %>%
  standardiseGeometryName() %>%
  mutate(
    agreementType = classifyAgreementType(campaign, mechanism)
  ) %>%
  filter(
    program == "CMP",
    funded == "Yes",
    agreementType == "tender"
  ) %>%
  left_join(covenantCampaignMatches, by = "campaign") %>%
  st_transform(st_crs(properties)) %>%
  st_make_valid()

covenantPropertyMatches <- tenderBidCovenantsRaw %>%
  st_join(
    properties %>%
      select(PropID) %>%
      st_make_valid(),
    join = st_intersects,
    left = TRUE,
    largest = TRUE
  )

tenderBidCovenants <- covenantPropertyMatches %>%
  st_transform(st_crs(tenderBidCampaigns)) %>%
  transmute(
    tendID,
    PropID,
    campaignNumber,
    campaignCode,
    campaignName,
    agreementID = ss_agreeme,
    caseID = ss_case_id,
    crmID = crm_identi,
    registeredAreaHa = reg_area_h,
    gisAreaHa = gis_area_h,
    agreementStatus = agr_status,
    geometry = geometry
  )

message("Writing GeoPackage")
writeLayer(tenderBidCampaigns, outputPath, "tenderBidCampaigns")
writeLayer(properties, outputPath, "properties")
writeLayer(tenderBidAvailableProperties, outputPath, "tenderBidAvailableProperties")
writeLayer(tenderBidCovenants, outputPath, "tenderBidCovenants")

message("Validating output")
outputLayers <- st_layers(outputPath)$name
expectedLayers <- c(
  "tenderBidCampaigns",
  "properties",
  "tenderBidAvailableProperties",
  "tenderBidCovenants"
)

missingLayers <- setdiff(expectedLayers, outputLayers)

if (length(missingLayers) > 0) {
  stop("Missing output layers: ", paste(missingLayers, collapse = ", "))
}

unmatchedCampaignCount <- tenderBidCovenants %>%
  st_drop_geometry() %>%
  filter(is.na(tendID)) %>%
  nrow()

unmatchedPropertyCount <- tenderBidCovenants %>%
  st_drop_geometry() %>%
  filter(is.na(PropID)) %>%
  nrow()

if (any(is.na(tenderBidAvailableProperties$tendID))) {
  stop("Missing tendID values in tenderBidAvailableProperties")
}

if (any(is.na(tenderBidAvailableProperties$PropID))) {
  stop("Missing PropID values in tenderBidAvailableProperties")
}

message("")
message("Tender-bid database V1 complete")
message("Output: ", outputPath)
message("Rows:")
message("  tenderBidCampaigns: ", nrow(tenderBidCampaigns))
message("  properties: ", nrow(properties))
message("  tenderBidAvailableProperties: ", nrow(tenderBidAvailableProperties))
message("  tenderBidCovenants: ", nrow(tenderBidCovenants))
message("Unmatched tender-bid covenant campaigns: ", unmatchedCampaignCount)
message("Unmatched tender-bid covenant properties: ", unmatchedPropertyCount)
