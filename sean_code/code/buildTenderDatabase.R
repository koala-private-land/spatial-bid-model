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
    return(file.path(getwd(), "code", "buildTenderDatabase.R"))
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
        str_detect(cleanCovenantName, "snow gum.*north") ~ "snow gum grassy woodland and grasslands north",
        str_detect(cleanCovenantName, "snowgum.*south") ~ "snow gum grassy woodland and grasslands south",
        str_detect(cleanCovenantName, "snow gum.*south") ~ "snow gum grassy woodland and grasslands south",
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
outputPath <- file.path(projectPath, "outputs", "tenderDatabase.gpkg")

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

message("Reading tender campaigns")
tenderCampaigns <- st_read(tenderBoundaryPath, quiet = TRUE) %>%
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

message("Building tender available properties")
tenderCampaignsProjected <- tenderCampaigns %>%
  st_transform(st_crs(properties)) %>%
  st_make_valid()

propertiesForTender <- properties %>%
  filter(conservationAvailability == "Available for Conservation Agreements") %>%
  st_make_valid()

tenderAvailableProperties <- propertiesForTender %>%
  st_filter(tenderCampaignsProjected) %>%
  st_intersection(tenderCampaignsProjected) %>%
  mutate(
    overlapAreaHa = as.numeric(st_area(geometry)) / 10000,
    eligibleForTender = TRUE
  ) %>%
  transmute(
    tendID,
    PropID,
    campaignNumber,
    campaignCode,
    campaignName,
    availableAreaHa,
    overlapAreaHa,
    eligibleForTender,
    geometry = geometry
  )

# Placeholder for future eligibility logic:
# - Join campaign-specific eligibility rules.
# - Filter or flag records by minimum hectares, region rules, and other criteria.
# - Keep the final field names unchanged.

message("Reading and matching tender covenants")
covenantCampaignMatches <- st_read(covenantPath, quiet = TRUE) %>%
  filter(
    program == "CMP",
    funded == "Yes",
    mechanism == "ct"
  ) %>%
  st_drop_geometry() %>%
  pull(campaign) %>%
  matchCovenantCampaign(tenderCampaigns)

tenderCovenantsRaw <- st_read(covenantPath, quiet = TRUE) %>%
  standardiseGeometryName() %>%
  filter(
    program == "CMP",
    funded == "Yes",
    mechanism == "ct"
  ) %>%
  left_join(covenantCampaignMatches, by = "campaign") %>%
  st_transform(st_crs(properties)) %>%
  st_make_valid()

covenantPropertyMatches <- tenderCovenantsRaw %>%
  st_join(
    properties %>%
      select(PropID) %>%
      st_make_valid(),
    join = st_intersects,
    left = TRUE,
    largest = TRUE
  )

tenderCovenants <- covenantPropertyMatches %>%
  st_transform(st_crs(tenderCampaigns)) %>%
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
writeLayer(tenderCampaigns, outputPath, "tenderCampaigns")
writeLayer(properties, outputPath, "properties")
writeLayer(tenderAvailableProperties, outputPath, "tenderAvailableProperties")
writeLayer(tenderCovenants, outputPath, "tenderCovenants")

message("Validating output")
outputLayers <- st_layers(outputPath)$name
expectedLayers <- c(
  "tenderCampaigns",
  "properties",
  "tenderAvailableProperties",
  "tenderCovenants"
)

missingLayers <- setdiff(expectedLayers, outputLayers)

if (length(missingLayers) > 0) {
  stop("Missing output layers: ", paste(missingLayers, collapse = ", "))
}

unmatchedCampaignCount <- tenderCovenants %>%
  st_drop_geometry() %>%
  filter(is.na(tendID)) %>%
  nrow()

unmatchedPropertyCount <- tenderCovenants %>%
  st_drop_geometry() %>%
  filter(is.na(PropID)) %>%
  nrow()

if (any(is.na(tenderAvailableProperties$tendID))) {
  stop("Missing tendID values in tenderAvailableProperties")
}

if (any(is.na(tenderAvailableProperties$PropID))) {
  stop("Missing PropID values in tenderAvailableProperties")
}

message("")
message("Tender database V1 complete")
message("Output: ", outputPath)
message("Rows:")
message("  tenderCampaigns: ", nrow(tenderCampaigns))
message("  properties: ", nrow(properties))
message("  tenderAvailableProperties: ", nrow(tenderAvailableProperties))
message("  tenderCovenants: ", nrow(tenderCovenants))
message("Unmatched tender covenant campaigns: ", unmatchedCampaignCount)
message("Unmatched tender covenant properties: ", unmatchedPropertyCount)
