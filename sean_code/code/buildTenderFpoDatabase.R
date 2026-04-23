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
library(parallel)

sf_use_s2(FALSE)

getScriptPath <- function() {
  fileArgument <- grep("^--file=", commandArgs(FALSE), value = TRUE)

  if (length(fileArgument) == 0) {
    return(file.path(getwd(), "code", "buildTenderFpoDatabase.R"))
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

standardiseFixedId <- function(value) {
  value %>%
    coalesce("") %>%
    str_to_upper() %>%
    str_replace("^FRO", "FPO") %>%
    str_extract("FPO[0-9]+")
}

matchFpoCampaignToFixedId <- function(campaignName) {
  cleanCampaign <- campaignName %>%
    coalesce("") %>%
    str_to_lower() %>%
    str_squish()

  case_when(
    cleanCampaign == "fixed price offer - batch one" ~ "FPO1",
    cleanCampaign == "fixed price offer - batch two" ~ "FPO2",
    cleanCampaign == "fixed price offer - batch three" ~ "FPO3",
    cleanCampaign == "fixed price offer - batch three - phase 2" ~ "FPO3",
    cleanCampaign == "fixed price offer - batch four" ~ "FPO4",
    cleanCampaign == "fixed price offer - batch five" ~ "FPO5",
    cleanCampaign == "fixed price offer - batch six" ~ "FPO6",
    cleanCampaign == "fixed price offer - batch seven" ~ "FPO7",
    cleanCampaign == "fixed price offer - batch eight" ~ "FPO8",
    cleanCampaign == "fixed price offer - batch nine" ~ "FPO9",
    TRUE ~ NA_character_
  )
}

extractAcceptedDate <- function(agreementData) {
  # Date remains deferred in V1 because the current inputs do not expose
  # a reliable acceptance date field for fixed-price agreements.
  rep(as.Date(NA), nrow(agreementData))
}

normaliseLgaName <- function(lgaName) {
  lgaName %>%
    coalesce("") %>%
    str_to_upper() %>%
    str_replace_all("&", " AND ") %>%
    str_replace_all("[[:punct:]]+", " ") %>%
    str_replace_all("\\bCITY COUNCIL\\b", " ") %>%
    str_replace_all("\\bSHIRE COUNCIL\\b", " ") %>%
    str_replace_all("\\bREGIONAL COUNCIL\\b", " ") %>%
    str_replace_all("\\bCITY\\b", " ") %>%
    str_replace_all("\\bSHIRE\\b", " ") %>%
    str_replace_all("\\bREGIONAL\\b", " ") %>%
    str_replace_all("\\bCOUNCIL\\b", " ") %>%
    str_replace_all("\\bFAR WEST\\b", " ") %>%
    str_replace_all("\\bNORTH\\b", " ") %>%
    str_replace_all("\\bSOUTH\\b", " ") %>%
    str_squish()
}

decodeWorkbookCell <- function(cellXml, sharedValues) {
  ref <- sub('.* r="([^"]+)".*', "\\1", cellXml)

  if (!grepl("<v>", cellXml, fixed = TRUE)) {
    return(list(ref = ref, value = ""))
  }

  rawValue <- sub(".*<v>([^<]+)</v>.*", "\\1", cellXml)
  isShared <- grepl(' t="s"', cellXml, fixed = TRUE)

  if (isShared && nzchar(rawValue)) {
    rawValue <- sharedValues[as.integer(rawValue) + 1]
  }

  list(ref = ref, value = rawValue)
}

readWorkbookMetadata <- function(workbookPath) {
  workbookTempDirectory <- tempfile("xlsx_")
  dir.create(workbookTempDirectory)
  unzip(workbookPath, exdir = workbookTempDirectory)

  sharedXml <- paste(
    readLines(
      file.path(workbookTempDirectory, "xl", "sharedStrings.xml"),
      warn = FALSE
    ),
    collapse = ""
  )

  sharedValues <- gsub(
    "</?t[^>]*>",
    "",
    unlist(regmatches(sharedXml, gregexpr("<t[^>]*>[^<]*</t>", sharedXml, perl = TRUE)))
  )

  sharedValues <- gsub("&amp;", "&", sharedValues, fixed = TRUE)

  workbookXml <- paste(
    readLines(
      file.path(workbookTempDirectory, "xl", "workbook.xml"),
      warn = FALSE
    ),
    collapse = ""
  )

  sheetMatches <- unlist(
    regmatches(
      workbookXml,
      gregexpr('<sheet name="[^"]+" sheetId="[^"]+" r:id="[^"]+"', workbookXml, perl = TRUE)
    )
  )

  sheetNames <- sub('.*name="([^"]+)".*', "\\1", sheetMatches)

  list(
    workbookTempDirectory = workbookTempDirectory,
    sharedValues = sharedValues,
    sheetNames = sheetNames
  )
}

readWorksheetRows <- function(workbookMetadata, sheetNumber) {
  worksheetPath <- file.path(
    workbookMetadata$workbookTempDirectory,
    "xl",
    "worksheets",
    paste0("sheet", sheetNumber, ".xml")
  )

  worksheetXml <- paste(readLines(worksheetPath, warn = FALSE), collapse = "")
  rowXml <- unlist(regmatches(worksheetXml, gregexpr("<row[^>]*>.*?</row>", worksheetXml, perl = TRUE)))

  lapply(rowXml, function(thisRowXml) {
    cellXml <- unlist(
      regmatches(
        thisRowXml,
        gregexpr("<c[^>]*>.*?</c>|<c[^>]*/>", thisRowXml, perl = TRUE)
      )
    )

    decodedCells <- lapply(cellXml, decodeWorkbookCell, sharedValues = workbookMetadata$sharedValues)
    cellValues <- vapply(decodedCells, function(cell) cell$value, character(1))
    names(cellValues) <- vapply(decodedCells, function(cell) cell$ref, character(1))
    cellValues
  })
}

extractFixedPriceRegionsFromWorkbook <- function(workbookPath, lgaLookup) {
  workbookMetadata <- readWorkbookMetadata(workbookPath)

  on.exit(
    unlink(workbookMetadata$workbookTempDirectory, recursive = TRUE),
    add = TRUE
  )

  fpoSheetNumbers <- which(str_detect(workbookMetadata$sheetNames, "^FPO"))
  workbookFixedIds <- character(0)
  fixedPriceRegionRows <- list()

  for (sheetIndex in fpoSheetNumbers) {
    sheetName <- workbookMetadata$sheetNames[[sheetIndex]]
    roundFixedId <- standardiseFixedId(sheetName)

    if (is.na(roundFixedId) || identical(roundFixedId, "FPO9")) {
      next
    }

    workbookFixedIds <- c(workbookFixedIds, roundFixedId)
    worksheetRows <- readWorksheetRows(workbookMetadata, sheetIndex)

    firstColumnValues <- vapply(
      worksheetRows,
      function(thisRow) {
        firstMatch <- grep("^A[0-9]+$", names(thisRow), value = TRUE)

        if (length(firstMatch) == 0) {
          return(NA_character_)
        }

        thisRow[[firstMatch[[1]]]]
      },
      character(1)
    )

    lgaValues <- firstColumnValues[-1]
    lgaValues <- lgaValues[!is.na(lgaValues)]
    lgaValues <- lgaValues[str_squish(lgaValues) != ""]
    lgaValues <- lgaValues[!str_detect(lgaValues, "^LSC CLASS")]
    lgaValues <- lgaValues[!str_detect(lgaValues, "^AREA THRESHOLD$")]
    lgaValues <- unique(lgaValues)

    if (length(lgaValues) == 0) {
      next
    }

    fixedPriceRegionRows[[length(fixedPriceRegionRows) + 1]] <- data.frame(
      FixedID = rep(roundFixedId, length(lgaValues)),
      workbookLGA = lgaValues,
      LGAKey = normaliseLgaName(lgaValues),
      stringsAsFactors = FALSE
    )
  }

  fixedPriceRegionsRaw <- bind_rows(fixedPriceRegionRows) %>%
    filter(!LGAKey %in% c("", "N A")) %>%
    left_join(lgaLookup, by = "LGAKey")

  fixedPriceRegions <- fixedPriceRegionsRaw %>%
    filter(!is.na(LGA)) %>%
    distinct(FixedID, LGA)

  unmatchedWorkbookLgas <- fixedPriceRegionsRaw %>%
    filter(is.na(LGA)) %>%
    distinct(workbookLGA) %>%
    arrange(workbookLGA) %>%
    pull(workbookLGA)

  list(
    fixedPriceRegions = fixedPriceRegions,
    workbookFixedIds = sort(unique(workbookFixedIds)),
    unmatchedWorkbookLgas = unmatchedWorkbookLgas
  )
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

writeAttributeLayer <- function(attributeData, outputPath, layerName) {
  tempCsvPath <- tempfile(fileext = ".csv")
  write.csv(attributeData, tempCsvPath, row.names = FALSE, na = "")

  sf::gdal_utils(
    util = "vectortranslate",
    source = tempCsvPath,
    destination = outputPath,
    options = c(
      "-f", "GPKG",
      "-nln", layerName,
      "-oo", "AUTODETECT_TYPE=YES",
      "-lco", "ASPATIAL_VARIANT=GPKG_ATTRIBUTES"
    )
  )

  unlink(tempCsvPath)
}

buildPropertyLgaChunk <- function(lgaName, propertyPath, lgaPath, outputDirectory) {
  propertyData <- st_read(propertyPath, quiet = TRUE) %>%
    standardiseGeometryName()

  lgaData <- st_read(lgaPath, quiet = TRUE) %>%
    standardiseGeometryName() %>%
    filter(LGA == lgaName) %>%
    st_transform(st_crs(propertyData))

  if (nrow(lgaData) == 0) {
    return(NA_character_)
  }

  lgaBoundingBox <- st_as_sfc(st_bbox(lgaData))

  bboxCandidates <- propertyData %>%
    st_filter(lgaBoundingBox)

  if (nrow(bboxCandidates) == 0) {
    return(NA_character_)
  }

  propertyHits <- st_intersects(bboxCandidates, lgaData)
  matchedProperties <- bboxCandidates[lengths(propertyHits) > 0, , drop = FALSE]

  if (nrow(matchedProperties) == 0) {
    return(NA_character_)
  }

  outputPath <- file.path(
    outputDirectory,
    paste0("property_lga_", gsub("[^A-Z0-9]+", "_", lgaName), ".gpkg")
  )

  matchedProperties <- matchedProperties %>%
    mutate(LGA = lgaName) %>%
    transmute(
      PropID,
      LGA,
      geometry = geometry
    )

  st_write(
    matchedProperties,
    outputPath,
    layer = "PropertyLga",
    append = FALSE,
    quiet = TRUE
  )

  outputPath
}

scriptPath <- getScriptPath()
projectPath <- normalizePath(file.path(dirname(scriptPath), ".."), winslash = "/")
inputPath <- file.path(projectPath, "inputs")
outputPath <- file.path(projectPath, "outputs", "tenderFpoDatabase.gpkg")

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

workbookPath <- file.path(
  inputPath,
  "tender-boundaries",
  "BCT_campaign_parameters.xlsx"
)

tenderBoundaryPath <- file.path(
  inputPath,
  "tender-boundaries",
  "BCT_tender_boundaries_incomplete.shp"
)

lgaPath <- file.path(
  inputPath,
  "study-area",
  "nsw-lga",
  "LocalGovernmentArea.shp"
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

message("Reading LGA boundaries")
lgaReadTime <- system.time({
  lgaBoundaries <- st_read(lgaPath, quiet = TRUE) %>%
    standardiseGeometryName() %>%
    transmute(
      LGA = as.character(LGANAME),
      LGAKey = normaliseLgaName(as.character(LGANAME)),
      geometry = geometry
    ) %>%
    filter(LGAKey != "") %>%
    st_make_valid()
})

lgaLookup <- lgaBoundaries %>%
  st_drop_geometry() %>%
  distinct(LGAKey, LGA)

message("Reading workbook FixedID-LGA mapping")
workbookReadTime <- system.time({
  workbookExtraction <- extractFixedPriceRegionsFromWorkbook(
    workbookPath = workbookPath,
    lgaLookup = lgaLookup
  )
})

fixedPriceRegions <- workbookExtraction$fixedPriceRegions
workbookFixedIds <- workbookExtraction$workbookFixedIds
unmatchedWorkbookLgas <- workbookExtraction$unmatchedWorkbookLgas

# Deferred for later:
# - Land_Capability
# - Price_Time

message("Reading FPO boundary IDs for consistency check")
fixedPriceBoundaries <- st_read(tenderBoundaryPath, quiet = TRUE) %>%
  standardiseGeometryName() %>%
  mutate(
    mechanism = as.character(mechanism),
    program = as.character(program),
    campaignName = as.character(name_long),
    campaignCode = as.character(code),
    FixedID = standardiseFixedId(campaignCode)
  ) %>%
  filter(
    str_to_lower(mechanism) == "fpo",
    program == "CMP",
    campaignName != "Cultural Biodiversity Conservation Offer - Pilot"
  ) %>%
  filter(!is.na(FixedID))

boundaryFixedIds <- fixedPriceBoundaries %>%
  st_drop_geometry() %>%
  distinct(FixedID) %>%
  arrange(FixedID) %>%
  pull(FixedID)

message("Reading properties")
propertyReadTime <- system.time({
  properties <- st_read(propertyPath, layer = propertyLayer, quiet = TRUE) %>%
    standardiseGeometryName() %>%
    transmute(
      PropID = as.character(GURAS_PROPID),
      conservationAvailability = Conservation_Availability,
      geometry = geometry
    ) %>%
    filter(conservationAvailability == "Available for Conservation Agreements")
})

message("Building property-to-LGA lookup")
propertyLgaTime <- system.time({
  propertyWorkerPath <- file.path(
    tempdir(),
    paste0("availableProperties_", Sys.getpid(), ".gpkg")
  )

  lgaWorkerPath <- file.path(
    tempdir(),
    paste0("lgaBoundaries_", Sys.getpid(), ".gpkg")
  )

  propertyLgaChunkDirectory <- file.path(
    tempdir(),
    paste0("property_lga_chunks_", Sys.getpid())
  )

  if (!dir.exists(propertyLgaChunkDirectory)) {
    dir.create(propertyLgaChunkDirectory, recursive = TRUE)
  }

  st_write(properties %>% select(PropID), propertyWorkerPath, quiet = TRUE, append = FALSE)
  st_write(lgaBoundaries %>% select(LGA, LGAKey), lgaWorkerPath, quiet = TRUE, append = FALSE)

  lgaValues <- lgaBoundaries %>%
    st_drop_geometry() %>%
    distinct(LGA) %>%
    arrange(LGA) %>%
    pull(LGA)

  propertyLgaWorkerCount <- min(4, max(1, detectCores() - 1), length(lgaValues))
  message("Building property-to-LGA lookup with ", propertyLgaWorkerCount, " worker(s)")

  propertyLgaChunkPaths <- character(0)

  if (propertyLgaWorkerCount == 1) {
    propertyLgaChunkPaths <- unlist(lapply(
      lgaValues,
      buildPropertyLgaChunk,
      propertyPath = propertyWorkerPath,
      lgaPath = lgaWorkerPath,
      outputDirectory = propertyLgaChunkDirectory
    ))
  } else {
    workerCluster <- makeCluster(propertyLgaWorkerCount)

    clusterEvalQ(workerCluster, {
      library(sf)
      library(dplyr)
      sf_use_s2(FALSE)
      NULL
    })

    clusterExport(
      workerCluster,
      varlist = c(
        "standardiseGeometryName",
        "buildPropertyLgaChunk",
        "propertyWorkerPath",
        "lgaWorkerPath",
        "propertyLgaChunkDirectory"
      ),
      envir = environment()
    )

    propertyLgaChunkPaths <- unlist(parLapply(
      workerCluster,
      lgaValues,
      function(lgaName) {
        buildPropertyLgaChunk(
          lgaName = lgaName,
          propertyPath = propertyWorkerPath,
          lgaPath = lgaWorkerPath,
          outputDirectory = propertyLgaChunkDirectory
        )
      }
    ))

    stopCluster(workerCluster)
  }

  propertyLgaChunkPaths <- propertyLgaChunkPaths[!is.na(propertyLgaChunkPaths)]

  if (length(propertyLgaChunkPaths) == 0) {
    propertyLga <- properties[0, , drop = FALSE] %>%
      mutate(LGA = character(0)) %>%
      transmute(
        PropID,
        LGA,
        geometry = geometry
      )
  } else {
    propertyLga <- do.call(
      rbind,
      lapply(
        propertyLgaChunkPaths,
        function(chunkPath) {
          st_read(chunkPath, quiet = TRUE) %>%
            standardiseGeometryName()
        }
      )
    ) %>%
      distinct(PropID, LGA, .keep_all = TRUE)
  }

  unlink(propertyWorkerPath)
  unlink(lgaWorkerPath)
  unlink(propertyLgaChunkDirectory, recursive = TRUE)
})

message("Building Available")
availableJoinTime <- system.time({
  available <- propertyLga %>%
    left_join(fixedPriceRegions, by = "LGA") %>%
    filter(!is.na(FixedID)) %>%
    transmute(
      FixedID,
      PropID,
      geometry = geometry
    ) %>%
    distinct(FixedID, PropID, .keep_all = TRUE)
})

# Deferred for later:
# - Prop_Preds

message("Reading fixed-price agreements")
fixedPriceAgreements <- st_read(covenantPath, quiet = TRUE) %>%
  standardiseGeometryName() %>%
  mutate(
    agreementType = classifyAgreementType(campaign, mechanism),
    FixedID = matchFpoCampaignToFixedId(campaign),
    Date = extractAcceptedDate(cur_data())
  ) %>%
  filter(
    program == "CMP",
    funded == "Yes",
    agreementType == "fixed"
  ) %>%
  st_transform(st_crs(properties)) %>%
  st_make_valid()

message("Building EOIsAccepted")
acceptedWithProperties <- fixedPriceAgreements %>%
  st_join(
    properties %>%
      select(PropID) %>%
      st_make_valid(),
    join = st_intersects,
    left = TRUE,
    largest = TRUE
  )

eoisAccepted <- acceptedWithProperties %>%
  transmute(
    FixedID,
    PropID,
    Date,
    geometry = geometry
  )

# Deferred for later:
# - Grids
# - Adjacency Matrix

message("Writing GeoPackage")
writeAttributeLayer(fixedPriceRegions, outputPath, "FixedPriceRegions")
writeLayer(available, outputPath, "Available")
writeLayer(eoisAccepted, outputPath, "EOIsAccepted")

message("Validating output")
outputLayers <- st_layers(outputPath)$name
expectedLayers <- c(
  "FixedPriceRegions",
  "Available",
  "EOIsAccepted"
)

missingLayers <- setdiff(expectedLayers, outputLayers)

if (length(missingLayers) > 0) {
  stop("Missing output layers: ", paste(missingLayers, collapse = ", "))
}

if (any(is.na(fixedPriceRegions$FixedID))) {
  stop("Missing FixedID values in FixedPriceRegions")
}

if (any(is.na(fixedPriceRegions$LGA))) {
  stop("Missing LGA values in FixedPriceRegions")
}

if (any(is.na(available$FixedID))) {
  stop("Missing FixedID values in Available")
}

if (any(is.na(available$PropID))) {
  stop("Missing PropID values in Available")
}

unmatchedAcceptedCampaigns <- fixedPriceAgreements %>%
  st_drop_geometry() %>%
  filter(is.na(FixedID)) %>%
  distinct(campaign) %>%
  arrange(campaign) %>%
  pull(campaign)

unmatchedAcceptedPropertyCount <- eoisAccepted %>%
  st_drop_geometry() %>%
  filter(is.na(PropID)) %>%
  nrow()

workbookOnlyFixedIds <- setdiff(workbookFixedIds, boundaryFixedIds)
boundaryOnlyFixedIds <- setdiff(boundaryFixedIds, workbookFixedIds)

zeroPropertyLgas <- fixedPriceRegions %>%
  anti_join(
    available %>%
      st_drop_geometry() %>%
      distinct(FixedID) %>%
      inner_join(fixedPriceRegions, by = "FixedID") %>%
      distinct(LGA),
    by = "LGA"
  ) %>%
  distinct(LGA) %>%
  arrange(LGA) %>%
  pull(LGA)

message("")
message("Tender-fpo database V1 complete")
message("Output: ", outputPath)
message("Rows:")
message("  FixedPriceRegions: ", nrow(fixedPriceRegions))
message("  Available: ", nrow(available))
message("  EOIsAccepted: ", nrow(eoisAccepted))
message("Workbook FixedIDs: ", paste(workbookFixedIds, collapse = ", "))
message("Boundary FixedIDs: ", paste(boundaryFixedIds, collapse = ", "))
message(
  "Workbook-only FixedIDs: ",
  if (length(workbookOnlyFixedIds) == 0) "none" else paste(workbookOnlyFixedIds, collapse = ", ")
)
message(
  "Boundary-only FixedIDs: ",
  if (length(boundaryOnlyFixedIds) == 0) "none" else paste(boundaryOnlyFixedIds, collapse = ", ")
)
message(
  "Workbook LGAs unmatched to LGA layer: ",
  if (length(unmatchedWorkbookLgas) == 0) "none" else paste(unmatchedWorkbookLgas, collapse = "; ")
)
message(
  "LGAs with zero matched properties: ",
  if (length(zeroPropertyLgas) == 0) "none" else paste(zeroPropertyLgas, collapse = "; ")
)
message(
  "Unmatched fixed-price campaigns: ",
  if (length(unmatchedAcceptedCampaigns) == 0) "none" else paste(unmatchedAcceptedCampaigns, collapse = "; ")
)
message("EOIsAccepted rows with missing PropID: ", unmatchedAcceptedPropertyCount)
message("Timing (elapsed seconds):")
message("  workbook extraction: ", round(workbookReadTime[["elapsed"]], 2))
message("  property read: ", round(propertyReadTime[["elapsed"]], 2))
message("  LGA read: ", round(lgaReadTime[["elapsed"]], 2))
message("  property-to-LGA: ", round(propertyLgaTime[["elapsed"]], 2))
message("  Available join: ", round(availableJoinTime[["elapsed"]], 2))
