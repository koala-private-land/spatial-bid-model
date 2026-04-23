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

sf_use_s2(FALSE)

getScriptPath <- function() {
  fileArgument <- grep("^--file=", commandArgs(FALSE), value = TRUE)

  if (length(fileArgument) == 0) {
    return(file.path(getwd(), "code", "buildPropertyAdjacency.R"))
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

readPropertyData <- function(propertyPath, propertyLayer) {
  st_read(propertyPath, layer = propertyLayer, quiet = TRUE) %>%
    standardiseGeometryName() %>%
    transmute(
      PropID = as.character(GURAS_PROPID),
      geometry = geometry
    ) %>%
    filter(!is.na(PropID), PropID != "") %>%
    st_make_valid() %>%
    st_transform(8058) %>%
    arrange(PropID)
}

buildAdjacencyList <- function(adjacencyConstants) {
  adjacencyList <- vector("list", adjacencyConstants$N)
  startPosition <- 1L

  for (propertyIndex in seq_len(adjacencyConstants$N)) {
    thisNeighborCount <- adjacencyConstants$num[[propertyIndex]]

    if (thisNeighborCount == 0) {
      adjacencyList[[propertyIndex]] <- integer(0)
      next
    }

    endPosition <- startPosition + thisNeighborCount - 1L
    adjacencyList[[propertyIndex]] <- adjacencyConstants$adj[startPosition:endPosition]
    startPosition <- endPosition + 1L
  }

  adjacencyList
}

buildAdjacencyConstants <- function(propertyData) {
  neighborIndexList <- st_intersects(propertyData)

  neighborIndexList <- lapply(
    seq_along(neighborIndexList),
    function(propertyIndex) {
      unique(sort(setdiff(neighborIndexList[[propertyIndex]], propertyIndex)))
    }
  )

  neighborCount <- lengths(neighborIndexList)
  adjacencyVector <- unlist(neighborIndexList, use.names = FALSE)
  weightVector <- rep(1, length(adjacencyVector))

  list(
    adj = as.integer(adjacencyVector),
    num = as.integer(neighborCount),
    weights = as.numeric(weightVector),
    N = as.integer(nrow(propertyData)),
    L = as.integer(length(adjacencyVector))
  )
}

validateAdjacencyConstants <- function(adjacencyConstants, propertyData) {
  if (length(adjacencyConstants$num) != adjacencyConstants$N) {
    stop("Length of num does not match N.")
  }

  if (length(adjacencyConstants$adj) != adjacencyConstants$L) {
    stop("Length of adj does not match L.")
  }

  if (length(adjacencyConstants$weights) != adjacencyConstants$L) {
    stop("Length of weights does not match L.")
  }

  adjacencyList <- buildAdjacencyList(adjacencyConstants)

  for (propertyIndex in seq_len(adjacencyConstants$N)) {
    thisNeighbors <- adjacencyList[[propertyIndex]]

    if (length(thisNeighbors) == 0) {
      next
    }

    for (neighborIndex in thisNeighbors) {
      if (!(propertyIndex %in% adjacencyList[[neighborIndex]])) {
        stop(
          "Adjacency is not symmetric for property index ",
          propertyIndex,
          " and neighbor index ",
          neighborIndex,
          "."
        )
      }
    }
  }

  invisible(TRUE)
}

scriptPath <- getScriptPath()
projectPath <- normalizePath(file.path(dirname(scriptPath), ".."), winslash = "/")
inputPath <- file.path(projectPath, "inputs")

lookupOutputPath <- Sys.getenv(
  "PROPERTY_ADJ_LOOKUP_OUTPUT_PATH",
  unset = file.path(projectPath, "outputs", "propertyAdjacencyLookup.csv")
)

constantsOutputPath <- Sys.getenv(
  "PROPERTY_ADJ_CONSTANTS_OUTPUT_PATH",
  unset = file.path(projectPath, "outputs", "propertyAdjacencyConstants.rds")
)

maxPropertyCount <- as.integer(Sys.getenv("PROPERTY_ADJ_MAX_PROPERTIES", unset = "0"))

if (file.exists(lookupOutputPath)) {
  stop(
    "Lookup output already exists: ",
    lookupOutputPath,
    "\nMove or rename this file before rerunning the script."
  )
}

if (file.exists(constantsOutputPath)) {
  stop(
    "Constants output already exists: ",
    constantsOutputPath,
    "\nMove or rename this file before rerunning the script."
  )
}

if (!dir.exists(dirname(lookupOutputPath))) {
  dir.create(dirname(lookupOutputPath), recursive = TRUE)
}

if (!dir.exists(dirname(constantsOutputPath))) {
  dir.create(dirname(constantsOutputPath), recursive = TRUE)
}

propertyPath <- paste0(
  "/vsizip/",
  file.path(
    inputPath,
    "properties",
    "NSW_Property_Conservation_Availability_260128.gdb.zip"
  )
)

propertyLayer <- "NSW_Property_Conservation_Availability_260128"

message("Reading property layer")
propertyReadTime <- system.time({
  propertyData <- readPropertyData(propertyPath, propertyLayer)
})

if (!is.na(maxPropertyCount) && maxPropertyCount > 0) {
  propertyData <- propertyData %>%
    slice_head(n = maxPropertyCount)
}

message("Building adjacency constants")
adjacencyBuildTime <- system.time({
  adjacencyConstants <- buildAdjacencyConstants(propertyData)
})

message("Validating adjacency constants")
adjacencyValidationTime <- system.time({
  validateAdjacencyConstants(adjacencyConstants, propertyData)
})

propertyLookup <- tibble(
  propertyIndex = seq_len(nrow(propertyData)),
  PropID = propertyData$PropID
)

message("Writing lookup output")
write.csv(propertyLookup, lookupOutputPath, row.names = FALSE, na = "")

message("Writing adjacency constants output")
saveRDS(adjacencyConstants, constantsOutputPath)

if (!file.exists(lookupOutputPath)) {
  stop("Lookup output was not created: ", lookupOutputPath)
}

if (!file.exists(constantsOutputPath)) {
  stop("Constants output was not created: ", constantsOutputPath)
}

message("")
message("Property adjacency build complete")
message("Lookup output: ", lookupOutputPath)
message("Constants output: ", constantsOutputPath)
message("Property count (N): ", adjacencyConstants$N)
message("Adjacency length (L): ", adjacencyConstants$L)
message("Zero-neighbor properties: ", sum(adjacencyConstants$num == 0))
message("Timing (elapsed seconds):")
message("  property read: ", round(propertyReadTime[["elapsed"]], 2))
message("  adjacency build: ", round(adjacencyBuildTime[["elapsed"]], 2))
message("  adjacency validation: ", round(adjacencyValidationTime[["elapsed"]], 2))
