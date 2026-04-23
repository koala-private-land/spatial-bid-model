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
    return(file.path(getwd(), "code", "buildPropertyGrid.R"))
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

readNswBoundary <- function(nswPath) {
  nswBoundary <- st_read(nswPath, quiet = TRUE) %>%
    standardiseGeometryName() %>%
    st_make_valid() %>%
    st_transform(8058)

  st_sf(
    stateId = 1L,
    geometry = st_union(nswBoundary)
  )
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
    st_transform(8058)
}

buildChunkIndex <- function(nswBoundary, propertyData, chunkSizeMeters) {
  stateBoundingBox <- st_bbox(nswBoundary)
  chunkGeometry <- st_make_grid(
    st_as_sfc(stateBoundingBox),
    cellsize = chunkSizeMeters,
    square = TRUE
  )

  st_sf(
    chunkId = seq_along(chunkGeometry),
    geometry = chunkGeometry,
    crs = st_crs(nswBoundary)
  ) %>%
    st_filter(nswBoundary, .predicate = st_intersects) %>%
    st_filter(propertyData, .predicate = st_intersects) %>%
    arrange(chunkId)
}

buildGridChunk <- function(
    chunkData,
    nswBoundary,
    propertyData,
    stateBoundingBox,
    gridCellSizeMeters,
    gridColumnCount
) {
  propertyCandidates <- propertyData %>%
    st_filter(chunkData, .predicate = st_intersects)

  if (nrow(propertyCandidates) == 0) {
    return(NULL)
  }

  gridGeometry <- st_make_grid(
    chunkData,
    cellsize = gridCellSizeMeters,
    square = TRUE
  )

  gridCells <- st_sf(
    geometry = gridGeometry,
    crs = st_crs(chunkData)
  ) %>%
    st_filter(nswBoundary, .predicate = st_intersects) %>%
    st_filter(propertyCandidates, .predicate = st_intersects)

  if (nrow(gridCells) == 0) {
    return(NULL)
  }

  gridCoordinates <- st_coordinates(st_centroid(gridCells))
  gridCells <- gridCells %>%
    mutate(
      gridColumn = floor((gridCoordinates[, "X"] - stateBoundingBox[["xmin"]]) / gridCellSizeMeters),
      gridRow = floor((gridCoordinates[, "Y"] - stateBoundingBox[["ymin"]]) / gridCellSizeMeters),
      GridID = as.integer((gridRow * gridColumnCount) + gridColumn + 1)
    )

  overlapPieces <- suppressWarnings(
    st_intersection(
      gridCells %>% select(GridID),
      propertyCandidates %>% select(PropID)
    )
  )

  if (nrow(overlapPieces) == 0) {
    return(NULL)
  }

  bestAssignments <- overlapPieces %>%
    mutate(overlapArea = as.numeric(st_area(geometry))) %>%
    st_drop_geometry() %>%
    group_by(GridID, PropID) %>%
    summarise(
      overlapArea = sum(overlapArea),
      .groups = "drop"
    ) %>%
    group_by(GridID) %>%
    slice_max(order_by = overlapArea, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(GridID, PropID)

  if (nrow(bestAssignments) == 0) {
    return(NULL)
  }

  gridCells %>%
    select(GridID) %>%
    inner_join(bestAssignments, by = "GridID") %>%
    select(GridID, PropID, geometry) %>%
    arrange(GridID)
}

writeGridChunk <- function(gridChunk, outputPath, appendLayer) {
  st_write(
    gridChunk,
    outputPath,
    layer = "Grids",
    append = appendLayer,
    quiet = TRUE
  )
}

scriptPath <- getScriptPath()
projectPath <- normalizePath(
  Sys.getenv(
    "PROPERTY_GRID_PROJECT_ROOT",
    unset = file.path(dirname(scriptPath), "..")
  ),
  winslash = "/",
  mustWork = FALSE
)
inputPath <- normalizePath(
  Sys.getenv(
    "PROPERTY_GRID_INPUT_ROOT",
    unset = file.path(projectPath, "inputs")
  ),
  winslash = "/",
  mustWork = FALSE
)
outputPath <- Sys.getenv(
  "PROPERTY_GRID_OUTPUT_PATH",
  unset = file.path(projectPath, "outputs", "propertyGrid.gpkg")
)

gridCellSizeMeters <- 100
chunkSizeMeters <- as.numeric(Sys.getenv("PROPERTY_GRID_CHUNK_SIZE_METERS", unset = "25000"))
maxChunkCount <- as.integer(Sys.getenv("PROPERTY_GRID_MAX_CHUNKS", unset = "0"))
shardCount <- as.integer(Sys.getenv("PROPERTY_GRID_SHARD_COUNT", unset = "1"))
shardIndex <- as.integer(Sys.getenv("PROPERTY_GRID_SHARD_INDEX", unset = "1"))

if (is.na(shardCount) || shardCount < 1) {
  stop("PROPERTY_GRID_SHARD_COUNT must be at least 1.")
}

if (is.na(shardIndex) || shardIndex < 1 || shardIndex > shardCount) {
  stop("PROPERTY_GRID_SHARD_INDEX must be between 1 and PROPERTY_GRID_SHARD_COUNT.")
}

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

nswPath <- file.path(
  inputPath,
  "study-area",
  "NSW",
  "NSW_epsg8058.shp"
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

message("Reading NSW boundary")
nswReadTime <- system.time({
  nswBoundary <- readNswBoundary(nswPath)
})

message("Reading property layer")
propertyReadTime <- system.time({
  propertyData <- readPropertyData(propertyPath, propertyLayer)
})

message("Building chunk index")
chunkIndexTime <- system.time({
  chunkIndex <- buildChunkIndex(
    nswBoundary = nswBoundary,
    propertyData = propertyData,
    chunkSizeMeters = chunkSizeMeters
  )
})

if (nrow(chunkIndex) == 0) {
  stop("No chunks intersected both NSW and the property layer.")
}

if (!is.na(maxChunkCount) && maxChunkCount > 0) {
  chunkIndex <- chunkIndex %>%
    slice_head(n = maxChunkCount)
}

chunkIndex <- chunkIndex[
  ((seq_len(nrow(chunkIndex)) - 1L) %% shardCount) + 1L == shardIndex,
  ,
  drop = FALSE
]

if (nrow(chunkIndex) == 0) {
  stop(
    "No chunks assigned to shard ",
    shardIndex,
    " of ",
    shardCount,
    "."
  )
}

stateBoundingBox <- st_bbox(nswBoundary)
gridColumnCount <- ceiling(
  (stateBoundingBox[["xmax"]] - stateBoundingBox[["xmin"]]) / gridCellSizeMeters
)

message("Writing grid output to ", outputPath)
gridBuildTime <- system.time({
  appendLayer <- FALSE
  writtenRowCount <- 0L
  processedChunkCount <- 0L

  for (chunkPosition in seq_len(nrow(chunkIndex))) {
    thisChunk <- chunkIndex[chunkPosition, , drop = FALSE]

    message(
      "Processing chunk ",
      chunkPosition,
      " of ",
      nrow(chunkIndex),
      " (chunkId ",
      thisChunk$chunkId,
      ")"
    )

    gridChunk <- buildGridChunk(
      chunkData = thisChunk,
      nswBoundary = nswBoundary,
      propertyData = propertyData,
      stateBoundingBox = stateBoundingBox,
      gridCellSizeMeters = gridCellSizeMeters,
      gridColumnCount = gridColumnCount
    )

    if (is.null(gridChunk) || nrow(gridChunk) == 0) {
      next
    }

    if (any(is.na(gridChunk$PropID))) {
      stop("Chunk ", thisChunk$chunkId, " contains missing PropID values.")
    }

    if (any(duplicated(gridChunk$GridID))) {
      stop("Chunk ", thisChunk$chunkId, " contains duplicate GridID values.")
    }

    writeGridChunk(
      gridChunk = gridChunk,
      outputPath = outputPath,
      appendLayer = appendLayer
    )

    appendLayer <- TRUE
    writtenRowCount <- writtenRowCount + nrow(gridChunk)
    processedChunkCount <- processedChunkCount + 1L
  }
})

if (!file.exists(outputPath)) {
  stop("Grid output was not created: ", outputPath)
}

if (writtenRowCount == 0) {
  stop("Grid output was created but no property-matched cells were written.")
}

message("")
message("Property grid build complete")
message("Output: ", outputPath)
message("Shard: ", shardIndex, " of ", shardCount)
message("Chunks indexed: ", nrow(chunkIndex))
message("Chunks written: ", processedChunkCount)
message("Rows written: ", writtenRowCount)
message("Timing (elapsed seconds):")
message("  NSW read: ", round(nswReadTime[["elapsed"]], 2))
message("  property read: ", round(propertyReadTime[["elapsed"]], 2))
message("  chunk index: ", round(chunkIndexTime[["elapsed"]], 2))
message("  grid build: ", round(gridBuildTime[["elapsed"]], 2))
