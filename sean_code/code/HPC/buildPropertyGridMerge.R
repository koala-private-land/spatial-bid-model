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

shardDirectory <- normalizePath(
  Sys.getenv("PROPERTY_GRID_MERGE_INPUT_DIR"),
  winslash = "/",
  mustWork = TRUE
)

outputPath <- normalizePath(
  Sys.getenv("PROPERTY_GRID_MERGE_OUTPUT_PATH"),
  winslash = "/",
  mustWork = FALSE
)

if (file.exists(outputPath)) {
  stop(
    "Output already exists: ",
    outputPath,
    "\nMove or rename this file before rerunning the merge."
  )
}

if (!dir.exists(dirname(outputPath))) {
  dir.create(dirname(outputPath), recursive = TRUE)
}

shardPaths <- list.files(
  shardDirectory,
  pattern = "[.]gpkg$",
  full.names = TRUE
) %>%
  sort()

if (length(shardPaths) == 0) {
  stop("No grid shard GeoPackages found in ", shardDirectory)
}

appendLayer <- FALSE
writtenRowCount <- 0L
seenGridIds <- integer(0)

for (shardPath in shardPaths) {
  message("Merging shard: ", basename(shardPath))

  shardData <- st_read(shardPath, layer = "Grids", quiet = TRUE) %>%
    select(GridID, PropID, geometry) %>%
    arrange(GridID)

  if (nrow(shardData) == 0) {
    next
  }

  if (any(is.na(shardData$GridID)) || any(is.na(shardData$PropID))) {
    stop("Shard contains missing GridID or PropID values: ", shardPath)
  }

  if (any(duplicated(shardData$GridID))) {
    stop("Shard contains duplicate GridID values: ", shardPath)
  }

  duplicateAcrossShards <- intersect(seenGridIds, shardData$GridID)

  if (length(duplicateAcrossShards) > 0) {
    stop(
      "GridID values were duplicated across shards. First duplicate: ",
      duplicateAcrossShards[[1]]
    )
  }

  st_write(
    shardData,
    outputPath,
    layer = "Grids",
    append = appendLayer,
    quiet = TRUE
  )

  appendLayer <- TRUE
  writtenRowCount <- writtenRowCount + nrow(shardData)
  seenGridIds <- c(seenGridIds, shardData$GridID)
}

if (!file.exists(outputPath)) {
  stop("Merged grid output was not created: ", outputPath)
}

message("")
message("Property grid merge complete")
message("Shard count: ", length(shardPaths))
message("Rows written: ", writtenRowCount)
message("Output: ", outputPath)
