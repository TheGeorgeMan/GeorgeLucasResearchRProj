# Data downloaded from: https://readme.onemap.mv/#details


# Load required packages
library(tidyverse)
library(sf)
library(archive)  # for extracting .rar cross-platform

install.packages("archive")

# Clear the environment
rm(list = ls())

# Master island list
islands <- read_csv("https://readme.onemap.mv/csv/IslandList_20211101.csv")


# Island shape file
# 1. Download the .rar file
url <- "https://readme.onemap.mv/Datashare/Islands.rar"
destfile <- tempfile(fileext = ".rar")
download.file(url, destfile, mode = "wb")

# 2. Extract the .rar archive
extract_dir <- tempdir()
archive::archive_extract(destfile, dir = extract_dir)

# 3. Find the .shp file path
shp_file <- list.files(extract_dir, pattern = "\\.shp$", full.names = TRUE)[1]

# 4. Read shapefile
islands_sf <- st_read(shp_file)

# 5. Extract attribute table
islands_attr <- st_drop_geometry(islands_sf)

# 6. View or use
glimpse(islands)
glimpse(islands_attr)

# Check for non-overlaps between main table and attribute table
islands |> 
  select(FCODE, atoll, islandName) |> 
  anti_join(islands_attr  |> 
              select(FCODE, atoll, islandName))

islands_attr |> 
  select(FCODE, atoll, islandName) |> 
  anti_join(islands  |> 
              select(FCODE, atoll, islandName))

# What about just by FCODE? (looks like typos causing conflicts)
islands |> 
  select(FCODE) |> 
  anti_join(islands_attr  |> 
              select(FCODE))

