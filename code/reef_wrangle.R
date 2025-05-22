#### Part 1: Extract Data ####
# Data downloaded from: https://readme.onemap.mv/#details

# Load required packages
library(tidyverse)
library(sf)
library(archive)  # for extracting .rar cross-platform

# Clear the environment
rm(list = ls())

# Master island list
islands <- read_csv("https://readme.onemap.mv/csv/IslandList_20211101.csv")


# Reef shape file
# 1. Download the .rar file
url <- "https://readme.onemap.mv/Datashare/Reefs.rar"
destfile <- tempfile(fileext = ".rar")
download.file(url, destfile, mode = "wb")

# 2. Extract the .rar archive
extract_dir <- tempdir()
archive::archive_extract(destfile, dir = extract_dir)

# 3. Find the .shp file path
reef_dir <- file.path(extract_dir, "Reef")  #Reef files exported into subfolder 'Reef'
shp_file <- list.files(reef_dir, pattern = "\\.shp$", full.names = TRUE)[1]

# 4. Read shapefile
reefs_sf <- st_read(shp_file)

# 5. Extract attribute table
reefs_attr <- st_drop_geometry(reefs_sf)

# 6. View or use
glimpse(islands)
glimpse(reefs_attr)

#### Part 2: Try and Join by Attributes ####
# Check for non-overlaps between main table and attribute table
islands |> 
  select(FCODE, atoll, islandName) |> 
  anti_join(islands  |> 
              select(FCODE, atoll, islandName))

reefs_attr |> 
  select(FCODE, Atoll, name) |> 
  anti_join(reefs_attr  |> 
              select(FCODE, Atoll, name))

# What about just by FCODE? (looks like typos causing conflicts)
islands |> 
  select(FCODE) |> 
  anti_join(reefs_attr  |> 
              select(FCODE))

#### Part 3: Join by Closest Island ####
#Create a function to convert Degrees, Minutes, Seconds to Decimal Degrees
dms_to_decimal <- function(dms){
  matches<-str_match(dms, "(\\d+)°\\s*(\\d+)'\\s*([0-9.]+)\"?\\s*([NSEW])")
  
  deg<-as.numeric(matches[,2])
  min<-as.numeric(matches[,3])
  sec<-as.numeric(matches[,4])
  dir<-matches[,5]
  
  decimal<-deg + min/60 + sec/3600
  
  decimal[dir %in% c("S","W")] <- -decimal[dir %in% c("S","W")]
  
  return(decimal)
}

# 1. Convert DMS to Decimal Degrees
islands<-islands%>%
  mutate(lon_dd=dms_to_decimal(longitude),
         lat_dd=dms_to_decimal(latitude))
reefs<-reefs_attr%>%
  mutate(lon_dd=dms_to_decimal(Longitude),
         lat_dd=dms_to_decimal(Latitude))

# 2. Convert to Spatial Format
st_crs(reefs_sf)

islands_point_sf <- islands %>%
  st_as_sf(coords=c("lon_dd","lat_dd"),crs = st_crs(reefs_sf))
reefs_point_sf <- reefs %>%
  st_as_sf(coords=c("lon_dd","lat_dd"),crs = st_crs(reefs_sf))

# 3. Find islands that fall within a reef
islands_in_reefs <- st_join(islands_point_sf, reefs_sf, join = st_within)
islands_in_reefs %>%
  count(reef_id = row_number(), island = islandName)

# 4. Find reefs with at least one island
reefs_with_islands<-reefs_sf %>%
  filter(lengths(st_contains(., islands_point_sf)) > 0)

# 5. Export as a Shapefile
st_write(reefs_with_islands, "reefs_with_islands.shp")
