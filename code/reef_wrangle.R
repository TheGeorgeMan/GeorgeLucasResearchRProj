#### Part 1: Extract Data ####
# Data downloaded from: https://readme.onemap.mv/#details

# Load required packages
library(tidyverse)
library(sf)
library(archive)  # for extracting .rar cross-platform

# Clear the environment
rm(list = ls())

#### Download Data Files
### Master Island List
islands <- read_csv("https://readme.onemap.mv/csv/IslandList_20211101.csv")

### Islands ShapeFile
#Download .rar file
url <- "https://readme.onemap.mv/Datashare/Islands.rar"
destfile <- tempfile(fileext = ".rar")
download.file(url, destfile, mode = "wb") 
#Extract .rar archive
extract_dir <- tempdir()
archive::archive_extract(destfile, dir = extract_dir)  
#Find shapefile path
shp_file <- list.files(extract_dir, pattern = "\\.shp$", full.names = TRUE)[1]
#Read shapefile
islands_sf <- st_read(shp_file)
#Extract attribute table
islands_attr <- st_drop_geometry(islands_sf)

### Reefs ShapeFile
#Download the .rar file
url <- "https://readme.onemap.mv/Datashare/Reefs.rar"
destfile <- tempfile(fileext = ".rar")
download.file(url, destfile, mode = "wb")
#Extract the .rar archive
extract_dir <- tempdir()
archive::archive_extract(destfile, dir = extract_dir)
#Find the .shp file path
reef_dir <- file.path(extract_dir, "Reef")  #Reef files exported into subfolder 'Reef'
shp_file <- list.files(reef_dir, pattern = "\\.shp$", full.names = TRUE)[1]
#Read shapefile
reefs_sf <- st_read(shp_file)
#Extract attribute table
reefs_attr <- st_drop_geometry(reefs_sf)

#View Items
glimpse(islands)
glimpse(reefs_sf)
glimpse(reefs_attr)
glimpse(islands_sf)
glimpse(islands_attr)

#### Part 2: Join by Closest Island ####
###Create function to convert Degrees, Minutes, Seconds to Decimal Degrees
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

#Convert DMS to Decimal Degrees
islands_sf<-islands_sf%>%
  mutate(lon_dd=dms_to_decimal(longitude),
         lat_dd=dms_to_decimal(latitude))
reefs_sf<-reefs_sf%>%
  mutate(lon_dd=dms_to_decimal(Longitude),
         lat_dd=dms_to_decimal(Latitude))

#Filter islands smaller than 5 ha
islands_sf <- islands_sf %>%
  filter(Area_ha >= 5)

#Filter reefs larger than 5000 ha
reefs_sf <- reefs_sf %>%
  filter(Areaha <= 5000)

#Make islands into points for analysis
islands_point_sf <- islands_sf %>%
  st_as_sf(coords=c("lon_dd","lat_dd"),crs = 4326)

#Transform island points to coordinate reference system of reefs_sf
st_crs(reefs_sf) #Check coordinate reference systems
islands_point_sf <- st_transform(islands_point_sf,crs=st_crs(reefs_sf))
st_crs(islands_point_sf)

#Find reefs that contain islands
reefs_containing_islands <- st_contains(reefs_sf, islands_point_sf)

#Filter for reefs that contain at least one island
reefs_with_islands <- reefs_sf[lengths(reefs_containing_islands) > 0 ,]

glimpse(reefs_with_islands)

#### Part 3. Selecting Random Reefs ####
#Set seed for reproducibility
set.seed(42)

#Randomly select one reef per atoll
sampled_reefs <- reefs_with_islands %>%
  group_by(Atoll) %>%
  slice_sample(n=1) %>%
  ungroup()

#Check result
glimpse(sampled_reefs)

ggplot() +
  geom_sf(data = sampled_reefs, fill = "red", color = "darkred", size = 1.2) +
  theme_minimal() +
  labs(
    title = "Sampled Reefs with Islands"
  )

#Export as shapefile
st_write(sampled_reefs, "sampled_reefs.shp")
