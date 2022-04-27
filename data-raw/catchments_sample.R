## code to prepare `catchments_sample` dataset goes here

library(sf)
library(dplyr)
library(devtools)

# load catchments from kba project (just using this dataset because its synced on my laptop, could also load full dataset from gisdata)
catchments <- sf::st_read("C:/Users/MAEDW7/Dropbox (BEACONs)/RENR491 Capstone 2022/gisdata/catchments/YRW_catch50K.shp")

catchments_sample <- catchments %>%
  dplyr::filter(FDAHUC8 == "09EA") %>%
  dplyr::select(CATCHNUM, SKELUID, STRAHLER, ORDER1, ORDER2, ORDER3, BASIN, Area_Land, Area_Water, Area_Total, STRMLEN_1, FDAHUC8, ZONE, MDA, Isolated, intact) %>%
  sf::st_snap(x = ., y = ., tolerance = 0.0001)

catchments_sample$CATCHNUM <- as.integer(catchments_sample$CATCHNUM)
catchments_sample$SKELUID <- as.integer(catchments_sample$SKELUID)
names(catchments_sample)[names(catchments_sample) == "STRMLEN_1"] <- "STRMLEN"

usethis::use_data(catchments_sample, overwrite = TRUE)
