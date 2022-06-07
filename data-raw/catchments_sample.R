## code to prepare `builder_catchments_sample` dataset goes here

library(sf)
library(dplyr)
library(devtools)

# load catchments from kba project (just using this dataset because its synced on my laptop, could also load full dataset from gisdata)
catchments <- sf::st_read("C:/Users/MAEDW7/Dropbox (BEACONs)/RENR491 Capstone 2022/gisdata/catchments/YRW_catch50K.shp")

builder_catchments_sample <- catchments %>%
  dplyr::filter(FDAHUC8 == "09EA") %>%
  dplyr::select(CATCHNUM, SKELUID, STRAHLER, ORDER1, ORDER2, ORDER3, BASIN, Area_Land, Area_Water, Area_Total, STRMLEN_1, FDAHUC8, ZONE, MDA, Isolated, intact) %>%
  sf::st_snap(x = ., y = ., tolerance = 0.1)

builder_catchments_sample$CATCHNUM <- as.integer(builder_catchments_sample$CATCHNUM)
builder_catchments_sample$SKELUID <- as.integer(builder_catchments_sample$SKELUID)
names(builder_catchments_sample)[names(builder_catchments_sample) == "STRMLEN_1"] <- "STRMLEN"

builder_catchments_sample$ZONE <- as.character(builder_catchments_sample$ZONE)
builder_catchments_sample$BASIN <- as.character(builder_catchments_sample$BASIN)
builder_catchments_sample$Isolated <- as.integer(builder_catchments_sample$Isolated)

usethis::use_data(builder_catchments_sample, overwrite = TRUE)


# get existing reserves in FDA 09EA
temp <- file.path(tempdir(), "CPCAD-BDCAPC_Dec2021.gdb.zip")
download.file("https://cws-scf.ca/CPCAD-BDCAPC_Dec2021.gdb.zip", temp) # unzip manually in temp file
reserves <- st_read(file.path(tempdir(), "CPCAD-BDCAPC_Dec2021.gdb/CPCAD-BDCAPC_Dec2021.gdb"), layer = "CPCAD_BDCAPC_Dec2021")

reserves <- st_transform(reserves, st_crs(builder_catchments_sample))
names(reserves)[names(reserves) == "Shape"] <- "geometry"
st_geometry(reserves)="geometry"

fda <- builder_catchments_sample %>%
  summarise(geometry = st_union(geometry))

reserves <- reserves[reserves$NAME_E == "Tombstone",]

reserves_fda <- reserves %>%
  st_intersection(fda) %>%
  st_cast("POLYGON") %>%
  select("NAME_E") %>%
  mutate(area_km2 = as.numeric(st_area(geometry))/1000000) %>%
  filter(area_km2 > 1)

reserves_fda$NAME_E <- c("Tombstone_1", "Tombstone_2", "Tombstone_3")
names(reserves_fda)[names(reserves_fda) == "NAME_E"] <- "reserve"
reserves_fda <- reserves_fda[c("reserve", "area_km2", "geometry")]
existing_reserves_sample <- reserves_fda
row.names(existing_reserves_sample) <- c("1","2","3")

usethis::use_data(existing_reserves_sample, overwrite = TRUE)
