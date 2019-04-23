# LOAD RASTERS

# Load Soil nutrients information (Rasters)
soilC <- raster('data/TZA_ORCDRC_T__M_sd1_1km.tif')
soilPolsen <- soilC
soilPolsen[] <- 15
soilK <- raster('data/TZA_EXKX_T__M_xd1_1km.tif')
soilpH <- raster('data/TZA_PHIHOX_T__M_sd1_1km.tif') / 10

# Crop predictions
WY <- raster('data/GYGAClimateZones_TZA_YW.tif') %>% resample(soilC) #'data/africa_1km_crop_TZA.tif'

# Boundary Raster
TZA_boundary <- shapefile('D:/Sebastian/QUEFTS/data/laea_TZA.shp')