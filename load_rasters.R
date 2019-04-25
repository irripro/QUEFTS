# LOAD RASTERS

# Load Soil nutrients information (Rasters)
soilC <- raster('data2/soilC5.tif') #('data/TZA_ORCDRC_T__M_sd1_1km.tif')
soilPolsen <- soilC
soilPolsen[] <- 15
soilK <- raster('data2/soilK5.tif') #('data/TZA_EXKX_T__M_xd1_1km.tif')
soilpH <- raster('data2/soilpH5.tif') #('data/TZA_PHIHOX_T__M_sd1_1km.tif') / 10

# Crop predictions
WY <- raster('data2/WY5.tif') #('data/GYGAClimateZones_TZA_YW.tif') %>% resample(soilC) #'data/africa_1km_crop_TZA.tif'

# Boundary Raster
TZA_boundary <- shapefile('data/laea_TZA.shp')