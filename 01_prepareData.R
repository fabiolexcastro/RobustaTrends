
# -------------------------------------------------------------------------
# Author: Fabio A. Castro
# Web: fabiolexcastrosig.com
# July 2020
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, trend, tidyverse, sf, hablar, foreach, doSNOW, parallel)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use --------------------------------------------------------
extMskNC <- function(vr, yr){
  
  # vr <- 'pet'
  # yr <- 1980
  
  fle <- grep(vr, fls, value = T) %>% grep(yr, ., value = T)
  bck <- raster::brick(fle)
  nms <- names(bck) %>% gsub('\\.', '_', .)
  print(paste0('To convert to velox object ', vr, ' ', yr))
  bck <- bck %>% raster::crop(bff)
  nms <- names(bck)
  bck <- unstack(bck)
  Map('writeRaster', x = bck, filename = paste0('../tif/zne/', vr, '_', nms, '.asc'))
  print('Done!')
}


# Load data ---------------------------------------------------------------

# Presences
occ <- read_csv('../tbl/Robusta_global.csv')
shp <- st_read('../shp/all_countries.shp')
shp <- shp %>% dplyr::select(ENGLISH)

# Climate
pth <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_guatemala/_data/_nc/_world'
fls <- list.files(pth, full.names = T, pattern = '.nc$')
yrs <- basename(fls) %>% str_sub(., start = nchar(.) - 6, end = nchar(.) - 3) %>% unique()
vrs <- basename(fls) %>% str_sub(., start = 14, end = nchar(.) - 8) %>% unique()

# Table to shapefile ------------------------------------------------------
occ <- st_as_sf(x = occ, coords = c('lon', 'lat'))
st_crs(occ) <- st_crs(shp)

# Intersection between points and shapefile -------------------------------
occ_shp <- st_intersection(x = occ, y = shp)
cnt <- pull(occ_shp, 2) %>% unique() %>% as.character()

# To make a buffer --------------------------------------------------------
occ <- st_transform(x = occ, crs = '+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=60 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
bff <- st_buffer(x = occ, dist = 10000)
bff <- st_transform(x = occ, crs = st_crs(shp))

st_write(obj = bff, dsn = '../shp', layer = 'buffer_occ_10k', driver = 'ESRI Shapefile')

# Cutting the nc files ----------------------------------------------------

# Precipitation
cl <- makeCluster(5)
registerDoSNOW(cl)

ppt <- foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'stringr'), .verbose = TRUE) %dopar% {
  extMskNC(vr = 'ppt', yr = yrs[i])
}
stopCluster(cl)

# Minimum temperature
cl <- makeCluster(8)
registerDoSNOW(cl)

tmin <- foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'stringr'), .verbose = TRUE) %dopar% {
  extMskNC(vr = 'tmin', yr = yrs[i])
}
stopCluster(cl)

# Maximum temperature
cl <- makeCluster(5)
registerDoSNOW(cl)

tmax <- foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'stringr'), .verbose = TRUE) %dopar% {
  extMskNC(vr = 'tmax', yr = yrs[i])
}
stopCluster(cl)
