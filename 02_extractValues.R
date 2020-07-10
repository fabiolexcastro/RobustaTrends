

# -------------------------------------------------------------------------
# Author: Fabio A. Castro
# Web: fabiolexcastrosig.com
# July 2020
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, trend, tidyverse, sf, hablar, dismo, foreach, doSNOW, parallel)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use --------------------------------------------------------
create_bios <- function(yr){
  
  # yr <- yrs[1]
  
  fl <- grep(yr, fls, value = T) 
  fl <- grep(paste0(vrs, collapse = '|'), fl, value = T)
  
  pt <- grep('ppt', fl, value = T) %>% stack()
  tx <- grep('tmax', fl, value = T) %>% stack()
  tn <- grep('tmin', fl, value = T) %>% stack()
  
  bs <- biovars(prec = pt, tmin = tn, tmax = tx)
  Map('writeRaster', x = unstack(bs), filename = paste0('../tif/bios/bio_', 1:19, '_', yr, '.tif'))
  return(bs)
  
}

# Load data ---------------------------------------------------------------
fls <- list.files('../tif/zne', full.names = T, pattern = '.asc$')
occ <- read_csv('../tbl/Robusta_global.csv')
shp <- st_read('../shp/all_countries.shp')
shp <- shp %>% dplyr::select(ENGLISH)

vrs <- c('ppt', 'tmax', 'tmin')
yrs <- basename(fls) %>% grep('tmax', ., value = TRUE) %>%  str_sub(., start = 7, end = 10) %>% unique()

# To calc the bioclim variables -------------------------------------------
cl <- makeCluster(4)
registerDoSNOW(cl)

tmin <- foreach(i = 2:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'stringr', 'dismo'), .verbose = TRUE) %dopar% {
  create_bios(yr = yrs[i])
}

stopCluster(cl)

