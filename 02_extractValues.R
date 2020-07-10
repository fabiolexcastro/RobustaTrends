

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
extract_values <- function(bio){
  
  fle <- grep(bio, fls , value = T)
  stk <- stack(fle)
  vls <- raster::extract(stk, occ[,2:3])
  vls <- as.data.frame(vls)
  vls <- cbind(occ[,1:3], vls)
  vls <- as_tibble(vls)
  vls <- vls %>% 
    gather(var, value, -OBJECTID, -lon, -lat) %>% 
    mutate(variable = str_sub(var, start = 1, end = nchar(var) - 5),
           year = str_sub(var, start = nchar(var) - 3, end = nchar(var)))
  return(vls)
}
calc_slope <- function(id, vr){
  
  vl <- vls %>% filter(variable == vr, OBJECTID == 1)
  s <- vl %>% pull(6) %>% sens.slope()
  p <- s$p.value
  i <- s$conf.int
  m <- s$estimates
  d <- data.frame(int_min = i[1], int_max = i[2], slope = m, valor_p = p, variable = as.character(vr), gid = id)
  print('Done!')
  return(d)
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

foreach(i = 2:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'stringr', 'dismo'), .verbose = TRUE) %dopar% {
  create_bios(yr = yrs[i])
}

stopCluster(cl)

# To extract the values from Bioclimatic variables ------------------------
fls <- list.files('../tif/bios', full.names = TRUE, pattern = '.tif')
bios <- c('bio_1_', 'bio_5_', 'bio_6_', 'bio_8_', 'bio_12_')

vls <- map(.x = bios, .f = extract_values)
dir.create('../rds')
vls <- bind_rows(vls)
vls <- vls %>% dplyr::select(OBJECTID, lon, lat, variable, year, value)
saveRDS(object = vls, file = '../rds/values_bios.rds')

ids <- unique(vls$OBJECTID)
vrs <- unique(vls$variable)

# To calc the slope -------------------------------------------------------
slp <- lapply(1:length(ids), function(x) lapply(1:length(vrs), function(y) calc_slope(id = ids[x], vr = vrs[y])))
slp <- flatten(slp)
slp <- bind_rows(slp)
saveRDS(object = slp, file = '../rds/slope.rds')
