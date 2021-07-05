# This code reclassifies cropscape layers and populates rasters with the
# corresponding amounts of pesticide used

# load packages
library(rgdal)
library(terra)
library(tidyverse)
library(tictoc)

##############################
###SET UP GLOBAL PARAMETERS###
##############################

# set working dir
setwd('/Volumes/SSD/pesticide_wbb')

# load shpfile of us states
states_shp <- vect('cb_2018_us_state_500k/cb_2018_us_state_500k.shp')

# load cropscape rasters
crop_files <- list.files(path = 'cropscape', pattern = glob2rx('*.img'), 
                         full.names = T, recursive = T)

crop_ras <- lapply(crop_files, function(x) rast(x))

rm(crop_files)

# reproject shapefile of us states
states_shp <- terra::project(states_shp, crop_ras[[1]])

# create list of us states to process
states <- c('WA', 'OR', 'CA', 'AZ', 'NV', 'ID', 'MT', 'UT', 'AZ', 'NM', 'CO',
            'WY', 'MT', 'ND', 'SD', 'NE')

################
###Herbicides###
################

# CHECK SECTIONS BELOW!!!

# read reference info csv
ref <- read.csv('reference/beetox_H_cdl_reclass.20210607.csv')

# set column name with crop ID values
crop_id <- 'value'

# set column name with pesticide values
pest_val <- 'kg_ha'

# set output name prefix, no spaces!!
out_pref <- 'herbicides'

# CHECK SECTIONS ABOVE!!!

# loop through states
for(st in states){
  
  # subset shapefile for single state
  state_shp <- subset(states_shp, states_shp$STUSPS == st)
  
  # subset raster stack for single state
  crop_ras_state <- lapply(crop_ras, function(x){
    crop(x, state_shp) %>% mask(., state_shp)
  })
  
  # loop through years to classify
  crop_ras_state <- lapply(seq_along(crop_ras_state), function(x){
    
    # set year starting in 2008
    yr <- 2007 + x
    
    # create class matrix
    class <- ref %>% filter(state_alpha == st & ref$year == yr) %>%
      select(all_of(crop_id), all_of(pest_val)) %>% as.matrix
    
    # add NaN class to output as NaN
    class <- rbind(class, c(NaN, NA))
    
    # classify individual year bands
    classify(crop_ras_state[[x]], class)
    
  })
  
  # merge as raster stack
  crop_ras_state <- rast(crop_ras_state)
  
  # write as stack
  writeRaster(crop_ras_state, filename = str_c('output/', out_pref, '_', st, '_2008_2014.tif'))
  
  # remove temp files
  tmpFiles(remove = TRUE)
}

####################
###Chlorothalonil###
####################

# CHECK SECTIONS BELOW!!!

# read reference info csv
ref <- read.csv('reference/CHLOROTHALONIL.csv')

# set column name with crop ID values
crop_id <- 'value'

# set column name with pesticide values
pest_val <- 'kg_ha'

# set output name prefix, no spaces!!
out_pref <- 'chlorothalonil'

# CHECK SECTIONS ABOVE!!!

# loop through states
for(st in states){
  
  # subset shapefile for single state
  state_shp <- subset(states_shp, states_shp$STUSPS == st)
  
  # subset raster stack for single state
  crop_ras_state <- lapply(crop_ras, function(x){
    crop(x, state_shp) %>% mask(., state_shp)
  })
  
  # loop through years to classify
  crop_ras_state <- lapply(seq_along(crop_ras_state), function(x){
    
    # set year starting in 2008
    yr <- 2007 + x
    
    # create class matrix
    class <- ref %>% filter(state_alpha == st & ref$year == yr) %>%
      select(all_of(crop_id), all_of(pest_val)) %>% as.matrix
    
    # add NaN class to output as NaN
    class <- rbind(class, c(NaN, NA))
    
    # classify individual year bands
    classify(crop_ras_state[[x]], class)
    
  })
  
  # merge as raster stack
  crop_ras_state <- rast(crop_ras_state)
  
  # write as stack
  writeRaster(crop_ras_state, filename = str_c('output/', out_pref, '_', st, '_2008_2014.tif'))
  
  # remove temp files
  tmpFiles(remove = TRUE)
}

################
###Fungicides###
################

# CHECK SECTIONS BELOW!!!

# read reference info csv
ref <- read.csv('reference/beetox_F_cdl_reclass.20210607.csv')

# set column name with crop ID values
crop_id <- 'value'

# set column name with pesticide values
pest_val <- 'kg_ha'

# set output name prefix, no spaces!!
out_pref <- 'fungicides'

# CHECK SECTIONS ABOVE!!!

# loop through states
for(st in states){
  
  # subset shapefile for single state
  state_shp <- subset(states_shp, states_shp$STUSPS == st)
  
  # subset raster stack for single state
  crop_ras_state <- lapply(crop_ras, function(x){
    crop(x, state_shp) %>% mask(., state_shp)
  })
  
  # loop through years to classify
  crop_ras_state <- lapply(seq_along(crop_ras_state), function(x){
    
    # set year starting in 2008
    yr <- 2007 + x
    
    # create class matrix
    class <- ref %>% filter(state_alpha == st & ref$year == yr) %>%
      select(all_of(crop_id), all_of(pest_val)) %>% as.matrix
    
    # add NaN class to output as NaN
    class <- rbind(class, c(NaN, NA))
    
    # classify individual year bands
    classify(crop_ras_state[[x]], class)
    
  })
  
  # merge as raster stack
  crop_ras_state <- rast(crop_ras_state)
  
  # write as stack
  writeRaster(crop_ras_state, filename = str_c('output/', out_pref, '_', st, '_2008_2014.tif'))
  
  # remove temp files
  tmpFiles(remove = TRUE)
}

##########################
###neonicotinoids cyano###
##########################

# CHECK SECTIONS BELOW!!!

# read reference info csv
ref <- read.csv('reference/beetox_neonic_cdl_reclass.20210607.csv')

# set column name with crop ID values
crop_id <- 'value'

# set column name with pesticide values
pest_val <- 'kg_ha_cyano'

# set output name prefix, no spaces!!
out_pref <- 'neonicotinoids_cyano'

# CHECK SECTIONS ABOVE!!!

# loop through states
for(st in states){
  
  # subset shapefile for single state
  state_shp <- subset(states_shp, states_shp$STUSPS == st)
  
  # subset raster stack for single state
  crop_ras_state <- lapply(crop_ras, function(x){
    crop(x, state_shp) %>% mask(., state_shp)
  })
  
  # loop through years to classify
  crop_ras_state <- lapply(seq_along(crop_ras_state), function(x){
    
    # set year starting in 2008
    yr <- 2007 + x
    
    # create class matrix
    class <- ref %>% filter(state_alpha == st & ref$year == yr) %>%
      select(all_of(crop_id), all_of(pest_val)) %>% as.matrix
    
    # add NaN class to output as NaN
    class <- rbind(class, c(NaN, NA))
    
    # classify individual year bands
    classify(crop_ras_state[[x]], class)
    
  })
  
  # merge as raster stack
  crop_ras_state <- rast(crop_ras_state)
  
  # write as stack
  writeRaster(crop_ras_state, filename = str_c('output/', out_pref, '_', st, '_2008_2014.tif'))
  
  # remove temp files
  tmpFiles(remove = TRUE)
}

##########################
###neonicotinoids nitro###
##########################

# CHECK SECTIONS BELOW!!!

# read reference info csv
ref <- read.csv('reference/beetox_neonic_cdl_reclass.20210607.csv')

# set column name with crop ID values
crop_id <- 'value'

# set column name with pesticide values
pest_val <- 'kg_ha_nitro'

# set output name prefix, no spaces!!
out_pref <- 'neonicotinoids_nitro'

# CHECK SECTIONS ABOVE!!!

# loop through states
for(st in states){
  
  # subset shapefile for single state
  state_shp <- subset(states_shp, states_shp$STUSPS == st)
  
  # subset raster stack for single state
  crop_ras_state <- lapply(crop_ras, function(x){
    crop(x, state_shp) %>% mask(., state_shp)
  })
  
  # loop through years to classify
  crop_ras_state <- lapply(seq_along(crop_ras_state), function(x){
    
    # set year starting in 2008
    yr <- 2007 + x
    
    # create class matrix
    class <- ref %>% filter(state_alpha == st & ref$year == yr) %>%
      select(all_of(crop_id), all_of(pest_val)) %>% as.matrix
    
    # add NaN class to output as NaN
    class <- rbind(class, c(NaN, NA))
    
    # classify individual year bands
    classify(crop_ras_state[[x]], class)
    
  })
  
  # merge as raster stack
  crop_ras_state <- rast(crop_ras_state)
  
  # write as stack
  writeRaster(crop_ras_state, filename = str_c('output/', out_pref, '_', st, '_2008_2014.tif'))
  
  # remove temp files
  tmpFiles(remove = TRUE)
}

####################
###neonicotinoids###
####################

# CHECK SECTIONS BELOW!!!

# read reference info csv
ref <- read.csv('reference/beetox_neonic_cdl_reclass.20210607.csv')

# set column name with crop ID values
crop_id <- 'value'

# set column name with pesticide values
pest_val <- 'kg_ha'

# set output name prefix, no spaces!!
out_pref <- 'neonicotinoids'

# CHECK SECTIONS ABOVE!!!

# loop through states
for(st in states){
  
  # subset shapefile for single state
  state_shp <- subset(states_shp, states_shp$STUSPS == st)
  
  # subset raster stack for single state
  crop_ras_state <- lapply(crop_ras, function(x){
    crop(x, state_shp) %>% mask(., state_shp)
  })
  
  # loop through years to classify
  crop_ras_state <- lapply(seq_along(crop_ras_state), function(x){
    
    # set year starting in 2008
    yr <- 2007 + x
    
    # create class matrix
    class <- ref %>% filter(state_alpha == st & ref$year == yr) %>%
      select(all_of(crop_id), all_of(pest_val)) %>% as.matrix
    
    # add NaN class to output as NaN
    class <- rbind(class, c(NaN, NA))
    
    # classify individual year bands
    classify(crop_ras_state[[x]], class)
    
  })
  
  # merge as raster stack
  crop_ras_state <- rast(crop_ras_state)
  
  # write as stack
  writeRaster(crop_ras_state, filename = str_c('output/', out_pref, '_', st, '_2008_2014.tif'))
  
  # remove temp files
  tmpFiles(remove = TRUE)
}

########################
###toxic loading oral###
########################

# CHECK SECTIONS BELOW!!!

# read reference info csv
ref <- read.csv('reference/beetox_I_cdl_reclass.20210607.csv')

# set column name with crop ID values
crop_id <- 'value'

# set column name with pesticide values
pest_val <- 'ld50_or_ha_bil'

# set output name prefix, no spaces!!
out_pref <- 'toxic_loading_oral'

# CHECK SECTIONS ABOVE!!!

# loop through states
for(st in states){
  
  # subset shapefile for single state
  state_shp <- subset(states_shp, states_shp$STUSPS == st)
  
  # subset raster stack for single state
  crop_ras_state <- lapply(crop_ras, function(x){
    crop(x, state_shp) %>% mask(., state_shp)
  })
  
  # loop through years to classify
  crop_ras_state <- lapply(seq_along(crop_ras_state), function(x){
    
    # set year starting in 2008
    yr <- 2007 + x
    
    # create class matrix
    class <- ref %>% filter(state_alpha == st & ref$year == yr) %>%
      select(all_of(crop_id), all_of(pest_val)) %>% as.matrix
    
    # add NaN class to output as NaN
    class <- rbind(class, c(NaN, NA))
    
    # classify individual year bands
    classify(crop_ras_state[[x]], class)
    
  })
  
  # merge as raster stack
  crop_ras_state <- rast(crop_ras_state)
  
  # write as stack
  writeRaster(crop_ras_state, filename = str_c('output/', out_pref, '_', st, '_2008_2014.tif'))
  
  # remove temp files
  tmpFiles(remove = TRUE)
}

###########################
###toxic loading contact###
###########################

# CHECK SECTIONS BELOW!!!

# read reference info csv
ref <- read.csv('reference/beetox_I_cdl_reclass.20210607.csv')

# set column name with crop ID values
crop_id <- 'value'

# set column name with pesticide values
pest_val <- 'ld50_ct_ha_bil'

# set output name prefix, no spaces!!
out_pref <- 'toxic_loading_contact'

# CHECK SECTIONS ABOVE!!!

# loop through states
for(st in states){
  
  # subset shapefile for single state
  state_shp <- subset(states_shp, states_shp$STUSPS == st)
  
  # subset raster stack for single state
  crop_ras_state <- lapply(crop_ras, function(x){
    crop(x, state_shp) %>% mask(., state_shp)
  })
  
  # loop through years to classify
  crop_ras_state <- lapply(seq_along(crop_ras_state), function(x){
    
    # set year starting in 2008
    yr <- 2007 + x
    
    # create class matrix
    class <- ref %>% filter(state_alpha == st & ref$year == yr) %>%
      select(all_of(crop_id), all_of(pest_val)) %>% as.matrix
    
    # add NaN class to output as NaN
    class <- rbind(class, c(NaN, NA))
    
    # classify individual year bands
    classify(crop_ras_state[[x]], class)
    
  })
  
  # merge as raster stack
  crop_ras_state <- rast(crop_ras_state)
  
  # write as stack
  writeRaster(crop_ras_state, filename = str_c('output/', out_pref, '_', st, '_2008_2014.tif'))
  
  # remove temp files
  tmpFiles(remove = TRUE)
}