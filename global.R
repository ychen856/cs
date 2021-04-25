library(shiny)
library(sf)
library(tigris)
library(dplyr)
library(colorspace)

options(tigris_use_cache = TRUE)
mapviewOptions(legend.pos = "bottomright")
#2010 usage data
usage_2010_df <- read.csv("Energy_Usage_2010.csv", sep = ",", header = TRUE)
################### read file ####################
chicago_tract_df <- tracts(state = "IL", county = "Cook", year = "2010")
chicago_block_df <- blocks(state = "IL", county = "Cook", year = "2010")

usage_2010_df$GEOID10 <- usage_2010_df$CENSUS.BLOCK
usage_2010_df$TRACT.ID <- substr(usage_2010_df$GEOID10, 1, 11)
################## update data type #################
usage_2010_df$CENSUS.BLOCK <- as.character(usage_2010_df$CENSUS.BLOCK)

usage_2010_df$GEOID10 <- usage_2010_df$CENSUS.BLOCK

################ ui options ####################
community_area_dist <- unique(usage_2010_df$COMMUNITY.AREA.NAME)
building_type_dist <- unique(usage_2010_df$BUILDING.TYPE)
