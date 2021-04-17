

#2010 usage data
usage_2010_df <- read.csv("Energy_Usage_2010.csv", sep = ",", header = TRUE)
#2010 boundaries data
chicago_area_df <- sf::read_sf("Comm_20Areas/CommAreas.shp")
chicago_tract_df_2 <- sf::read_sf("CensusTracts2010/CensusTractsTIGER2010.shp")
#chicago_block_df <- sf::read_sf("CensusBlocks2010/CensusBlockTIGER2010.shp")
################### read file ####################
chicago_tract_df <- tracts(state = "IL", county = "Cook", year = "2010")
chicago_block_df <- blocks(state = "IL", county = "Cook", year = "2010")

chicago_area_df$COMMAREA <- chicago_area_df$AREA_NUMBE
chicago_area_df$geometry <- NULL

usage_2010_df$GEOID10 <- usage_2010_df$CENSUS.BLOCK
usage_2010_df$TRACT.ID <- substr(usage_2010_df$GEOID10, 1, 10)
################## update data type #################
usage_2010_df$CENSUS.BLOCK <- as.character(usage_2010_df$CENSUS.BLOCK)

usage_2010_df$GEOID10 <- usage_2010_df$CENSUS.BLOCK

################## get area code mapping table ###################
area_tract_mapping_df <- chicago_tract_df_2 %>% left_join(chicago_area_df)
area_tract_mapping_df <- area_tract_mapping_df[-c(1:3, 5:6, 8:15, 17:19)]
area_tract_mapping_df$geometry <- NULL

################ ui options ####################
community_area_dist <- unique(usage_2010_df$COMMUNITY.AREA.NAME)
building_type_dist <- unique(usage_2010_df$BUILDING.TYPE)