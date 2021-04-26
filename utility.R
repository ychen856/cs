source("global.R")
library(mapview)
library(colorspace)

getData <- function(area, option, month, bType) {
  print("======= get data start =======")
  print(area)
  print(option)
  print(month)
  print(bType)
  print("======= get data end =======")
  
  #extract area
  #area_usage_df <- subset(usage_2010_df, substr(GEOID10, 1, 11) %in% unique(subset(area_tract_mapping_df, tolower(COMMUNITY) == tolower(area))$GEOID10))
  area_usage_df <- subset(usage_2010_df, str_replace_all(string=tolower(COMMUNITY.AREA.NAME), pattern=" ", repl="") == str_replace_all(string=tolower(area), pattern=" ", repl=""))
  selected_block_df <- subset(chicago_block_df, GEOID10 %in% area_usage_df$GEOID10)
  
  
  #building type
  if(!"All" %in% bType) {
    area_usage_df <- subset(area_usage_df, BUILDING.TYPE %in% bType)
  }
  
  #sum rows by option
  if(nrow(area_usage_df) < 1)
    return ("")
  
  if (option == "Electricity") {
    #pick column by month
    if (month == "January") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.JANUARY.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.JANUARY.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "February") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.FEBRUARY.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.FEBRUARY.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "March") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.MARCH.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.MARCH.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "April") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.APRIL.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.APRIL.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "May") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.MAY.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.MAY.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "June") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.JUNE.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.JUNE.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "July") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.JULY.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.JULY.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "August") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.AUGUST.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.AUGUST.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "September") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.SEPTEMBER.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.SEPTEMBER.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "October") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.OCTOBER.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.OCTOBER.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "November") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.NOVEMBER.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.NOVEMBER.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "December") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.DECEMBER.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.DECEMBER.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "All") {
      area_usage_df <- subset(area_usage_df, !is.na(TOTAL.KWH))
      area_usage_df <- aggregate(area_usage_df$TOTAL.KWH, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
  }
  else if (option == "Gas") {
    #pick column by month
    if (month == "January") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.JANUARY.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.JANUARY.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "February") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.FEBRUARY.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.FEBRUARY.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "March") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.MARCH.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.MARCH.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "April") {
      area_usage_df <- subset(area_usage_df, !is.na(TERM.APRIL.2010))
      area_usage_df <- aggregate(area_usage_df$TERM.APRIL.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "May") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.MAY.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.MAY.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "June") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.JUNE.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.JUNE.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "July") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.JULY.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.JULY.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "August") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.AUGUST.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.AUGUST.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "September") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.SEPTEMBER.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.SEPTEMBER.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "October") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.OCTOBER.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.OCTOBER.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "November") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.NOVEMBER.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.NOVEMBER.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "December") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.DECEMBER.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.DECEMBER.2010, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "All") {
      area_usage_df <- subset(area_usage_df, !is.na(TOTAL.THERMS))
      area_usage_df <- aggregate(area_usage_df$TOTAL.THERMS, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
  }
  else if (option == "Building Type") {
    #print(area_usage_df)
    area_usage_df <- area_usage_df[, c(2:3)]
    area_usage_df <- area_usage_df[!duplicated(area_usage_df), ]
    #print(area_usage_df)
    #area_usage_df_temp <- aggregate(BUILDING.TYPE ~ CENSUS.BLOCK, data = area_usage_df, c)
    #area_usage_df <- rbind(area_usage_df, area_usage_df_temp)
    #print(area_usage_df)
    #near_west_side_type_sum_df <- aggregate(BUILDING.TYPE ~ CENSUS.BLOCK, data = near_west_side_usage_df, c)
  }
  else if (option == "Building Age") {
    area_usage_df <- aggregate(area_usage_df$AVERAGE.BUILDING.AGE, by=list(area_usage_df$CENSUS.BLOCK), FUN=mean, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
  }
  else if (option == "Building Height") {
    area_usage_df <- aggregate(area_usage_df$AVERAGE.STORIES, by=list(area_usage_df$CENSUS.BLOCK), FUN=mean, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
  }
  else if (option == "Total Population") {
    area_usage_df <- aggregate(area_usage_df$TOTAL.POPULATION, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
  }
  else if(option == "Average House Size") {
    area_usage_df <- aggregate(area_usage_df$AVERAGE.HOUSESIZE, by=list(area_usage_df$CENSUS.BLOCK), FUN=mean, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
  }
  else if(option == "Total Units") {
    area_usage_df <- aggregate(area_usage_df$TOTAL.UNITS, by=list(area_usage_df$CENSUS.BLOCK), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
  }
  
  names(area_usage_df)[1] <- c("GEOID10")
  names(area_usage_df)[2] <- c("AMOUNT")
  
  #selected_block_df <- subset(chicago_block_df, substr(GEOID10, 1, 11) %in% unique(subset(area_tract_mapping_df, str_replace_all(string=tolower(COMMUNITY), pattern=" ", repl="") == str_replace_all(string=tolower(area), pattern=" ", repl=""))$GEOID10))
  
  usage_block_df <- selected_block_df %>% left_join(area_usage_df)
  
  #print(nrow(usage_block_df))
  #print(usage_block_df)
  return (usage_block_df)
}


getPlotData <- function(area) {
  usage_id_df <- subset(usage_2010_df, COMMUNITY.AREA.NAME == area)
  
  if(nrow(usage_id_df) < 1)
    return (NULL)
  plot_df <- data.frame(matrix(ncol = 3, nrow = 0))
  plot_col <- c("AMOUNT", "SOURCE", "MONTH")
  colnames(plot_df) <- plot_col
  
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'KWH.JANUARY.2010'], na.rm = TRUE), "Electricity", "1"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'KWH.FEBRUARY.2010'], na.rm = TRUE), "Electricity", "2"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'KWH.MARCH.2010'], na.rm = TRUE), "Electricity", "3"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'KWH.APRIL.2010'], na.rm = TRUE), "Electricity", "4"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'KWH.MAY.2010'], na.rm = TRUE), "Electricity", "5"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'KWH.JUNE.2010'], na.rm = TRUE), "Electricity", "6"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'KWH.JULY.2010'], na.rm = TRUE), "Electricity", "7"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'KWH.AUGUST.2010'], na.rm = TRUE), "Electricity", "8"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'KWH.SEPTEMBER.2010'], na.rm = TRUE), "Electricity", "9"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'KWH.OCTOBER.2010'], na.rm = TRUE), "Electricity", "10"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'KWH.NOVEMBER.2010'], na.rm = TRUE), "Electricity", "11"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'KWH.DECEMBER.2010'], na.rm = TRUE), "Electricity", "12"))
  
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'THERM.JANUARY.2010'], na.rm = TRUE), "Gas", "1"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'THERM.FEBRUARY.2010'], na.rm = TRUE), "Gas", "2"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'THERM.MARCH.2010'], na.rm = TRUE), "Gas", "3"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'TERM.APRIL.2010'], na.rm = TRUE), "Gas", "4"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'THERM.MAY.2010'], na.rm = TRUE), "Gas", "5"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'THERM.JUNE.2010'], na.rm = TRUE), "Gas", "6"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'THERM.JULY.2010'], na.rm = TRUE), "Gas", "7"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'THERM.AUGUST.2010'], na.rm = TRUE), "Gas", "8"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'THERM.SEPTEMBER.2010'], na.rm = TRUE), "Gas", "9"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'THERM.OCTOBER.2010'], na.rm = TRUE), "Gas", "10"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'THERM.NOVEMBER.2010'], na.rm = TRUE), "Gas", "11"))
  plot_df <- rbind(plot_df, c(sum(usage_id_df[, 'THERM.DECEMBER.2010'], na.rm = TRUE), "Gas", "12"))
  
  
  names(plot_df)[1] <- c("AMOUNT")
  names(plot_df)[2] <- c("SOURCE")
  names(plot_df)[3] <- c("MONTH")
  plot_df$AMOUNT=as.numeric(plot_df$AMOUNT)
  plot_df$MONTH=as.numeric(plot_df$MONTH)
  
  
  return (plot_df)
}

getPlotData_t <- function() {
  plot_df <- data.frame(matrix(ncol = 3, nrow = 0))
  plot_col <- c("AMOUNT", "SOURCE", "MONTH")
  colnames(plot_df) <- plot_col
  
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'KWH.JANUARY.2010'], na.rm = TRUE), "Electricity", "1"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'KWH.FEBRUARY.2010'], na.rm = TRUE), "Electricity", "2"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'KWH.MARCH.2010'], na.rm = TRUE), "Electricity", "3"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'KWH.APRIL.2010'], na.rm = TRUE), "Electricity", "4"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'KWH.MAY.2010'], na.rm = TRUE), "Electricity", "5"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'KWH.JUNE.2010'], na.rm = TRUE), "Electricity", "6"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'KWH.JULY.2010'], na.rm = TRUE), "Electricity", "7"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'KWH.AUGUST.2010'], na.rm = TRUE), "Electricity", "8"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'KWH.SEPTEMBER.2010'], na.rm = TRUE), "Electricity", "9"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'KWH.OCTOBER.2010'], na.rm = TRUE), "Electricity", "10"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'KWH.NOVEMBER.2010'], na.rm = TRUE), "Electricity", "11"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'KWH.DECEMBER.2010'], na.rm = TRUE), "Electricity", "12"))
  
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'THERM.JANUARY.2010'], na.rm = TRUE), "Gas", "1"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'THERM.FEBRUARY.2010'], na.rm = TRUE), "Gas", "2"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'THERM.MARCH.2010'], na.rm = TRUE), "Gas", "3"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'TERM.APRIL.2010'], na.rm = TRUE), "Gas", "4"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'THERM.MAY.2010'], na.rm = TRUE), "Gas", "5"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'THERM.JUNE.2010'], na.rm = TRUE), "Gas", "6"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'THERM.JULY.2010'], na.rm = TRUE), "Gas", "7"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'THERM.AUGUST.2010'], na.rm = TRUE), "Gas", "8"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'THERM.SEPTEMBER.2010'], na.rm = TRUE), "Gas", "9"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'THERM.OCTOBER.2010'], na.rm = TRUE), "Gas", "10"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'THERM.NOVEMBER.2010'], na.rm = TRUE), "Gas", "11"))
  plot_df <- rbind(plot_df, c(sum(usage_2010_df[, 'THERM.DECEMBER.2010'], na.rm = TRUE), "Gas", "12"))
 
  
  names(plot_df)[1] <- c("AMOUNT")
  names(plot_df)[2] <- c("SOURCE")
  names(plot_df)[3] <- c("MONTH")
  plot_df$AMOUNT=as.numeric(plot_df$AMOUNT)
  plot_df$MONTH=as.numeric(plot_df$MONTH)
  
  return (plot_df)
}


getMaxAmount <- function(df) {
  return (max(df$AMOUNT, na.rm = TRUE))
}

generateMap <- function(color, option, area, df) {
  if(color == "Set 1") {
    blue2 = colorRampPalette(c('#ffffd9', '#edf8b1', '#c7e9b4', '#7fcdbb', '#41b6c4', '#1d91c0', '#225ea8', '#253494', '#081d58'))
    red1 = colorRampPalette(c('#3288bd', '#66c2a5', '#abdda4', '#e6f598', '#ffffbf', '#fee08b', '#fdae61', '#f46d43', '#d53e4f'))
    purple2 = colorRampPalette(c('#f7fcfd', '#e0ecf4', '#bfd3e6', '#9ebcda', '#8c96c6', '#8c6bb1', '#88419d', '#810f7c', '#4d004b'))
    green2 = colorRampPalette(c('#fff7fb', '#ece2f0', '#d0d1e6', '#a6bddb', '#67a9cf', '#3690c0', '#02818a', '#016c59', '#014636'))
    red2 = colorRampPalette(c('#ffffcc', '#ffeda0', '#fed976', '#feb24c', '#fd8d3c', '#fc4e2a', '#e31a1c', '#bd0026', '#800026'))
    pink2 = colorRampPalette(c('#f7f4f9', '#e7e1ef', '#d4b9da', '#c994c7', '#df65b0', '#e7298a', '#ce1256', '#980043', '#67001f'))
    green1 = colorRampPalette(c('#f7fcfd', '#e5f5f9', '#ccece6', '#99d8c9', '#66c2a4', '#41ae76', '#238b45', '#006d2c', '#00441b'))
    cyan1 = colorRampPalette(c('#fff7fb', '#ece7f2', '#d0d1e6', '#a6bddb', '#74a9cf', '#3690c0', '#0570b0', '#045a8d', '#023858'))
    brown1 = colorRampPalette(c('#ffffe5', '#fff7bc', '#fee391', '#fec44f', '#fe9929', '#ec7014', '#cc4c02', '#993404', '#662506'))
  }
  else if(color == "Set 2") {
    blue2 = heat_hcl(12, h=c(75, 40), c=c(40, 80), power = 1)
    red1 = heat_hcl(12, c=c(80, 30), power = c(1/5, 1.5))
    purple2 = diverge_hcl(12, h=c(255, 330), l = c(40, 90))
    green2 = terrain_hcl(12, c=c(65, 0), l=c(45, 95), power=c(1/3, 1.5))
    red2 = diverge_hcl(12, c=100, l=c(50, 90), power = 1)
    pink2 = diverge_hcl(12, h=c(128, 330), c=98, l=c(65, 90))
    green1 = diverge_hcl(12, h=c(130, 43), c=100, l= c(70, 90))
    cyan1 = diverge_hcl(12, h=c(180, 70), c=70, l=c(90, 95))
    brown1 = diverging_hcl(12, h = c(246, 40), c=96)
  }
  else if(color == "Set 3") {
    blue2 = heat_hcl(12, h=c(75, 40), c=c(40, 80), power = 1, rev=TRUE)
    red1 = heat_hcl(12, c=c(80, 30), power = c(1/5, 1.5), rev = TRUE)
    purple2 = diverge_hcl(12, h=c(255, 330), l = c(40, 90), rev = TRUE)
    green2 = terrain_hcl(12, c=c(65, 0), l=c(45, 95), power=c(1/3, 1.5), rev = TRUE)
    red2 = diverge_hcl(12, c=100, l=c(50, 90), power = 1, rev = TRUE)
    pink2 = diverge_hcl(12, h=c(128, 330), c=98, l=c(65, 90), rev = TRUE)
    green1 = diverge_hcl(12, h=c(130, 43), c=100, l= c(70, 90), rev = TRUE)
    cyan1 = diverge_hcl(12, h=c(180, 70), c=70, l=c(90, 95), rev = TRUE)
    brown1 = diverging_hcl(12, h = c(246, 40), c=96, rev = TRUE)
  }
  
  if(option == "Electricity" || option == "10% Most Electricity") {
    mapview(df, zcol = "AMOUNT", layer.name = paste(area, 'Elec(KWH)'), col.regions = blue2)@map
  }
  else if (option == "Gas" || option =="10% Most Gas") {
    mapview(df, zcol = "AMOUNT", layer.name = paste(area, 'GAS(THERMS)'), col.regions = red1)@map
  }
  else if (option == "Building Type") {
    df$AMOUNT[length(df$AMOUNT) < 1] <- "Unknown"
    m <- NULL
    if(nrow(subset(df, AMOUNT == "Residential")) > 0) {
      m <- mapview(subset(df, AMOUNT == "Residential"), layer.name = "Residential", zcol = "AMOUNT", col.regions = "#F5793A")
    }
    if(nrow(subset(df, AMOUNT == "Commercial")) > 0) {
      if(is.null(m)) {
        m <- mapview(subset(df, AMOUNT == "Commercial"), zcol = "AMOUNT", layer.name = "Commercial", col.regions = "#A95AA1")
      }
      if(!is.null(m)) {
        m <- m + mapview(subset(df, AMOUNT == "Commercial"), zcol = "AMOUNT", layer.name = "Commercial", col.regions = "#A95AA1")
      }
    }
    if(nrow(subset(df, AMOUNT == "Industrial")) > 0) {
      if(is.null(m))
        m <- mapview(subset(df, AMOUNT == "Industrial"), zcol = "AMOUNT", layer.name = "Industrial", col.regions = "#85C0F9") 
      if(!is.null(m)) 
        m <- m + mapview(subset(df, AMOUNT == "Industrial"), zcol = "AMOUNT", layer.name = "Industrial", col.regions = "#85C0F9") 
    }
    if(nrow(subset(df, AMOUNT == "Unknown"))) {
      if(is.null(m))
        m <- mapview(subset(df, AMOUNT == "Unknown"), zcol = "AMOUNT", layer.name = "Unknown", col.regions = "grey") 
      if(!is.null(m)) 
        m <- m + mapview(subset(df, AMOUNT == "Unknown"), zcol = "AMOUNT", layer.name = "Unknown", col.regions = "grey") 
    }
    m <- m@map
  }
  else if (option == "Building Age" || option == "10% Oldest Buildings" || option == "10% Newest Buildings") {
    mapview(df, zcol = "AMOUNT", layer.name = 'Average Building Age', col.regions = purple2)@map
  }
  else if (option == "Building Height" || option == "10% Tallest Buildings") {
    mapview(df, zcol = "AMOUNT", layer.name = 'Average Building Height', col.regions = green2)@map
  }
  else if (option == "Total Population" || option == "10% Most Population") {
    mapview(df, zcol = "AMOUNT", layer.name = 'Total Population', col.regions = pink2)@map
  }
  else if(option == "10% Most Occupied") {
    mapview(df, zcol = "AMOUNT", layer.name = 'Average Occupied Rate', col.regions = green1)@map
  }
  else if (option == "10% Highest Renting Rate") {
    mapview(df, zcol = "AMOUNT", layer.name = 'Average Renting Rate', col.regions = red2)@map
  }
  else if (option == "Total Units") {
    mapview(df, zcol = "AMOUNT", layer.name = 'Total Units', col.regions = cyan1)@map
  }
  else if(option == "Average House Size") {
    mapview(df, zcol = "AMOUNT", layer.name = 'Average House Size', col.regions = brown1)@map
  }
}

getTrackData <- function(option, month, bType) {
  print("======= get data start =======")
  print(option)
  print(month)
  print("======= get data end =======")
  
  area_usage_df <- usage_2010_df
  selected_df <- subset(chicago_tract_df, GEOID10 %in% area_usage_df$TRACT.ID)
  
  if(!"All" %in% bType) {
    area_usage_df <- subset(area_usage_df, BUILDING.TYPE %in% bType)
  }
  
  
  if (option == "Electricity") {
    #pick column by month
    if (month == "January") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.JANUARY.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.JANUARY.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "February") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.FEBRUARY.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.FEBRUARY.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "March") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.MARCH.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.MARCH.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "April") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.APRIL.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.APRIL.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "May") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.MAY.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.MAY.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "June") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.JUNE.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.JUNE.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "July") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.JULY.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.JULY.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "August") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.AUGUST.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.AUGUST.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "September") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.SEPTEMBER.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.SEPTEMBER.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "October") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.OCTOBER.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.OCTOBER.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "November") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.NOVEMBER.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.NOVEMBER.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "December") {
      area_usage_df <- subset(area_usage_df, !is.na(KWH.DECEMBER.2010))
      area_usage_df <- aggregate(area_usage_df$KWH.DECEMBER.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
    else if (month == "All") {
      area_usage_df <- subset(area_usage_df, !is.na(TOTAL.KWH))
      area_usage_df <- aggregate(area_usage_df$TOTAL.KWH, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
  }
  else if(option == "10% Most Electricity") {
    area_usage_df <- subset(area_usage_df, !is.na(TOTAL.KWH))
    area_usage_df <- aggregate(area_usage_df$TOTAL.KWH, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    area_usage_df <- area_usage_df[order(-area_usage_df[2]),]
    area_usage_df <- head(area_usage_df, 0.1*nrow(area_usage_df))
  }
  else if (option == "Gas") {
    #pick column by month
    if (month == "January") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.JANUARY.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.JANUARY.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "February") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.FEBRUARY.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.FEBRUARY.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "March") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.MARCH.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.MARCH.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "April") {
      area_usage_df <- subset(area_usage_df, !is.na(TERM.APRIL.2010))
      area_usage_df <- aggregate(area_usage_df$TERM.APRIL.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "May") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.MAY.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.MAY.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "June") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.JUNE.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.JUNE.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "July") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.JULY.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.JULY.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "August") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.AUGUST.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.AUGUST.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "September") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.SEPTEMBER.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.SEPTEMBER.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "October") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.OCTOBER.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.OCTOBER.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "November") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.NOVEMBER.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.NOVEMBER.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "December") {
      area_usage_df <- subset(area_usage_df, !is.na(THERM.DECEMBER.2010))
      area_usage_df <- aggregate(area_usage_df$THERM.DECEMBER.2010, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=FALSE, na.action=NULL)
    }
    else if (month == "All") {
      area_usage_df <- subset(area_usage_df, !is.na(TOTAL.THERMS))
      area_usage_df <- aggregate(area_usage_df$TOTAL.THERMS, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    }
  }
  else if(option == "10% Most Gas") {
    area_usage_df <- subset(area_usage_df, !is.na(TOTAL.THERMS))
    area_usage_df <- aggregate(area_usage_df$TOTAL.THERMS, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    area_usage_df <- area_usage_df[order(-area_usage_df[2]),]
    area_usage_df <- head(area_usage_df, 0.1*nrow(area_usage_df))
  }
  else if (option == "Building Type") {
    #print(area_usage_df)
    area_usage_df <- area_usage_df[, c(75, 3)]
    area_usage_df <- area_usage_df[!duplicated(area_usage_df), ]
  }
  else if (option == "Building Age") {
    area_usage_df <- aggregate(area_usage_df$AVERAGE.BUILDING.AGE, by=list(area_usage_df$TRACT.ID), FUN=mean, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
  }
  else if (option == "10% Oldest Buildings") {
    area_usage_df <- aggregate(area_usage_df$AVERAGE.BUILDING.AGE, by=list(area_usage_df$TRACT.ID), FUN=mean, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    area_usage_df <- area_usage_df[order(-area_usage_df[2]),]
    area_usage_df <- head(area_usage_df, 0.1*nrow(area_usage_df))
  }
  else if (option == "10% Newest Buildings") {
    area_usage_df <- aggregate(area_usage_df$AVERAGE.BUILDING.AGE, by=list(area_usage_df$TRACT.ID), FUN=mean, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    area_usage_df <- area_usage_df[order(area_usage_df[2]),]
    area_usage_df <- head(area_usage_df, 0.1*nrow(area_usage_df))
  }
  else if (option == "Building Height") {
    area_usage_df <- aggregate(area_usage_df$AVERAGE.STORIES, by=list(area_usage_df$TRACT.ID), FUN=mean, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
  }
  else if(option == "10% Tallest Buildings") {
    area_usage_df <- aggregate(area_usage_df$AVERAGE.STORIES, by=list(area_usage_df$TRACT.ID), FUN=mean, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    area_usage_df <- area_usage_df[order(-area_usage_df[2]),]
    area_usage_df <- head(area_usage_df, 0.1*nrow(area_usage_df))
  }
  else if (option == "Total Population") {
    area_usage_df <- aggregate(area_usage_df$TOTAL.POPULATION, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
  }
  else if(option == "10% Most Population") {
    area_usage_df <- aggregate(area_usage_df$TOTAL.POPULATION, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    area_usage_df <- area_usage_df[order(-area_usage_df[2]),]
    area_usage_df <- head(area_usage_df, 0.1*nrow(area_usage_df))
  }
  else if (option == "10% Most Occupied") {
    area_usage_df <- aggregate(area_usage_df$OCCUPIED.UNITS.PERCENTAGE, by=list(area_usage_df$TRACT.ID), FUN=mean, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    area_usage_df <- area_usage_df[order(-area_usage_df[2]),]
    area_usage_df <- head(area_usage_df, 0.1*nrow(area_usage_df))
  }
  else if(option == "10% Highest Renting Rate") {
    area_usage_df <- aggregate(area_usage_df$RENTER.OCCUPIED.HOUSING.PERCENTAGE, by=list(area_usage_df$TRACT.ID), FUN=mean, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
    area_usage_df <- area_usage_df[order(-area_usage_df[2]),]
    area_usage_df <- head(area_usage_df, 0.1*nrow(area_usage_df))
  }
  else if(option == "Average House Size") {
    area_usage_df <- aggregate(area_usage_df$AVERAGE.HOUSESIZE, by=list(area_usage_df$TRACT.ID), FUN=mean, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
  }
  else if(option == "Total Units") {
    area_usage_df <- aggregate(area_usage_df$TOTAL.UNITS, by=list(area_usage_df$TRACT.ID), FUN=sum, keep.names = TRUE, na.rm=TRUE, na.action=NULL)
  }
  
  
  names(area_usage_df)[1] <- c("GEOID10")
  names(area_usage_df)[2] <- c("AMOUNT")
  
  #selected_df <- subset(chicago_tract_df, GEOID10 %in% area_tract_mapping_df$GEOID10)
  
  usage_block_df <- selected_df %>% left_join(area_usage_df)
  return (usage_block_df)
}