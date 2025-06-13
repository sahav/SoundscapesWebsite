#ADD context metadata to hourly TOLs and creates report graphics

#INPUTS: output of HrTOLs_ONMS.R, loads the most recent file; ONMS metadata; wind Model
# works for each monitoring site

# ADD: wind pie charts

# LIBRARIES ####
rm(list=ls()) 
options(java.parameters = "-Xmx4g")
library(PAMscapes)
library(scales)
library(gridExtra)
library(ggplot2)
library(tidyverse)
#library(xlsx)
library(openxlsx)
library(reshape)
library(gtable)
library(grid)
library(plotly)

#SITES ####
ONMSsites = c("sb01", "sb03","mb01","mb02", "pm01","oc02", "cb11","as01","hi01")
## directories ####
outDir =   "F:\\CODE\\GitHub\\SoundscapesWebsite\\"
outDirG =  paste0(outDir, "content\\resources") #save graphics
outDirC =  paste0(outDir,"context\\") #context

#PARAMETERS ####
DC = Sys.Date()
project = "ONMS"
fqIn = "TOL_125" 
fqInShip = c("TOL_63", "TOL_125")
ab = 65 # threshold for above frequency in
fqIn2 = "TOL_500" # no wind model for 125 Hz- ugh!!!
fqIn2name = "500 Hz"
ab2 = 0 #5
windUpp = 22.6 #which wind model result to show on plot
windLow = 1
windH = 10 #measured wind speeds
windL = 5

# CONTEXT ####
metaFile = paste0(outDirG,"\\ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( read.xlsx(metaFile, sheet  = "Summary") ) #xlsx::read.xlsx(metaFile, sheetName = "Summary")
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
## TIMES OF INTEREST ####
TOI = as.data.frame ( read.xlsx(metaFile, sheet = "Time period of interest") )
TOI = TOI[!apply(TOI, 1, function(row) all(is.na(row))), ]
## FREQUENCIES OF INTEREST ####
FOI = as.data.frame ( read.xlsx(metaFile, sheet = "Frequency of Interest") )
FOI = FOI[!apply(FOI, 1, function(row) all(is.na(row))), ]
FOI$Sanctuary = tolower(FOI$Sanctuary)
FOI = FOI[FOI$`Show.on.plot?` == "Y",]
## TOL CONVERSION ####
TOL_convert = read.csv(paste0(outDirC,"TOLconvert.csv"))
TOL_convert$Nominal = paste0("TOL_",TOL_convert$Center)
## WIND NOISE MODEL ####
windFile = list.files(outDirC, pattern = paste0("WindModel_", project), full.names = T)
file_info = file.info(windFile)
load( windFile[which.max(file_info$ctime)] ) #only load the most recent!

# PROCESS BY SITE ####
# uu = 1
for (uu in 1:length(ONMSsites)) {
  
  cat("Processing... ", ONMSsites[uu],"\n" )
  site =  ONMSsites[uu]
  
  if (site == "cb11") {
    outDirP = paste0( outDir,"products\\", substr(tolower(site), start = 1, stop =2),"\\" ) #products
    site1 = "NRS11"
    site3 = "cbnrs11"
    site = "NRS11"
  } else {
    site1 = site
    site3 = site
    outDirP = paste0( outDir,"products\\", substr(tolower(site), start = 1, stop =2),"\\" )#products
  }

  ## SITE PARAMETERS ####
  siteInfo = lookup[lookup$`NCEI ID` == tolower(site1),]
  siteInfo = siteInfo[!is.na(siteInfo$`NCEI ID`), ]
  siteInfo = siteInfo[siteInfo$Project == "ONMS Sound", ]
  ##frequency of interest ####
  FOIs = FOI [ FOI$Sanctuary == substr(site, 1,2), ]
  ##times of interest ####
  TOIs = TOI [ TOI$Site == (site1), ]
  TOIs <- TOIs %>%
    mutate(
      Start_Julian = as.numeric(format(as.Date(start_date), "%j")),
      End_Julian = as.numeric(format(as.Date(end_date), "%j")),
      Mid_Julian = (Start_Julian + End_Julian) / 2  # Midpoint for annotation
    )
  TOIs$yr = TOIs$Year
  ##seasonality ####
  sidx = siteInfo$Seasonality
  #put in alphetical order so plots line up!!!
  if ( length(sidx) == 0 ) { # default
    season = data.frame(
      Season = c("Fall", "Spring",  "Summer", "Winter"  ),
      Months = c("10,11,12", "4,5,6","7,8,9", "1,2,3"   ),
      values = c(   "#E69F00",  "#009E73", "#CC79A7", "#56B4E9") )
    sidx = "wssf"
    seasonLabel = "Winter (Jan-Mar), Spring (Apr-Jun), Summer (Jul-Sep), Fall (Oct-Dec)"
  }else if  ( sidx == "biological") {
    season = data.frame(
      Season = c("Early", "Peak", "Late", "Non"),
      Months = c("10,11,12", "1,2,3", "4,5,6", "7,8,9") ,
      values = c(  "#56B4E9",  "#009E73","#CC79A7", "#E69F00") )
    seasonLabel = "Peak (Jan-Mar), Late (Apr-Jun), Non (Jul-Sep), Early (Oct-Dec)"
  }else if  ( sidx == "upwelling") {
    season = data.frame(
      Season = c("Post-Upwelling", "Upwelling", "Winter"),
      Months = c("7,8,9,10,11", "3,4,5,6", "12,1,2") ,
      values = c(  "#CC79A7",  "#009E73","#56B4E9") )
    seasonLabel = "Upwelling (Mar-Jun), Post-Upwelling (Jul-Nov), Winter (Dec-Feb)"
  }else if ( sidx == "wssf") {
    season = data.frame(
      Season = c("Fall", "Spring",  "Summer", "Winter"  ),
      Months = c("10,11,12", "4,5,6","7,8,9", "1,2,3"   ) ,
      values = c(   "#E69F00",  "#009E73", "#CC79A7", "#56B4E9") )
    seasonLabel = "Winter (Jan-Mar), Spring (Apr-Jun), Summer (Jul-Sep), Fall (Oct-Dec)"
  }
 
  ##SPL data ####
  inFile = list.files(outDirP, pattern = paste0("data_", tolower(site1), "_HourlySPL-gfs_.*\\.Rda$"), full.names = T)
  file_info = file.info(inFile) 
  load( inFile[which.max(file_info$ctime)] ) #only load the most recent!
  st = as.Date( min(gps$UTC) )
  ed = as.Date( max(gps$UTC) )
  udays = length( unique(as.Date(gps$UTC)) )
  cat("Input Data - ", site, " has ", udays, " unique days (", as.character(st), " to ",as.character(ed), ")\n")
  Fq = as.numeric( as.character( gsub("TOL_", "",  colnames(gps)[grep("TOL", colnames(gps))] ) ))
 
  ### percentiles for all the data ####
  tol_columns = grep("TOL", colnames(gps))
  all_quantiles = apply(gps[, tol_columns, drop = FALSE], 2, quantile, probs = c(0.90, 0.75, 0.50, 0.25, 0.10), na.rm = TRUE)
  # get the spectrum band levels from the TOLs
  tol_columns = grep("TOL", colnames(gps))
  All = as.data.frame( all_quantiles )
  All$Quantile = rownames(All)
  tol_columns = grep("TOL", colnames(All))
  for( cc in 1:length(tol_columns)) {
    toltmp = colnames(All)[tol_columns[cc]]
    bw = TOL_convert$Bandwidth[which(TOL_convert$Nominal == toltmp) ] 
    dtmp = All[,tol_columns[cc] ]
    All[,tol_columns[cc] ] = 10*log10 ( (10^(dtmp/10))/ bw )
  }
  All$Year = "all"
  mALL = melt(All, id.vars = c("Quantile","Year"), measure.vars = tol_columns)
  mALL$variable = as.numeric( as.character( gsub("TOL_", "", mALL$variable )))
  colnames(mALL) = c("Quantile", "Year", "Frequency" , "SoundLevel" )
 
  # by season
  tol_columns = grep("TOL", colnames(gps))
  seas = unique(season$Season)
  for( ss in 1:length(seas) ){
    moi = as.numeric(unlist(strsplit(as.character(season$Months[ss]), ","))) 
    gps$Season[gps$mth %in% moi] = season$Season[ss]   }
  season_split = split(gps, gps$Season)
  season_quantiles = lapply(season_split, function(season_data) {
    apply(season_data[, tol_columns, drop = FALSE], 2, quantile, 
          probs = c(0.90, 0.75, 0.50, 0.25, 0.10), na.rm = TRUE)   })
  
  # by year - names(gps)
  yr_split = split(gps, gps$yr) # Calculate quantiles for each year
  year_quantiles = lapply(yr_split, function(season_data) {
    apply(season_data[, tol_columns, drop = FALSE], 2, quantile,  
          probs = c(0.90, 0.75, 0.50, 0.25, 0.10), na.rm = TRUE)   })
  
  # by year 1 - names(gps)
  yr1 = min( gps$yr )
  tol_columns = grep("TOL", colnames(gps))
  yr1.df = gps[ gps$yr == yr1,]
  yr1_quantiles = apply(yr1.df[, tol_columns, drop = FALSE], 2, quantile, probs = c(0.90, 0.75, 0.50, 0.25, 0.10), na.rm = TRUE)
  
  ### WIND category ####
  gps$wind_category = NA
  gps <- gps %>%
    mutate(wind_category = case_when(
      is.na(windMag) ~ NA_character_,
      windMag < windL ~ "low",
      windMag >= windL & windV <= windH ~ "med",
      windMag > windH ~ "high"
    ))
  # Calculate the counts for wind each category
  category_counts <- gps %>%
    count(wind_category) %>%
    mutate(label = paste(wind_category, ":", n))
  subtitle_text <- paste(category_counts$label, collapse = ", ")
  windInfo = windModel[tolower(windModel$si) == site3,]
  widx = which( as.numeric(as.character( (colnames(windInfo)) ) )  == max(Fq) )
  windInfo = windInfo[,1:widx] #wind noise by frequency (columns) and speed (rows)
  #re-structure for ggplot
  mwindInfo = melt(windInfo, id.vars = c("windSpeed"), measure.vars = colnames(windInfo)[4:ncol(windInfo)])
  mwindInfo$variable = as.numeric( as.character(mwindInfo$variable ))
  
  ## wind bar
  gps$wind_category <- factor(gps$wind_category, levels = c("low", "med", "high"))
  # Create the horizontal stacked bar plot with the counts of wind speed categories
  l = ggplot(gps, aes(x = "", fill = wind_category)) +
    geom_bar(stat = "count", position = position_stack(reverse = TRUE)) +  # Stacked bar chart
    coord_flip() +  # Flip the coordinates to make it horizontal
    facet_wrap(~yr)+
    ggtitle("% time in different wind speed categories ") +  # Add the main title
    theme_minimal() +
    labs(x = NULL, y = NULL,
         subtitle = paste0("(low <",windL, ", med ", windL,"-",windH, ", high >",windH, "m/s)")) +  # Remove x-axis label
    theme(
      plot.title = element_text(hjust = 0),  # Align the title to the left
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),  # Remove x-axis labels (now categories will appear below)
      axis.ticks.x = element_blank(),  # Remove x-axis ticks
      axis.title.x = element_blank(),  # Remove x-axis title
      legend.position = "bottom",  # Position the legend at the bottom
      legend.title = element_blank(),  # Optional: remove legend title
      legend.text = element_text(size = 10)  # Optional: adjust legend text size
    ) +
    #scale_x_discrete(labels = category_counts$wind_category) +  # Place the category labels under the plot
    scale_fill_manual(values = c("low" = "lightgray",  "med" = "gray", "high" = "darkgray") )
  l
  
  #(1) EFFORT ####
  ## by month (days/ month-year) ####
  summary <- gps %>%
    mutate(
      year = year(UTC),  # Extract Year
      month = format(UTC, "%m")  # Extract Month (numeric format)
    ) %>%
    count(year, month)  # Count occurrences (hours) in each year-month
  summary$dy = round(summary$n/ 24)
  p1 = ggplot(summary, aes(x = month, y = dy, fill = as.factor(year))) +
    geom_col(position = "dodge", width = .3) +  # Use dodge to separate bars for each year within the same month
    labs(
      title = "monitoring effort by year",
      caption = paste0(toupper(site), " has ", udays, 
                       " unique days: ", as.character(st), " to ", as.character(ed)),
      x = "",
      y = "Days",
      fill = "Year"
    ) +
    scale_x_discrete(labels = month.abb) +  # Show month names instead of numbers
    #scale_fill_manual(values = rev(gray.colors(length(unique(summary$year))))) +  # Create grayscale colors
    scale_fill_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      axis.text.x = element_text(angle = 30, hjust = 1),  # Rotate x-axis labels for readability
      legend.position = "none"  # Place the legend at the top
    )
  
  p1
  ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_Effort.jpg"), plot = p1, width = 10, height = 4, dpi = 300)
  
  ## by season (hours/ season in each year ####
  summary2 <- gps %>%
    mutate(
      year = year(UTC),  # Extract Year
      month = format(UTC, "%m")  # Extract Month (numeric format)
    ) %>%
    count(year, Season)  # Count occurrences (hours) in each year-month
  summary2$dy = round(summary2$n/ 24)
  p2 = ggplot(summary2, aes(x = as.character(year), y = dy, fill = as.factor(Season))) +
    geom_col(position = "dodge", width = .3) +  # Use dodge to separate bars for each year within the same month
    labs(
      title = "monitoring effort by season",
      caption  = paste0(toupper(site), " has ", udays, 
                        " unique days: ", as.character(st), " to ", as.character(ed), "\n",
                        seasonLabel),
      x = "",      y = "Days",      fill = "Year"
    ) +
    scale_fill_manual(values = season$values) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),  # Rotate x-axis labels for readability
      legend.position = "none"  # Place the legend at the top
    )
  p2
  ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_EffortSeason.jpg"), plot = p2, width = 10, height = 4, dpi = 300)
  
  #(2) ANNUAL ANALYSIS & PLOT ####
  if (sidx == "biological"){ #only keep peak
    gpsAll = gps
    my_subtitle = "Data summarized for Peak season only"
    gps = gps[gps$Season == "Peak",]
  } else {
    my_subtitle = "" }
  tol_columns = grep("TOL", colnames(gps))
  yearAll = NULL
  for (ii in 1: length(year_quantiles) ) {
    tmp = as.data.frame ( year_quantiles[ii] ) 
    colnames(tmp) = colnames(gps)[tol_columns]
    tmp$Quantile = rownames(tmp)
    tmp$Year = names(year_quantiles)[ii]
    rownames(tmp) = NULL
    yearAll = rbind(yearAll,tmp)
  }
  yearAllbb = yearAll # save the bb measurement
  # convert 1 Hz- divide unlog, by the bandwidth, and re-log
  tol_columns = grep("TOL", colnames(yearAll))
  gpsBB = gps
  for( cc in 1:length(tol_columns)) {
    toltmp = colnames(yearAll)[tol_columns[cc]]
    bw = TOL_convert$Bandwidth[which(TOL_convert$Nominal == toltmp) ] 
    dtmp = yearAll[,tol_columns[cc] ]
    yearAll[,tol_columns[cc] ] = 10*log10 ( (10^(dtmp/10))/ bw )
  }
  
  ### format for plot ####
  mallData = melt(yearAll, id.vars = c("Quantile","Year"), measure.vars = tol_columns)
  mallData$variable = as.numeric( as.character( gsub("TOL_", "", mallData$variable )))
  colnames(mallData) = c("Quantile", "Year", "Frequency" , "SoundLevel" )
  fqupper = max(as.numeric( as.character( mallData$Frequency) ))
  
  p = ggplot() +
    geom_ribbon(data = mallData %>% 
                  pivot_wider(names_from = Quantile, values_from = SoundLevel),
                aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year),
                alpha = 0.1) +  # Use alpha for transparency
    
    #median TOL values
    geom_line(data = mallData[mallData$Quantile == "50%",], aes(x = Frequency, y = SoundLevel, color = Year), linewidth = 2) +
    geom_line(data = mALL[mALL$Quantile == "50%",], aes(x = Frequency, y = SoundLevel), color = "black", linewidth = 1,
              linetype = "dotted") +
    geom_rect(data = FOIs, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), 
              fill = "gray", alpha = 0.2) +  # Adjust alpha for transparency
    #wind model
    geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black", linewidth = 1) +
    geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black", linewidth = 1) +
    scale_x_log10(labels = label_number(),limits = (c(10,fqupper))) +  # Log scale for x-axis
    scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
    scale_fill_manual(values =  rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
    # Add vertical lines at FQstart
    geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
    # Add labels at the bottom of each line
    geom_text(data = FOIs, aes(x = FQstart, y = 40, label = Label), angle = 90, vjust = 1, hjust = 0.5, size = 4) +
    
    # Additional aesthetics
    theme_minimal() +
    labs(
      title = paste0(toupper(site), "(",siteInfo$`Sanctuary watch habitat`[siteInfo$`Location ID` == site], ")"), 
      caption  = paste0("Vertical lines/shaded area indicate frequencies for sounds of interest in this soundscape \n",
                        "black lines are modeled wind noise at this depth [", windLow,"m/s & ",windUpp, "m/s] \n",
                        "dotted sound level curve is the median for all data"),
      x = "Frequency Hz",
      y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) ),
      if (sidx == "biological"){
        subtitle = paste0( "Data summarized for Peak season only" ) 
      }) +
    theme(legend.position = "right",
          plot.title = element_text(size = 16, face = "bold", hjust = 0),
          plot.caption = element_text(size = 12, face = "italic"), 
          plot.legend = element_text(size =12), # Caption text size
          axis.title.x = element_text(size = 12),           # X-axis label size
          axis.title.y = element_text(size = 12),           # Y-axis label size
          axis.text = element_text(size = 12)
    ) 
  p
  separator <- grid.rect(gp = gpar(fill = "black"), height = unit(2, "pt"), width = unit(1, "npc"))
  # arranged_plot = grid.arrange(p, separator, l, heights =c(4, 0.05, 0.8))
  pYear = grid.arrange(p, separator, p1, heights =c(4, 0.1, 1))
  ### save: plot yearly spectra ####
  ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_YearSPL.jpg"), plot = pYear, width = 10, height = 12, dpi = 300)
  
  #(3) SEASONAL CONDITIONS ####
  tol_columns = grep("TOL", colnames(gps))
  seasonAll = NULL
  for (ii in 1: length(season_quantiles) ) {
    tmp = as.data.frame ( season_quantiles[ii] ) 
    colnames(tmp) = colnames(gps)[tol_columns]
    tmp$Quantile = rownames(tmp)
    tmp$Season = names(season_quantiles)[ii]
    rownames(tmp) = NULL
    seasonAll = rbind(seasonAll,tmp)
  }
  seasonAllbb = seasonAll #save the bb measurement
  tol_columns = grep("TOL", colnames(seasonAll))
  #divide unlog, by the bandwidth, and re-log
  for( cc in 1:length(tol_columns)) {
    toltmp = colnames(seasonAll)[tol_columns[cc]]
    bw = TOL_convert$Bandwidth[which(TOL_convert$Nominal == toltmp) ] 
    dtmp = seasonAll[,tol_columns[cc] ]
    seasonAll[,tol_columns[cc] ] = 10*log10 ( (10^(dtmp/10))/ bw )
  }
  
  ### format for plot ####
  mallData = melt(seasonAll, id.vars = c("Quantile","Season"), measure.vars = tol_columns)
  mallData$variable = as.numeric( as.character( gsub("TOL_", "", mallData$variable )))
  colnames(mallData) = c("Quantile", "Season", "Frequency" , "SoundLevel" )
  fqupper = max(as.numeric( as.character( mallData$Frequency) ))
  p = ggplot() +
    # Add shaded area for 25%-75% range
    geom_ribbon(data = mallData %>% 
                  pivot_wider(names_from = Quantile, values_from = SoundLevel),
                aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Season),
                alpha = 0.1) +  # Use alpha for transparency
    
    # Median (50%) TOL values
    geom_line(data = mallData[mallData$Quantile == "50%",], 
              aes(x = Frequency, y = SoundLevel, color = Season), linewidth = 2) +
    
    # Set color and fill to match seasons
    scale_color_manual(values = season$values )+
    scale_fill_manual(values  = season$values ) +
    
    # Wind model values
    geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black",  linewidth = 1) +
    geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black",  linewidth = 1) +
    scale_x_log10(labels = label_number(),limits = (c(10,fqupper))) +  # Log scale for x-axis
    
    # Add vertical lines at FQstart
    geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
    geom_rect(data = FOIs, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), 
              fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
    # Add labels at the bottom of each line
    geom_text(data = FOIs, aes(x = FQstart, y = 40, label = Label), angle = 90, vjust = 1, hjust = 0.5, size = 4) +
    
    # Additional aesthetics
    theme_minimal()+
    theme(legend.position = "right",
          plot.title = element_text(size = 16, face = "bold", hjust = 0),
          plot.caption = element_text(size = 12, face = "italic"), 
          plot.legend = element_text(size =12), # Caption text size
          axis.title.x = element_text(size = 12),           # X-axis label size
          axis.title.y = element_text(size = 12),           # Y-axis label size
          axis.text = element_text(size = 12)               # Tick mark labels
          ) +  # This line removes the legend
    labs(
      title = paste0(toupper(site), "(",siteInfo$`Sanctuary watch habitat`[siteInfo$`Location ID` ==site], ")"), #, "- ", 
      #tolower(FOIs$Oceanographic.setting[1]), " monitoring site" ),
      #subtitle = paste0("data summarized from ", st, " to ", ed, "\n vertical lines indicate frequencies for sounds of interest in this soundscape" ),
      caption = paste0("vertical lines/shaded area indicate frequencies for sounds of interest in this soundscape \n black lines are expected wind noise at this depth [", windLow,"m/s & ",windUpp, "m/s]"), 
      x = "Frequency Hz",
      y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) )
    )
  
  separator <- grid.rect(gp = gpar(fill = "black"), height = unit(2, "pt"), width = unit(1, "npc"))
  # arranged_plot = grid.arrange(p, separator, l, heights =c(4, 0.05, 0.8))
  arranged_plot = grid.arrange(p, separator, p2, heights =c(4, 0.1, 1))
  ### save: plot seasonal spectra ####
  ggsave(filename = paste0(outDirG, "//plot_", tolower(site), "_SeasonalSPL.jpg"), plot = arranged_plot, width = 10, height = 12, dpi = 300)
  
  # (4) TIME SERIES - 125 Hz TOL ####
  # Questions: How many days are above typical conditions for vessel noise?
  cols_to_select = c("UTC", "windMag","wind_category",fqIn)
  gpsFQ = gps %>% select(all_of(cols_to_select))
  wspeeds = unique( (windModel$windSpeed) )
  gpsFQ$closest_windMag = wspeeds[pmax(1, findInterval(gpsFQ$windMag, wspeeds)+1)]
  mALL$FrequencyName = paste0("TOL_", mALL$Frequency)
  thresholds = mALL[mALL$FrequencyName == fqIn,]
  #this is the median of all the data at a specified frequency
  threshold_mid =  thresholds$SoundLevel[thresholds$Quantile == "50%"]
  threshold_up  = thresholds$SoundLevel[thresholds$Quantile  == "75%"]
  threshold_lo  = thresholds$SoundLevel[thresholds$Quantile == "25%"]
 
  tol_columns = grep("TOL", colnames(gpsFQ))
  bw = TOL_convert$Bandwidth[which(TOL_convert$Nominal == colnames(gpsFQ)[tol_columns]) ]
  gpsFQ$SpecBand = 10*log10 ( (10^( gpsFQ[,tol_columns]/10))/ bw )
  
  ### daily percentiles ####
  dailyFQ <- gpsFQ %>%
    mutate(Date = as.Date(UTC)) %>%
    group_by(Date) %>%
    summarise(
      TOL_25 = quantile(SpecBand, 0.25, na.rm = TRUE),
      TOL_50 = quantile(SpecBand, 0.50, na.rm = TRUE),
      TOL_75 = quantile(SpecBand, 0.75, na.rm = TRUE),
      windspeed = quantile(windMag, 0.50, na.rm = TRUE),
      count_above_threshold = sum(SpecBand > threshold_mid, na.rm = TRUE),
      percent_above_threshold = sum(SpecBand > threshold_mid, na.rm = TRUE) / 
        sum(!is.na(SpecBand)) * 100
    )
  dailyFQ$yr = year(dailyFQ$Date)
  dailyFQ$Julian = yday(dailyFQ$Date)
  dailyFQ_complete <- dailyFQ %>%
    group_by(yr) %>%
    complete(Julian = seq(min(Julian), max(Julian), by = 1)) %>%  # Fill in missing days
    arrange(yr, Julian) 
  monthly_sequence <- seq.Date(as.Date("2021-01-01"), as.Date("2021-12-01"), by = "month")
  month_names_seq   <- format(monthly_sequence, "%b")  # Extracts full month names
  days_of_year_for_months <- yday(monthly_sequence)
  
  ### annual status ####
  gpsFQ$yr = year(gpsFQ$UTC)
  yrFQ = gpsFQ %>% group_by(yr) %>%
    summarise(
      TOL_25 = quantile(SpecBand, 0.25, na.rm = TRUE),
      TOL_50 = quantile(SpecBand, 0.50, na.rm = TRUE),
      TOL_75 = quantile(SpecBand, 0.75, na.rm = TRUE) )
  yrFQ$TOL_50 - threshold_mid
    # Define named points 
  q90=thresholds$SoundLevel[thresholds$Quantile == "90%"]
  q75=thresholds$SoundLevel[thresholds$Quantile == "75%"]
  q50=thresholds$SoundLevel[thresholds$Quantile == "50%"]
  q25=thresholds$SoundLevel[thresholds$Quantile == "25%"]
  q10=thresholds$SoundLevel[thresholds$Quantile == "10%"]
  # Set up threshold bands with categories
  threshold_bands <- data.frame(
    category = c("Very Low", "Low", "Within Range", "High", "Very High"),
    xmin = c(-Inf,    q10,    q25,    q75,    q90),
    xmax = c(q10,     q25,    q75,    q90,    Inf),
    fill = c("#6699CC", "#99CCFF", "#CCCCCC", "#FF9999", "#CC0000")
  )
  threshold_bands$category <- factor(
    threshold_bands$category,
    levels = c("Very Low", "Low", "Within Range", "High", "Very High")
  )
  
  ### annual trend #### names(gpsFQ)
  gpsFQ$Julian = yday(gpsFQ$UTC)
  slopes_by_year <- gpsFQ %>%
    group_by(yr) %>%
    do({
      model <- lm(SpecBand ~ Julian, data = .)
      data.frame(slope = coef(model)["Julian"])
    }) %>%
    ungroup()
  slope_mean <- mean(slopes_by_year$slope, na.rm = TRUE)
  slope_sd   <- sd(slopes_by_year$slope, na.rm = TRUE)
    <- slopes_by_year %>%
    mutate(
      flag = abs(slope - slope_mean) > slope_sd,
      direction = case_when(
        slope > (slope_mean + slope_sd) ~ "Increasing",
        slope < (slope_mean - slope_sd) ~ "Decreasing",
        TRUE ~ "Stable"
      )
    )
  
  pthrs = ggplot() +
    # Threshold bands (same for all facets)
    geom_rect(data = threshold_bands,
              aes(xmin = xmin, xmax = xmax, ymin = 0.4, ymax = 0.6, fill = category),
              color = "white") +
    
    # Marker line per year
    geom_vline(data = yrFQ, aes(xintercept = TOL_50), 
               linetype = "dashed", color = "black", linewidth = 0.3) +
    
    # Facet by year
    facet_wrap(~yr, ncol = 1) +  
    
    # Customize scales and colors
    scale_fill_manual(values = setNames(threshold_bands$fill, threshold_bands$category)) +
    coord_cartesian(xlim = c(60, 90), ylim = c(0.3, 0.85)) +
    
    # Styling
    theme_void() +
    theme(legend.position = "top") +
    labs(title = "Annual Status", fill = "", strip.text = element_text(hjust = 0))
    
  
  plg =  ggplot(dailyFQ_complete, aes(x = Julian, y = TOL_50, group = yr) ) +
    
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = threshold_lo, ymax = threshold_up,
             fill = "lightgreen", alpha = 0.2) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = threshold_up, ymax = 90,
             fill = "lightcoral", alpha = 0.2) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = threshold_lo, ymax = 60,
             fill = "lightblue", alpha = 0.2) +
    ylim(60, 90) +
    
    geom_hline(aes(yintercept = threshold_mid),linetype = "dashed", color = "gray",linewidth = .2) +
    geom_hline(aes(yintercept = threshold_up),linetype = "dashed", color = "gray",linewidth = .2) +
    geom_hline(aes(yintercept = threshold_lo),linetype = "dashed", color = "gray",linewidth = .2) +
    
    geom_line() +
    facet_wrap(~yr, ncol = 1)+
    theme_minimal() +
    theme(legend.position = "none",
          strip.text = element_text(hjust = 0),
          plot.title = element_text(size = 16, face = "bold", hjust = 0),
          plot.subtitle = element_text(size = 12, face = "italic"),) +
    
    scale_x_continuous(breaks = days_of_year_for_months, labels = month_names_seq) +
    labs(
      title    = "Is large vessel noise above typical conditions?", 
      subtitle = toupper(site),
      caption  = "Thresholds are set at 75th and 25th percentiles of all the data", 
      x = "",
      y = expression(paste("Daily Median Sound Levels (dB re 1 ", mu, " Pa/Hz at 125 Hz)" ) )
    )
  
  plg = grid.arrange(plg, pthrs, nrow = 1, widths = c(2, 1))
 
  #### save: plot 125 Hz time series with thresholds ####
  ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_LgVesselNoise125.jpg"), plot = plg, width = 10, height = 12, dpi = 300)
  
  ## TIME SERIES- percentiles ####
  yrs = unique(dailyFQ_complete$yr)
  dailyFQ_complete$facet_title = NA
  for ( ii in 1:length(yrs ) ) {
    idx = which(dailyFQ_complete$yr == yrs[ii])
    idx2 =  which(percentage_above$year == yrs[ii])
    dailyFQ_complete$facet_title[idx] = paste0(yrs[ii], "- ", round(percentage_above$percentage_above[idx2],1),"% above" )
  }
  
  TOIs = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr))
  if (nrow( TOIs)  > 0) {
    for (ii in 1:nrow(TOIs) ) {
      TOIs$facet_title[ii] = dailyFQ_complete$facet_title[which(dailyFQ_complete$yr == TOIs$Year[ii])] [1]
    }
  }
  
  p0 = ggplot(dailyFQ_complete, aes(x = Julian, y = TOL100_50, group = yr, color = factor(yr))) 
  
  if (nrow(TOIs) > 0) {
    p0 <-  p0 + geom_rect(
      data = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr)), 
      inherit.aes = FALSE,
      aes(xmin = Start_Julian, xmax = End_Julian, ymin = -Inf, ymax = Inf), 
      fill = "grey", alpha = 0.2  # Optional styling
    )
  }

  p <- p0 + geom_line() +
    
    scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year)))))  +
    geom_line(linewidth = 1, na.rm = TRUE) +
    geom_ribbon(aes(ymin = TOL100_25, ymax = TOL100_75), fill = "gray", alpha = 0.5) +
    facet_wrap(~facet_title, nrow = length(unique(dailyFQ$yr)) ) +
    theme_minimal() +
    scale_x_continuous(breaks = days_of_year_for_months, labels = month_names_seq) +  
   geom_hline(aes(yintercept = ab), linetype = "dashed", color = "gray", linewidth = 0.7) +
    
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, hjust =0, vjust = 0),  # Facet labels inside (centered)
      strip.background = element_blank(),  # Remove background behind facet labels
      panel.spacing = unit(.1, "lines") ) + # Adjust the spacing between facets) +
    labs(
      title = paste0("Soundscape Conditions at 125 Hz" ) ,
      subtitle =  paste0(toupper(site)), #, ", a ", tolower(FOIs$Oceanographic.setting[1]), " monitoring site \nshaded areas represents ", TOIs$Label[1] ),
      caption = paste0("% time above threshold = ", ab, " dB"),
      x = "",
      y = expression(paste("Sound Levels (125 Hz dB re 1", mu, " Pa/Hz)" ) ),
      color = "Year"  # Label for the color legend
    ) 
  p
  #### save: plot 125 Hz ####
  ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_125Hz.jpg"), plot = p, width = 10, height = 12, dpi = 300)
  
  ## TIME SERIES - interactive ####
  yearly_data <- split(dailyFQ_complete, dailyFQ_complete$yr)
  plots <- lapply(yearly_data, function(df) {
    plot_ly(df, x = ~Julian, y = ~TOL100_50, type = 'scatter', mode = 'lines') %>%
      layout(showlegend = FALSE,
    yaxis = list(title = expression(paste("Sound Levels (125 Hz dB re 1", mu, " Pa/Hz)" )) ),
    title = paste0("Soundscape Conditions at 125 Hz at ", toupper(site) ) )
  })
  fig <- subplot(plots, nrows = length(plots), shareX = TRUE, shareY = TRUE, titleY = TRUE)
  annotations <- list()
  for (i in seq_along(plots)) {
    annotations[[i]] <- list(
      x = 0, y = 1 - (i - 1) / length(plots),
      text = paste("Year:", names(yearly_data)[i]),
      xref = "paper", yref = "paper",
      xanchor = "left", yanchor = "top",
      showarrow = FALSE, font = list(size = 12)
    )
  }
  fig=fig %>% layout(annotations = annotations)
  fig
  #### save: plotly ####
  htmlwidgets::saveWidget(as_widget(fig), 
  paste0( outDirG, "\\plot_", toupper(site), "_TS125ptly.html") ) 
  
  ## TIME SERIES - above year 1 median ####
  cols_to_select = c("UTC", "windMag","wind_category", "Season", fqInShip)
  gpsFQ = gps %>% select(all_of(cols_to_select))
  gpsFQ$yr = year(gpsFQ$UTC) # unique(  gpsFQ$yr)
  gpsFQ$mth = month(gpsFQ$UTC)
  names(gpsFQ)
  monthMed = gpsFQ %>%
    mutate(Date = as.Date(UTC)) %>%
    group_by(mth, yr) %>%
    summarise(
      hrs = n(),  # Count of observations
      `63Hz`  = quantile(TOL_63, 0.50, na.rm = TRUE),
      `125Hz` = quantile(TOL_125, 0.50, na.rm = TRUE), # Median
      
    )
  monthDiff <- monthMed %>%
    group_by(mth) %>%
    mutate(
      base_med1 = `63Hz`[which.min(yr)],
      base_med2 =  `125Hz`[which.min(yr)],
      med1_diff = `63Hz` - base_med1,
      med2_diff =  `125Hz`  - base_med2
    ) %>%
    ungroup()
  
  monthDiff <- monthDiff %>%
    mutate(
      Date = as.Date(paste(yr, mth, "15", sep = "-")),
      Month = factor(month.abb[mth], levels = month.abb)  # Ensure correct order
    )
  
  monthDiff_long <- monthDiff %>%
    pivot_longer(cols = c(med1_diff, med2_diff), names_to = "Metric", values_to = "Anomaly") %>%
    mutate(Direction = ifelse(Anomaly > 0, "Positive", "Negative"))
 
  monthDiff_long = subset(monthDiff_long, !is.na(Anomaly) & Anomaly != 0)
  if ( nrow(monthDiff_long ) > 0 ) {
    p5 = ggplot(monthDiff_long, aes(x = Month, y = Anomaly, fill = Direction)) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      facet_grid(rows = vars(yr), cols = vars(Metric)) +
      scale_fill_manual(values = c("Positive" = "firebrick", "Negative" = "steelblue")) +
      ylim(c(-2,4)) +
      labs(
        title = paste0 (substr(fqInShip[1], start = 5, stop = 6), "Hz and ", 
                        substr(fqInShip[2], start = 5, stop = 7), "Hz"),
        x = "",
        caption = paste0(toupper(site), 
                         " (",siteInfo$`Sanctuary watch habitat`[siteInfo$`Location ID` ==site], ")"),
        y = paste0 ("Difference from year-1 (decibel)")
      ) +
      theme_minimal() +
      theme(
        strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5),
        strip.text.y = element_text(angle = 0),
        strip.text.x = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    p5
    
    #### save: above year-1 median ####
    ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_year1above.jpg"), plot = p5, width = 10, height = 12, dpi = 300)
    
    ### % time above year 1 median- compare the hourly values
    baseline = as.data.frame ( monthDiff[monthDiff$med1_diff == 0, 1:5] )
    gpsFQ$diff63 = -99
    gpsFQ$diff125= -99
    for (mm in 1:length(unique(baseline$mth))) {
      idx = which( gpsFQ$mth == baseline$mth[mm] )
      gpsFQ$diff63[idx] = gpsFQ$TOL_63[idx]  - baseline$`63Hz`[mm]
      gpsFQ$diff125[idx] = gpsFQ$TOL_125[idx] - baseline$`125Hz`[mm]
      
    }
    
    timeAbove <- gpsFQ %>%
      group_by(yr, mth) %>%
      summarise(
        pct_above_63  = mean(diff63 > 0, na.rm = TRUE) * 100,
        pct_above_125 = mean(diff125 > 0, na.rm = TRUE) * 100,
        .groups = "drop"
      )
    
    timeAbove <- timeAbove %>%
      mutate(
        Date = as.Date(paste(yr, mth, "15", sep = "-")),
        Month = factor(month.abb[mth], levels = month.abb)  # Ensure correct order
      )
    
    
    timeAbove
    timeAbove_long <- timeAbove %>%
      pivot_longer(cols = c(pct_above_63, pct_above_125), names_to = "Metric", values_to = "Anomaly") %>%
      mutate(Direction = ifelse(Anomaly > 0, "Positive", "Negative"))
    timeAbove_long = subset(timeAbove_long, !is.na(Anomaly) & Anomaly != 50)
    # timeAbove_long = timeAbove_long[timeAbove_long$Anomaly != 50,]
    
    p6 = ggplot(timeAbove_long, aes(x = Month, y = Anomaly)) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 50, linetype = "dashed", color = "gray40") +
      facet_grid(rows = vars(yr), cols = vars(Metric)) +
      scale_fill_manual(values = "gray")  +
      ylim(c(0,100)) +
      labs(
        title = paste0 ("% Time Above: ", substr(fqInShip[1], start = 5, stop = 6), "Hz and ", 
                        substr(fqInShip[2], start = 5, stop = 7), "Hz"),
        x = "",
        caption = paste0(toupper(site), 
                         " (",siteInfo$`Sanctuary watch habitat`[siteInfo$`Location ID` ==site], ")"),
        y = paste0 ("% Time Above year-1 median")
      ) +
      theme_minimal() +
      theme(
        strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5),
        strip.text.y = element_text(angle = 0),
        strip.text.x = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    p6
    ### save: above year-1 median ####
    ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_year1Pctabove.jpg"), plot = p6, width = 10, height = 12, dpi = 300)
  } # not baseline data to compare
  
  ## TIME SERIES - Decibels Above Wind Noise at XX Hz  ####
  cols_to_select = c("UTC", "windMag","wind_category", "Season", fqIn2)
  gpsFQ = gps %>% select(all_of(cols_to_select))
  #ADD wind speed estimate for each hour of data in frequency of interest
  wspeeds = unique( (windModel$windSpeed) )
  gpsFQ$closest_windMag = wspeeds[pmax(1, findInterval(gpsFQ$windMag, wspeeds)+1)]
  # what is the spl values for that windspeed?
  fqIdx = which( colnames( windInfo) == substr( fqIn2, 5,8)) #'500'
  wsIdx = match(gpsFQ$closest_windMag, windInfo$windSpeed)
  gpsFQ$WindModelfq = windInfo[wsIdx, fqIdx]
  tol_col = grep("TOL", colnames(gpsFQ))
  gpsFQ$Exceed = gpsFQ[,tol_col] -  gpsFQ$WindModelfq # actual - model for each model
  gpsFQ$yr = year(gpsFQ$UTC)
  
  gpsFQ$Windthres = "unk"
  gpsFQ$Windthres[gpsFQ$Exceed <= ab2] = "below"
  gpsFQ$Windthres[gpsFQ$Exceed > ab2] = "above"

  seasonalNE = gpsFQ %>%
    mutate(Date = as.Date(UTC)) %>%
    group_by(Season, yr) %>%
    summarise(
      hrs = n(),  # Count of observations
      percent_above = sum(Windthres == "below", na.rm = TRUE) / hrs * 100,
      #Exceed_25 = quantile(Exceed, 0.25, na.rm = TRUE),
      Exceed_50 = quantile(Exceed, 0.50, na.rm = TRUE), # Median
      #Exceed_75 = quantile(Exceed, 0.75, na.rm = TRUE),
      #TOL100_25 = quantile(.data[[fqIn2]], 0.25, na.rm = TRUE),
      TOL100_50 = quantile(.data[[fqIn2]], 0.50, na.rm = TRUE),
      #TOL100_75 = quantile(.data[[fqIn2]], 0.75, na.rm = TRUE),
      windspeed = quantile(windMag, 0.50, na.rm = TRUE)
    )
  
  seasonalNE = as.data.frame( seasonalNE )
  
  sidx = siteInfo$Seasonality
  #put in alphetical order so plots line up!!!
  if ( length(sidx) == 0 ) {
    seasonalNE = seasonalNE %>%
      mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))
  }else if  ( sidx == "biological") {
    seasonalNE = seasonalNE %>%
      mutate(Season = factor(Season, levels = c("Early", "Peak", "Late-Upwelling", "Non")))
 
  }else if  ( sidx == "upwelling") {
    seasonalNE = seasonalNE %>%
      mutate(Season = factor(Season, levels = c("Upwelling", "Winter", "Post-Upwelling")))
  
  }else {
    seasonalNE = seasonalNE %>%
      mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))
  }
  
  seasonalTime_wide = seasonalNE %>%
    select(Season, yr, percent_above) %>%  
    mutate(percent_above = round(percent_above, 0) ) %>% 
    pivot_wider(names_from = yr, values_from = percent_above) %>%
    arrange(Season) 
  seasonalTime_wide <- seasonalTime_wide %>%
    select(Season, sort(names(seasonalTime_wide)[-1]))  # Skip the Season column and sort the year columns
  
  seasonalNE_wide <- seasonalNE %>%
    select(Season, yr, Exceed_50) %>%  
    mutate(Exceed_50 = round(Exceed_50, 1),
           yr = factor(yr, levels = sort(unique(yr))) 
    ) %>%
    pivot_wider(names_from = yr, values_from = Exceed_50 ) %>%
    arrange(Season) 
  seasonalNE_wide <- seasonalNE_wide %>%
    select(Season, sort(names(seasonalNE_wide)[-1]))
  ### save: table dB above ####
  title_grob = textGrob( paste0("% Time Above Wind Noise (", site, ") \n", 
                                "(hourly values ", ab2 , " dB or more above wind model value at ", fqIn2name,")"), 
                         gp = gpar(fontsize = 12, fontface = "bold") , vjust = 1)
  table_grob = tableGrob(as.data.frame( seasonalTime_wide), rows = NULL )
  title_grob2 = textGrob( paste0("Decibels Above Wind Noise \n",
                                 "(median decibels difference from wind model at ", fqIn2name,")"), 
                          gp = gpar(fontsize = 12, fontface = "bold") , vjust = 1)
  table_grob2 = tableGrob(as.data.frame( seasonalNE_wide), rows = NULL )
  combined_grob = arrangeGrob(title_grob, table_grob, title_grob2, table_grob2, ncol = 2)  
  
  ggsave(paste0(outDirG, "\\table_", toupper(site) , "_AboveWind.jpg"), combined_grob, width = 10, height = 8)
  
  dailyFQ = gpsFQ %>%
    mutate(Date = as.Date(UTC)) %>%
    group_by(Date) %>%
    summarise(
      Exceed_25 = quantile(Exceed, 0.25, na.rm = TRUE),
      Exceed_50 = quantile(Exceed, 0.50, na.rm = TRUE), # Median
      Exceed_75 = quantile(Exceed, 0.75, na.rm = TRUE),
      TOL100_25 = quantile(.data[[fqIn2]], 0.25, na.rm = TRUE),
      TOL100_50 = quantile(.data[[fqIn2]], 0.50, na.rm = TRUE),
      TOL100_75 = quantile(.data[[fqIn2]], 0.75, na.rm = TRUE),
      windspeed = quantile(windMag, 0.50, na.rm = TRUE)
    )
  
  #names(dailyFQ)
  dailyFQ$yr = year(dailyFQ$Date)
  dailyFQ$Julian = yday(dailyFQ$Date)
  dailyFQ_complete <- dailyFQ %>%
    group_by(yr) %>%
    complete(Julian = seq(min(Julian), max(Julian), by = 1)) %>%  # Fill in missing days
    arrange(yr, Julian) 
  ### calculate % above ####
  percentage_above <- dailyFQ %>%
    mutate(year = year(Date)) %>%  # Create 'year' column from Date
    group_by(year) %>%  # Group by year
    summarise(
      total_count = n(),  # Total number of rows for each year
      count_above = sum(Exceed_50 > ab2, na.rm = TRUE),  # Count of values > 60
      percentage_above = (count_above / total_count) * 100  # Percentage calculation
    )
  #percentage_above
  #graphic titles
  yrs = unique(dailyFQ_complete$yr)
  dailyFQ_complete$facet_title = NA
  for ( ii in 1:length(yrs ) ) {
    idx = which(dailyFQ_complete$yr == yrs[ii])
    idx2 =  which(percentage_above$year == yrs[ii])
    dailyFQ_complete$facet_title[idx] = paste0(yrs[ii], "- ", round(percentage_above$percentage_above[idx2],1),"% above" )
  }
  
  TOIs = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr))
  if(nrow( TOIs ) > 0 ) {
    for (ii in 1:nrow(TOIs) ) {
      TOIs$facet_title[ii] = dailyFQ_complete$facet_title[which(dailyFQ_complete$yr == TOIs$Year[ii])] [1]
    }
  }
  
  pE0 = ggplot(dailyFQ_complete, aes(x = Julian, y = Exceed_50, group = yr, color = factor(yr))) 
    
  if (nrow(TOIs) > 0) {
    pE0 <-  pE0 + geom_rect(
      data = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr)), 
      inherit.aes = FALSE,
      aes(xmin = Start_Julian, xmax = End_Julian, ymin = -Inf, ymax = Inf), 
      fill = "grey", alpha = 0.2  # Optional styling
    )
  }
  
  pE <- pE0 + geom_line(size = 1, na.rm = T) +
    
    #geom_point(size = 2) +
    geom_ribbon(aes(ymin = Exceed_25, ymax = Exceed_75), fill = "gray", alpha = 0.5) +
    #geom_col(aes(y = windspeed) , color = "gray", alpha = 1) +
    facet_wrap(~facet_title, nrow = length(unique(dailyFQ$yr)) ) +
    theme_minimal()+
    scale_x_continuous( breaks = days_of_year_for_months, label = month_names_seq) +  # Show every 30 days
    scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
    
    geom_hline(aes(yintercept = ab2), linetype = "dashed", color = "gray", size = .7) +
    #geom_hline(aes(yintercept = 10), linetype = "dashed", color = "gray",size = .5) +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, hjust =0, vjust = 0),  # Facet labels inside (centered)
      strip.background = element_blank(),  # Remove background behind facet labels
      panel.spacing = unit(.1, "lines"), 
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      plot.caption = element_text(size = 10, face = "italic"), 
      axis.title.x = element_text(size = 12),           # X-axis label size
      axis.title.y = element_text(size = 12),           # Y-axis label size
      axis.text = element_text(size = 12) ) + # Adjust the spacing between facets) +
    
    labs(
      title = paste0("Decibels Above Wind Noise" ),
      subtitle = paste0(toupper(site), "(",siteInfo$`Sanctuary watch habitat`[siteInfo$`Location ID` ==site], ")"), #, ", a ", tolower(FOIs$Oceanographic.setting[1]), " monitoring site \nshaded areas represents ", TOIs$Label[1] ) ,
      caption  = paste0("difference between measured sound level and expected wind noise levels \n given local wind speed ", 
                        "(threshold for ", fqIn2name, " % time above = ", ab2, "dB)"),
      x = "",
      y = paste0("Decibels Above Wind Noise at ", fqIn2name),
      color = "Year"  # Label for the color legend
    ) 
  pE
  ### save: plot NE time series ####
  # arranged_plot = grid.arrange(p, separator, l, heights =c(4, 0.05, 0.8))
  pNE = grid.arrange(pE, l, nrow = 1,widths = c(2, 1))
  ggsave(filename = paste0(outDirG, "\\plot_", toupper(site), "_Exceed100.jpg"), plot = pNE, width = 12, height = 12, dpi = 300)
  
  ## TIME SERIES - % Time Wind Dominated  ####
   gpsFQ$Day = as.Date( gpsFQ$UTC )
  dayNE = gpsFQ %>%
    mutate(Date = as.Date(UTC)) %>%
    group_by(Season, Day) %>%
    summarise(
      hrs = n(),  # Count of observations
      percent_below = sum(Windthres == "below", na.rm = TRUE) / hrs * 100,
      #Exceed_50 = quantile(Exceed, 0.50, na.rm = TRUE), # Median
      #TOL100_50 = quantile(.data[[fqIn2]], 0.50, na.rm = TRUE),
      windspeed = quantile(windMag, 0.50, na.rm = TRUE)
    )
  
  dayNE = as.data.frame( dayNE )
  dayNE$yr = year(dayNE$Day )
  dayNE$Julian = yday(dayNE$Day)
  
  windD = ggplot(dayNE, aes(x = Julian, y = as.numeric( percent_below), color = Season) )+
    geom_line() +
    facet_wrap(~yr, ncol = 1)+
    scale_x_continuous(
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    ) +
    labs(
      title = paste0("Percent Time Wind Noise Dominated at ", fqIn2name),
      subtitle = paste0(toupper(site), "(",siteInfo$`Sanctuary watch habitat`[siteInfo$`Location ID` == site], ")"), #, ", a ", tolower(FOIs$Oceanographic.setting[1]), " monitoring site \nshaded areas represents ", TOIs$Label[1] ) ,
      caption  = paste0("difference between measured sound level and expected wind noise levels given local wind speed \n in ", unique(dayNE$Season), " season(s)" ),
      x = "",
      y = paste0("% of Hours in a day wind noise dominated at ", fqIn2name),
      color = "Year"  # Label for the color legend
    ) +
    theme_minimal()+
    theme(
      legend.position = "none")
  
  ggsave(filename = paste0(outDirG, "\\plot_", toupper(site), "_WindDomin.jpg"), plot = windD, width = 12, height = 12, dpi = 300)
  
  ## SAVE UPDATED DATA ####
  save(gps, file = paste0(outDirP, "\\data_", tolower(site), "_HourlySPL-gfs-season_", DC, ".Rda") )

  ## 
  
  }

# NOT USED ####
## calculate % above VSR ####
# add VSR to hourly data

# gpsFQ$VSR = "no"
# for (tt in 1:nrow( TOIs )){
#   idx = which(gpsFQ$UTC >= TOIs$start_date[tt] & gpsFQ$UTC < TOIs$end_date[tt]+1)
#   cat(year( TOIs$start_date[tt]) , ": ", as.character( gpsFQ$UTC[idx[1]]), "\n")
#   gpsFQ$VSR[idx] = "yes"
#   rm(idx)
# }
# names(gpsFQ)
# 
# # percent above wind noise threshold, summarized by year for all three categories (above, below, NA)
# percentage_above <- gpsFQ %>%
#   mutate(year = year(UTC), 
#          mth = month(UTC, label = TRUE)) %>%  # Extract month name
#   group_by(year, mth, VSR) %>%
#   summarise(
#     total_count = n(),
#     count_above = sum(Exceed >= ab2, na.rm = TRUE),
#     count_below = sum(Exceed < ab2, na.rm = TRUE),
#     count_na = sum(is.na(Exceed)),  # Count NAs
#     .groups = "drop"
#   ) %>%
#   mutate(
#     percentage_above = (count_above / total_count) * 100,
#     percentage_below = (count_below / total_count) * 100,
#     percentage_na = (count_na / total_count) * 100
#   ) %>%
#   select(year, mth, VSR, percentage_above, percentage_below, percentage_na) %>%
#   pivot_longer(cols = c(percentage_above, percentage_below, percentage_na),
#                names_to = "Threshold_Category",
#                values_to = "Percentage")
# 
# #output table Year- reformat
# percentage_aboveT = percentage_above[percentage_above$Threshold_Category == "percentage_above",]
# percentage_aboveT$Percentage = round( percentage_aboveT$Percentage )
# df_wide <- percentage_aboveT %>%
#   pivot_wider(
#     names_from = year,  # The column containing year
#     values_from = Percentage,  # The column containing values (percentage)
#     #names_prefix = "Year_"  # Optional prefix for year columns
#   )
# df_wide <- df_wide %>%
#   select(-Threshold_Category)
# 
# title_grob <- textGrob( paste0("Percent Time Above Wind Noise Threshold (", ab2, "dB)"), gp = gpar(fontsize = 16, fontface = "bold"))
# table_grob <- tableGrob(df_wide)
# combined_grob <- arrangeGrob(title_grob, table_grob, ncol = 1, heights = c(0.1, 0.9))  # Adjust title height
# 
# # ggsave(paste0(outDirG, "table_", site, "_Exceed.jpg"), combined_grob, width = 8, height = 5)
# percentage_above$Threshold_Category <- factor(percentage_above$Threshold_Category, 
#                                               levels = c("percentage_below", "percentage_above", "percentage_na"))
# ggplot(percentage_above, aes(x = mth, y = Percentage, fill = Threshold_Category,color = VSR)) +
#   geom_bar(stat = "identity", position = "stack") +  # Stacked to show proportion with correct ordering
#   facet_wrap(~year, nrow = 1) +  # One row per year
#   labs(x = "", y = "Percentage", 
#        title = paste0("Percent Time Above Wind Noise Threshold (", ab2, "dB)")) +
#   scale_fill_manual(values = c("percentage_above" = "tomato",  # 'percentage_above' on bottom
#                                "percentage_below" = "gray",
#                                "percentage_na" = "lightgray")) +  # Custom colors
#   scale_x_discrete(breaks = levels(percentage_above$mth)[seq(1, length(levels(percentage_above$mth)), by = 3)]) +  
#   scale_color_manual(values = c("yes" = "black", "no" = NA)) +  # Outline months with VSR
#   # Show every third month
#   theme_minimal()