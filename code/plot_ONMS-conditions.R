# Soundscape Condition | tracking how sound levels change in different frequencies using the soundscape metrics

# ADDS context metadata to hourly TOLs-- season
# INPUTS: output of HrTOLs_ONMS.R, loads the most recent file; ONMS metadata; wind Model
# works for each monitoring site

rm(list=ls()) 

# LIBRARIES ####
devtools::install_github('TaikiSan21/PAMscapes')
library(patchwork)
library(PAMscapes)
library(scales)
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(reshape)
library(gtable)
library(grid)
library(ggtext)
library(plotly)

#SITES ####
ONMSsites = c("sb01", "sb03", "mb01", "mb02", "pm01", "oc02", "cb11", "as01", "hi01")
## directories ####
outDir =   "F:\\CODE\\GitHub\\SoundscapesWebsite\\"
outDirG =  paste0(outDir, "content\\resources") #where save graphics
outDirC =  paste0(outDir,"context\\") #where to get context

#PARAMETERS ####
DC = Sys.Date()
project = "ONMS"
fqIn = "TOL_125"
fqInN = "125 Hz"
fqInShip = c("TOL_63", "TOL_125") # ship noise frequencies
ab = 65 # threshold for above frequency in
fqIn2 = "TOL_500" # wind model comparison- wind dominated frequency
fqIn2name = "500 Hz" # wind model comparison- wind dominated frequency
ab2 = 0 #threshold for dB above a reference value
windUpp = 22.6 #which wind model result to show on plot
windLow = 1 #which wind model result to show on plot
windH = 10 #wind speeds categories
windL = 5 #wind speeds categories

# CONTEXT ####
#reads information for all sites
metaFile = paste0(outDirG,"\\ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( openxlsx :: read.xlsx(metaFile, sheet  = "Summary") ) #xlsx::read.xlsx(metaFile, sheetName = "Summary")
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
## TIMES OF INTEREST ####
TOI = as.data.frame (openxlsx :: read.xlsx(metaFile, sheet = "Time period of interest") )
TOI = TOI[!apply(TOI, 1, function(row) all(is.na(row))), ]
## FREQUENCIES OF INTEREST ####
FOI = as.data.frame ( openxlsx ::read.xlsx(metaFile, sheet = "Frequency of Interest") )
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
for (uu in 1:length(ONMSsites)) { # uu = 1
  
  cat("Processing... ", ONMSsites[uu],"\n" )
  site =  ONMSsites[uu]
  
  #renaming for NRS sites
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
  cat(site, "context: season-", sidx, ";times of interest- ", nrow(TOIs),  ";frequencies of interest- ", nrow(FOIs) )
  
  ## SPL data product ####
  inFile = list.files(outDirP, pattern = paste0("data_", tolower(site1), "_HourlySPL-gfs_.*\\.Rda$"), full.names = T)
  file_info = file.info(inFile) 
  load( inFile[which.max(file_info$ctime)] ) #only load the most recent!
  if (nrow (outData) > 0 ) {
    gps = outData
    rm(outData)
  }
  st = as.Date( min(gps$UTC) )
  ed = as.Date( max(gps$UTC) )
  udays = length( unique(as.Date(gps$UTC)) )
  cat("Input Data - ", site, " has ", udays, " unique days (", as.character(st), " to ",as.character(ed), ")\n")
  Fq = as.numeric( as.character( gsub("TOL_", "",  colnames(gps)[grep("TOL", colnames(gps))] ) ))
  ### calculate spectrum band levels in the TOLs ####
  tol_columns = grep("TOL", colnames(gps))
  gpsTOL = gps
  for( cc in 1:length(tol_columns)) {
    toltmp = colnames(gps)[tol_columns[cc]]
    #find the bandwidth edges
    bw   = TOL_convert$Bandwidth[which(TOL_convert$Nominal == toltmp) ] 
    # cat("bandwidth for", toltmp, "is", bw, "\n" ) 
    dtmp = gps[,tol_columns[cc] ] 
    if ( length(bw) > 0 ){
      gps[,tol_columns[cc] ] = 10*log10 ( (10^(dtmp/10))/ bw )  
    } else {
      gps[,tol_columns[cc] ] = NA
    }
    
  }
  #CHECK difference
  # gpsTOL$TOL_250 - gps$TOL_250 #should be the same offset for each frequency
  
  ### percentiles for all the data ####
  tol_columns = grep("TOL", colnames(gps))
  all_quantiles = apply(gps[, tol_columns, drop = FALSE], 2, quantile, 
                        probs = c(0.90, 0.75, 0.50, 0.25, 0.10), na.rm = TRUE)
  All = as.data.frame( all_quantiles )
  All$Quantile = rownames(All)
  All$Year = "all"
  tol_columns = grep("TOL", colnames(All))
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
  
  # by year 
  yr_split = split(gps, gps$yr) # Calculate quantiles for each year
  year_quantiles = lapply(yr_split, function(season_data) {
    apply(season_data[, tol_columns, drop = FALSE], 2, quantile,  
          probs = c(0.90, 0.75, 0.50, 0.25, 0.10), na.rm = TRUE)   })
  
  # by year 1 
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
  category_counts = gps %>%
    count(wind_category) %>%
    mutate(label = paste(wind_category, ":", n))
  subtitle_text <- paste(category_counts$label, collapse = ", ")
  windInfo = windModel[tolower(windModel$si) == site3,]
  colnums = suppressWarnings(as.numeric(colnames(windInfo)))
  widx = which(!is.na(colnums) & colnums == max(Fq))
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
    scale_fill_manual(values = c("low" = "blue",  "med" = "orange", "high" = "red") )
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
  
  caption_text = paste0(
    "<b>", toupper(site), "</b> (", siteInfo$`Oceanographic category`, ")<br>",
    "<b>Vertical lines/shaded area</b> indicate frequencies for sounds of interest in this soundscape<br>",
    "<b>Black lines</b> are modeled wind noise at this depth [", windLow, " m/s & ", windUpp, " m/s]<br>",
    "<b>Dotted sound level</b> curve is the median for all data"   )
  
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

  
  ### format for plot ####
  tol_columns = grep("TOL", colnames(yearAll))
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
      #title = paste0(toupper(site), "(",siteInfo$`Oceanographic category`, ")"), 
      caption  =caption_text,
      x = "Frequency Hz",
      y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) ),
      if (sidx == "biological"){
        subtitle = paste0( "Data summarized for Peak season only" ) 
      }) +
    theme(legend.position = "right",
          plot.caption = ggtext::element_markdown(hjust = 0),
          #plot.caption = element_text(size = 12, hjust = 0),
          plot.title = element_text(size = 16, face = "bold", hjust = 0),
          plot.legend = element_text(size  = 12), # Caption text size
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
  tol_columns = grep("TOL", colnames(seasonAll))
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
     geom_line(data = mALL[mALL$Quantile == "50%",], aes(x = Frequency, y = SoundLevel), color = "black", linewidth = 1,
               linetype = "dotted")+ 
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
          plot.caption = ggtext::element_markdown(hjust = 0),
          #plot.caption = element_text(size = 12, face = "italic"), 
          plot.legend = element_text(size =12), # Caption text size
          axis.title.x = element_text(size = 12),           # X-axis label size
          axis.title.y = element_text(size = 12),           # Y-axis label size
          axis.text = element_text(size = 12)               # Tick mark labels
          ) +  # This line removes the legend
    labs(
      caption  = caption_text,
      if (sidx == "biological"){
        subtitle = paste0( "Data summarized for Peak season only" ) 
      } ,
      x = "Frequency Hz",
      y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) )
    )
  p
  separator <- grid.rect(gp = gpar(fill = "black"), height = unit(2, "pt"), width = unit(1, "npc"))
  # arranged_plot = grid.arrange(p, separator, l, heights =c(4, 0.05, 0.8))
  arranged_plot = grid.arrange(p, separator, p2, heights =c(4, 0.1, 1))
  ### save: plot seasonal spectra ####
  ggsave(filename = paste0(outDirG, "//plot_", tolower(site), "_SeasonalSPL.jpg"), plot = arranged_plot, width = 10, height = 12, dpi = 300)
  
  # (4) TIME SERIES - fqIn (125 Hz TOL) ####
  # Questions: How many days are above typical conditions for vessel noise?
  cols_to_select = c("UTC", "windMag","wind_category",fqIn)
  gpsFQ = gps %>% select(all_of(cols_to_select))
  wspeeds = unique( (windModel$windSpeed) )
  gpsFQ$closest_windMag = wspeeds[pmax(1, findInterval(gpsFQ$windMag, wspeeds)+1)]
  #Thresholds from all data percentiles
  mALL$FrequencyName = paste0("TOL_", mALL$Frequency)
  thresholds = mALL[mALL$FrequencyName == fqIn,]
  threshold_mid =  thresholds$SoundLevel[thresholds$Quantile == "50%"]
  threshold_up  = thresholds$SoundLevel[thresholds$Quantile  == "75%"]
  threshold_lo  = thresholds$SoundLevel[thresholds$Quantile == "25%"]
  gpsFQ$SpecBand = gpsFQ[fqIn]
  
  ### daily percentiles ####
  dailyFQ = gpsFQ %>%
    mutate(Date = as.Date(UTC)) %>%
    group_by(Date) %>%
    summarise(
      TOL_25 = quantile(SpecBand, 0.25, na.rm = TRUE),
      TOL_50 = quantile(SpecBand, 0.50, na.rm = TRUE),
      TOL_75 = quantile(SpecBand, 0.75, na.rm = TRUE),
      windspeed = quantile(windMag, 0.50, na.rm = TRUE),
      count_above_threshold = sum(SpecBand > threshold_up, na.rm = TRUE),
      percent_above_threshold = sum(SpecBand > threshold_up, na.rm = TRUE) / 
        sum(!is.na(SpecBand)) * 100
    )
  dailyFQ$yr = year(dailyFQ$Date)
  dailyFQ$Julian = yday(dailyFQ$Date)
  # Fill in missing days
  dailyFQ_complete = dailyFQ %>%
    group_by(yr) %>%
    complete(Julian = seq(min(Julian), max(Julian), by = 1)) %>% 
    arrange(yr, Julian) 
  monthly_sequence = seq.Date(as.Date("2021-01-01"), as.Date("2021-12-01"), by = "month")
  month_names_seq   = format(monthly_sequence, "%b")  # Extracts full month names
  days_of_year_for_months = yday(monthly_sequence)
  
  ### annual status ####
  gpsFQ$yr = year(gpsFQ$UTC)
  yrFQ = gpsFQ %>% group_by(yr) %>%
    summarise(
      TOL_25 = quantile(SpecBand, 0.25, na.rm = TRUE),
      TOL_50 = quantile(SpecBand, 0.50, na.rm = TRUE),
      TOL_75 = quantile(SpecBand, 0.75, na.rm = TRUE) )
  ### thresholds for typical ####
  q90=thresholds$SoundLevel[thresholds$Quantile == "90%"]
  q75=thresholds$SoundLevel[thresholds$Quantile == "75%"]
  q50=thresholds$SoundLevel[thresholds$Quantile == "50%"]
  q25=thresholds$SoundLevel[thresholds$Quantile == "25%"]
  q10=thresholds$SoundLevel[thresholds$Quantile == "10%"]
  #threshold bands with categories
  threshold_bands <- data.frame(
    category = c("Very Low", "Low", "Within Range", "High", "Very High"),
    xmin = c(q10-5,    q10,    q25,    q75,    q90),
    xmax = c(q10,     q25,    q75,    q90,    q90+5),
    fill = c("#6699CC", "#99CCFF", "#CCCCCC", "#FF9999", "#CC0000")
  )
  threshold_bands$category <- factor(
    threshold_bands$category,
    levels = c("Very Low", "Low", "Within Range", "High", "Very High")
  )

  ### plot: threshold bars ####
  pthrs = ggplot() +
    # Threshold bands (same for all facets)
    geom_rect(data = threshold_bands,
              aes(xmin = xmin, xmax = xmax, ymin = 0.4, ymax = 0.6, fill = category),
              color = "white") +
    # Marker line per year
    geom_vline(data = yrFQ, aes(xintercept = TOL_50), 
               linetype = "dashed", color = "black", linewidth = .5) +
    # Facet by year
    facet_wrap(~yr, ncol = 1) +  
    # Customize scales and colors
    scale_fill_manual(values = setNames(threshold_bands$fill, threshold_bands$category)) +
    coord_cartesian(xlim = c(60, 90), ylim = c(0.3, 0.8)) +
    # more formatting
    theme_void() +
    theme( legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(title = "Annual Status", fill = "", 
         subtitle = paste0(toupper(site), "(",siteInfo$`Oceanographic category`, ")"),
         caption  = "status is set at median for the year",
         strip.text = element_text(hjust = 0))
  pthrs 
  ### plot: time series ####
  plg =  ggplot(dailyFQ_complete, aes(x = Julian, y = TOL_50, group = yr) ) +
    
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = threshold_lo, ymax = threshold_up,
             fill = "#CCCCCC", alpha = 0.2) +
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
      title    = "Is large vessel noise within typical conditions?", 
      subtitle =  paste0(toupper(site), "(",siteInfo$`Oceanographic category`, ")"), #toupper(site),
      caption  = "Typical conditions are in gray area (25th and 75th percentiles of all the data)", 
      x = "",
      y = substitute(
        paste("Daily Median Sound Levels (dB re 1 ", mu, " Pa/Hz at ", f, " Hz)"),
        list(f = fqInN) )
        # expression(paste("Daily Median Sound Levels (dB re 1 ", mu, " Pa/Hz at,", fqInN, "Hz)" ) )
    )
  ### save: plot 125 Hz time series with thresholds ####
  plg2 = plg + pthrs + plot_layout(ncol = 2, widths = c(2, 1))  #plg = grid.arrange(plg, pthrs, nrow = 1, widths = c(2, 1))
  plg2
  ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_LgVesselNoise125.jpg"), plot = plg2, width = 10, height = 12, dpi = 300)
  
  # (4b) TIME SERIES- fqIn ####
  # plot with error bars and median with times of interest, and hours above 75th percentile in title
  # hours above 75th percentile
  yrs = unique(dailyFQ_complete$yr)
  dailyFQ_complete$facet_title = NA
  for ( ii in 1: length(yrs) ) {
    idx  =  which(dailyFQ_complete$yr == yrs[ii])
    tmp = dailyFQ_complete[ dailyFQ_complete$yr == yrs[ii] ,] 
    # count total hours above threshold
    daysAbove = round( sum( tmp$count_above_threshold , na.rm = T) )
    dailyFQ_complete$facet_title[idx] = paste0(yrs[ii], "- ", daysAbove," hours above" )
  }
  #time periods of interest
  TOIs = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr))
  if ( nrow( TOIs)  > 0) {
    for (ii in 1:nrow(TOIs) ) {
      TOIs$facet_title[ii] = dailyFQ_complete$facet_title[which(dailyFQ_complete$yr == TOIs$Year[ii])] [1]
    }
  }

  
  p0 = ggplot(dailyFQ_complete, aes(x = Julian, y = TOL_50, group = yr, color = factor(yr))) 
  # add shaded areas for times of interest
  if (nrow(TOIs) > 0) {
    p0 = p0 + geom_rect(
      data = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr)), 
      inherit.aes = FALSE,
      aes(xmin = Start_Julian, xmax = End_Julian, ymin = -Inf, ymax = Inf), 
      fill = "grey", alpha = 0.2  # Optional styling
    )
  }

  p <- p0 + geom_line() +
    
    scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year)))))  +
    geom_line(linewidth = 1, na.rm = TRUE) +
    geom_ribbon(aes(ymin = TOL_25 , ymax = TOL_75 ), fill = "gray", alpha = 0.5) +
    facet_wrap(~facet_title, nrow = length(unique(dailyFQ$yr)) ) +
    
    scale_x_continuous(breaks = days_of_year_for_months, labels = month_names_seq) +  
    geom_hline(aes(yintercept = threshold_mid), linetype = "dashed", color = "gray", linewidth = 0.7) +
    
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, hjust = 0, vjust = 0),  # Facet labels inside (centered)
      strip.background = element_blank(),  # Remove background behind facet labels
      panel.spacing = unit(.1, "lines") ) + # Adjust the spacing between facets) +
    labs(
      title = paste0("How oftern are sound levels above typical conditions? (", fqInN,")" ) ,
      subtitle = paste0(toupper(site), "(",siteInfo$`Oceanographic category`, ")"),
      caption = paste0("Dashed line- median for all data"),
      x = "",
      y = substitute(
        paste("Daily Median Sound Levels (dB re 1 ", mu, " Pa/Hz at ", f, ")"),
        list(f = fqInN) ),
      color = "Year"  # Label for the color legend
    ) 
  p
  #### save: plot 125 Hz ####
  ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_125Hz.jpg"), plot = p, width = 10, height = 12, dpi = 300)
  
  # (4c) TIME SERIES - fqIn, interactive ####
  yearly_data <- split(dailyFQ_complete, dailyFQ_complete$yr)
  plots <- lapply(yearly_data, function(df) {
    plot_ly(df, x = ~Julian, y = ~TOL_50, type = 'scatter', mode = 'lines') %>%
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
  
  #(5) TIME SERIES- wind dominated in fqIn2 ####
  
  

  # SAVE UPDATED DATA ####
  #save(gps, file = paste0(outDirP, "\\data_", tolower(site), "_HourlySPL-gfs-season_", DC, ".Rda") )
}




