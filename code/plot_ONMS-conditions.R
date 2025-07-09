# Soundscape Condition | tracking how sound levels change in different frequencies using the soundscape metrics

# ADDS context metadata to hourly TOLs-- season
# INPUTS: output of HrTOLs_ONMS.R, loads the most recent file; ONMS metadata; wind Model
# works for each monitoring site

rm(list=ls()) 

# LIBRARIES ####
#devtools::install_github('TaikiSan21/PAMscapes')

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
library(viridis)

#SITES ####
ONMSsites = c("sb01", "sb03", "hi01", "hi03", "hi04", "hi08", "pm01", "as01", "mb01", "mb02", "oc02", "cb11" )
## directories ####
outDir =   "F:\\CODE\\GitHub\\SoundscapesWebsite\\"
outDirG =  paste0(outDir, "content\\resources") #where save graphics
outDirGe =  paste0(outDir, "content\\resources\\extra") #where extra save graphics
outDirC =  paste0(outDir,"context\\") #where to get context

#PARAMETERS ####
DC = Sys.Date()
project = "ONMS"
fqIn = "TOL_125"
fqInN = "125 Hz"
ab = 65 # threshold for above frequency in
fqIn2 = "TOL_500" # wind model comparison- wind dominated frequency
fqIn2name = "500 Hz" # wind model comparison- wind dominated frequency
ab2 = 0 #threshold for dB above a reference value
windUpp = 22.6 #which wind model result to show on plot
windLow = 1 #which wind model result to show on plot
windH = 10 #wind speeds categories
windL = 5 #wind speeds categories
removess = 0 # set to 1 if you want to truncate the time series

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
endSS = as.data.frame ( openxlsx :: read.xlsx(metaFile, sheet  = "SantSound") )
endSS$endSS =  as.Date(endSS$endSS, origin = "1899-12-30")
## FREQUENCIES OF INTEREST ####
FOI = as.data.frame ( openxlsx ::read.xlsx(metaFile, sheet = "Frequency of Interest") )
FOI = FOI[!apply(FOI, 1, function(row) all(is.na(row))), ]
FOI$Sanctuary = tolower(FOI$Sanctuary)
FOI = FOI[FOI$`Show.on.plot?` == "Y",]
FOIp = FOI[FOI$`Track.this.FQ.as.indicator.for.sources?` == "Y",]
## TOL CONVERSION ####
TOL_convert = read.csv(paste0(outDirC,"TOLconvert.csv"))
TOL_convert$Nominal = paste0("TOL_",TOL_convert$Center)
## WIND NOISE MODEL ####
windFile = list.files(outDirC, pattern = paste0("WindModel_", project), full.names = T)
file_info = file.info(windFile)
load( windFile[which.max(file_info$ctime)] ) #only load the most recent!

# PROCESS BY SITE #### 
for (uu in 1:length(ONMSsites)) { # uu = 1
  
  suppressWarnings ( rm(gps, outData) )
  cat("Processing... ", ONMSsites[uu],"\n" )
  site =  ONMSsites[uu]
  
  #renaming for NRS sites
  if (site == "cb11") {
    outDirP = paste0( outDir,"products\\", substr(tolower(site), start = 1, stop =2),"\\" ) #products
    site1 = "NRS11"
    site3 = "cbnrs11"
    site = "NRS11"
    site5 = "cb11"
  } else {
    site1 = site
    site3 = site
    site5 = site
    outDirP = paste0( outDir,"products\\", substr(tolower(site), start = 1, stop =2),"\\" )#products
  }

  ## SITE PARAMETERS ####
  siteInfo = lookup[lookup$`NCEI ID` == tolower(site1),]
  siteInfo = siteInfo[!is.na(siteInfo$`NCEI ID`), ]
  ##frequency of interest ####
  FOIs = FOI [ FOI$Sanctuary == substr(site5, 1,2), ]
  ##frequency(s) to track
  FOIst = FOIs [ FOIs$`Track.this.FQ.as.indicator.for.sources?` == "Y", ] 
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
  #put in alphabetical order so plots line up!!!
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
  }else if ( sidx == "southernHem") {
    season = data.frame(
      Season = c( "Humpback", "Humpback peak", "Hurricane", "Tradewind" ),
      Months = c("6,7,8,9,10" , "8,9,10" ,"11,12,1,2,3,4",  "5,6,7,8,9,10") ,
      values = c(   "#56B4E9",  "#009E73", "#CC79A7", "#E69F00") )
    seasonLabel = "Humpback (Jun-Oct), Humpback peak (Aug-Oct), Hurricane (Nov-Apr), Tradewind (May-Oct)"
    
    
  }
  
  ## SPL data product ####
  inFile = list.files(outDirP, pattern = paste0("data_", tolower(site1), "_HourlySPL-gfs_.*\\.Rda$"), full.names = T)
  file_info = file.info(inFile) 
  load( inFile[which.max(file_info$ctime)] ) #only load the most recent!
  if( exists("outData") ) {
    gps = outData
    rm(outData)
  }
  st = as.Date( min(gps$UTC) )
  ed = as.Date( max(gps$UTC) )
  udays = length( unique(as.Date(gps$UTC)) )
  #cat("Input Data - ", site, " has ", udays, " unique days (", as.character(st), " to ",as.character(ed), ")\n")
  Fq = as.numeric( as.character( gsub("TOL_", "",  colnames(gps)[grep("TOL", colnames(gps))] ) ))
  
  ##REMOVE sanctsound data ####
  if (removess == 1){
    idxDD = which(endSS$site == site)
    if (length(idxDD) > 0) {
      gpsT = gps[gps$UTC > endSS$endSS[idxDD],] 
      gps = gpsT
    }
    
  }
  ## calculate spectrum band levels in the TOLs ####
  # assumes all TOL data are in broadband measurements, NOT per/Hz
  # this is needed to compare with the wind model results 
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
  
  ## add SEASON ####
  seas = unique(season$Season)
  for( ss in 1:length(seas) ){
    moi = as.numeric(unlist(strsplit(as.character(season$Months[ss]), ","))) 
    gps$Season[gps$mth %in% moi] = season$Season[ss]   }
  
  ##REMOVE years with low data ####
  # to make sure at least 15 day in a given year.
  gps$yr = year(gps$UTC)
  years_to_keep <- gps %>%
    count(yr) %>%
    filter(n >= 360) %>%
    pull(yr)
  
  gps = gps %>%
    filter(yr %in% years_to_keep)
  
  st = as.Date( min(gps$UTC) )
  ed = as.Date( max(gps$UTC) )
  udays = length( unique(as.Date(gps$UTC)) )
 
  # CHECK: DATA SUMMARY ####
  cat(site, "context summary:", siteInfo$`Oceanographic category`,  ";season-",  unique(gps$Season), ";times of interest- ", nrow(TOIs), 
      ";frequencies of interest- ", nrow(FOIst), "\n",
      "Input Data - ", udays, " unique days (", as.character(st), " to ",as.character(ed), ")\n")
  
  # PERCENTILES for all the data ####
  # all data - NOTE recalculated below if only plotting peak
  tol_columns = grep("TOL", colnames(gps))
  all_quantiles = apply(gps[, tol_columns, drop = FALSE], 2, quantile, 
                        probs = c(0.99, 0.90, 0.75, 0.50, 0.25, 0.10, .01), na.rm = TRUE)
  # this re-formats the quantiles for plotting
  All = as.data.frame( all_quantiles )
  All$Quantile = rownames(All)
  All$Year = "all"
  tol_columns = grep("TOL", colnames(All))
  mALL = melt(All, id.vars = c("Quantile","Year"), measure.vars = tol_columns)
  mALL$variable = as.numeric( as.character( gsub("TOL_", "", mALL$variable )))
  colnames(mALL) = c("Quantile", "Year", "Frequency" , "SoundLevel" )
  # by season
  tol_columns = grep("TOL", colnames(gps))
  season_split = split(gps, gps$Season)
  season_quantiles = lapply(season_split, function(season_data) {
    apply(season_data[, tol_columns, drop = FALSE], 2, quantile, 
          probs = c(0.99, 0.90, 0.75, 0.50, 0.25, 0.10, .01), na.rm = TRUE)   })
  # by year -- recalculated below if only plotting peak
  yr_split = split(gps, gps$yr) # Calculate quantiles for each year
  year_quantiles = lapply(yr_split, function(season_data) {
    apply(season_data[, tol_columns, drop = FALSE], 2, quantile,  
          probs = c(0.99, 0.90, 0.75, 0.50, 0.25, 0.10, .01), na.rm = TRUE)   })
  # by year 1 
  yr1 = min( gps$yr )
  tol_columns = grep("TOL", colnames(gps))
  yr1.df = gps[ gps$yr == yr1,]
  yr1_quantiles = apply(yr1.df[, tol_columns, drop = FALSE], 2, quantile, 
                        probs = c(0.99, 0.90, 0.75, 0.50, 0.25, 0.10, .01), na.rm = TRUE)
  
  #(1) WIND category ####
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
    facet_wrap(~yr, ncol = 1)+
    ggtitle("Percent time in different wind speed categories ") +  # Add the main title
    theme_minimal() +
    labs(x = NULL, y = NULL,
         subtitle = paste0("low <", windL, " | med ", windL,"-",windH, " | high >",windH, "m/s")) +  # Remove x-axis label
    theme(
      plot.title = element_text(hjust = 0, size = 16),  # Align the title to the left
      plot.subtitle = element_text(hjust = 0, size = 14),
      strip.text  = element_text(size = 14),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),  # Remove x-axis labels (now categories will appear below)
      axis.ticks.x = element_blank(),  # Remove x-axis ticks
      axis.title.x = element_blank(),  # Remove x-axis title
      legend.position = "bottom",  # Position the legend at the bottom
      legend.title = element_blank(),  # Optional: remove legend title
      legend.text = element_text(size = 12)  # Optional: adjust legend text size
    ) +
    #scale_x_discrete(labels = category_counts$wind_category) +  # Place the category labels under the plot
    scale_fill_manual(values = c("low" = "blue",  "med" = "orange", "high" = "red") )
  l
  #save figure ####
  ggsave(filename = paste0(outDirGe, "//plot_", toupper(site), "_windSpeed.jpg"), plot = l , width = 10, height = 12, dpi = 300)
  
  #(2) EFFORT ALL DATA ####
  ## by month (days/ month-year) ####
  summary <- gps %>%
    mutate(
      year = year(UTC),  # Extract Year
      month = format(UTC, "%m")  # Extract Month (numeric format)
    ) %>%
    count(year, month)  # Count occurrences (hours) in each year-month
  summary$dy = round(summary$n/ 24)
  p1 = ggplot(summary, aes(x = month, y = dy, fill = as.factor(year))) +
    geom_col(position = "dodge", width = .4) +  # Use dodge to separate bars for each year within the same month
    #coord_flip() +
    labs(
      title = "monitoring effort by year (all data)",
      subtitle = paste0(toupper(site), " has ", udays, 
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
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14, hjust = 1, angle = 30),  
      plot.subtitle = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "right" 
    )
  
  p1
  ##save figure ####
  ggsave(filename = paste0(outDirGe, "//plot_", toupper(site), "_Effort.jpg"), plot = p1, width = 10, height = 4, dpi = 300)
  
  ## by season (hours/ season in each year ####
  summary2 <- gps %>%
    mutate(
      year = year(UTC),  # Extract Year
      month = format(UTC, "%m")  # Extract Month (numeric format)
    ) %>%
    count(year, Season)  # Count occurrences (hours) in each year-month
  summary2$dy = round(summary2$n/ 24)
  seasont = season %>% filter(Season %in% unique(summary2$Season) )
  p2 = ggplot(summary2, aes(x = as.character(year), y = dy, fill = as.factor(Season))) +
    geom_col(position = "dodge", width = .3) +  # Use dodge to separate bars for each year within the same month
    #coord_flip()+ 
    labs(
      title = "monitoring effort by season (all data)",
      subtitle  = paste0(toupper(site), " has ", udays, 
                        " unique days: ", as.character(st), " to ", as.character(ed), "\n",
                        seasonLabel),
      x = "",      y = "Days",      fill = "Year"
    ) +
    scale_fill_manual(values = seasont$values) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14, hjust = 1, angle = 30),  
      plot.subtitle = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "right" 
    )
  p2
  ##save figure ####
  ggsave(filename = paste0(outDirGe, "//plot_", toupper(site), "_EffortSeason.jpg"), plot = p2, width = 10, height = 4, dpi = 300)
  
  #(3) SEASONAL CONDITION PLOT ####
  caption_text = paste0(
    "<b>",toupper(site) , " </b> (", siteInfo$`Oceanographic category`, ")<br>",
    "<b>Vertical lines/shaded area</b> indicate frequencies for sounds of interest in this soundscape<br>",
    "<b>Black lines</b> are modeled wind noise at this depth [", windLow, " m/s & ", windUpp, " m/s]<br>",
    "<b>Dotted sound level</b> curve is the median for all data")
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
  seasont = season %>% filter(Season %in% unique(mallData$Season) )
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
    # Set color and fill to match season
    
    scale_color_manual(values  = seasont$values ) +
    scale_fill_manual (values  = seasont$values ) +
    
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
    labs(
      caption  = caption_text,
      if (sidx == "biological"){
        subtitle = paste0( "Data summarized for Peak season only" ) 
      } ,
      x = "Frequency Hz",
      y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) )
    ) + 
    # Additional aesthetics
    theme_minimal()+
    theme(legend.position = "right",
          plot.caption = ggtext::element_markdown(hjust = 0, size = 14),
          axis.title.x = element_text(size = 14),           
          axis.title.y = element_text(size = 14), 
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 14)             
    )
  
  p
  separator <- grid.rect(gp = gpar(fill = "black"), height = unit(2, "pt"), width = unit(1, "npc"))
  # arranged_plot = grid.arrange(p, separator, l, heights =c(4, 0.05, 0.8))
  arranged_plot = grid.arrange(p, separator, p2, heights =c(4, 0.1, 1))
  ## save figure ####
  ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_SeasonalSPL.jpg"), plot = arranged_plot, width = 10, height = 12, dpi = 300)
  
  #(4) ANNUAL COMPARISION plots ####
  # truncates data to peak season for biological sites- all plots after this are peak only!!!
  if (sidx == "biological"){ #only keep peak
    gpsAll = gps
    my_subtitle = "peak season only"
    gps = gps[gps$Season == "Peak",]
    #redo effort plot so not confusing
    summary <- gps %>%
      mutate(
        year = year(UTC),  # Extract Year
        month = format(UTC, "%m")  # Extract Month (numeric format)
      ) %>%
      count(year, month)  # Count occurrences (hours) in each year-month
    summary$dy = round(summary$n/ 24)
    p1 = ggplot(summary, aes(x = month, y = dy, fill = as.factor(year))) +
      geom_col(position = "dodge", width = .4) +  # Use dodge to separate bars for each year within the same month
      #coord_flip() +
      labs(
        title = "monitoring effort by year (peak only)",
        subtitle = paste0(toupper(site), " has ", udays, 
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
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, hjust = 1, angle = 30),  
        plot.subtitle = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right" 
      )
    
    p1
    ## re-save effort figure ####
    ggsave(filename = paste0(outDirGe, "//plot_", toupper(site), "_Effort.jpg"), plot = p1, width = 10, height = 4, dpi = 300)
  } else {
    gpsAll = gps
    my_subtitle = "all data" 
  }
  
  caption_text = paste0(
    "<b>",toupper(site) , " </b> (", siteInfo$`Oceanographic category`, ")<br>",
    "<b>Vertical lines/shaded area</b> indicate frequencies for sounds of interest in this soundscape<br>",
    "<b>Black lines</b> are modeled wind noise at this depth [", windLow, " m/s & ", windUpp, " m/s]<br>",
    "<b>Dotted sound level</b> curve is the median for ",my_subtitle   )
 
  ## re-calculate percentiles for all the data ####
  # all data - mALL 
  tol_columns = grep("TOL", colnames(gps))
  all_quantiles = apply(gps[, tol_columns, drop = FALSE], 2, quantile, 
                        probs = c(0.99, 0.90, 0.75, 0.50, 0.25, 0.10, .01), na.rm = TRUE)
  All = as.data.frame( all_quantiles )
  All$Quantile = rownames(All)
  All$Year = "all"
  tol_columns = grep("TOL", colnames(All))
  mALL = melt(All, id.vars = c("Quantile","Year"), measure.vars = tol_columns)
  mALL$variable = as.numeric( as.character( gsub("TOL_", "", mALL$variable )))
  colnames(mALL) = c("Quantile", "Year", "Frequency" , "SoundLevel" )
 # by year- mallData 
  tol_columns = grep("TOL", colnames(gps))
  yr_split = split(gps, gps$yr) # Calculate quantiles for each year
  year_quantiles = lapply(yr_split, function(season_data) {
    apply(season_data[, tol_columns, drop = FALSE], 2, quantile,  
          probs = c(0.99, 0.90, 0.75, 0.50, 0.25, 0.10, .01), na.rm = TRUE)   })
  
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
    
    #median TOL values- each year
    geom_line(data = mallData[mallData$Quantile == "50%",], aes(x = Frequency, y = SoundLevel, color = Year), linewidth = 2) +
    #median TOL values- all data
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
      caption  = caption_text,
      x = "Frequency Hz",
      y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) ),
      if (sidx == "biological"){
        subtitle = paste0( "Data summarized for Peak season only" ) 
      }) +
    theme(legend.position = "right",
          plot.caption = ggtext::element_markdown(hjust = 0, size = 12),
          axis.title.x = element_text(size = 14),           # X-axis label size
          axis.title.y = element_text(size = 14),           # Y-axis label size
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 12)
    ) 
  p
  separator <- grid.rect(gp = gpar(fill = "black"), height = unit(2, "pt"), width = unit(1, "npc"))
  # arranged_plot = grid.arrange(p, separator, l, heights =c(4, 0.05, 0.8))
  pYear = grid.arrange(p, separator, p1, heights =c(4, 0.1, 1))
  
  ### save figure ####
  ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_YearSPL.jpg"), plot = pYear, width = 10, height = 12, dpi = 300)
  
  #(5) TIME SERIES- FOI ####
  # plot with error bars and median and hours above 75th percentile in title
  if ( nrow(FOIst) > 0 ) {
    for (tt in 1: nrow(FOIst) ){
     
      #check to see if the FOI is broad band
      if (FOIst$FQstart [tt] == FOIst$FQend [tt] ){
        ftN = paste0("TOL_", FOIst$FQstart [tt]) #fqIn
        ft = FOIst$FQstart [tt]
        cols_to_select = c("UTC", "windMag","wind_category",ftN )
        gpsFQ = gps %>% select(all_of(cols_to_select))
        wspeeds = unique( (windModel$windSpeed) )
        gpsFQ$closest_windMag = wspeeds[pmax(1, findInterval(gpsFQ$windMag, wspeeds)+1)]
        #Thresholds from all data percentiles
        mALL$FrequencyName = paste0("TOL_", mALL$Frequency)
        thresholds = mALL[mALL$FrequencyName == ftN,]
        threshold_min  = thresholds$SoundLevel[thresholds$Quantile == "1%"]
        threshold_mid  = thresholds$SoundLevel[thresholds$Quantile == "50%"]
        threshold_up   = thresholds$SoundLevel[thresholds$Quantile  == "75%"]
        threshold_lo   = thresholds$SoundLevel[thresholds$Quantile == "25%"]
        threshold_max  = thresholds$SoundLevel[thresholds$Quantile == "99%"]
        
        q01 = thresholds$SoundLevel[thresholds$Quantile == "1%"]
        q10 = thresholds$SoundLevel[thresholds$Quantile == "10%"]
        q25 = thresholds$SoundLevel[thresholds$Quantile == "25%"]
        q50 = thresholds$SoundLevel[thresholds$Quantile == "50%"]
        q75 = thresholds$SoundLevel[thresholds$Quantile  == "75%"]
        q90 = thresholds$SoundLevel[thresholds$Quantile == "90%"]
        q99 = thresholds$SoundLevel[thresholds$Quantile == "99%"]
        
        gpsFQ$SpecBand = gpsFQ[ftN]
      }else {
        
        ft = paste0( FOIst$FQstart [tt], "-",  ft = FOIst$FQend [tt])
        
        tolc = ( grep("TOL", colnames(gps)) )
        toln =  as.numeric( gsub("TOL_", "", colnames(gps)[tolc]) )
        idx = tolc[( toln >= FOIst$FQstart[tt] & toln <= FOIst$FQend [tt] ) ]
        ftN = colnames(gps)[idx]
        cols_to_select = c("UTC", "windMag","wind_category",ftN )
        gpsFQ = gps %>% select(all_of(cols_to_select))
        tmpBB = gpsFQ %>% select(all_of(ftN))
        # calculate a BB measurement for TOL FQ- unlog, sum, re-log
        gpsFQ$SpecBand = 10*log10 ( rowSums( 10^(tmpBB/10) ) ) 
        
        # thresholds for typical-- BB 
        # calculate a BB measurement for TOL FQ- unlog, sum, re-log
        mALL$FrequencyName = paste0("TOL_", mALL$Frequency)
        thresholds = mALL[mALL$FrequencyName %in% ftN, ]
        thresholds2 <- thresholds %>%
          group_by(Quantile) %>%
          summarise(
            summed_dB = 10 * log10(sum(10^(SoundLevel/10), na.rm = TRUE))
          )
        # all data thresholds 
        q01 = thresholds2$summed_dB[thresholds2$Quantile == "1%"]
        q10 = thresholds2$summed_dB[thresholds2$Quantile == "10%"]
        q25 = thresholds2$summed_dB[thresholds2$Quantile == "25%"]
        q50 = thresholds2$summed_dB[thresholds2$Quantile == "50%"]
        q75 = thresholds2$summed_dB[thresholds2$Quantile  == "75%"]
        q90 = thresholds2$summed_dB[thresholds2$Quantile == "90%"]
        q99 = thresholds2$summed_dB[thresholds2$Quantile == "99%"]
      }
      
      ### daily percentiles with hours above threshold- 75th precentile
      dailyFQ = gpsFQ %>%
        mutate(Date = as.Date(UTC)) %>%
        group_by(Date) %>%
        summarise(
          TOL_25 = quantile(SpecBand, 0.25, na.rm = TRUE),
          TOL_50 = quantile(SpecBand, 0.50, na.rm = TRUE),
          TOL_75 = quantile(SpecBand, 0.75, na.rm = TRUE),
          windspeed = quantile(windMag, 0.50, na.rm = TRUE),
          count_above_threshold   = sum(SpecBand > q75, na.rm = TRUE),
          percent_above_threshold = sum(SpecBand > q75, na.rm = TRUE) / 
            sum(!is.na(SpecBand)) * 100
        )
      dailyFQ$yr = year(dailyFQ$Date)
      dailyFQ$Julian = yday(dailyFQ$Date)
      #(hist(dailyFQ$percent_above_threshold))
    
      # Fill in missing days
      dailyFQ_complete = dailyFQ %>%
        group_by(yr) %>%
        complete(Julian = seq(min(Julian), max(Julian), by = 1)) %>% 
        arrange(yr, Julian) 
      monthly_sequence = seq.Date(as.Date("2021-01-01"), as.Date("2021-12-01"), by = "month")
      month_names_seq   = format(monthly_sequence, "%b")  # Extracts full month names
      days_of_year_for_months = yday(monthly_sequence)
      
      ### annual status
      gpsFQ$yr = year(gpsFQ$UTC)
      yrFQ = gpsFQ %>% group_by(yr) %>%
        summarise(
          TOL_25 = quantile(SpecBand, 0.25, na.rm = TRUE),
          TOL_50 = quantile(SpecBand, 0.50, na.rm = TRUE),
          TOL_75 = quantile(SpecBand, 0.75, na.rm = TRUE) )
      
      #threshold bands with categories-- from all data
      threshold_bands <- data.frame(
        category = c("Very Low", "Low", "Within Range", "High", "Very High"),
        xmin = c(q01,    q10,    q25,    q75,    q90),
        xmax = c(q10,     q25,    q75,    q90,    q99),
        fill = c("#6699CC", "#99CCFF", "#CCCCCC", "#FF9999", "#CC0000")
      )
      
      threshold_bands$category <- factor(
        threshold_bands$category,
        levels = c("Very Low", "Low", "Within Range", "High", "Very High")
      )
      yrFQ$yr = factor(yrFQ$yr, levels = rev(sort(unique(yrFQ$yr))))
     
      ### plot: threshold bars
      pthrs = ggplot() +
        # Threshold bands (same for all facets)
        geom_rect(data = threshold_bands,
                  aes(xmin = xmin, xmax = xmax, ymin = 0.4, ymax = 0.6, fill = category),
                  color = "white") +
        # Marker line per year- median
        geom_vline(data = yrFQ, aes(xintercept = TOL_50), 
                   linetype = "dashed", color = "black", linewidth = .5) +
        # Facet by year
        facet_wrap(~yr, ncol = 1) +  
        # Customize scales and colors
        scale_fill_manual(values = setNames(threshold_bands$fill, threshold_bands$category)) +
        #coord_cartesian(xlim = c(threshold_lo-1, threshold_up +1), ylim = c(0.2, 0.9)) +
        # more formatting
        theme_void() +
        theme( legend.position = "right",
               plot.title = element_text(hjust = 0.5, size = 16),
               plot.caption = element_text(size = 12, hjust = 1),
               plot.subtitle = element_text(hjust = 0.5, size = 14),
               axis.title.x = element_text(size = 14),
               strip.text = element_text(size = 14) ) +
        labs(title = paste0("Annual Status for ", ft, "Hz" ), fill = "", 
             #subtitle = paste0(toupper(site), " (",siteInfo$`Oceanographic category`, ")"),
             subtitle  = "status is set at median for the year",
             strip.text = element_text(hjust = 0))
      pthrs 
      ### plot: time series ####
      dailyFQ_complete$yr = factor(dailyFQ_complete$yr, levels = rev(sort(unique(dailyFQ_complete$yr))))
     
      plg =  ggplot(dailyFQ_complete, aes(x = Julian, y = TOL_50, group = yr) ) +
        
        annotate("rect",
                 xmin = -Inf, xmax = Inf,
                 ymin = q25, ymax = q75,
                 fill = "#CCCCCC", alpha = 0.2) +
        annotate("rect",
                 xmin = -Inf, xmax = Inf,
                 ymin = q75, ymax = q99 ,
                 fill = "lightcoral", alpha = 0.2) +
        annotate("rect",
                 xmin = -Inf, xmax = Inf,
                 ymin = q01, ymax = q25,
                 fill = "lightblue", alpha = 0.2) +
        ylim(q01, q99) +
        
        geom_hline(aes(yintercept = q50),linetype = "dashed", color = "gray",linewidth = .2) +
        geom_hline(aes(yintercept = q75),linetype = "dashed", color = "gray",linewidth = .2) +
        geom_hline(aes(yintercept = q25),linetype = "dashed", color = "gray",linewidth = .2) +
        
        geom_line() +
        facet_wrap(~yr, ncol = 1)+
        scale_x_continuous(breaks = days_of_year_for_months, labels = month_names_seq) +
        labs(
          title    = paste0("Are sound levels above \ntypical conditions for ", ft, "Hz?" ) , 
          subtitle =  paste0(toupper(site), " (",siteInfo$`Oceanographic category`, ")"), #toupper(site),
          caption  = "Typical conditions are in gray area (25th and 75th percentiles of all the data)", 
          x = "",
          y = substitute(
            paste("Daily Median Sound Levels (dB re 1 ", mu, " Pa/Hz at ", f, " Hz)"),
            list(f = ft) )
          # expression(paste("Daily Median Sound Levels (dB re 1 ", mu, " Pa/Hz at,", fqInN, "Hz)" ) )
        ) +
        theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(size = 16, face = "bold", hjust = 0),
              axis.text.x = element_text(size = 14, hjust = 1, angle = 30),
              axis.text.y = element_text(size = 14, hjust = 1),
              strip.text = element_text(hjust = 0, size = 12),
              plot.caption = element_text(size = 12, hjust = 0),
              plot.subtitle = element_text(size = 12)) 
        
      ## save: plot 125 Hz time series with thresholds ####
      plg
      plg2 = plg + pthrs + plot_layout(ncol = 2, widths = c(2, 1))  #plg = grid.arrange(plg, pthrs, nrow = 1, widths = c(2, 1))
      plg2
      
      ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "-", ft, "_status.jpg"), plot = plg2, width = 10, height = 12, dpi = 300)
      
    }
  }
  
  #(5) Wind Dominated in fqIn2 ####
  # select only data for fqIn2
  cols_to_select = c("UTC", "windMag","wind_category",fqIn2)
  gpsFQ = gps %>% select(all_of(cols_to_select))
  gpsFQ$Day = as.Date( gpsFQ$UTC )
  gpsFQ$yr = year(gpsFQ$UTC )
  gpsFQ$mth = month(gpsFQ$UTC )
  # what wind speed catergory is the measured value closest to?
  wspeeds = unique( (windModel$windSpeed) )
  gpsFQ$closest_windMag = wspeeds[pmax(1, findInterval(gpsFQ$windMag, wspeeds)+1)]
  # what is the spl values for that windspeed?
  fqIdx = which( colnames( windInfo) == substr( fqIn2, 5,8)) #'500'
  wsIdx = match(gpsFQ$closest_windMag, windInfo$windSpeed)
  gpsFQ$WindModelfq = windInfo[wsIdx, fqIdx]
  # what difference from the measured vs modeled?
  tol_col = grep("TOL", colnames(gpsFQ))
  gpsFQ$Exceed = gpsFQ[,tol_col] -  gpsFQ$WindModelfq 
  # is the difference above or below 0?
  gpsFQ$Windthres = "unk"
  gpsFQ$Windthres[gpsFQ$Exceed <= ab2] = "below"
  gpsFQ$Windthres[gpsFQ$Exceed > ab2]  = "above"
  # what are the all data thresholds for this frequency (not used)
  thresholds = mALL[mALL$FrequencyName == fqIn2,]
  threshold_mid =  thresholds$SoundLevel[thresholds$Quantile == "50%"]
  threshold_up  = thresholds$SoundLevel[thresholds$Quantile  == "75%"]
  threshold_lo  = thresholds$SoundLevel[thresholds$Quantile == "25%"]
  gpsFQ$SpecBand = gpsFQ[fqIn2]

  dayNE = gpsFQ %>%
    mutate(Date = as.Date(UTC)) %>%
    group_by(yr, mth) %>%
    summarise(
      # Count of non-NA hours (remove NAs)
      hrs = sum(!is.na(Exceed ) ) ,  
      
      # hours for the year-day that are at or below wind speed estimate
      n_below = sum(Windthres == "below", na.rm = TRUE),
      n_above = sum(Windthres == "above", na.rm = TRUE),
      
      #percent_below = sum(Windthres == "below", na.rm = TRUE) / hrs * 100,
      #percent_above = sum(Windthres == "above", na.rm = TRUE)/ hrs * 100,
      
      percent_below = ifelse(hrs > 0, n_below / hrs * 100, NA_real_),
      percent_above = ifelse(hrs > 0, n_above / hrs * 100, NA_real_),
      
      # windspeed during the year-day
      windspeed = quantile(windMag, 0.50, na.rm = TRUE),
      .groups = "drop"
    )
  # CHECK: dayNE$percent_below + dayNE$percent_above
  
  dayNE = as.data.frame( dayNE )
  dayNE$yr = factor(dayNE$yr, levels = rev(sort(unique(dayNE$yr))))
  dayNE$mth = factor(dayNE$mth, levels = rev(c(1,2,3,4,5,6,7,8,9,10, 11,12)))
  
  # is there a relationship between percent below and windspeed?
  #plot(dayNE$percent_below, dayNE$windspeed)
  
  windD = ggplot(dayNE, aes(x = factor(mth), y = as.numeric(percent_below), fill = factor( mth ) ) ) +
    geom_col(width = 1) +
    facet_wrap(~yr, ncol = 1) +
    coord_flip() +
    ylim(0,100) +
    scale_fill_viridis_d(option = "D") +
    scale_x_discrete(
      breaks = c("1", "3", "5", "7", "9", "11"),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")
    ) +
    #scale_fill_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
    labs(
      title = paste0("How often can the habitat be considered acoustically inactive? \n(wind-driven noise at ", fqIn2name, ")"),
      subtitle  = paste0(toupper(site), " (",siteInfo$`Oceanographic category`, ")"),
      x = "",
      y = paste0("% of hours in a month wind noise dominate at ", fqIn2name, 
                 "\n (when measured sound level are similar to predicted sound level at known wind speed)"),
      #color = "Year"  # Label for the color legend
    ) +
    theme_minimal()+
    theme(
      legend.position = "none",
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      strip.text  = element_text(size = 14),
      axis.title.x  = element_text(size = 14),
      axis.title.y  = element_text(size = 14),
      axis.text.x = element_text(size = 14, hjust = 1, angle = 30),
      axis.text.y = element_text(size = 14, hjust = 1) )
  windD
  ggsave(filename = paste0(outDirGe, "//plot_", toupper(site), "_WindDominated.jpg"), plot = windD, width = 10, height = 12, dpi = 300)
 
  windB = ggplot(dayNE, aes(x = factor(mth), y = as.numeric(percent_above), fill = factor( mth ) ) ) +
    geom_col(width = 1) +
    facet_wrap(~yr, ncol = 1) +
    coord_flip() +
    ylim(0,100) +
    scale_fill_viridis_d(option = "D") +
    scale_x_discrete(
      breaks = c("1", "3", "5", "7", "9", "11"),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")
    ) +
    #scale_fill_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
    labs(
      title = paste0("How often are sources of interest likey present? \n(e.g. vocalizing species or vessel activity) "),
      subtitle  = paste0(toupper(site), " (",siteInfo$`Oceanographic category`, ")"),
      #caption = "Calculated as % hours when measured sound levels are above predicted level based on wind speed",
      x = "",
      y = paste0("% of hours above wind noise ", fqIn2name, "\n (calculated as % hours when measured sound levels are above predicted level based on wind speed)"),
      #color = "Year"  # Label for the color legend
    ) +
    theme_minimal()+
    theme(
      strip.text  = element_text(size = 14),
      plot.caption = element_text(size = 12, face = "italic", hjust = 0), 
      axis.text.x = element_text(size = 16, hjust = 1),
      axis.title.x  = element_text(size = 14),
      axis.title.y  = element_text(size = 14),
      axis.text.y = element_text(size = 14, hjust = 1), 
      legend.position = "none")
  windB
  ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_AboveWind.jpg"), plot = windB, width = 10, height = 12, dpi = 300)
  
  # SAVE UPDATED DATA ####
  # names(gps)
  save(gps, file = paste0(outDirP, "\\data_", tolower(site), "_HourlySPL-gfs-season-spectrumlevel_", DC, ".Rda") )
}




