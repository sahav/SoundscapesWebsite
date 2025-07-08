# Soundscape Condition | tracking how sound levels change when vessel noise is present

# INPUTS: output of plot_ONMS-conditions.R, loads the most recent file; ONMS metadata
# works for each monitoring site AND output of format_AIStransit.R


rm(list=ls()) 

#SITES ####
ONMSsites = c("sb01","sb03","oc02", "cb11" )
## directories ####
outDir   =  "F:\\CODE\\GitHub\\SoundscapesWebsite\\"
outDirG  =  paste0(outDir, "content\\resources\\") #where save graphics
outDirGe =  paste0(outDir, "content\\resources\\extra\\") #where extra save graphics
outDirC  =  paste0(outDir,"context\\") #where to get context
outDirP =   paste0(outDir,"products\\") #where to get context

#INPUT PARAMS ####
DC = Sys.Date()
project = "ONMS"
AISUpp = 5 
AISLow = 2
windUpp = 22.6 #which wind model result to show on plot
windLow = 1
fqShip = "TOL_125"
fqShipN = "125 Hz"

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
## WIND NOISE MODEL ####
windFile = list.files(outDirC, pattern = paste0("WindModel_", project), full.names = T)
file_info = file.info(windFile)
load( windFile[which.max(file_info$ctime)] ) #only load the most recent!
## LOAD AIS data ####
load(paste0(outDirC, "Combine_ONMS_AIStransits_dataF.Rda") )

for (ss in 1:length(ONMSsites)) {
  
  
  # LOOP THROUGH SITES ####
  site   = ONMSsites[ss] #"sb03" # "sb03" nrs11 mb02"
  if (site == "cb11"){
    site1  =  "NRS11" # cbnrs11 is weird..
    site2  = "cbnrs11"
  } else {
    site1 = ONMSsites[ss]
    site2 = ONMSsites[ss]
  }
  stn = substr(site, start = 1, stop= 2)
  
  ## LOAD SOUND DATA ####
  inFile = list.files( paste0(outDirP, stn, "\\"), pattern = paste0("data_", tolower(site1), "_HourlySPL-gfs-season-spectrumlevel.*\\.Rda$"), full.names = T)
  file_info = file.info(inFile) 
  load( inFile[which.max(file_info$ctime)] ) #only load the most recent!
  st = as.Date( min(gps$UTC) )
  ed = as.Date( max(gps$UTC) )
  udays = length( unique(as.Date(gps$UTC)) )
  cat("Input Data - ", site1, " has ", udays, " unique days (", as.character(st), " to ",as.character(ed), ")\n")
  Fq = as.numeric( as.character( gsub("TOL_", "",  colnames(gps)[grep("TOL", colnames(gps))] ) ))
  
  ## SITE PARAMETERS ####
  siteInfo = lookup[lookup$`NCEI ID` == tolower(site1),]
  siteInfo = siteInfo[!is.na(siteInfo$`NCEI ID`), ]
  #frequency of interest
  FOIs = FOI [ FOI$Sanctuary == substr(site1, 1,2), ]
  ##frequency(s) to track
  FOIst = FOIs [ FOIs$`Track.this.FQ.as.indicator.for.sources?` == "Y", ] 
  #times of interest
  TOIs = TOI [ TOI$Site == (site1), ]
  TOIs <- TOIs %>%
    mutate(
      Start_Julian = as.numeric(format(as.Date(start_date), "%j")),
      End_Julian = as.numeric(format(as.Date(end_date), "%j")),
      Mid_Julian = (Start_Julian + End_Julian) / 2  # Midpoint for annotation
    )
  TOIs$yr = TOIs$Year
  cat(site1, "context summary:", siteInfo$`Oceanographic category`,"site ;times of interest- ", nrow(TOIs), 
      ";frequencies of interest- ", nrow(FOIst), "\n")
  
  ## LOAD WIND MODEL ####
  windInfo = windModel[tolower(windModel$si) == site2,]
  colnums = suppressWarnings(as.numeric(colnames(windInfo)))
  widx = which(!is.na(colnums) & colnums == max(Fq))
  windInfo = windInfo[,1:widx] #wind noise by frequency (columns) and speed (rows)
  #re-structure for ggplot
  mwindInfo = melt(windInfo, id.vars = c("windSpeed"), measure.vars = colnames(windInfo)[4:ncol(windInfo)])
  mwindInfo$variable = as.numeric( as.character(mwindInfo$variable ))
  
  ais = aisONMS[ tolower(aisONMS$loc_id) == tolower( site1 ),]
  ais$Start = as.POSIXct( gsub("[+]00", "", ais$start_time_utc), tz = "GMT" ) 
  ais$End   = as.POSIXct( gsub("[+]00", "", ais$end_time_utc), tz = "GMT" ) 
  cat("AIS data for ", site1, as.character( min(ais$Start) ), " to ", as.character( max(ais$Start) ), "\n")
  ais = ais %>%
    mutate(Start_hour = floor_date(Start, unit = "hour"))
  ## TRUNCATE sound data to AIS ####
  gpsAIS  = gps[ gps$UTC > min(ais$Start) & gps$UTC <=  max(ais$Start), ]
  cat("SPL data with AIS for", site1, as.character( min(gpsAIS$UTC) ), " to ", as.character( max(gpsAIS$UTC) ), "\n" )
  cat("removed ", nrow(gps) - nrow(gpsAIS), "hours because no AIS data \n")
  
  ## MATCH AIS ####
  ais_summary = ais %>%
    group_by(Start_hour) %>%
    summarise(
      numAIS = n(),
      minAIS = min(dist_nm, na.rm = TRUE),
      avgAIS = mean(as.numeric(as.character(avg_sog_dw)), na.rm = TRUE),
      .groups = "drop"
    )
  gpsAIS$Start_hour = gpsAIS$UTC

  gpsAIS = gpsAIS %>%
    left_join(ais_summary, by = "Start_hour")
 # gpsAIS$UTC[!is.na(gpsAIS$avgAIS)]
  
  gpsAIS = gpsAIS %>%
    mutate(
      numAIS = ifelse(is.na(numAIS), 0, numAIS)
    )
  #check overlap before moving to graphics
  olap = (length( gpsAIS$UTC[!is.na(gpsAIS$avgAIS) ] ) / nrow(gpsAIS))*100
  if (olap < 10){
    stop("Less than 10% of data with matching AIS")
  }
  cat(site1, "hour with max # ships:", max( gpsAIS$numAIS ) )
  
  
  ## DEFINE AIS categories ####
  gpsAIS$ais_category = NA
  gpsAIS <- gpsAIS %>%
    mutate(ais_category = case_when(
      is.na(numAIS) ~ NA_character_,
      numAIS == 0 ~ "0-none",
      numAIS > 0 & numAIS <= AISLow ~ "1-low",
      numAIS > AISLow & numAIS <= AISUpp ~ "2-med",
      numAIS > AISUpp ~ "3-high"
    ))
  category_counts <- gpsAIS %>%
    count(ais_category) %>%
    mutate(label = paste(ais_category, ":", n))
  subtitle_text <- paste(category_counts$label, collapse = ", ")
  
  # (optional) save: updated data ####
  #save(gpsAIS, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL-gfs-season-ais_", DC, ".Rda") )
  
  ## QUANTILES by ais ####
  tol_columns = grep("TOL", colnames(gpsAIS))
  cat_split = split(gpsAIS, gpsAIS$ais_category) # Calculate quantiles for each season
  ais_quantiles = lapply(cat_split, function(season_data) {
    apply(season_data[, tol_columns, drop = FALSE], 2, quantile, na.rm = TRUE)
  })
  
  tol_columns = grep("TOL", colnames(gpsAIS))
  All = NULL
  for (ii in 1: length(ais_quantiles) ) {
    tmp = as.data.frame ( ais_quantiles[ii] ) 
    colnames(tmp) = colnames(gpsAIS)[tol_columns]
    tmp$Quantile = rownames(tmp)
    tmp$ais = names(ais_quantiles)[ii]
    rownames(tmp) = NULL
    All = rbind(All,tmp)
  }
  
  ## SPECTRAL ANALYSIS ####
  #formatting for plot
  tol_columns = grep("TOL", colnames(All))
  mallData = melt(All, id.vars = c("Quantile","ais"), measure.vars = tol_columns)
  mallData$variable = as.numeric( as.character( gsub("TOL_", "", mallData$variable )))
  colnames(mallData) = c("Quantile", "AIS", "Frequency" , "SoundLevel" )
  #names(mallData)
  #unique(mallData$AIS)
  stAIS = as.Date( min(gpsAIS$UTC) )
  edAIS = as.Date( max(gpsAIS$UTC) )
  
  ## PLOT- % samples ####
  lais = ggplot(gpsAIS, aes(x = "", fill = ais_category)) +
    geom_bar(stat = "count", position = "stack") +  # Stacked bar chart
    coord_flip() +  # Flip the coordinates to make it horizontal
    ggtitle("number of hours in AIS vessels categories") +  # Add the main title
    theme_minimal() +
    labs(x = NULL, y = NULL ) +
    #caption = "low < 3 | med 3-5 | high >5") +  # Remove x-axis label
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
    scale_x_discrete(labels = category_counts$wind_category) +  # Place the category labels under the plot
    scale_fill_manual(values = c("0-none" = "#56B4E9", "1-low" = "#009E73", "3-high" = "#CC79A7", 
                                 "2-med" = "#E69F00"))  # Reverse the legend order
  
  lais
  dBIncrease = NULL
  dBIncrease$dB[1] = round( mallData$SoundLevel[mallData$Frequency == 63 & mallData$Quantile == "50%" & mallData$AIS == "1-low"]  -
                              mallData$SoundLevel[mallData$Frequency == 63 & mallData$Quantile == "50%" & mallData$AIS == "0-none"] )
  dBIncrease$dB[2] = round( mallData$SoundLevel[mallData$Frequency == 125 & mallData$Quantile == "50%" & mallData$AIS == "1-low"]  -
                              mallData$SoundLevel[mallData$Frequency == 125 & mallData$Quantile == "50%" & mallData$AIS == "0-none"] )
  dBIncrease$dBval[1] = mallData$SoundLevel[mallData$Frequency == 63 & mallData$Quantile == "50%" & mallData$AIS == "0-none"]
  dBIncrease$dBval[2] = mallData$SoundLevel[mallData$Frequency == 125 & mallData$Quantile == "50%" & mallData$AIS == "0-none"]
  
  caption_text = paste0(
    "<b>",toupper(site) , " </b> (", siteInfo$`Oceanographic category`, ")<br>",
    "<b>Vertical lines </b> indicate typical ship noise frequencies <br>",
    "<b>RESULT for 63 Hz = </b>", dBIncrease$dB[1],"dB increase <br>",
    "<b>RESULT for 125 Hz = </b>", dBIncrease$dB[2],"dB increase <br>", 
    "<b>AIS categories: </b> low = 1-3 ships | med = 3-5 ships | high = >5 ships <br>",
    "<b>Black lines</b> are modeled wind noise at this depth [", windLow, " m/s & ", windUpp, " m/s]<br>"
  )
  
  ## PLOT- spectra in each category ####
  pais = ggplot() +
    # Add shaded area for 25%-75% range
    geom_ribbon(data = mallData %>% 
                  pivot_wider(names_from = Quantile, values_from = SoundLevel),
                aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = AIS),
                alpha = 0.1) +  # Use alpha for transparency
    
    #median TOL values
    geom_line(data = mallData[mallData$Quantile == "50%",], 
              aes(x = Frequency, y = SoundLevel, color = AIS), linewidth = 2) +
    
    scale_color_manual(values = c("0-none" = "#56B4E9", 
                                  "1-low" = "#009E73", 
                                  "3-high" = "#CC79A7", 
                                  "2-med" = "#E69F00")) +
    scale_fill_manual(values = c("0-none" = "#56B4E9", "1-low" = "#009E73", 
                                 "3-high" = "#CC79A7", "2-med" = "#E69F00")) + 
    
    geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == "22.6",], 
              aes(x = variable, y = value), color = "black",  linewidth = 1) +
    geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == "1",], 
              aes(x = variable, y = value), color = "black",  linewidth = 1) +
    geom_vline(aes(xintercept = 125, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
    
    geom_vline(aes(xintercept = 63, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
    scale_x_log10(labels = label_number()) +  # Log scale for x-axis
    
    # Additional aesthetics
    geom_text(aes(x = 63,  y = dBIncrease$dBval[1]+5, label = "63 Hz"), angle = 0, vjust = 0, hjust = 0, size = 3) +
    geom_text(aes(x = 125, y = dBIncrease$dBval[1]+5, label = "125 Hz"), angle = 0, vjust = 0, hjust = 0, size = 3) +
    theme_minimal() +
    labs(
      title = paste0("Overview of the contribution of ship noise to the soundscape at ", toupper(site) ),
      subtitle = paste0( "data summarized from ", st, " to ", ed),
      caption  =  caption_text,
      x = "Frequency Hz",
      y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) )
    ) +
    theme(legend.position = "bottom",
          plot.caption = ggtext::element_markdown(hjust = 0),
          plot.title = element_text(size = 16, face = "bold", hjust = 0) )
  
  pais
  
  arranged_plot = grid.arrange(pais,lais,heights = c(4, .9))
  
  ## SAVE: AIS spectra ####
  ggsave(filename = paste0(outDirG, "plot_", toupper(site), "_AISNoise.jpg"), plot = arranged_plot, width = 10, height = 10, dpi = 300)
  
  ## PLOT: 125Hz DISTRIBUTION ####
  gpsAIS$ais_category  = ( substr(gpsAIS$ais_category, start = 3, stop = 6) )
  gpsAIS$ais_category2 = factor(gpsAIS$ais_category, 
                                levels = c("none", "low", "med", "high"), 
                                ordered = TRUE)
  medians <- gpsAIS %>%
    group_by(ais_category2) %>%
    summarise(median_value = median(.data[[fqShip]], na.rm = TRUE), .groups = "drop")
  
  # Step 1: Create Pie Chart Data
  pie_data <- gpsAIS %>%
    group_by(ais_category2) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count))
  
  # Step 2: Create Pie Chart
  pie_chart = ggplot(pie_data, aes(x = "", y = percentage, fill = ais_category2)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +  
    scale_fill_brewer(palette = "Set3") +  
    theme_void() +  # Remove background, axes, and gridlines
    theme(legend.position = "none") +
    labs(title = "Proportion of Hours") # Hide legend in pie chart
  
  # Convert pie chart to grob
  pie_grob <- ggplotGrob(pie_chart)
  
  caption_text = paste0(
    "<b>",toupper(site) , " </b> (", siteInfo$`Oceanographic category`, ")<br>",
    "<b>Vertical lines </b> median sound level for each AIS category <br>",
    "<b>AIS categories: </b> low = 1-3 ships | med = 3-5 ships | high = >5 ships <br>" )
  
  pais2 = ggplot(gpsAIS, aes(x = TOL_125, fill = ais_category2)) +
    geom_histogram(binwidth = 1, alpha = 0.6, position = "identity") + 
    geom_vline(data = medians, aes(xintercept = median_value, color = ais_category2), 
               linetype = "solid", size = 1) +  # Add median lines
    labs(
      title = paste0("How much do sound levels at ",fqShipN ," increase when ships are nearby?" ),
      subtitle = paste0(  toupper(site), " (data summarized from ", st, " to ", ed, ")"),
      caption = caption_text,
      x = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) ), 
      y = "Count of Hours",
      fill = "Ships transiting nearby",  # Change legend title for fill
      color = "Ships transiting nearby")  +  # Change legend title for color) +
    scale_fill_brewer(palette = "Set3") +  
    scale_color_brewer(palette = "Set3") +  # Match colors for clarity
    theme_minimal() +
    theme(legend.position = "top",
          plot.caption = ggtext::element_markdown(hjust = 0),
          plot.title = element_text(size = 16, face = "bold", hjust = 0) ) 
  #annotation_custom(pie_grob, xmin = max(gpsAIS$TOL_125) - 20, xmax = max(gpsAIS$TOL_125),
  #ymin = min(pie_data$count), ymax =min(pie_data$count)+300 )  
  
  pais2
  ## SAVE: AIS histogram ####
  ggsave(filename = paste0(outDirG, "plot_", toupper(site), "_AIShist.jpg"), plot = pais2, width = 10, height = 10, dpi = 300)
  
  ## table results ####
  gpsAIS$mth = month( gpsAIS$UTC )
  gpsAIS$yr = year( gpsAIS$UTC )
  #Group by month and AIS category, calculate count
  pie_table <- gpsAIS %>%
    group_by(mth, ais_category2) %>%
    summarise(count = n(), .groups = "drop")  # Get count for each AIS category in each month
  # Calculate percentage of total count per month
  pie_table <- pie_table %>%
    group_by(mth) %>%
    mutate(percentage = round(count / sum(count) * 100))  # Percentage relative to total count for each month
  pie_table$percentage = round(pie_table$percentage,1)
  #Pivot to get months as rows, AIS categories as columns
  pie_table_wide <- pie_table %>%
    select(-count) %>%  # Remove count column
    pivot_wider(names_from = ais_category2, values_from = (percentage), values_fill = list(percentage = 0))
  
  # format table
  # rm(table_grob)
  table_grob <- tableGrob(as.data.frame( pie_table_wide) )
  table_grob <- tableGrob(
    pie_table_wide,
    rows = NULL,           # no row names
    theme = ttheme_minimal(
      core = list(padding = unit(c(2, 2), "mm")),
      colhead = list(padding = unit(c(2, 2), "mm"))
    )
  )
  title_text <- textGrob(
    paste0(paste0("% of hours \n in each AIS\n category (", toupper(site), ")") ) ,
    gp = gpar(fontsize = 12, fontface = "bold")
  )
  table_grob <- gtable::gtable_add_rows(
    table_grob,
    heights = grobHeight(title_text) + unit(4, "mm"), # adjust for padding
    pos = 0
  )
  table_grob <- gtable::gtable_add_grob(
    table_grob,
    list(title_text),
    t = 1, l = 1, r = ncol(table_grob)  # span all columns
  )
  ## SAVE: table ####
  ggsave(paste0(outDirG, "table_", toupper(site), "_AIShist.jpg"), table_grob, width = 8, height = 5)
  
  ## MONTHLY ABOVE AT 125 HZ ####
  # calculate - each month-year median level for each category
  gpsAIS$day = as.Date(gpsAIS$UTC)
  gpsAIS$mth = month(gpsAIS$UTC)
  gpsAIS$yr  = year(gpsAIS$UTC)
  gpsAIS$ais = "present"
  gpsAIS$ais[gpsAIS$ais_category2 == "none"] = "none"
  medians <- gpsAIS %>%
    group_by(mth, yr, ais) %>%
    summarise(median_value = median(.data[[fqShip]], na.rm = TRUE), .groups = "drop")
  # calculate - difference in median levels 
  medians_diff <- gpsAIS %>%
    group_by(mth, yr, ais) %>%
    summarise(median_value = median(TOL_125, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = ais, values_from = median_value) %>%  # Make columns for "none" & "present"
    mutate(difference = `present` - `none`)  # Calculate difference
  
  medians_diff = as.data.frame( medians_diff )
  medians_diff$yr = factor(medians_diff$yr, levels = rev(sort(unique(medians_diff$yr))))
  medians_diff$mth = factor(medians_diff$mth, levels = rev(c(1,2,3,4,5,6,7,8,9,10, 11,12)))
  pais3 = ggplot(medians_diff, aes(x = factor(mth), y = as.numeric(difference), fill = factor( mth ) ) ) +
    geom_col(width = 1) +
    facet_wrap(~yr, ncol = 1) +
    coord_flip() +
    scale_fill_viridis_d(option = "D") +
    scale_x_discrete(
      breaks = c("1", "3", "5", "7", "9", "11"),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")
    ) +
    labs(
      title = paste0("How much do sound levels at ",fqShipN ," increase when ships are nearby?" ),
      subtitle  = paste0(toupper(site), " (",siteInfo$`Oceanographic category`, ")"),
      y = "Sound Level difference (dB) ",
      x = "",
      caption = "(monthly median of hourly differences)"
    ) + 
    theme_minimal() +
    theme(
      legend.position = "none")
  pais3
  ## SAVE: MONTHLY ABOVE ####
  ggsave(filename = paste0(outDirG, "plot_", toupper(site), "_AISMonthAbove.jpg"), plot = pais3, width = 12, height = 8, dpi = 300)
  
  ## table results ####
  medians_diff$month_name = month(as.numeric( as.character( medians_diff$mth )) , label = TRUE )
  medians_summary <- medians_diff %>%
    group_by(month_name, yr) %>%
    summarise(difference = mean(difference, na.rm = TRUE), .groups = "drop")
  medians_summary$difference = round( medians_summary$difference,  digits = 1)
  # Pivot wider 
  medians_wide <- medians_summary %>%
    pivot_wider(names_from = month_name, values_from = difference)
  # format the table
  rm( table_grob)
  table_grob <- tableGrob(
    medians_wide,
    rows = NULL,           # no row names
    theme = ttheme_minimal(
      core = list(padding = unit(c(1, 1), "mm")),
      colhead = list(padding = unit(c(1, 1), "mm"))
    )
  )
  
  title_text <- textGrob(
    paste0("Sound level difference at ", fqShipN, " for ", toupper(site)),
    gp = gpar(fontsize = 12, fontface = "bold")
  )
  table_grob <- gtable::gtable_add_rows(
    table_grob,
    heights = grobHeight(title_text) + unit(2, "mm"), # adjust for padding
    pos = 0
  )
  table_grob <- gtable::gtable_add_grob(
    table_grob,
    list(title_text),
    t = 1, l = 1, r = ncol(table_grob)  # span all columns
  )
  
  #grid.newpage()
  #grid.draw(table_grob)
  ## SAVE: table ####
  ggsave(paste0(outDirG, "table_", toupper(site), "_AISMonthAbove.jpg"), table_grob, width = 8, height = 5)
  
}



