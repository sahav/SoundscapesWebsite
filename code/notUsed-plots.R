# ADD BACK IN ####

# (6) TIME SERIES - above year 1 median ####
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
  
  ### save: above year-1 median ####
  ggsave(filename = paste0(outDirG, "//plot_", toupper(site), "_year1above.jpg"), plot = p5, width = 10, height = 12, dpi = 300)
  
  #(6b) % time above year 1 median- compare the hourly values ####
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

# (7) TIME SERIES - Decibels Above Wind Noise at XX Hz  ####
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


# GANTT CHART ####
#summar_NRS-GCP.R
# ADD details ####
# load(file = fileName )
matched_data = merge(output, lookup, by.x = "Site", by.y = "NCEI ID", all.x = TRUE) # matched_data = output %>%   left_join(lookup, by = c("Site" = "NCEI ID"))
output$Region = matched_data$Region
output$Identifer = matched_data$`Common Name/Identifers`
output$Description = matched_data$`Site Description/Driver for Monitoring Location Choice`

# MAP PLOT + DATA ####
# remove ONMS non-monitoring sites
if (projectN ==  "ONMS") {
  outputT  = output[!is.na(output$Region),] 
  outputTt = outputT[,c(1,10, 9, 3:6, 13, 7:8, 11:12) ] #reorganized
} else {
  outputT  = output
  outputTt = output
}

outputMap =  as.data.frame(
  outputT %>%
    group_by(Site) %>%
    summarise(
      total_days = sum(Days, na.rm = TRUE),           # Summing total duration for each site
      min_start_date = min(Start_Date, na.rm = TRUE),
      min_lat = min(Lat, na.rm = TRUE),
      min_lon = min(Lon, na.rm = TRUE)# Getting the minimum start date for each site
    )
)
outputMap$Project  = projectN
outputMap2a = outputMap

# Create the map
colnames(outputMap2a) = c("Site","TotalDays","Start Date", "Latitude", "Longitude","Project")
outputMap2a$TotalDays = as.numeric(as.character(outputMap2a$TotalDays))
data_sf = st_as_sf(outputMap2a, coords = c("Longitude", "Latitude"), crs = 4326)

# Get world land data (in low resolution, change to 'medium' or 'large' if needed)
world=ne_countries(scale = "medium", returnclass = "sf")
bbox=st_bbox(data_sf)
# Create the map with land backgroun
p = ggplot() +
  geom_sf(data = world, fill = "gray80", color = "gray40") +   # Add land background
  geom_sf(data = data_sf, aes(color = Project, size = TotalDays)) +  # Color by Region, size by total days
  theme_minimal() +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]+5), 
           ylim = c(bbox["ymin"], bbox["ymax"]+5), 
           expand = FALSE) +  # Trim map to data points
  labs(title = "Map of Sites by Region and Total Days",
       size = "Total Days",
       color = "Project") +
  scale_size_continuous(range = c(.5, 10))  # Adjust point size range
p

fileName = paste0(outputDir, "\\", projectN, "_dataMap_", DC )
save(outputMap2a, file = paste0( fileName, ".Rda") )
write.csv(outputMap2a, file = paste0( fileName, ".csv") )
ggsave(filename = paste0( fileName, ".jpg"), plot = p, width = 8, height = 6, dpi = 300)

# GANTT PLOT + DATA ####
uColors = unique(outputT$Instrument)
if (projectN == "onms"){
  instrument_colors <- c(
    "SoundTrap 500" = "#88CCEE",  
    "SoundTrap 600" = "#CC6677", #88CCEE",  
    "SoundTrap 300" = "#44AA99", 
    "HARP" =          "#DDCC77") #DDCC77
} else if (projectN == "NRS") {
  instrument_colors <- c(
    "NRS" = "#88CCEE",  
    "AUH" = "#CC6677",#88CCEE",  
    "HARP" = "#44AA99") 
}

pT = ggplot(outputT, aes(y = Site, x = Start_Date, xend = End_Date)) +
  geom_tile(aes(x = Start_Date, width = as.numeric(End_Date - Start_Date), fill = Instrument), 
            color = "black", height = 0.4) +  # Fill color by Instrument and outline in black
  scale_fill_manual(values = instrument_colors) +  # Use specific colors for instruments
  labs(x = "", y = "", title = paste0(toupper(projectN),  " - Ocean Sound Monitoring Data Summary"),
       subtitle = paste0("NCEI google cloud platform (", typ, ") as of ", format(Sys.Date(), "%B %d, %Y"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(angle = 10, size = 12))
pT
fileName = paste0(outputDir, "\\", projectN, "_dataGantt_", DC )
save(outputT, file = paste0( fileName, ".Rda") )
write.csv(outputT, file = paste0( fileName, ".csv") )
ggsave(filename = paste0( fileName, ".jpg"), plot = pT, width = 8, height = 6, dpi = 300)

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