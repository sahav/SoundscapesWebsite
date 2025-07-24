# PURPOSE -  query NCEI gcp passive acoustic archive to create gantt charts of data in the archive
# NOTES - use command line approach to access files, assumes open sites (do not need authentication step)
# INPUT - gcp directory for project- assumes metadata files are present
# OUTPUT - Rdat file to use in plotting

rm(list=ls()) 

# LOAD LIBRARIES ####
library(stringr)   
library(openxlsx)
library(jsonlite)
library(curl)
library(tidyverse)
library(ncdf4)
library(googlesheets4)
library(openxlsx)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
DC = Sys.Date()

# SET GCP DIRECTORY ####
# get directories from NCEI GCP data viewer: 
#https://console.cloud.google.com/storage/browser/noaa-passive-bioacoustic?inv=1&invt=Ab0oyg

projectN = "NRS"
typ = "audio"
gcpDir  = paste0( "gs://noaa-passive-bioacoustic/", tolower(projectN), "/audio" )

outDir =   "F:\\CODE\\GitHub\\SoundscapesWebsite\\"
outDirR =  paste0(outDir, "content\\resources\\") #save graphics
outDirP =  paste0(outDir, "products\\",tolower(projectN), "\\")     #products

# CONTEXT ####
inFile = paste0(outDirR, "//ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( openxlsx :: read.xlsx(inFile) )
colnames(lookup) <- lookup[1, ]  # Set first row as column names
lookup <- as.data.frame( lookup[-1, ] ) # Remove the first row
colnames(lookup)[5] = "NCEI"
inFile = paste0(outDirP, "//early_nrs_datasets.xlsx")
NRSold = as.data.frame ( openxlsx :: read.xlsx(inFile) )
NRSold$Start_Date = as.Date(NRSold$Start_Date, origin = "1899-12-30")
NRSold$End_Date = as.Date(NRSold$End_Date, origin = "1899-12-30")

# LIST SUB DIRECTORIES ####
#these should be the "monitoring sites" you want to gather information about
command = "gsutil"
args = c("ls", gcpDir)
subdirsALL = system2(command, args, stdout = TRUE, stderr = TRUE) 
dirNames  = sapply(strsplit(basename( subdirsALL ), "/"), `[`, 1)
cat("Processing... ", projectN, length(dirNames), "directories" )

# GET INFORMATION FROM METADATA FILES ####
output = NULL
for (s in 1:length(subdirsALL) ) { # s=1
  
  ## read in files
  args = c("ls", "-r", subdirsALL[s])
  sFiles = ( system2(command, args, stdout = TRUE, stderr = TRUE)  )
  json_files = grep("\\.json$", sFiles, value = TRUE) #metadata files
  cat("Processing... ", dirNames[s], "[", s, " of ", length(dirNames),"]", length(sFiles), "\n" )
  
  for (jf in 1:length( json_files) ) { # jf = 1
    
    #path for this deployment metadata 
    path_parts <- unlist(strsplit(json_files[jf], "/"))
    deploymentPath <- paste(path_parts[1:(length(path_parts) - 2)], collapse = "/")
    
    url = paste0("https://storage.googleapis.com/", 
                 gsub ("gs://", '', paste(json_files[jf], collapse = "") ) )
    h = curl(url, "r")
    json_content = readLines(url)
    close(h)
    tmp = fromJSON(paste(url, collapse = ""))
    
    ## check which version
    if ( is.null( tmp$deployment$audio_start) ) { #not small caps
      
      name  = tmp$SITE 
      deploy = tmp$DEPLOYMENT_NAME
      instr = tmp$INSTRUMENT_TYPE
      start = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_START), format = "%Y-%m-%d")
      end   = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_END), format = "%Y-%m-%d")
      lat   = tmp$DEPLOYMENT$DEPLOY_LAT
      lon   = tmp$DEPLOYMENT$DEPLOY_LON
      
      #save to output data - each deployment
      output = rbind(output, c(deploymentPath, jf, name, deploy, instr, 
                               as.character(start), as.character(end), 
                               lat, lon) )
      
      
    } else if ( !is.null( tmp$deployment$audio_start) ) { #small caps
      
      name  = tmp$site 
      names(tmp)[names(tmp) == "INSTRUMENT_TYPE"] =  "instrument_type"
      instr = tmp$instrument_type
      deploy =  tmp$deployment_id
      start = as.Date( gsub("T"," ", tmp$deployment$audio_start), format = "%Y-%m-%d")
      end   = as.Date( gsub("T"," ", tmp$recover$audio_end), format = "%Y-%m-%d")
      lat   = tmp$deployment$lat
      lon   = tmp$deployment$lon
       
      #save to output data - each deployment
      output = rbind(output, c(deploymentPath, jf, name, deploy, instr, 
                               as.character(start), as.character(end), 
                               lat, lon) )
    } else {
      cat(deploymentPath, "- Not correct format", "\n" )
      
      output = rbind(output, c(deploymentPath, jf, NA, NA, 
                               NA, NA, 
                               NA, NA) )
      
    } 
  }
}

# FORMAT OUTPUT ####
ouputTest = output # output = ouputTest
output = as.data.frame(output)
colnames(output) = c("Path", "FileCount", "SiteName", "DeploymentName", "Instrument", "Start_Date", "End_Date","Lat","Lon")
output$Site = paste0(basename((output$Path)))
output$Start_Date = as.Date(output$Start_Date, format = "%Y-%m-%d")
output$End_Date = as.Date(output$End_Date, format = "%Y-%m-%d")
output$Days = difftime( output$End_Date, output$Start_Date,"days")
output$Instrument = "AUH"

# ADD OLD DEPLOYMENTS
if (projectN == "NRS"){ output2 = rbind(output, NRSold )}
output2$Site = paste0( tolower( projectN ), "_", output2$SiteName ) 
output = output2
output$Site = gsub("_", "", output$Site)
## ADD INFO from context file ####
lookup_selected = lookup %>% select(NCEI, Region,`Common Name/Identifiers`,`Site Description/Driver for Monitoring Location Choice`)
output = left_join(output, lookup_selected, by = c("Site" = "NCEI"), relationship = "many-to-many")
output$Duration = difftime( output$End_Date, output$Start_Date,"days")
output$Project [is.na(output$Region)]  = "NRS" 
output$Project [!is.na(output$Region)] = "ONMS-sound" 

## SAVE only long-term monitoring sites ####
save(output, file = paste0(outDirP, "\\data_gantt_NRS_gantt_ALL", DC, ".Rda") ) #all data
cat("Not long-term sites...", setdiff(unique(output$Site), unique(lookup$NCEI)) ) #only long-term monitoring sites
outputT = output[!is.na(output$Region),] 
save(outputT, file = paste0(outDirP, "\\data_gantt_NRS_gantt_", DC, ".Rda") )
write.csv(outputT, file = paste0(outDirP, "\\data_gantt_NRS_gantt_", DC, ".csv") )

# GANTT CHART  ####
# load(file = paste0(outDirP, "\\data_gantt_NRS_gantt_2025-04-28.Rda") )
## COLOR ####
project_colors <- c(
  "NRS" = "#53B0D7",  
  "ONMS-sound"= "#004295") 


## geom_tile option ####
colnames(outputT)
outputT <- outputT %>%
  mutate(Site = fct_reorder2(Site, Region, Start_Date))  # Reorders Site within Region
output$sanctuaryName = substr(output$Site, start = 1, stop =2)

pT = ggplot(output, aes(y = Site, x = Start_Date, xend = End_Date, fill = Project ) ) +
  geom_tile(aes(x = Start_Date, width = as.numeric(End_Date - Start_Date) ) , 
            color = "gray", height = 0.6) +     # Fill color by Instrument and outline in black
  scale_fill_manual(values = project_colors) +  # Use specific colors for instruments
  labs(x = "", y = "", title = "",
       caption = paste0("Data available as of ", format(Sys.Date(), "%B %d, %Y"))) +
  #facet_wrap(~Region, scales = "free_y") +
  theme_minimal(base_size = 16) +
  theme( legend.position = "right", legend.justification = "left",
         axis.text.x = element_text(angle = 0, hjust = 1, size = 12),
         axis.text.y = element_text(angle = 0, size = 12))
pT
ggsave(filename = paste0(outDirR, "\\gantt_NRS.jpg"), plot = pT, width = 10, height = 6, dpi = 300)












