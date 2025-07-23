# COMPILE SOUNDSCAPE METRICS

# assumes data are already downloaded from cloud, stored locally
# runs one site at a time 
# checks for files already processed
# adds wind estimate from PAMscapes for any new data (takes time!)

# outputs:  hourly TOLs values with wind speed and list of files processed

#run this to make sure latest updates for PAMscapes
# devtools::install_github('TaikiSan21/PAMscapes')

library(PAMscapes)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)
library(xlsx)
library(openxlsx)

# SET UP PARAMS ####
rm(list=ls()) 
DC = Sys.Date()
site  = "sb01" 
site = tolower(site) 

# LOCAL DATA DIRECTORIES ####
dirGCP = paste0( "F:/ONMS/", site,"/") # NCEI GCP min HMD netCDFs

# LOCAL CODE REPO DIRECTORIES ####
outDir =  "F:\\CODE\\GitHub\\SoundscapesWebsite\\" 
outDirC = paste0( outDir,"content\\resources\\") #context
outDirP = paste0( outDir,"products\\", substr(tolower(site),start = 1, stop =2),"\\" )#products
outDirG = paste0( outDir,"report\\" ) #graphics

# ONMS Metadata ####
metaFile = paste0(outDirC,"ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( openxlsx :: read.xlsx(metaFile, sheet  = "Summary") ) #xlsx::read.xlsx(metaFile, sheetName = "Summary")
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
siteInfo = lookup[lookup$`NCEI ID` == site,]
siteInfo = siteInfo[!is.na(siteInfo$`NCEI ID`), ]

cat("CHECK: Read in data for: ", siteInfo$`NCEI ID
facilitates matching with metadata on gcp 
check gcp to see verify`)

# GET list of files to process ####
## PyPAM soundscape FILES- NEFSC-GCP ####
# e.g. NEFSC_SBNMS_201811_SB03_20181112.nc
inFilesPY = list.files(dirGCP, pattern = "_[0-9]{8}\\.nc$", recursive = T, full.names = T)
tmp = sapply( strsplit(basename(inFilesPY), "[.]"), "[[", 1)
dysPy = as.Date(sapply( strsplit(tmp, "_"), "[", 5),format = "%Y%m%d")
cat("Found ", length(inFilesPY), "PyPAM files for ", site, "(", as.character( min(dysPy , na.rm = T) ), " to ", as.character(max(dysPy , na.rm = T)),
    "with", sum( duplicated(dysPy)), "duplicated days\n")

## ONMS Sound FILES- NCEI-GCP ####
# e.g. ONMS_HI01_20231201_8021.1.48000_20231201_DAILY_MILLIDEC_MinRes.nc
inFilesON = list.files(dirGCP, pattern = "MinRes.nc", recursive = T, full.names = T)
dysON = as.Date(sapply( strsplit(basename(inFilesON), "_"), "[[", 5), format = "%Y%m%d")
cat("Found ", length(inFilesON), "NCEI files for ", site, "(", as.character(min( dysON , na.rm = T)), " to ", as.character(max( dysON , na.rm = T)),"with",
    sum( duplicated(dysON)), "duplicated days\n")

## COMBINE FILE LISTS ####
#check for duplicate days, remove MANTA
ixdR = which(dysON %in% dysPy)
inFiles = c( inFilesPY, inFilesON[-ixdR] )
ckFiles = as.data.frame(inFiles)
dys = c(dysPy, dysON[-ixdR])
cat("Found ", length(inFiles), " files for ", site, "with", sum( duplicated(dys)), "duplicated days\n")

## CHECK FOR PROCESSED FILES #### 
#updates list of files to process
pFile = list.files(path = (outDirP), pattern = paste0("filesProcesed_", site), full.names = T, recursive = T)
if ( length(pFile) > 0 ) {
  load(pFile)
  
  # are there any new files to process?
  inFilesN = inFiles[!basename(inFiles) %in% processedFiles]
  
  if ( length(inFilesN ) > 0 ) {
    
    # read in processed data to append results
    inFileP = list.files((outDir), 
                         pattern = paste0("data_", site, "_HourlySPL-gfs_\\d{4}-\\d{2}-\\d{2}\\.Rda$"), 
                         full.names = T, recursive = T)
    file_info = file.info(inFileP)
    load( inFileP[which.max(file_info$ctime)] )
    if( exists("outData") ) {
      processedData = outData
      rm(outData)
    }
    
    cat( "Processed data for ", site, ": ", 
         as.character( as.Date( min( processedData$UTC))) , " to ", 
         as.character( as.Date( max( processedData$UTC)) ), 
         " Found ", length(inFilesN), "new files to process\n")
    
    # these are the files that will be processed!
    inFiles = inFilesN 
    
  } else {
    stop("No new files to process... come back when you have more data")
  }
  
} else {
  cat("No processed files for ", site, " processing all new files")
  processedData = NULL
}

# PROCESS ONMS Sound FILES ####
cData = NULL  
cDatah = NULL

if (length(inFiles) > 0) { 
  for (f in 1:length(inFiles) ){ # 1245:1246 
   
     cat("Processing", f, "of", length(inFiles),basename(inFiles[f]), "\n")
    
    ncFile = inFiles[f]
    hmdData = loadSoundscapeData(ncFile) #only keeps quality 1 as default
    tolData = createOctaveLevel(hmdData, type='tol')
    
    # add software column
    if ( grepl("MinRes.nc", basename(inFiles[f]) ) ) {
      tolData$software = "manta"
    } else {  
      tolData$software = "pypam" }
    
    #combine data- but remove columns first
    # assumes they are in the same order and this will break if they are not (future fix recommended!)
    tolData = tolData[, setdiff(names(tolData), "platform"), drop = FALSE]
    cData = rbind(cData, tolData)
    
  } 
  
  #bin to hourly median values
  cDatah = binSoundscapeData(cData, bin = "1hour", method = c("median") )
}
# the time binning seems to remove any "extra columns" so just the UTC and TOL bands for the output
#names(cDatah)
#ADD a few basicc columns about the data
cDatah$yr  = year(cDatah$UTC)
cDatah$mth = month(cDatah$UTC)
cDatah$site = site

# GET WIND ####
# ## (ALT GET WIND) 
# ##append wind data - only if already ran previously but the SPL data were inaccurate!
# inWind = list.files("F:\\ONMS\\SS_Manta\\", pattern = paste0("data_", tolower(site), "_HourlySPL-gfs_.*\\.Rda$"), full.names = T)
# load( inWind[1] )
# ##names(gps)
# cols_to_keep = c("UTC",  "Latitude", "Longitude", "windU", "windV",
#                   "precRate", "matchLong", "matchLat",
#                   "matchTime", "windMag")
# gps_subset = gps[, intersect(cols_to_keep, names(gps))]
# ##names(gps_subset)
# rm(gps)
# merged_data = merge(cDatah, gps_subset, by = "UTC", all.x = TRUE )
# gps = merged_data
# ##names(gps)


if ( length(cDatah) > 0 ) {

  cat("ONMS data only ...") 
  cat("This takes a bit ... maybe grab a coffee or go for walk") 
  gps = matchGFS(cDatah)

}

# APPEND & SAVE NEW DATA FILES ####
if ( length(pFile) > 0 ){   #append old (processedData) and save out all processed data
  
  #remove any no matching headings
  data_mismatched = setdiff(colnames(gps), colnames(processedData))
  gps_clean = gps[, !colnames(gps) %in% data_mismatched] #new data with matching headings

  #re-order columns
  # setdiff(colnames(gps_clean), colnames(processedData))
  print(names(processedData))
  col_order = colnames(processedData)
  gps_clean1 = gps_clean[, col_order]
  names( processedData)
  names( gps_clean)
  
  # combine data
  outData = rbind(processedData, gps_clean)
  
  #track days added
  dys = length( unique( as.Date( outData$UTC) ) )
  dysA = dys - length( unique( as.Date( gps_clean$UTC) ) )
  
  # summary of data processed
  cat( "Output data for ", site, " has ", dys, "unique days: ", 
       as.character( as.Date( min( outData$UTC))) , " to ", 
       as.character( as.Date( max( outData$UTC)) ), "with ", dysA, " new days added")
 
   # writes new file with appended data
  save(outData, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL-gfs_", DC, ".Rda") )
  
  # write out processed files
  processedFiles = c(processedFiles, basename(inFiles) )
  # writes over previous file
  save(processedFiles, file = paste0(outDirP, "filesProcesed_", tolower(site), "_HourlySPL.Rda") )
  
} else {  # save out all the newly processed data
  # summary of data processed
  dysA = length( unique( as.Date( gps$UTC) ) )
  cat( "Output data for ", site, " has ", dysA, "unique days: ", 
       as.character( as.Date( min( gps$UTC))) , " to ", 
       as.character( as.Date( max( gps$UTC)) ))
  # writes new file with data
  outData = gps
  save(outData, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL-gfs_", DC, ".Rda") )
  # write out processed files
  processedFiles  = basename(inFiles)
  save(processedFiles, file = paste0(outDirP, "filesProcesed_", tolower(site), "_HourlySPL.Rda") )
}

