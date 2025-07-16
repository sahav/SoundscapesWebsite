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
site  = "sb03" 
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
dys = c(dysPy, dysON[-ixdR])
cat("Found ", length(inFiles), " files for ", site, "with", sum( duplicated(dys)), "duplicated days\n")

## CHECK FOR PROCESSED FILES #### 
# NEED TO UPDATE WHEN I ADD NEW FILES
#updates list of files to process
pFile = list.files(path = (outDirP), pattern = paste0("filesProcesed_", site), full.names = T, recursive = T)
if ( length(pFile) > 0 ) {
  load(pFile)
  cat("Already processed ", length(processedFiles), " files for ", site, "\n")
  
  # are there any new files to process?
  inFilesPY = inFilesPY[!basename(inFilesPY) %in% processedFiles]
  inFilesON = inFilesON[!basename(inFilesON) %in% processedFiles]
  inFilesN  = c(inFilesPY,inFilesON)

  
  if ( length(inFilesN ) > 0 ) {
    cat("Found ", length(inFilesN), "new files to process")
    # read in processed data to append results
    inFileP = list.files((outDir), 
                         pattern = paste0("data_", site, "_HourlySPL-gfs_\\d{4}-\\d{2}-\\d{2}\\.Rda$"), 
                         full.names = T, recursive = T)
    file_info = file.info(inFileP)
    load( inFileP[which.max(file_info$ctime)] )
    if( exists("outData") ) {
      gps = outData
      rm(outData)
    }
    processedData = gps
    cat( "Processed data for ", site, ": ", 
         as.character( as.Date( min( processedData$UTC))) , " to ", 
         as.character( as.Date( max( processedData$UTC)) ))
    
    rm(gps)
    
  } else {
    stop("No new files to process... come back when you have more data")
  }
  
} else {
  cat("No processed files for ", site, " processing all new files")
  processedData = NULL
  aData = NULL # this is the already processed data that needs to be binded to new data
}

# PROCESS ONMS Sound FILES ####
cData_list = vector("list", length(inFiles))
if (length(inFiles) > 0) {
  for (f in seq_along(inFiles)) {
    cat("Processing", f, "of", length(inFiles), "\n")
    ncFile = inFiles[f]
    hmdData = loadSoundscapeData(ncFile)  # only keeps quality 1 as default
    tolData = createOctaveLevel(hmdData, type = 'tol')
    tolData$site = site
    if ( grepl("MinRes.nc", basename(inFiles[f]) ) ) {
 
      tolData$software = "manta"
    } else {  tolData$software = "pypam"}
    

    cData_list[[f]] = tolData
  }
  
  cData  = bind_rows(cData_list)
  cDatah = binSoundscapeData(cData, bin = "1hour", method = c("mean"))
}
cDatah$yr  = year(cDatah$UTC)
cDatah$mth = month(cDatah$UTC)
cDatah$site = site

# append wind data (only because I ran this already!!!!)
inFile = list.files(outDirP, pattern = paste0("data_", tolower(site1), "_HourlySPL-gfs_.*\\.Rda$"), full.names = T)

# COMBINE DATA & GET WIND ####
if ( length(sData) > 0 & length(cDatah) > 0 ) {
  
  cat("SancSound + ONMS data ....")
  
  aData = NULL  
  sData$Latitude   = cDatah$Latitude[1]
  sData$Longitude  = cDatah$Longitude[1]
  cData_mismatched = setdiff(colnames(cDatah), colnames(sData))
  cData_cleaned    = cDatah[, !colnames(cDatah) %in% cData_mismatched]
  
  cData_cleaned = cData_cleaned[, colnames(sData)]
  aData = rbind(cData_cleaned, sData)
  aData$Latitude  = as.numeric( as.character( siteInfo$Latitude ))
  aData$Longitude = as.numeric( as.character( siteInfo$Longitude ))
  
  #GET WIND/WEATHER DATA
  cat("This takes a bit ... maybe grab a coffee or go for walk") 
  gps = matchGFS(aData)
  gps$yr  = year(gps$UTC)
  gps$mth = month(gps$UTC)
  gps$site = site
  
  
} else if (length(sData) == 0 & length(cDatah) > 0 ) {

  cat("ONMS data only ...") 
  cat("This takes a bit ... maybe grab a coffee or go for walk") 
  gps = matchGFS(cDatah)

}

# APPEND & SAVE NEW DATA FILES ####
if ( length(pFile) > 0 ){   #append old and save out all processed data
  data_mismatched = setdiff(colnames(gps), colnames(processedData))
  gps_clean = gps[, !colnames(gps) %in% data_mismatched]
  
  #data
  outData = rbind(processedData,gps_clean)
  dys = length( unique( as.Date( outData$UTC) ) )
  cat( "Output data for ", site, " has ", dys, "unique days: ", 
       as.character( as.Date( min( outData$UTC))) , " to ", 
       as.character( as.Date( max( outData$UTC)) ))
  save(outData, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL-gfs_", DC, ".Rda") )
  
  #processed files
  # writes new file with date
  processedFiles = c(processedFiles, basename(inFilesSS), basename(inFilesON) )
  length(processedFiles) - length(unique(processedFiles))
  # writes over previous file
  save(processedFiles, file = paste0(outDirP, "filesProcesed_", tolower(site), "_HourlySPL.Rda") )
  
} else {  # save out all the newly processed data
  #data
  # writes new file with date
  dys = length( unique( as.Date( gps$UTC) ) )
  cat( "Output data for ", site, " has ", dys, "unique days: ", 
       as.character( as.Date( min( gps$UTC))) , " to ", 
       as.character( as.Date( max( gps$UTC)) ))
  outData = gps
  save(gps, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL-gfs_", DC, ".Rda") )
  
  #processed files
  processedFiles  = c(basename(inFilesSS), basename(inFilesON))
  save(processedFiles, file = paste0(outDirP, "filesProcesed_", tolower(site), "_HourlySPL.Rda") )
}

