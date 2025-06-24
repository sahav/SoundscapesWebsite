# COMPILE SOUNDSCAPE METRICS

# assumes data are already downloaded from cloud, stored locally
# runs for SanctSound and SoundCoop data files (combines)
# runs one site at a time
# checks for files already processed
# adds wind estimate from PAMscapes for any new data

# outputs hourly TOLs values with wind speed and list of files processed

devtools::install_github('TaikiSan21/PAMscapes')

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
site  = "SB01" 
site = tolower(site) 

# LOCAL DATA DIRECTORIES ####
dirSS  = paste0( "F:/ONMS/", site,"/SanctSound" ) # SANCTSOUND hourly TOLs csvs
dirGCP = paste0( "F:/ONMS/", site,"/") # NCEI GCP min HMD netCDFs

# LOCAL CODE REPO DIRECTORIES ####
outDir =  "F:\\CODE\\GitHub\\SoundscapesWebsite\\" 
outDirC = paste0( outDir,"content\\resources\\") #context
outDirP = paste0( outDir,"products\\", substr(tolower(site),start = 1, stop =2),"\\" )#products
outDirG = paste0( outDir,"report\\" ) #graphics

# LOAD ONMS Metadata ####
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

## SanctSound FILES ####
# NOTE: might need to change these in some of the files 31_5 to 31.5 and UTC with : not _
inFiles = list.files(path = dirSS, pattern = toupper(site), full.names = T, recursive = T)
filesTOL = inFiles[grepl("TOL_1h", inFiles)] 
inFiles = filesTOL[!grepl("/analysis/", filesTOL)] 
inFiles = inFiles[!grepl("1h.nc", inFiles)] 
inFiles = inFiles[!grepl("xml", inFiles)] 
inFiles = inFiles[!grepl("json", inFiles)] 
inFilesSS = inFiles[!grepl("1h.nc", inFiles)] 
cat("Found ", length(inFilesSS), "SanctSound files")

## Manta Files- NCEI ####
inFiles = list.files(dirGCP, pattern = "MinRes", recursive = T, full.names = T)
inFiles = inFiles[!grepl(".png",inFiles) ]
inFiles = inFiles[!grepl(".csv",inFiles) ]
inFilesON = inFiles[!grepl("_netCDF",inFiles) ]
cat("Found ", length(inFilesON), "ONMS files")

# CHECK FOR PROCESSED FILES #### 
pFile = list.files(path = (outDirP), pattern = paste0("filesProcesed_",site), full.names = T, recursive = T)
if ( length(pFile) > 0 ) {
  load(pFile)
  cat("Already processed ", length(processedFiles), " files for ", site, "\n")
  
  # are there any new files to process?
  inFilesSS = inFilesSS[!basename(inFilesSS) %in% processedFiles]
  inFilesON = inFilesON[!basename(inFilesON) %in% processedFiles]
  inFilesN  = c(inFilesSS,inFilesON)
  
  if ( length(inFilesN ) > 0 ) {
    cat("Found ", length(inFilesN), "new files to process")
    # read in processed data to append results
    inFileP = list.files((outDir), 
                         pattern = paste0("data_", site, "_HourlySPL-gfs_\\d{4}-\\d{2}-\\d{2}\\.Rda$"), 
                         full.names = T, recursive = T)
    file_info = file.info(inFileP)
    load( inFileP[which.max(file_info$ctime)] )
    processedData = gps
    rm(gps)
    
  } else {
    stop("No new files to process... come back when you have more data")
  }
  
} else {
  cat("No processed files for ", site, " processing all new files")
  processedData = NULL
  aData = NULL # this is the already processed data that needs to be binded to new data
}

# PROCESS SANCTSOUND FILES ####
sData = NULL
if ( length(inFilesSS) > 0 ) {
 
  if (length(inFilesSS) > 0 ) {
    for (ii in 1:length(inFilesSS)) { # ii = 3
      
      tmpFile = inFilesSS[ii]
      typ = sapply( strsplit(basename(tmpFile), "[.]"), "[[",2)
      tmp = loadSoundscapeData( inFilesSS[ii], extension = typ)
      cat( inFilesSS[ii], "Start = ", as.character( as.Date( min(tmp$UTC) ) ),"\n")
      sData = rbind(sData, tmp)
    }
    sData$site = tolower(site)
    sData$yr   = year(sData$UTC)
    sData$mth  = month(sData$UTC)
    inFilesS = inFilesSS
  }
  
}

# PROCESS ONMS FILES ####
cData = NULL  
cDatah = NULL
if ( length(inFilesON) > 0 ) {
  if (length(inFilesON) > 0 ) {
    for (f in 1:length(inFilesON) ){
      ncFile = inFilesON[f]
      hmdData = loadSoundscapeData(ncFile)
      tolData = createOctaveLevel(hmdData, type='tol')
      tolData$site = site
      cData = rbind( cData, tolData )
    }
    cData$yr  = year(cData$UTC)
    cData$mth = month(cData$UTC)
    cDatah = binSoundscapeData(cData, bin = "1hour", method = c("median") )
  }
  cDatah$yr  = year(cDatah$UTC)
  cDatah$mth = month(cDatah$UTC)
  cDatah$site = site
}
rm(cData,hmdData, tmp, tolData)

## COMBINE DATA ####
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
if ( length(pFile) > 0 ){
  #append old and save out all processed data
  data_mismatched = setdiff(colnames(gps), colnames(processedData))
  gps_clean = gps[, !colnames(gps) %in% data_mismatched]
  
  #data
  outData = rbind(processedData,gps_clean)
  dys = length( unique( as.Date( outData$UTC) ) )
  cat( "Output data for ", site, " has ", dys, "unique days: ", 
       as.character( as.Date( min(outData$UTC))) , " to ", 
       as.character( as.Date( max( outData$UTC)) ))
  save(outData, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL-gfs_", DC, ".Rda") )
  
  #processed files
  processedFiles = c(processedFiles, basename(inFilesSS), basename(inFilesON) )
  length(processedFiles) - length(unique(processedFiles))
  save(processedFiles, file = paste0(outDirP, "filesProcesed_", tolower(site), "_HourlySPL.Rda") )
  
} else {
  # save out all the newly processed data
  #data
  save(gps, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL-gfs_", DC, ".Rda") )
  
  #processed files
  processedFiles  = c(basename(inFilesSS), basename(inFilesON))
  save(processedFiles, file = paste0(outDirP, "filesProcesed_", tolower(site), "_HourlySPL.Rda") )
}

