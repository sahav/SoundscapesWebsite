# REMOVE SANCTSOUND DATA FROM OUTPUT
rm(list=ls()) 
library(PAMscapes)

ONMSsites = c("sb01", "sb03", "hi01", "hi03", "hi04", "hi08", "pm01", "as01", "mb01", "mb02", "oc02", "cb11" )

dirSS  = paste0( "F:/ONMS/" ) # SANCTSOUND hourly TOLs csvs
inFiles = list.files(path = dirSS, pattern = "TOL_1h", full.names = T, recursive = T)
inFiles = inFiles[!grepl("/analysis/", inFiles)] 
inFiles = inFiles[!grepl("1h.nc", inFiles)] 
inFiles = inFiles[!grepl("xml", inFiles)] 
inFiles = inFiles[!grepl("json", inFiles)] 
inFiles = inFiles[!grepl("compare", inFiles)] 
inFilesSS = inFiles[!grepl("1h.nc", inFiles)] 
cat("Found ", length(inFilesSS), "SanctSound files")

sites = unique( sapply(strsplit(inFilesSS, "/"), `[`, 3) )
sites = sites[sites %in% ONMSsites]

outSS = NULL
for (ii in 1:length(sites)) {
  dirTmp  = paste0( "F:/ONMS/", sites[ii] )
  
  inTmp = inFiles[grepl(sites[ii], inFiles)] 
  
  #idx =  which.max( as.numeric( substr(sapply(strsplit(inTmp, "/"), `[`, 5), start = 6, stop = 8) ) )
  
  tmpFile = inTmp[length(inTmp)]
  
  typ = sapply( strsplit(basename(tmpFile), "[.]"), "[[",2)
  tmp = loadSoundscapeData( tmpFile, extension = typ)
  outSS = rbind(outSS, c(sites[ii] , as.character( as.Date( max( tmp$UTC ) ) ) ) )
  
  cat("processing... ", ii, ":", tmpFile, ":", as.character(max( tmp$UTC ) ), "\n")
  
}
outSS = as.data.frame(outSS)


outDir =  "F:\\CODE\\GitHub\\SoundscapesWebsite\\" 
inFileD = list.files(outDir, pattern = "_HourlySPL-gfs_.*\\.Rda$", recursive = T, full.names = T)
siteP = sapply( strsplit(basename(inFileD), "_"), "[[",2)
siteP = siteP[siteP %in% sites]

for (ss in 1:length(siteP) ){
  inFile = list.files(outDir, pattern = paste0(siteP[ss], "_HourlySPL-gfs_.*\\.Rda$"),  recursive = T, full.names = T)
  file_info = file.info(inFile) 
  load( inFile[which.max(file_info$ctime)] ) #only load the most recent!
  #get cut off date
  cDate = outSS[ outSS$V1 == siteP[ss],2]
  gps2 = gps[gps$UTC > cDate,]
  
  sremove = nrow(gps)- nrow(gps2)
  cat(siteP[ss] , ": removed ", sremove, "hours, starting before", as.character( min(gps2$UTC) ), "\n")
  gps = gps2
  outDirP = paste0( outDir,"products\\", substr(tolower(siteP[ss]),start = 1, stop =2),"\\" )#products
  save(gps, file = paste0(outDirP, "data_", tolower(siteP[ss]), "_HourlySPL-gfs_removeSS.Rda") )
}


