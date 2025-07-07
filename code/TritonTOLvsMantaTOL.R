#compare Manta output with Triton-remora output
# use PAMscapes to summarise
rm(list=ls()) 
library(ncdf4)

deploy = c("hi01", "HI01_05", "onms_hi01_20221201-20230510_hmd") 
deploy = c("hi03", "HI03_05", "onms_hi03_20221201-20230502_hmd")
run = 2

# LOCAL DATA DIRECTORIES ####
outDir =   "F:\\CODE\\GitHub\\SoundscapesWebsite\\"
outDirC =  paste0(outDir,"context\\") #where to get context
## TOL CONVERSION ####
TOL_convert = read.csv(paste0(outDirC,"TOLconvert.csv"))
TOL_convert$Nominal = paste0("TOL_",TOL_convert$Center)

# Triton-remora data
dirSS  = paste0( "F:/ONMS/compare" ) # SANCTSOUND hourly TOLs csvs
tmpFile = paste0(dirSS, "//SanctSound_", deploy[2], "_TOL_1h.csv")
typ = sapply( strsplit(basename(tmpFile), "[.]"), "[[",2)
SSdata = loadSoundscapeData( tmpFile, extension = typ)
SSdata$Code = "Triton"

# MANTA DATA ####
dirGCP = paste0( "F:/ONMS/", deploy[1],"//", deploy[3]) 
inFiles = list.files(dirGCP, pattern = "MinRes", recursive = T, full.names = T)
inFiles = inFiles[!grepl(".png",inFiles) ]
inFiles = inFiles[!grepl(".csv",inFiles) ]
inFilesON = inFiles[!grepl("_netCDF",inFiles) ]
cat("Found ", length(inFilesON), "ONMS files")
ncFile = nc_open( inFilesON[1] )
ncFile$var$psd$longname

hmdData = loadSoundscapeData(inFilesON[2]) 
tolData = createOctaveLevel(hmdData, type='ol') #,normalized = TRUE) 


# ONLY RUN FIRST TIME ####
if (run == 1) {
  cData = NULL
  for (f in 1:length(inFilesON) ){
    ncFile = inFilesON[f]
    hmdData = loadSoundscapeData(ncFile) #only keeps quality 1 as default
    tolData = createOctaveLevel(hmdData, type='tol',normalized = TRUE) 
    # assumes HMD data are per/Hz, so for >455 Hz, 
    # adds bandwidth back to the levels, before summing the bands in the TOL then outputs TOL levels
    tolData$site = deploy[1]
    cData = rbind( cData, tolData )
  }
  cData$Code = "Manta"
  save(cData, file = paste0(dirSS, "//", deploy[3], "_",deploy[2], ".Rda") )
}else {
  file = paste0(dirSS, "//", deploy[3], "_",deploy[2], ".Rda")
  load(file)
  cData$Code = "Manta"
}

# Get HOURLY values ####
cDatah = binSoundscapeData(cData, bin = "1hour", method = c("median") )

# look at one frequency bin and plot
fqIN = "TOL_125" # "TOL_2000"
cols_to_select = c("UTC", fqIN)
dManta  = cDatah %>% select(all_of(cols_to_select))
colnames( dManta) = c("UTC", "manta")

dTriton = SSdata %>% select(all_of(cols_to_select))
colnames( dTriton) = c("UTC", "triton")

dcombine <- dManta %>%
  full_join(dTriton, by = "UTC")
rm(dcombine)
dcombine = full_join(dManta, dTriton, by = "UTC")

idx = which( is.na(dcombine$triton) ) #triton is missing a few
dcombine$UTC[idx]
dcombine$diff = dcombine$manta - dcombine$triton
#hist(dcombine$diff)
ggplot(dcombine, aes(x = UTC, y = diff)) +
  geom_line()+
  ggtitle (paste0( "Difference in ", fqIN, "TOL (Manta - Triton)- median"))+
  labs(subtitle = paste0( deploy[3], " to ",deploy[2] ) )
mean(dcombine$diff, na.rm = T )

# NOT NEEDED #####
cDatahMean = binSoundscapeData(cData, bin = "1hour", method = c("mean") )
# look at one frequency bin and plot
cols_to_select = c("UTC", "TOL_500")
dMantaM  = cDatahMean %>% select(all_of(cols_to_select))
colnames( dMantaM) = c("UTC", "manta")

dTriton = SSdata %>% select(all_of(cols_to_select))
colnames( dTriton) = c("UTC", "triton")

dcombineM = dMantaM %>%
  left_join(dTriton, by = "UTC")
#names(dcombine)

dcombineM$diff = dcombineM$manta - dcombineM$triton
#hist(dcombine$diff)

ggplot(dcombineM, aes(x = UTC, y = diff)) +
  geom_line()+
  ggtitle (" Difference in 500 Hz TOL (Manta - Triton)- mean")



# Since Triton is consistantly lower- maybe it is actually per Hz, 
# what happens if I divide manta output by the 500 Hz TOL bandwidth
bw = 115
dcombine$mantaN = 10*log10 ( (10^(dcombine$manta/10))/ bw  )  

dcombine$diff2 = dcombine$mantaN - dcombine$triton
dcombine$mantaN - dcombine$manta
ggplot(dcombine, aes(x = UTC, y = diff2)) +
  geom_line()+
  ggtitle (" Difference in 500 Hz TOL (Manta/bw - Triton)")


### NORMALIZE: calculate spectrum band levels in the TOLs ####
tol_columns = grep("TOL", colnames(cData))
cDataTOL = cData
for( cc in 1:length(tol_columns)) {
  toltmp = colnames(cData)[tol_columns[cc]]
  #find the bandwidth edges
  bw   = TOL_convert$Bandwidth[which(TOL_convert$Nominal == toltmp) ] 
  cat("bandwidth for", toltmp, "is", bw, "\n" ) 
  dtmp = cData[,tol_columns[cc] ] 
  if ( length(bw) > 0 ){
    cData[,tol_columns[cc] ] = 10*log10 ( (10^(dtmp/10))/ bw )  
  } else {
    cData[,tol_columns[cc] ] = NA
  }
  
}

# are the values the same
cols_to_select = c("UTC", "TOL_500")
dManta  = cDatah %>% select(all_of(cols_to_select))
colnames( dManta) = c("UTC", "TOL_500-manta")
dMantaN  = cDatahN %>% select(all_of(cols_to_select))
colnames( dMantaN) = c("UTC", "TOL_500-mantaN")

dTriton = SSdata %>% select(all_of(cols_to_select))
colnames( dTriton) = c("UTC", "TOL_500-remora")

combine = dManta %>%
  left_join(dTriton, by = "UTC")
combine$diff = combine$`TOL_500-manta` - combine$`TOL_500-remora`
hist(combine$diff)

combine = combine %>%
  left_join(dMantaN, by = "UTC")


  
