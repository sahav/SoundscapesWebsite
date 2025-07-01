#compare Manta output with Triton-remora output
# use PAMscapes to summarise
rm(list=ls()) 

site  = "HI03" 
site = tolower(site) 

# LOCAL DATA DIRECTORIES ####
dirSS  = paste0( "F:/ONMS/", site,"/SanctSound" ) # SANCTSOUND hourly TOLs csvs
dirGCP = paste0( "F:/ONMS/", site,"//onms_hi03_20221201-20230502_hmd") # NCEI GCP min HMD netCDFs
outDir =   "F:\\CODE\\GitHub\\SoundscapesWebsite\\"
outDirG =  paste0(outDir, "content\\resources") #where save graphics
outDirGe =  paste0(outDir, "content\\resources\\extra") #where extra save graphics
outDirC =  paste0(outDir,"context\\") #where to get context

## TOL CONVERSION ####
TOL_convert = read.csv(paste0(outDirC,"TOLconvert.csv"))
TOL_convert$Nominal = paste0("TOL_",TOL_convert$Center)

# SANCTSOUND DATA ####
# assumnes data are median hourly values in TOL bands as PSD (divided by bandwidth)
tmpFile = paste0(dirSS, "//SanctSound_HI03_05_TOL_1h.csv")
typ = sapply( strsplit(basename(tmpFile), "[.]"), "[[",2)
SSdata = loadSoundscapeData( tmpFile, extension = typ)
# data are NOT normalized to the bandwidth- need to divide by the bandwidth

# MANTA DATA ####
inFiles = list.files(dirGCP, pattern = "MinRes", recursive = T, full.names = T)
inFiles = inFiles[!grepl(".png",inFiles) ]
inFiles = inFiles[!grepl(".csv",inFiles) ]
inFilesON = inFiles[!grepl("_netCDF",inFiles) ]
cat("Found ", length(inFilesON), "ONMS files")
# ONLY RUN FIRST TIME ####
cData = NULL
for (f in 1:length(inFilesON) ){
  ncFile = inFilesON[f]
  hmdData = loadSoundscapeData(ncFile) #only keeps quality 1 as default
  tolData = createOctaveLevel(hmdData, type='tol') #keeps it as broadband levels in each TOL
  tolData$site = site
  cData = rbind( cData, tolData )
}
save(cData, file = paste0(dirSS, "//", tolower(site), "_05_MantaOutputCombined.Rda") )

file = paste0(dirSS, "//", tolower(site), "_05_MantaOutputCombined.Rda")
load(file)
# data are NOT normalized to the bandwidth- need to divide by the bandwidth
cDatah = binSoundscapeData(cData, bin = "1hour", method = c("median") )

# look at one frequency bin and plot
names( cDatah)
cols_to_select = c("UTC", "TOL_500")
dManta  = cDatah %>% select(all_of(cols_to_select))
colnames( dManta) = c("UTC", "manta")

dTriton = SSdata %>% select(all_of(cols_to_select))
colnames( dTriton) = c("UTC", "triton")

dcombine = dManta %>%
  left_join(dTriton, by = "UTC")
names(dcombine)

dcombine$diff = dcombine$manta - dcombine$triton
hist(dcombine$diff)

ggplot(dcombine, aes(x = UTC, y = diff)) +
  geom_line()+
  ggtitle (" Difference in 500 Hz TOL (Manta - Triton)- median")


# look at one frequency bin and plot-- 2 kHz
cols_to_select = c("UTC", "TOL_2000")
dManta  = cDatah %>% select(all_of(cols_to_select))
colnames( dManta) = c("UTC", "manta")

dTriton = SSdata %>% select(all_of(cols_to_select))
colnames( dTriton) = c("UTC", "triton")

dcombine = dManta %>%
  left_join(dTriton, by = "UTC")
names(dcombine)

dcombine$diff = dcombine$manta - dcombine$triton
hist(dcombine$diff)

ggplot(dcombine, aes(x = UTC, y = diff)) +
  geom_line()+
  ggtitle (" Difference in TOL_2000 Hz TOL (Manta - Triton)- median")


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


  
