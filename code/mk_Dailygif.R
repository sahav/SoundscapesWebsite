
library(dplyr)
library(magick)
library(magrittr)

dirDaily = "F:\\ONMS\\pm01\\onms_pm01_20221001-20230704_hmd\\data"

list.files(path=dirDaily, pattern = '*_psd.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write(paste0(dirDaily, "\\FileName.gif") ) # write to current dir

