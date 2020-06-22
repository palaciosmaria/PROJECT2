source("followingScripts.R")
library(dplyr)
library(RHRV)
library(lubridate)
#hrv.data = CreateHRVData()
#hrv.data = SetVerbose(hrv.data, TRUE)
setwd("/Users/mariapalacios/Desktop/CEUTERCERO/PROJECT2/mitdb_csvs")
numeroDeFicheros = unique(sapply(strsplit(dir(pattern = "DataFrame.csv"), "_"), function(x)
  x[[1]]))

for (i in 1:length(numeroDeFicheros)) {

  csvs = read.csv(paste0(numeroDeFicheros[i], "_DataFrame.csv"),
                    header = TRUE)
  td = seconds_to_period(csvs$TIME)
  csvs$NEWTIME=sprintf('%02d:%5f',minute(td), second(td))
  csvs$CERO1=0
  csvs$CERO2=0
  csvs= csvs[ ,c(1,5,2,3,6,7,4)]
  csvs$TIME=as.integer(csvs$TIME*360)
  write.table(csvs,paste0(numeroDeFicheros[i], "_Annotations.txt"), quote =FALSE)
  
}

