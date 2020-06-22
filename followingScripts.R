library(dplyr)
library(RHRV)
hrv.data = CreateHRVData()
hrv.data = SetVerbose(hrv.data, TRUE)
setwd("/Users/mariapalacios/Desktop/CEUTERCERO/PROJECT2/mitdb_csvs")


dataFrame=list()
totales = 0

numeroDeFicheros = unique(sapply(strsplit(dir(pattern = "times.txt"), "_"), function(x)
  x[[1]]))

for (i in 1:length(numeroDeFicheros)) {
  #he metido la inicialización de los vectores dentro porque para que cada vuelta del bucle 
  #borre los datos de lo anterior (antes lo que hacía era agrupar, de tal manera que cuando 
  #llegaba al ultimo csv se imprimian los latidos de todos los archivos)
  vectorTime = c()
  vectorLatidos = c()
  vectorFilteredOrNot = c()
  hrv.data = LoadBeatAscii(hrv.data,
                           paste0(numeroDeFicheros[i], "_times.txt"),
                           RecordPath = ".")
  labels = read.csv(paste0(numeroDeFicheros[i], "_labels.txt"),
                    header = FALSE,
                    quote = "")
  valid_annotations = !grepl("[~|+]", labels$V1)
  labels = data.frame(V1 = droplevels(labels[valid_annotations, ]))
  hrv.data$Beat = hrv.data$Beat[valid_annotations, , drop = FALSE]
  print(nrow(hrv.data$Beat) == nrow(labels))
  original_times = hrv.data$Beat$Time
  hrv.data$Beat$labels = labels$V1
  hrv.data = BuildNIHR(hrv.data)
  hrv.data = FilterNIHR(hrv.data)
  RR = hrv.data$Beat
  filtrado = hrv.data$Beat
  labels$V1 = as.character(labels$V1)
  #indexes = labels$V1 %in% c("A", "N", "F")
  #labels$V1[indexes] = "N"
  #labels$V1[!indexes] = "V"
  for (k in 1:(length(original_times) - 1)) {
    totales =  totales + 1
    original_time = original_times[k]
    if (original_time %in% hrv.data$Beat$Time == FALSE) {
      filtrado = original_times[k]
      vectorTime = c(vectorTime, original_times[k])
      vectorLatidos = c(vectorLatidos, as.character(labels$V1[k]))
      vectorFilteredOrNot = c(vectorFilteredOrNot, paste0("1"))
    }
    if (original_time %in% hrv.data$Beat$Time == TRUE) {
      filtrado = original_times[k]
      vectorTime = c(vectorTime, original_times[k])
      vectorLatidos = c(vectorLatidos, as.character(labels$V1[k]))
      vectorFilteredOrNot = c(vectorFilteredOrNot, paste0("0"))
    }
  }
  vectorLatidos[vectorLatidos=="\""]='0'
  dataFrame[[i]]= data.frame(
    TIME = vectorTime,
    TYPE = vectorLatidos,
    FILTERED = vectorFilteredOrNot,
    stringsAsFactors = FALSE
  )
  write.csv(dataFrame[i],paste0(numeroDeFicheros[i], "_DataFrame.csv"), quote =FALSE)
  
}
