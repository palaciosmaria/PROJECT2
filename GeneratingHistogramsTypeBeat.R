library(dplyr)
library(RHRV)
library(lubridate)
library(moments)
setwd("/Users/mariapalacios/Desktop/CEUTERCERO/PROJECT2/mitdb_csvs")

numeroDeFicheros = unique(sapply(strsplit(dir(pattern = "DataFrame.csv"), "_"), function(x)
  x[[1]]))
newCsv= data.frame()
vectorFile=c()
vectorMeanadelante=c()
vectorMeanatras=c()
vectorDesvadelante=c()
vectorDesvatras=c()
vectorCurtosisadelante=c()
vectorCurtosisatras=c()
vectorMeanBeatTypeNforwardRR=c()
vectorMeanBeatTypeNbackwardRR=c()
vectorDesvBeatTypeNforwardRR=c()
vectorDesvBeatTypeNbackwardRR=c()
vectorCurtosisBeatTypeNforwardRR= c()
vectorCurtosisBeatTypeNbackwardRR= c()
vectorMeanBeatTypeAMinusforwardRR=c()
vectorMeanBeatTypeAMinusbackwardRR=c()
vectorDesvBeatTypeAMinusforwardRR=c()
vectorDesvBeatTypeAMinusbackwardRR=c()
vectorCurtosisBeatTypeAMinusforwardRR= c()
vectorCurtosisBeatTypeAMinusbackwardRR= c()
vectorMeanBeatTypeAforwardRR=c()
vectorMeanBeatTypeAbackwardRR=c()
vectorDesvBeatTypeAforwardRR=c()
vectorDesvBeatTypeAbackwardRR=c()
vectorCurtosisBeatTypeAforwardRR= c()
vectorCurtosisBeatTypeAbackwardRR= c()
vectorMeanBeatTypeFforwardRR=c()
vectorMeanBeatTypeFbackwardRR=c()
vectorDesvBeatTypeFforwardRR=c()
vectorDesvBeatTypeFbackwardRR=c()
vectorCurtosisBeatTypeFforwardRR= c()
vectorCurtosisBeatTypeFbackwardRR= c()
vectorMeanBeatTypeVforwardRR=c()
vectorMeanBeatTypeVbackwardRR=c()
vectorDesvBeatTypeVforwardRR=c()
vectorDesvBeatTypeVbackwardRR=c()
vectorCurtosisBeatTypeVforwardRR= c()
vectorCurtosisBeatTypeVbackwardRR= c()

min_value=200
max_value=2600
for (i in 1:length(numeroDeFicheros)) {
  annotations = read.table(paste0(numeroDeFicheros[i], "_Annotations.txt"),
                           header = TRUE)
  vector= annotations$TIME/360
  finalVector=vector *1000
  L=length(finalVector)
  RRs=finalVector[2:L]-finalVector[1:(L-1)]
  annotations$RRadelante=c(RRs,NA)
  annotations$RRatras=c(NA,RRs)
  write.csv(annotations,paste0(numeroDeFicheros[i], "_AnnotationsWithRRs.csv"), quote =FALSE)
  newCsv=rbind(newCsv,annotations)
  RRforward= na.omit(annotations$RRadelante)
  RRbackward=na.omit(annotations$RRatras)

    valid_RRNadelante=RRforward[RRforward >= min_value && RRforward <= max_value]
    valid_RRNatras=RRbackward[RRbackward >= min_value && RRbackward <= max_value]
    if (length(valid_RRNadelante)>1 && length(valid_RRNatras)>1){
      pdf(file=paste(numeroDeFicheros[i],"_Histogram.pdf"))
      hist(valid_RRNadelante, main = paste0(numeroDeFicheros[i]," histogram forward RR"),breaks = seq(min_value-1e-6,max_value+1e-6, len=15), xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
      hist(valid_RRNatras, main = paste0(numeroDeFicheros[i]," histogram backward RR"), breaks = seq(min_value-1e-6,max_value+1e-6, len=15), xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
  }
  dev.off()
  
  vectorMeanadelante=c(vectorMeanadelante,mean(RRforward))
  vectorMeanatras=c(vectorMeanatras,mean(RRbackward))
  vectorDesvadelante=c(vectorDesvadelante,sd(RRforward))
  vectorDesvatras=c(vectorDesvatras,sd(RRbackward))
  vectorCurtosisadelante= c(vectorCurtosisadelante, kurtosis(RRforward))
  vectorCurtosisatras= c(vectorCurtosisatras,kurtosis(RRbackward))
  vectorFile= c(vectorFile,paste0(numeroDeFicheros[i],"_Annotations.txt"))
  
  
  #por paciente y por latido tambien
  TypeN= filter(annotations, TYPE == 'N')
  write.csv(TypeN,paste0(numeroDeFicheros[i],"_AnnotationsWithRRsBeatTypeN.csv"), quote =FALSE)
  TypeV=filter(annotations, TYPE == 'V')
  write.csv(TypeV,paste0(numeroDeFicheros[i],"_AnnotationsWithRRsBeatTypeV.csv"), quote =FALSE)
  TypeA=filter(annotations, TYPE == 'A')
  write.csv(TypeA,paste0(numeroDeFicheros[i],"_AnnotationsWithRRsBeatTypeA.csv"), quote =FALSE)
  TypeF=filter(annotations, TYPE == 'F')
  write.csv(TypeF,paste0(numeroDeFicheros[i],"_AnnotationsWithRRsBeatTypeF.csv"), quote =FALSE)
  Typea= filter(annotations, TYPE == 'a')
  write.csv(Typea,paste0(numeroDeFicheros[i],"_AnnotationsWithRRsBeatTypeAMinus.csv"), quote =FALSE)
  
  RRforwardN= na.omit(TypeN$RRadelante)
  RRbackwardN=na.omit(TypeN$RRatras)
  pdf(file=paste0(numeroDeFicheros[i],"_PacientBeatTypeNHistogram.pdf"))
  valid_RRNadelanteN=RRforwardN[RRforwardN >= min_value && RRforwardN <= max_value]
  valid_RRNatrasN=RRbackwardN[RRbackwardN >= min_value && RRbackwardN <= max_value]
  hist(valid_RRNadelanteN, main = paste0(numeroDeFicheros[i],"Register Beat Type N histogram forward RR"),breaks = seq(min_value-1e-6,max_value+1e-6, len=15), xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
  hist(valid_RRNatrasN, main = paste0(numeroDeFicheros[i],"Register Beat Type N histogram backward RR"), breaks = seq(min_value-1e-6,max_value+1e-6, len=15), xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
  vectorMeanBeatTypeNforwardRR=c(vectorMeanBeatTypeNforwardRR ,mean(RRforwardN))
  vectorMeanBeatTypeNbackwardRR= c(vectorMeanBeatTypeNbackwardRR ,mean(RRbackwardN))
  vectorDesvBeatTypeNforwardRR=c(vectorDesvBeatTypeNforwardRR, sd(RRforwardN))
  vectorDesvBeatTypeNbackwardRR=c(vectorDesvBeatTypeNbackwardRR, sd(RRbackwardN))
  vectorCurtosisBeatTypeNforwardRR=c(vectorCurtosisBeatTypeNforwardRR ,kurtosis(RRforwardN))
  vectorCurtosisBeatTypeNbackwardRR=c(vectorCurtosisBeatTypeNbackwardRR, kurtosis(RRbackwardN))
  
  if (length (Typea)>1){
    RRforwarda=na.omit(Typea$RRadelante)
    RRbackwarda=na.omit(Typea$RRatras)
    valid_RRNadelantea=RRforwarda[RRforwarda >= min_value && RRforwarda <= max_value]
    valid_RRNatrasa=RRbackwarda[RRbackwarda >= min_value && RRbackwarda <= max_value]
    hist(valid_RRNadelantea, main = paste0(numeroDeFicheros[i],"Register Beat Type a histogram forward RR"),breaks = seq(min_value-1e-6,max_value+1e-6), xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
    hist(valid_RRNatrasa, main = paste0(numeroDeFicheros[i],"Register Beat Type a histogram backward RR"),breaks = seq(min_value-1e-6,max_value+1e-6), xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
    vectorMeanBeatTypeAMinusforwardRR=c(vectorMeanBeatTypeAMinusforwardRR ,mean(RRforwarda))
    vectorMeanBeatTypeAMinusbackwardRR=c(vectorMeanBeatTypeAMinusbackwardRR ,mean(RRbackwarda))
    vectorDesvBeatTypeAMinusforwardRR=c(vectorDesvBeatTypeAMinusforwardRR ,sd(RRforwarda))
    vectorDesvBeatTypeAMinusbackwardRR=c(vectorDesvBeatTypeAMinusbackwardRR ,sd(RRbackwarda))
    vectorCurtosisBeatTypeAMinusforwardRR= c(vectorCurtosisBeatTypeAMinusforwardRR ,kurtosis(RRforwarda))
    vectorCurtosisABeatTypeMinusbackwardRR= c(vectorCurtosisBeatTypeAMinusbackwardRR ,kurtosis(RRbackwarda))
  }
  if (length (TypeA)>1){
    RRforwardA=na.omit(TypeA$RRadelante)
    RRbackwardA=na.omit(TypeA$RRatras)
    valid_RRNadelanteA=RRforwardA[RRforwardA >= min_value && RRforwardA <= max_value]
    valid_RRNatrasA=RRbackwardA[RRbackwardA >= min_value && RRbackwardA <= max_value]
    hist(valid_RRNadelanteA, main = paste0(numeroDeFicheros[i],"Register Beat Type A histogram forward RR"),breaks = seq(min_value-1e-6,max_value+1e-6, len=15), xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
    hist(valid_RRNatrasA, main = paste0(numeroDeFicheros[i],"Register Beat Type A histogram backward RR"),breaks = seq(min_value-1e-6,max_value+1e-6, len=15),  xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
    vectorMeanBeatTypeAforwardRR=c(vectorMeanBeatTypeAforwardRR ,mean(RRforwardA))
    vectorMeanBeatTypeAbackwardRR=c(vectorMeanBeatTypeAbackwardRR ,mean(RRbackwardA))
    vectorDesvBeatTypeAforwardRR=c(vectorDesvBeatTypeAforwardRR ,sd(RRforwardA))
    vectorDesvBeatTypeAbackwardRR=c(vectorDesvBeatTypeAbackwardRR ,sd(RRbackwardA))
    vectorCurtosisBeatTypeAforwardRR= c(vectorCurtosisBeatTypeAforwardRR ,kurtosis(RRforwardA))
    vectorCurtosisBeatTypeAbackwardRR= c(vectorCurtosisBeatTypeAbackwardRR ,kurtosis(RRbackwardA))
    
    }
  if (length (TypeF)>1){
    RRforwardF=na.omit(TypeF$RRadelante)
    RRbackwardF=na.omit(TypeF$RRatras)
    valid_RRNadelanteF=RRforwardF[RRforwardF >= min_value && RRforwardF <= max_value]
    valid_RRNatrasF=RRbackwardF[RRbackwardF >= min_value && RRbackwardF <= max_value]
    hist(valid_RRNadelanteF, main = paste0(numeroDeFicheros[i],"Register Beat Type F histogram forward RR"),breaks = seq(min_value-1e-6,max_value+1e-6, len=15), xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
    hist(valid_RRNatrasF, main = paste0(numeroDeFicheros[i],"Register Beat Type F histogram backward RR"), breaks = seq(min_value-1e-6,max_value+1e-6, len=15), xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
    vectorMeanBeatTypeFforwardRR=c(vectorMeanBeatTypeFforwardRR ,mean(RRforwardF))
    vectorMeanBeatTypeFbackwardRR=c(vectorMeanBeatTypeFbackwardRR ,mean(RRbackwardF))
    vectorDesvBeatTypeFforwardRR=c(vectorDesvBeatTypeFforwardRR ,sd(RRforwardF))
    vectorDesvBeatTypeFbackwardRR=c(vectorDesvBeatTypeFbackwardRR ,sd(RRbackwardF))
    vectorCurtosisBeatTypeFforwardRR=c(vectorCurtosisBeatTypeFforwardRR ,kurtosis(RRforwardF))
    vectorCurtosisBeatTypeFbackwardRR= c(vectorCurtosisBeatTypeFbackwardRR ,kurtosis(RRbackwardF))
    
    }
  if (length (TypeV)>1){
    RRforwardV=na.omit(TypeV$RRadelante)
    RRbackwardV=na.omit(TypeV$RRatras)
    valid_RRNadelanteV=RRforwardV[RRforwardV >= min_value && RRforwardV <= max_value]
    valid_RRNatrasV=RRbackwardV[RRbackwardV >= min_value && RRbackwardV <= max_value]
    hist(valid_RRNadelanteV, main = paste0(numeroDeFicheros[i],"Register Beat Type V histogram forward RR"),breaks = seq(min_value-1e-6,max_value+1e-6, len=15), xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
    hist(valid_RRNatrasV, main = paste0(numeroDeFicheros[i],"Register Beat Type V histogram backward RR"),breaks = seq(min_value-1e-6,max_value+1e-6, len=15),  xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
    vectorMeanBeatTypeVforwardRR=c(vectorMeanBeatTypeVforwardRR ,mean(RRforwardV))
    vectorMeanBeatTypeVbackwardRR=c(vectorMeanBeatTypeVbackwardRR ,mean(RRbackwardV))
    vectorDesvBeatTypeVforwardRR=c(vectorDesvBeatTypeVforwardRR ,sd(RRforwardV))
    vectorDesvBeatTypeVbackwardRR=c(vectorDesvBeatTypeVbackwardRR ,sd(RRbackwardV))
    vectorCurtosisBeatTypeVforwardRR=c(vectorCurtosisBeatTypeVforwardRR ,kurtosis(RRforwardV))
    vectorCurtosisBeatTypeVbackwardRR=c(vectorCurtosisBeatTypeVbackwardRR ,kurtosis(RRbackwardV))
    }
  dev.off()

}

dfEstatisticsRegisters= data.frame(
  FILE=vectorFile,
  meanRRforward = vectorMeanadelante,
  meanRRbackward = vectorMeanatras,
  deviationRRforward = vectorDesvadelante,
  deviationRRbackward = vectorDesvatras,
  kurtosisRRforward = vectorCurtosisadelante,
  kurtosisRRbackward = vectorCurtosisatras,
  
  meanBeatTypeNforwardRR=vectorMeanBeatTypeNforwardRR,
  meanBeatTypeNbackwardRR=vectorMeanBeatTypeNbackwardRR,
  desvBeatTypeNforwardRR=vectorDesvBeatTypeNforwardRR,
  desvBeatTypeNbackwardRR=vectorDesvBeatTypeNbackwardRR,
  curtosisBeatTypeNforwardRR= vectorCurtosisBeatTypeNforwardRR,
  curtosisBeatTypeNbackwardRR= vectorCurtosisBeatTypeNbackwardRR,
  
  meanBeatTypeAMinusforwardRR=vectorMeanBeatTypeAMinusforwardRR,
  meanBeatTypeAMinusbackwardRR=vectorMeanBeatTypeAMinusbackwardRR,
  desvBeatTypeAMinusforwardRR=vectorDesvBeatTypeAMinusforwardRR,
  desvBeatTypeAMinusbackwardRR=vectorDesvBeatTypeAMinusbackwardRR,
  curtosisBeatTypeAMinusforwardRR= vectorCurtosisBeatTypeAMinusforwardRR,
  curtosisABeatTypeMinusbackwardRR= vectorCurtosisBeatTypeAMinusbackwardRR,
  
  meanBeatTypeAforwardRR=vectorMeanBeatTypeAforwardRR,
  meanBeatTypeAbackwardRR=vectorMeanBeatTypeAbackwardRR,
  desvBeatTypeAforwardRR=vectorDesvBeatTypeAforwardRR,
  desvBeatTypeAbackwardRR=vectorDesvBeatTypeAbackwardRR,
  curtosisBeatTypeAforwardRR= vectorCurtosisBeatTypeAforwardRR,
  curtosisBeatTypeAbackwardRR= vectorCurtosisBeatTypeAbackwardRR,
  
  meanBeatTypeFforwardRR=vectorMeanBeatTypeFforwardRR,
  meanBeatTypeFbackwardRR=vectorMeanBeatTypeFbackwardRR,
  desvBeatTypeFforwardRR=vectorDesvBeatTypeFforwardRR,
  desvBeatTypeFbackwardRR=vectorDesvBeatTypeFbackwardRR,
  curtosisBeatTypeFforwardRR= vectorCurtosisBeatTypeFforwardRR,
  curtosisBeatTypeFbackwardRR= vectorCurtosisBeatTypeFbackwardRR,
  
  meanBeatTypeVforwardRR=vectorMeanBeatTypeVforwardRR,
  meanBeatTypeVbackwardRR=vectorMeanBeatTypeVbackwardRR,
  desvBeatTypeVforwardRR=vectorDesvBeatTypeVforwardRR,
  desvBeatTypeVbackwardRR=vectorDesvBeatTypeVbackwardRR,
  curtosisBeatTypeVforwardRR= vectorCurtosisBeatTypeVforwardRR,
  curtosisBeatTypeVbackwardRR= vectorCurtosisBeatTypeVbackwardRR,
  stringsAsFactors = FALSE
)
write.csv(dfEstatisticsRegisters,paste0("StatisticsOfRegistersAndPatients.csv"), quote =FALSE)

TypeN2= filter(newCsv, TYPE == 'N')
write.csv(TypeN,paste0("AnnotationsWithRRsBeatTypeN.csv"), quote =FALSE)
TypeV2=filter(newCsv, TYPE == 'V')
write.csv(TypeV,paste0("AnnotationsWithRRsBeatTypeV.csv"), quote =FALSE)
TypeA2=filter(newCsv, TYPE == 'A')
write.csv(TypeA,paste0("AnnotationsWithRRsBeatTypeA.csv"), quote =FALSE)
TypeF2=filter(newCsv, TYPE == 'F')
write.csv(TypeF,paste0("AnnotationsWithRRsBeatTypeF.csv"), quote =FALSE)
Typea2= filter(newCsv, TYPE == 'a')
write.csv(Typea,paste0("AnnotationsWithRRsBeatTypeAMinus.csv"), quote =FALSE)

#install.packages("moments")
RRforwardN2= na.omit(TypeN2$RRadelante)
RRbackwardN2=na.omit(TypeN2$RRatras)
pdf("BeatTypeNHistogram.pdf")
valid_RRNadelanteN2=RRforwardN2[RRforwardN2 >= min_value && RRforwardN2 <= max_value]
valid_RRNatrasN2=RRbackwardN2[RRbackwardN2 >= min_value && RRbackwardN2 <= max_value]
hist(valid_RRNadelanteN2, main = "Beat Type N histogram forward RR", xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
hist(valid_RRNatrasN2, main = "Beat Type N histogram backward RR",  xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
dev.off()


RRforwarda2=na.omit(Typea2$RRadelante)
RRbackwarda2=na.omit(Typea2$RRatras)
valid_RRNadelantea2=RRforwarda2[RRforwarda2 >= min_value && RRforwarda2 <= max_value]
valid_RRNatrasa2=RRbackwarda2[RRbackwarda >= min_value && RRbackwarda2 <= max_value]
pdf("BeatTypeAMinusHistogram.pdf")
hist(valid_RRNadelantea2, main = "Beat Type a histogram forward RR", xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
hist(valid_RRNatrasa2, main = "Beat Type a histogram backward RR",  xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
dev.off()

RRforwardA2=na.omit(TypeA2$RRadelante)
RRbackwardA2=na.omit(TypeA2$RRatras)
valid_RRNadelanteA2=RRforwardA2[RRforwardA2 >= min_value && RRforwardA2 <= max_value]
valid_RRNatrasA2=RRbackwardA2[RRbackwardA2 >= min_value && RRbackwardA2 <= max_value]
pdf("BeatTypeAHistogram.pdf")
hist(valid_RRNadelanteA2, main = "Beat Type A histogram forward RR", xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
hist(valid_RRNatrasA2, main = "Beat Type A histogram backward RR",  xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
dev.off()

RRforwardF2=na.omit(TypeF2$RRadelante)
RRbackwardF2=na.omit(TypeF2$RRatras)
valid_RRNadelanteF2=RRforwardF2[RRforwardF2 >= min_value && RRforwardF2 <= max_value]
valid_RRNatrasF2=RRbackwardF2[RRbackwardF2 >= min_value && RRbackwardF2 <= max_value]
pdf("BeatTypeFHistogram.pdf")
hist(valid_RRNadelanteF2, main = "Beat Type F histogram forward RR", xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
hist(valid_RRNatrasF2, main = "Beat Type F histogram backward RR",  xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
dev.off()

RRforwardV2=na.omit(TypeV2$RRadelante)
RRbackwardV2=na.omit(TypeV2$RRatras)
valid_RRNadelanteV2=RRforwardV2[RRforwardV2 >= min_value && RRforwardV2 <= max_value]
valid_RRNatrasV2=RRbackwardV2[RRbackwardV2 >= min_value && RRbackwardV2 <= max_value]
pdf("BeatTypeVHistogram.pdf")
hist(valid_RRNadelanteV2, main = "Beat Type V histogram forward RR", xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
hist(valid_RRNatrasV2, main = "Beat Type V histogram backward RR",  xlab = "RR distance in time (ms)", ylab = "Frequency", xlim=c(min_value,max_value))
dev.off()

dfEstatisticsBeatTypes= data.frame(
  meanBeatTypeNforwardRR=mean(RRforwardN2),
  meanBeatTypeNbackwardRR=mean(RRbackwardN2),
  desvBeatTypeNforwardRR=sd(RRforwardN2),
  desvBeatTypeNbackwardRR=sd(RRbackwardN2),
  curtosisBeatTypeNforwardRR= kurtosis(RRforwardN2),
  curtosisBeatTypeNbackwardRR= kurtosis(RRbackwardN2),
  
  meanBeatTypeAMinusforwardRR=mean(RRforwarda2),
  meanBeatTypeAMinusbackwardRR=mean(RRbackwarda2),
  desvBeatTypeAMinusforwardRR=sd(RRforwarda2),
  desvBeatTypeAMinusbackwardRR=sd(RRbackwarda2),
  curtosisBeatTypeAMinusforwardRR= kurtosis(RRforwarda2),
  curtosisABeatTypeMinusbackwardRR= kurtosis(RRbackwarda2),
  
  meanBeatTypeAforwardRR=mean(RRforwardA2),
  meanBeatTypeAbackwardRR=mean(RRbackwardA2),
  desvBeatTypeAforwardRR=sd(RRforwardA2),
  desvBeatTypeAbackwardRR=sd(RRbackwardA2),
  curtosisBeatTypeAforwardRR= kurtosis(RRforwardA2),
  curtosisBeatTypeAbackwardRR= kurtosis(RRbackwardA2),
  
  meanBeatTypeFforwardRR=mean(RRforwardF2),
  meanBeatTypeFbackwardRR=mean(RRbackwardF2),
  desvBeatTypeFforwardRR=sd(RRforwardF2),
  desvBeatTypeFbackwardRR=sd(RRbackwardF2),
  curtosisBeatTypeFforwardRR= kurtosis(RRforwardF2),
  curtosisBeatTypeFbackwardRR= kurtosis(RRbackwardF2),
  
  meanBeatTypeVforwardRR=mean(RRforwardV2),
  meanBeatTypeVbackwardRR=mean(RRbackwardV2),
  desvBeatTypeVforwardRR=sd(RRforwardV2),
  desvBeatTypeVbackwardRR=sd(RRbackwardV2),
  curtosisBeatTypeVforwardRR= kurtosis(RRforwardV2),
  curtosisBeatTypeVbackwardRR= kurtosis(RRbackwardV2),
  
  stringsAsFactors = FALSE
)

dfStatisticsBeatTypes <- as.data.frame(t(dfEstatisticsBeatTypes))
write.csv(dfStatisticsBeatTypes,paste0("StatisticsOfBeatTypes.csv"), quote =FALSE)
