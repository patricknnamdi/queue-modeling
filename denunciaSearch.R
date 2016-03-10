## This program finds the P1 and P2 for a given denuncia 
## dataset: inspecciones.csv and rociado.csv 

## setwd
setwd("/Users/patrickemedom/Desktop/Levy_lab/denuncia/")
inspecciones <- data.frame(read.csv("inspecciones - INSPECCIONES.csv"))
inspecciones <- inspecciones[, c("UNICODE.", "SITUACION","NRO_DENUNCIA","DIA", "MES", "ANIO", "IN_TCAP_TOT", "PD_TCAP_TOT", "INSP_COMPLETA")]

## Sum contents of IN_TCAP_TOT and PD_TCAP_TOT and INSP_COMPLETA and replace NA with zeros 
inspecciones$INSP_COMPLETA <- ifelse(is.na(inspecciones$INSP_COMPLETA), 0, inspecciones$INSP_COMPLETA)
inspecciones$IN_TCAP_TOT <- ifelse(is.na(inspecciones$IN_TCAP_TOT), 0, inspecciones$IN_TCAP_TOT)
inspecciones$PD_TCAP_TOT <- ifelse(is.na(inspecciones$PD_TCAP_TOT), 0, inspecciones$PD_TCAP_TOT)
inspecciones$sumTotal <- inspecciones$IN_TCAP_TOT + inspecciones$PD_TCAP_TOT
inspecciones$sumTotal <- ifelse(is.na(inspecciones$sumTotal), 0, inspecciones$sumTotal)

## replace $Situacion with only positive contents (sumtotal > 0)
indPos <- inspecciones[which(inspecciones$sumTotal != 0), ]

## Take starting at 2012 when P1 and P2 was utilized
indPos <- indPos[which(indPos$ANIO >= 2012),]

## function for total number of P1 or P2 for given denuncia no 
inspecciones.date <- inspecciones[which(inspecciones$ANIO >= 2012),]

## Get positive denuncias 
denunNo <- indPos$NRO_DENUNCIA[indPos$SITUACION == "D"]

## dNo is an int, while pValue is a string 
## TODO: take in  a data base as a function 
countPValue <- function(dNo, pValue) {
  indDeNun <- inspecciones.date[which(inspecciones.date$NRO_DENUNCIA == dNo),]
  sum <- sum(indDeNun$SITUACION == pValue)
  return(sum)
}

##########################################
          # insp_completa  #
 ##all houses that had an inspection##
##########################################
insp_completa <- inspecciones.date$INSP_COMPLETA[inspecciones.date$NRO_DENUNCIA[inspecciones.date$SITUACION == "D"]]

## function to count inspCompleta for each postive denuncia 
countCompleta <- function(dNo, inspCompleta) {
  indDeNun <- inspecciones.date[which(inspecciones.date$NRO_DENUNCIA == dNo),]
  sum <- sum(indDeNun$INSP_COMPLETA == inspCompleta)
  return(sum)
}

##########################################
          # sumTotal #
    ##Initial Age of infestation ##
##########################################
countTotalBugs <- function(dNo) {
  indDeNun <- inspecciones.date[which(inspecciones.date$NRO_DENUNCIA == dNo),]
  return(indDeNun$sumTotal)
}

## loop through count function to create table of P1 and P2 per denunNo including inspCompleta
valTable <- c()
for (i in denunNo) { 
  valTable <- rbind(valTable, c(i, countTotalBugs(i), countPValue(i, "P1"), countPValue(i, "P2"), countCompleta(i, "1")))
}
colnames(valTable) <- c("NRO_DENUNCIA", "P1", "P2", "INSP_COMPLETA")

valTable.df <- as.data.frame(valTable)
valTable.df.2 <- data.frame(valTable)
##histogram 
hist(valTable.df$P1)
