## This program finds the P1 and P2 for a given denuncia 
## dataset: inspecciones.csv and rociado.csv 

inspecciones <- data.frame(read.csv("/Users/patrickemedom/Desktop/denuncia/inspecciones - INSPECCIONES.csv"))
inspecciones <- inspecciones[, c("UNICODE.", "SITUACION","NRO_DENUNCIA","DIA", "MES", "ANIO", "IN_TCAP_TOT", "PD_TCAP_TOT")]
rociado <- data.frame(read.csv("/Users/patrickemedom/Desktop/denuncia/rociado - ROCIADO.csv"))
rociado <- rociado[, c("UNICODE", "SITUACION", "DIA", "MES", "ANIO", "IN_TCAP_TOT", "PD_TCAP_TOT")]

## merge datasets 
colnames(inspecciones)[1] <- "UNICODE"
dataset <- merge(inspecciones, rociado, by = "UNICODE", all = TRUE)

## TODO write in to function of no. 
denunVal <- inspecciones$SITUACION[inspecciones$NRO_DENUNCIA == i]

## Sum contents of IN_TCAP_TOT and PD_TCAP_TOT and replace NA with zeros 
inspecciones$IN_TCAP_TOT <- ifelse(is.na(inspecciones$IN_TCAP_TOT), 0, inspecciones$IN_TCAP_TOT)
inspecciones$PD_TCAP_TOT <- ifelse(is.na(inspecciones$PD_TCAP_TOT), 0, inspecciones$PD_TCAP_TOT)
inspecciones$sumTotal <- inspecciones$IN_TCAP_TOT + inspecciones$PD_TCAP_TOT
inspecciones$sumTotal <- ifelse(is.na(inspecciones$sumTotal), 0, inspecciones$sumTotal)


## replace $Situacion with only positive contents (sumtotal > 0)
indPos <- inspecciones[which(inspecciones$sumTotal != 0), ]
indPos <- indPos[which(indPos$ANIO >= 2011),]

## function for total number of P1 or P2 for given denuncia no 
denunNo <- indPos$NRO_DENUNCIA[indPos$SITUACION == "D"]

## dNo is an int, while pValue is a string 
count <- function(dNo, pValue) {
  indDeNun <- indPos[which(indPos$NRO_DENUNCIA == dNo),]
  sum <- sum(indDeNun$SITUACION == pValue)
  return(sum)
}


## loop through count function to create table of P1 and P2 per denunNo
valTable <- c()
for (i in denunNo) {
    valTable <- rbind(valTable, c(i, count(i, "P1"), count(i, "P2")))
    colnames(valTable) <- c("NRO_DENUNCIA", "P1", "P2")
}
