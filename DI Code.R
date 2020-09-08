################################################
##          DISTRIBUCIÓN INDICES              ##
################################################


################################################
# Comando para borra el enviroment 
rm(list=ls())

# Importar
library(readxl)
Indices <- read_excel("C:/Users/Rodro/Documents/R/DI/Base.xlsx")

# Librerias
library(moments)
library(MASS)
library(tseries)
library(fUnitRoots) #-- paquete de pruebas de raiz unitaria
library(urca)       #-- paquete auxiliar de funit
library(fitdistrplus)
library(extraDistr)
library(actuar)

## Construcción de Variables

p = Indices[,(2:6)] # matriz con solo los indices
nrp = nrow(p)
ncp = ncol(p)

r = (log(p[2:nrp,])-log(p[1:(nrp-1),]))*100 # matriz de los retornos

## Arreglos de fechas

Fecha  = as.Date(Indices$Fecha, format="%Y-%m-%d")
nf = length(Fecha)
Fecha1 = as.Date(Fecha[2:nf],format="%Y-%m-%d")
datelabels1<-format(Fecha1,"%Y-%m")

datelabels2 = Fecha1

for (k in 2:(nf-1)) {
  if (datelabels1[k] != datelabels1[k-1]) {
  } else {
    datelabels2[k] = NA
  }
}

datelabels3 = as.numeric(format(datelabels2,"%m"))
nnn = which(datelabels3!=6 & datelabels3!=12)

datelabels2[nnn] = NA
datelabels4 = format(datelabels2,"%Y-%m")

################################################

# 1.Desciptiva Indices

################################################

### 1.1. Gráfico Evolución Retornos

windows() ## create window to plot your file
par(mfrow=c(3,2))

plot(r$`S&P500`~Fecha1,main="S&P 500", 
     type="l",lty=1,col="black", ylab = '', xlab = '')
abline(h=0,col="black",lty=1) #línea en cero

plot(r$COLCAP~Fecha1,main="COLCAP", 
     type="l",lty=1,col="black", ylab = '', xlab = '')
abline(h=0,col="black",lty=1) #línea en cero

plot(r$IPyC~Fecha1,main="IPyC", 
     type="l",lty=1,col="black", ylab = '', xlab = '')
abline(h=0,col="black",lty=1) #línea en cero

plot(r$IPSA~Fecha1,main="IPSA", 
     type="l",lty=1,col="black", ylab = '', xlab = '')
abline(h=0,col="black",lty=1) #línea en cero

plot(r$SPBVLP~Fecha1,main="SP/BVL", 
     type="l",lty=1,col="black", ylab = '', xlab = '')
abline(h=0,col="black",lty=1) #línea en cero
par(mai=c(0,0,0,0))
plot.new()
title("Retornos diarios (%)", outer = TRUE, line = -1)

### 1.2. Estadísticas Descriptivas

des = matrix(data = NA, nrow = 9, ncol = 5)
colnames(des) <- names(r)
rownames(des) <- c("Mediana","Media","Desv. Estd.","Sharpe Ratio","Asimetria","Curtosis",
                   "Mínimo","Máximo","Rango")

des[1,] = round(sapply(r,median),4)
des[2,] = round(sapply(r,mean),4)
des[3,] = round(sapply(r,sd),4)
des[4,] = round(sapply(r,mean)/sapply(r,sd),4)
des[5,] = round(sapply(r,skewness),4)
des[6,] = round(sapply(r,kurtosis),4)
des[7,] = round(sapply(r,min),4)
des[8,] = round(sapply(r,max),4)
des[9,] = round(sapply(r,max) - sapply(r,min),4)

# Exportar tablas de resumen
write.table(des, file = "des.cvs", sep = ",")