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