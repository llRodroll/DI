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

################################################

## 2. IID

################################################

# Ventanas móviles de longitud fija 

n1 = nrow(r)                         #-- Number of observations
sm = c(60,250,500,1000)              #-- sub samples sizes
n_sm = length(sm)                    #-- Number of sub sample sizes

### 2.1. LB

# La idea es realizar Ljung-Box hasta encontrar el rezago M cuyo p-value<0.05
# porque la LB tiene como H0 que la correlación es cero conjuntamente hasta el rezago M
# Ho = independencia


lb = array(data = NA, dim = c(n1,n_sm,ncp))
dimnames(lb)[[2]] = c("S-60","S-250","S-500","S-1000")
dimnames(lb)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")

for (k in 1:ncp) {
  for (j in 1:n_sm) {
    for(i in sm[j]:(n1-1)){
      tmp = (i-sm[j]+1)
      for (z in 1:30) {
        a = Box.test(r[tmp:i,k], lag = z, type = "Ljung-Box")
        if (a$p.value<0.05) {
          lb[i,j,k] = z
          break
        } else {
          lb[i,j,k] = 0
        }
      }
    }
  }  
}

## Array para resumir resultados de todos los indicies lb

tlb = array(data = NA, dim = c(n_sm,31,ncp))
dimnames(tlb)[[1]] = c("60","250","500","1000")
dimnames(tlb)[[2]] = seq(0,30)  # rezagos la variable z del loop anterior
dimnames(tlb)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")

for (k in 1:ncp) {
  for (j in 0:30) {
    for (i in 1:n_sm) {
      
      tlb[i,(j+1),k] = sum(lb[,i,k]==j, na.rm = TRUE)
      
    }
  }
}

# Exportar tablas de resumen
write.table(tlb, file = "tlb.cvs", sep = ",")

### 2.2 ADF

#-- ADF: el max rezago se calcula de acuerdo a [12*(T/100)^(1/4)] donde [] es la parte entera del número
#-- se realiza un pequeño "algoritmo" donde se mira si el útlimo rezago tiene un abs(t-stat) > 1.6, si no
#-- se reduce en 1 la cantidad de rezagos en la prueba
#-- finalmente si no encuentra ningún rezago significativo se deja por default lag = 1

#--- adfp1 sin constante y sin tendencia

# Matriz adfp1: i = filas (fechas), j = submuestras, k = indices (S&P500, COLCAP, IPyC, IPSA y SPBVL)
adfp1 = array(data = NA, dim = c(n1,n_sm,ncp))
dimnames(adfp1)[[2]] = c("S-60","S-250","S-500","S-1000")
dimnames(adfp1)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")

for (k in 1:ncp) {
  for (j in 1:n_sm) {
    for(i in sm[j]:(n1-1)) {
      
      tmp = (i-sm[j]+1)
      tl = trunc(12*((sm[j]/100)^0.25))
      
      for(z in 0:(tl-1)) {
        
        a = adfTest(r[tmp:i,k], lags = (tl - z), type = "nc" )
        tts = summary(a@test$lm)[["coefficients"]][, "t value"]
        if (abs(tts[length(tts)]) > 1.6) {
          adfp1[i,j,k] = a@test$p.value
          break
          
        }
        if ((tl-z)==1){
          adfp1[i,j,k] = a@test$p.value
        }
      }
    }
  }
}

## Array para resumir resultados de todos los indicies adf-nc

tadfnc = array(data = NA, dim = c(n_sm,2,ncp))
dimnames(tadfnc)[[1]] = c("60","250","500","1000")
dimnames(tadfnc)[[2]] = c("Rechazo","No Rechazo")
dimnames(tadfnc)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")

for (k in 1:ncp) {
  for (i in 1:n_sm) {
    
    tadfnc[i,1,k] = sum(adfp1[,i,k]<0.05, na.rm = TRUE)
    tadfnc[i,2,k] = sum(adfp1[,i,k]>=0.05, na.rm = TRUE)
    
  }
}

#--- adfp2 con constante y sin tendencia

# Matriz adfp2: i = filas (fechas), j = submuestras, k = indices (S&P500, COLCAP, IPyC, IPSA y SPBVL)
adfp2 = array(data = NA, dim = c(n1,n_sm,ncp))
dimnames(adfp2)[[2]] = c("S-60","S-250","S-500","S-1000")
dimnames(adfp2)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")

for (k in 1:ncp) {
  for (j in 1:n_sm) {
    for(i in sm[j]:(n1-1)) {
      
      tmp = (i-sm[j]+1)
      tl = trunc(12*((sm[j]/100)^0.25))
      
      for(z in 0:(tl-1)) {
        
        a = adfTest(r[tmp:i,k], lags = (tl - z), type = "c" )
        tts = summary(a@test$lm)[["coefficients"]][, "t value"]
        if (abs(tts[length(tts)]) > 1.6) {
          adfp2[i,j,k] = a@test$p.value
          break
          
        }
        if ((tl-z)==1){
          adfp2[i,j,k] = a@test$p.value
        }
      }
    }
  }
}

## Array para resumir resultados de todos los indicies adf-c

tadfc = array(data = NA, dim = c(n_sm,2,ncp))
dimnames(tadfc)[[1]] = c("60","250","500","1000")
dimnames(tadfc)[[2]] = c("Rechazo","No Rechazo")
dimnames(tadfc)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")

for (k in 1:ncp) {
  for (i in 1:n_sm) {
    
    tadfc[i,1,k] = sum(adfp2[,i,k]<0.05, na.rm = TRUE)
    tadfc[i,2,k] = sum(adfp2[,i,k]>=0.05, na.rm = TRUE)
    
  }
}

#--- adfp3 con constante y con tendencia
# Matriz adfp3: i = filas (fechas), j = submuestras, k = indices (S&P500, COLCAP, IPyC, IPSA y SPBVL)
adfp3 = array(data = NA, dim = c(n1,n_sm,ncp))
dimnames(adfp3)[[2]] = c("S-60","S-250","S-500","S-1000")
dimnames(adfp3)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")

for (k in 1:ncp) {
  for (j in 1:n_sm) {
    for(i in sm[j]:(n1-1)) {
      
      tmp = (i-sm[j]+1)
      tl = trunc(12*((sm[j]/100)^0.25))
      
      for(z in 0:(tl-1)) {
        
        a = adfTest(r[tmp:i,k], lags = (tl - z), type = "ct" )
        tts = summary(a@test$lm)[["coefficients"]][, "t value"]
        if (abs(tts[length(tts)]) > 1.6) {
          adfp3[i,j,k] = a@test$p.value
          break
          
        }
        if ((tl-z)==1){
          adfp3[i,j,k] = a@test$p.value
        }
      }
    }
  }
}

## Array para resumir resultados de todos los indicies adf-tc

tadftc = array(data = NA, dim = c(n_sm,2,ncp))
dimnames(tadftc)[[1]] = c("60","250","500","1000")
dimnames(tadftc)[[2]] = c("Rechazo","No Rechazo")
dimnames(tadftc)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")

for (k in 1:ncp) {
  for (i in 1:n_sm) {
    
    tadftc[i,1,k] = sum(adfp3[,i,k]<0.05, na.rm = TRUE)
    tadftc[i,2,k] = sum(adfp3[,i,k]>=0.05, na.rm = TRUE)
    
  }
}

# Exportar tablas de resumen
write.table(tadfnc, file = "tadfnc.cvs", sep = ",")
getwd()# direccion donde quedó el archivo 
write.table(tadfc, file = "tadfc.cvs", sep = ",")
write.table(tadftc, file = "tadftc.cvs", sep = ",")

### 2.3. Estimación de Momentos

# Matriz 4-D: i = filas (fechas), j = submuestras, 
# k = indices (S&P500, COLCAP, IPyC, IPSA y SPBVL), z = estadistica (media, varianza, asimetira, curtosis)
m4d = array(data = NA, dim = c(n1,n_sm,ncp,4))
dimnames(m4d)[[2]] = c("S-60","S-250","S-500","S-1000")
dimnames(m4d)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")
dimnames(m4d)[[4]] = c("Media","Varianza","Asimetria","Curtosis")

for (k in 1:ncp) {
  for (j in 1:n_sm) {
    for(i in sm[j]:n1){
      m4d[i,j,k,1] = mean(r[(i-sm[j]+1):i,k])
      m4d[i,j,k,2] = var(r[(i-sm[j]+1):i,k])
      m4d[i,j,k,3] = skewness(r[(i-sm[j]+1):i,k])
      m4d[i,j,k,4] = kurtosis(r[(i-sm[j]+1):i,k]) #-- curtosis
    }
  }
}

### Gráfico

### 2x1 - SOLO S&P 500

windows() ## create window to plot your file
layout(matrix(c(1,2), ncol=2, byrow=TRUE), heights=c(4,1))
par(mai=c(0.9,0.9,0.5,0.2))

matplot(m4d[,,1,2], m4d[,,1,1],pch=20, ylab = 'Media', xlab = 'Varianza')
abline(h=0,col="black",lty=2)
abline(v=1,col="black",lty=2)

title("S&P500", outer = TRUE, line = -1)

matplot(m4d[,,1,3], m4d[,,1,4],pch=20, ylab = 'Curtosis', xlab = 'Asimetria')
abline(h=3,col="black",lty=2)
abline(v=0,col="black",lty=2)


### 4 indices MILA en una sola ventana

windows() ## create window to plot your file
layout(matrix(c(1,1,2,2,3,4,5,6,7,7,8,8,9,10,11,12,13,13,13,13), ncol=4, byrow=TRUE), heights=c(0.5,3,0.5,3,1))

par(mai=c(0,0,0.4,0))
plot.new()
title(main = "COLCAP")

par(mai=c(0,0,0.4,0))
plot.new()
title(main = "IPyC")

#COLCAP
par(mai=c(0.6,0.6,0,0))
matplot(m4d[,,2,2], m4d[,,2,1],pch=20, ylab = 'Media', xlab = 'Varianza')
abline(h=0,col="black",lty=2)
abline(v=1,col="black",lty=2)

par(mai=c(0.6,0.6,0,0))
matplot(m4d[,,2,3], m4d[,,2,4],pch=20, ylab = 'Curtosis', xlab = 'Asimetria')
abline(h=3,col="black",lty=2)
abline(v=0,col="black",lty=2)

#IPyC
par(mai=c(0.6,0.6,0,0))
matplot(m4d[,,3,2], m4d[,,3,1],pch=20, ylab = 'Media', xlab = 'Varianza')
abline(h=0,col="black",lty=2)
abline(v=1,col="black",lty=2)

par(mai=c(0.6,0.6,0,0.2))
matplot(m4d[,,3,3], m4d[,,3,4],pch=20, ylab = 'Curtosis', xlab = 'Asimetria')
abline(h=3,col="black",lty=2)
abline(v=0,col="black",lty=2)

par(mai=c(0,0,0.4,0))
plot.new()
title(main = "IPSA")

par(mai=c(0,0,0.4,0))
plot.new()
title(main = "SPBVL")

#IPSA
par(mai=c(0.6,0.6,0,0))
matplot(m4d[,,4,2], m4d[,,4,1],pch=20, ylab = 'Media', xlab = 'Varianza')
abline(h=0,col="black",lty=2)
abline(v=1,col="black",lty=2)

par(mai=c(0.6,0.6,0,0))
matplot(m4d[,,4,3], m4d[,,4,4],pch=20, ylab = 'Curtosis', xlab = 'Asimetria')
abline(h=3,col="black",lty=2)
abline(v=0,col="black",lty=2)

#SPBVL
par(mai=c(0.6,0.6,0,0))
matplot(m4d[,,5,2], m4d[,,5,1],pch=20, ylab = 'Media', xlab = 'Varianza')
abline(h=0,col="black",lty=2)
abline(v=1,col="black",lty=2)

par(mai=c(0.6,0.6,0,0.2))
matplot(m4d[,,5,3], m4d[,,5,4],pch=20, ylab = 'Curtosis', xlab = 'Asimetria')
abline(h=3,col="black",lty=2)
abline(v=0,col="black",lty=2)


par(mai=c(0,0,0,0))
plot.new()
legend(x="center", ncol=4,legend=c("60","250","500","1000"),pch=20,
       col = c("black","red","green","blue"), cex = 2, pt.cex = 5)

### 2.4. Normalidad - Jarque Bera

dn2 = array(data = NA, dim = c(n1,n_sm,ncp))
dimnames(dn2)[[2]] = c("S-60","S-250","S-500","S-1000")
dimnames(dn2)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")

for (k in 1:ncp) {
  for (j in 1:n_sm) {
    for(i in sm[j]:n1) {
      
      tmp = (i-sm[j]+1)
      f = jarque.bera.test(r[tmp:i,k])
      dn2[i,j,k] = f$p.value ## me da el nombre de la distribución con el menor aic
      
    }
  }
}

## Array para resumir resultados de todos los indicies jarque - bera

tdn2 = array(data = NA, dim = c(n_sm,2,ncp))
dimnames(tdn2)[[1]] = c("60","250","500","1000")
dimnames(tdn2)[[2]] = c("Rechazo","No Rechazo")
dimnames(tdn2)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")

for (k in 1:ncp) {
  for (i in 1:n_sm) {
    
    tdn2[i,1,k] = sum(dn2[,i,k]<0.05, na.rm = TRUE)
    tdn2[i,2,k] = sum(dn2[,i,k]>=0.05, na.rm = TRUE)
    
  }
}

# Exportar tablas de resumen
write.table(tdn2, file = "tdn2.cvs", sep = ",")

### 2.5. Otras distribuciones

dn = array(data = NA, dim = c(n1,n_sm,ncp))
dimnames(dn)[[2]] = c("S-60","S-250","S-500","S-1000")
dimnames(dn)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")

for (k in 1:ncp) {
  for (j in 1:n_sm) {
    for(i in sm[j]:n1) {
      
      tmp = (i-sm[j]+1)
      h   = r[tmp:i,k]
      fit_n  = fitdist(h, "norm")
      fit_t  = fitdist(h, "t", start = list(df = 10)) # los gl son iguales al tamaño de la muestra-1
      fit_c  = fitdist(h, "cauchy")
      fit_l  = fitdist(h, "laplace", start = list(mu = 0, sigma = 1))
      
      s = gofstat(list(fit_n, fit_t, fit_c, fit_l), 
                  fitnames = c("norm","t","cauchy","laplace"))
      
      dn[i,j,k] = names(sort(s$aic))[1] ## me da el nombre de la distribución con el menor aic
      
    }
  }
}

## Array para resumir resultados de todos los indicies dn

tdn = array(data = NA, dim = c(n_sm,4,ncp))
dimnames(tdn)[[1]] = c("60","250","500","1000")
dimnames(tdn)[[2]] = c("norm","t","cauchy","laplace")  
dimnames(tdn)[[3]] = c("S&P500","COLCAP","IPyC","IPSA","SPBVL")

for (k in 1:ncp) {
  for (i in 1:n_sm) {
    
    tdn[i,1,k] = sum(dn[,i,k]=="norm", na.rm = TRUE)
    tdn[i,2,k] = sum(dn[,i,k]=="t", na.rm = TRUE)
    tdn[i,3,k] = sum(dn[,i,k]=="cauchy", na.rm = TRUE)
    tdn[i,4,k] = sum(dn[,i,k]=="laplace", na.rm = TRUE)
    
  }
}

# Exportar tablas de resumen
write.table(tdn, file = "tdn.cvs", sep = ",")

