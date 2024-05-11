##Analítica Financiera. Tutorial Momentos Estadísticos Series Financiaras
##Instalaciones necesarias
library(fBasics)
library(PerformanceAnalytics)
library(xts)
library(quantmod)
library(ggplot2)
library(tseries)
library(dygraphs)
options(warn = - 1) 

##--------Obtención Datos (Recordar primer tutorial que empleamos una función muy similar):
start<-format(as.Date("2005-01-01"),"%Y-%m-%d")
end<-format(as.Date("2021-06-30"),"%Y-%m-%d")

#--------- Función para bajar precios y generar rendimientos:
rend<-function(simbolo,start,end) {
  ##---------Obtener precios de yahoo finance:
  datos<-getSymbols(simbolo, src = "yahoo", auto.assign = FALSE)
  ##---------eliminar datos faltantes:
  datos<-na.omit(datos)
  ##--------Mantener el precio de interés:
  datos<-datos[,4]
  ##--------Rendimientos simples:
  rend<-CalculateReturns(datos, method = 'log')
  #--------Para hacer dtos accesibles  GLobal ENv:
  assign(simbolo, rend, envir = .GlobalEnv)
}

#--------Llamar la función para cada activo particular:
rend("AMZN", start, end)
str(AMZ)

rend("IBM", start, end)
str(IBM)
## Gráfico:
rends<-merge.xts(AAPL, IBM)
colnames(rends)<-c("AAPL", "IBM")
dygraph(rends, main = "APPL & IBM Rendimientos") %>%
  dyAxis("y", label = "Rend %") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"))

###########################
#----------Estadisticas básicas 
basicStats(AAPL) ## Resumen estadísticos
mean(AAPL)
var(AAPL)
stdev(AAPL) # Desv Std
t.test(AAPL)  # Prueba que H0: mean return = 0
s3=skewness(AAPL)  #Sesgo
T=length(AAPL) # tamaño muestra
t3=s3/sqrt(6/T) # Prueba de sesgo
t3
pp=2*pt(abs(t3), T-1, lower=FALSE) # Calcula p-valor, si p valor > alfa, no se rechaza nula y por tanto sesgo de cero 
pp
s4=kurtosis(AAPL)
s4
t4=s4/sqrt(24/T) # Prueba de curtosis, en exceso
t4
pv=2*(1-pnorm(t4)) # p-valor,  si p valor > alfa, no se rechaza nula y por tanto exceso de curtosis de cero 
pv
normalTest(AAPL,method='jb') # Prueba Jaque Bera, H0: Normal


###################################################################################################
##---------- Segundo Activo
###Estadisticas básicas 
basicStats(IBM) ## Resumen estadísticos
mean(IBM)
var(IBM)
stdev(IBM) # Desv Std
t.test(IBM)  # Prueba que H0: mean return = 0
s3=skewness(IBM)  #Sesgo
T=length(IBM) # tamaño muestra
t3=s3/sqrt(6/T) # Prueba de sesgo
t3
pp=2*(1-pnorm(abs(t3))) 
pp
s4=kurtosis(IBM)
s4
t4=s4/sqrt(24/T) 
t4
pv=2*(1-pnorm(t4)) 
pv
normalTest(IBM,method='jb') 

##----------Gráfica Densidad ambos activos

library(PerformanceAnalytics)
par(mfrow=c(1,2))
chart.Histogram(AAPL, methods = c("add.normal", "add.density"), colorset = c("gray", "blue", "red"))
legend("topright", legend = c("Hist-APPLE" ,"APPLE dist","dnorm APPLE"), col=c("gray", "blue", "red"), lty=1, cex = 0.7)
chart.Histogram(IBM, methods = c("add.normal", "add.density"), colorset = c("gray", "blue", "red"))
legend("topright", legend = c("Hist-IBM" ,"IBM dist","dnorm IBM"), col=c("gray", "blue", "red"), lty=1, cex = 0.7)


