#Analítica Financiera
library(PerformanceAnalytics)
library(forecast)
library(fable)
library(quantmod)
library(TSstudio)
library(TSA)
library(dygraphs)
library(fpp2)
library(rugarch)
library(xts)
library(fUnitRoots)
library(ggplot2)
library(tseries)
library(lmtest)
library(TSA)
library(Metrics)
library(FitAR)
options(warn = - 1)

#Parte 1: A partir de las fechas del 1ro Julio 2019 al 30 julio 2021, obtén los precios diarios de cierre del siguiente activo: Amazon
# A partir de estas fechas y nombre del activo, consulta su símbolo en la base de datos de yahoo finance.
# Descarga su serie de precios de cierre mediante la función GetSymbols(), seleccionando la columna correspondiente de precios de cierre. Determina sus rendimientos logarítmicos.

##--------Obtención Datos (Recordar primer tutorial que empleamos una función muy similar):
start<-format(as.Date("2019-07-01"),"%Y-%m-%d")
end<-format(as.Date("2021-07-30"),"%Y-%m-%d")

#--------- Función para bajar precios y generar rendimientos:
rend<-function(simbolo,start,end) {
  ##---------Obtener precios de yahoo finance:
  datos<-getSymbols(simbolo, src = "yahoo", auto.assign = FALSE)
  ##---------eliminar datos faltantes:
  datos<-na.omit(datos)
  ##--------Mantener el precio de interés:
  datos<-datos[,4]
  ##--------Rendimientos simples:
  rend<-periodReturn(datos, period="daily", subset=paste(c(start, end), "::", sep=""), type='arithmetic')
  #--------Para hacer dtos accesibles  GLobal ENv:
  assign(simbolo, rend, envir = .GlobalEnv)
}

#--------Llamar la función para cada activo particular:
rend("AMZN", start, end)
str(AMZN)

###########################
#----------Estadisticas básicas 
basicStats(AMZN) ## Resumen estadísticos
mean(AMZN)
var(AMZN)
stdev(AMZN) # Desv Std
t.test(AMZN)  # Prueba que H0: mean return = 0
s3=skewness(AMZN)  #Sesgo
T=length(AMZN) # tamaño muestra
t3=s3/sqrt(6/T) # Prueba de sesgo
t3
pp=2*pt(abs(t3), T-1, lower=FALSE) # Calcula p-valor, si p valor > alfa, no se rechaza nula y por tanto sesgo de cero 
pp
s4=kurtosis(AMZN)
s4
t4=s4/sqrt(24/T) # Prueba de curtosis, en exceso
t4
pv=2*(1-pnorm(t4)) # p-valor,  si p valor > alfa, no se rechaza nula y por tanto exceso de curtosis de cero 
pv
normalTest(AMZN,method='jb') # Prueba Jaque Bera, H0: Normal

##Partimos serie, tomemos el 7% para la prueba
h <- round(length(AMZN)*0.02, digits = 0 )
h
train <- AMZN[1:(nrow(AMZN) - h), ]
test<- AMZN[(nrow(AMZN) - h + 1):nrow(AMZN), ]


#-------Modelo de Media de rendimientos, mediante EACF y auto.arima:
library(TSA) 
amzn<-as.data.frame(AMZN)
eacf=eacf(amzn$AMZN,10,10)      # Sería un arma(1,4)
auto.arima(amzn)               # Sería un arma(0,2) con media diferente de cero
#------ Desarollo modelos:
m1=arima(amzn,order=c(1,0,4))
m1
tsdiag(m1)
m2=arima(amzn, order=c(0,0,1))
m2
tsdiag(m2)      #Modelos similares en sus métricas AIC, tomemos el segundo por su simplicidad.
