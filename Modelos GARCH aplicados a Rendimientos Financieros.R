## Analítica Financiera. Ejemplo de modelo heterocedásticos en serie univariada.
 
library(rugarch)
library(forecast)
library(TSstudio)
library(quantmod)
library(TSA)
library(aTSA)
options(warn = - 1)

##--------Obtención Datos:
start<-format(as.Date("2017-01-01"),"%Y-%m-%d")
end<-format(as.Date("2021-06-30"),"%Y-%m-%d")

#--------- Parte I: Datos y Modelo de Media
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
rend("SPY", start, end)
str(SPY)

#--------Gráfico:
colnames(SPY)<-"SPY"
dygraph(SPY, main = "SPY Rendimientos") %>%
  dyAxis("y", label = "Rend %") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(1, "Set1"))

#-------Modelo de Media de rendimientos, mediante EACF y auto.arima:
library(TSA) 
spy<-as.data.frame(SPY)
eacf=eacf(spy$SPY,10,10)      # Sería un arma(1,4)
auto.arima(spy)               # Sería un arma(0,2) con media diferente de cero
#------ Desarollo modelos:
m1=arima(spy,order=c(1,0,4))
m1
tsdiag(m1)
m2=arima(spy, order=c(0,0,2))
m2
tsdiag(m2)      #Modelos similares en sus métricas AIC, tomemos el segundo por su simplicidad.

#-------Parte II. Probar Efecto heterocedástico 
#-------Probar efecto ARCH:
library(aTSA)
pacf(m2$residuals) 
##nivel de ARCH
arch.test(m2)

#--------Estadisticas básicas:
basicStats(spy)
s3=skewness(spy)  #Sesgo
T=length(spy) # tamaño muestra
t3=s3/sqrt(6/T) # Prueba de sesgo
t3
pp=2*(1-pnorm(abs(t3)))
pp

#---------Gráfico Densidad
par(mfrow=c(1,1))
library(PerformanceAnalytics)
chart.Histogram(SPY, methods = c("add.normal", "add.density"), colorset = c("gray", "blue", "red"))
legend("topright", legend = c("Hist-SPY" ,"SPY dist","dnorm SPY"), col=c("gray", "blue", "red"), lty=1)


# Contamos con una distribución de colas pesadas, pero con sesgo no significativo, podemos emplear una dist t (no sesgada).
# que requerirá de un parámetro más vs la normal: shape. A más bajo valor, más gordas las colas. Si hubiese sido sesgada,
# se emplea otro parámetro: epsilon de la dsitribución t-sesgada (sstd), si epsilon =1 es simétrico, si >1 sesgo positivo y viceversa.

#-------- Parte III: Modelamiento
#-------- Modelo de Volatilidad
#Con librería rugarch, pasos:
#1) ugarchspec(): Especifica que modelo de GARCH se desa emplear (media, varianza, distribución de innovaciones)
#2) ugarchfit(): Estima el modelo GARCH en la serie de rendimientos
#3) ugarchforecast():  Empleando el modelo estimado de GARCH, hace predicciones de volatilidad.

library(rugarch)
## Modelo 1
modelMean1=list(armaOrder = c(0, 2), include.mean = TRUE)
modelVar1=list(model = "sGARCH", garchOrder = c(1, 1))
modelGarch1=ugarchspec(variance.model=modelVar1,mean.model = modelMean1, distribution.model="std")
modelFit1=ugarchfit(spec=modelGarch1,data=SPY)
modelFit1
modelFit1@fit$coef
#plot(modelFit1,which="all")
#Media rendimientos:
mean1<-fitted(modelFit1)
plot(mean1)
#Volatilidades:
Vol1<-sigma(modelFit1)
plot(Vol1)

#Desv standar largo plazo:
uncvariance(modelFit1) #Vemos la varianza a largo plazo, 
sqrt(uncvariance(modelFit1))  #Su raíz, la desviación std establece que la desv std a largo plazo es del 4.9%

#--------- Pronóstico:
forc1<-ugarchforecast(fitORspec = modelFit1, n.ahead=10)
plot(forc1, which=1)
plot(forc1, which=3)

#---------- Simulación
###Valores de la media rendimientos futuros:
windows(width=10, height=8)
sim = ugarchsim(modelFit1, n.sim = 1000, m.sim = 25, startMethod="sample")  #n.sim: horizonte simulación. m.sim: num simulaciones.
plot(sim, which = 'all', cex=0.05)
##
