# programmer: Giovani C R

# link da fonte:
# https://datamarket.com/data/set/2323/daily-maximum-temperatures-in-melbourne-australia-1981-1990#!ds=2323&display=line

rm(list=ls(all=TRUE))

library(dygraphs)
library(xts)
library(zoo)
library(forecast)


# funçãoque calcula suavização exponencial
exps <- function(t,a){
  if(a<0 || a>1) return("Para de ser burro vc errou o segundo parametro da função")
  size <- length(t) # tamanho do vetor de retorno
  aux <- rep(0,size) # criando o vetor de retorno
  aux[1] <- t[1] # atribuindo o primeiro passo
  for(i in 2:size){ # calculando a suavização exponencial
    aux[i] <- a*t[i] + (1-a)*t[i-1]
  }
  return(aux) # retornando o vetor com a suavização esponencial
}



#lendo os dados
data = read.csv("daily-maximum-temperatures-in-me.csv",sep=",",header=T)

# criando o objeto ts serie temporal
ts2 = ts(data[,2],start=c(1981,1),frequency = 365)

# decomposição sazonal
fit2 = stl(ts2,s.window = 365)

# atribuindo a tendencia
trend = fit2$time.series[,2]

# criando xts serie temporal
ts3 = as.xts(data[,2],order.by = as.Date(data[,1],"%Y-%m-%d"))

# serie temporal do primeiro ano 1981
xts81 = xts(data[1:365,2],order.by = as.Date(data[1:365,1],"%Y-%m-%d"))

# criando dygraph
dygraph(ts3) %>%
#   dySeries("mdeaths", label = "Male") %>%
#   dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = FALSE) %>%
  dyRangeSelector(height = 20)

dygraph(xts81) %>%
dyRangeSelector(height = 20)

# estimando a sazonalidade
aux = data
for(i in 1:365)
{
  for(k in 1:9)
  {
    aux[i,2] = aux[i,2] + aux[i+(k*365),2]
  }
}
aux[,2] = aux[,2]/10
sazo = rep(aux[1:365,2],10)

# criando objeto ts para sazonalidade
sazots = ts(sazo, start = c(1981,1),frequency = 365 )

# criando objeto xts para sazonalidade
sazoxts = xts(sazo, order.by = as.Date(data[,1],"%Y-%m-%d"))

# plotando sazonalidade (sazoxts)
dygraph(sazoxts)

# subtraindo a sazonalidade da série
aux2 = data[,2] - sazo

# criando objeto ts da série sem subtraida pela sazonalidade
tsnosazo = ts(aux2, start = c(1981,1), frequency = 365)

# criando objeto xts da série subtraida pela sazonalidade
xtsnosazo = xts(aux2, order.by = as.Date(data[,1],"%Y-%m-%d"))

# plotando a serie subtraida pela sazonalidade
dygraph(xtsnosazo)

# # suavização exponencial com alpha = 0.7
# suav = exps(data[,2],0.7)
# 
# suavxts = xts(suav, order.by = as.Date(data[,1], "%Y-%m-%d"))

# diferenciando a serie temporal d vezes para tentar estacionalizar a série
tsdif = diff(ts2)
acf(tsdif,lag.max = 20)
pacf(tsdif,lag.max = 20)

## Modelando ##
# carregando pacote forecast
N = 3650
p = 2
q = 2

# MA(2) = ARIMA(0,0,1)
ma2 = Arima(tsnosazo, order = c(0,0,2))
##  SERIE - SERIEAJUSTADA = RESIDUO => SERIEAJUSTADA = SERIE - RESIDUO ##

# AR(2) = ARIMA(2,0,0)
ar2 = Arima(tsnosazo, order = c(2,0,0))
# componente estocastica ajustada
fitar2 =tsnosazo - ar2$residuals
# componente estocastica mais comp. sazonal = serie ajustada
artsfit = sazots + fitar2
# residuo levando em consideração a série estimada
arres = ts2 - artsfit
# plot da série estimada
aux = as.data.frame(artsfit)
arxtsfit = xts(aux[,1], order.by = as.Date(data[,1],"%Y-%m-%d"))
dygraph(arxtsfit)

# (tsnosazo - fitar2) == ar2$residuals 

# ARMA(2,2) = ARIMA(2,0,2)
arma22 = Arima(tsnosazo, order = c(2,0,2))

# ARIMA(2,1,2)
arima212 = Arima(ts2, order = c(2,1,2))


tss = cbind(ts3, arxtsfit)
dygraph(tss, main = "Series de temperatura") %>%
  dyRangeSelector()

