# Programmer: Giovani Carrara Rodrigues

# date: ago/23/2015

# description: Creating a fake time-series
	

rm(list=ls(TRUE))

set.seed(2015)

N <- 100 # quantidade de valores gerados

vari <- 1 # atribuindo variância do erro aleatório "a"
aver <- 0 # atribuindo média do erro aleatório "a"


# declarando vetores de cada componente da série temporal

z <- rep(0,N) # componente estocástica
a <- rep(0,N) # erro aletório gerado por uma dist. normal
tre <- rep(0,N) # tendencia 
x <- rep(0,N) # valor fake da série 


trend <- function(j){
# Coeficientes tendencia dada pelo polinômio trend = alpha0 + alpha1*t + alpha2*X^t
	alpha0 <- 1
	alpha1 <- 0.5
	alpha2 <- 0.8

	aux <- alpha0 + alpha1*j + alpha2*(j^2)
	return(aux)
}

#trend[1] <- alpha0 + alpha1*1 + alpha2*(1^2)

# atribuindo valor à primeira posição de x

a[1] <- rnorm(1,0,1)

z[1] <- 0

tre[1] <- trend(1)

x[1] <- trend(1) + z[1]

#Gerando a série temporal

for(i in 2:N){
	a[i] <- rnorm(1,aver,vari) 
	z[i] <- 0.5*z[i-1] + a[i]
	tre[i] <- trend(i)
	x[i] <- tre[i] + z[i]
	cat(i,"\n")	
}

ts.plot(x) # plotando o gráfico da série temporal

# Salvando o gráfico da série temporal em pdf
pdf(paste("time-series","mean=",as.character(aver),"var=",as.character(vari),"N=",as.character(N),".pdf"))
ts.plot(x)
dev.off()

 




