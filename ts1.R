# Programmer: Giovani Carrara Rodrigues

# date: ago/24/2015

# description: Creating a fake time-series and a moving average filter
	
# Inicio do exercício a) Creating a fake time-series
rm(list=ls(TRUE))

set.seed(2015)

N <- 100 # quantidade de valores gerados

vari <- 1 # atribuindo variância do erro aleatório "a"
aver <- 0 # atribuindo média do erro aleatório "a"


# declarando vetores de cada componente da série temporal

z   <- rep(0,N) # componente estocástica
a   <- rep(0,N) # erro aletório gerado por uma dist. normal
tre <- rep(0,N) # tendencia 
x   <- rep(0,N) # valor fake da série 


trend <- function(j){
# Coeficientes tendencia dada pelo polinômio trend = alpha0 + alpha1*t + alpha2*(t^2)
	alpha0 <- 1
	alpha1 <- 0.5
	alpha2 <- 0.8

	aux <- alpha0 + alpha1*j + alpha2*(j^2)
	return(aux)
}

#trend[1] <- alpha0 + alpha1*1 + alpha2*(1^2)

# atribuindo valor à primeira posição de x

a[1]   <- rnorm(1,0,1)

z[1]   <- 0

tre[1] <- trend(1)

x[1]   <- trend(1) + z[1]

#Gerando a série temporal

for(i in 2:N){
	a[i]   <- rnorm(1,aver,vari) 
	z[i]   <- 0.5*z[i-1] + a[i]
	tre[i] <- trend(i)
	x[i]   <- tre[i] + z[i]
	cat(i,"\n")	
}

ts.plot(x,main="Time-series Bjundas") # plotando o gráfico da série temporal

# Salvando o gráfico da série temporal em pdf
pdf(paste("time-series","mean=",as.character(aver),"var=",as.character(vari),"N=",as.character(N),".pdf"))
ts.plot(x,main="Time-series Bjundas")
dev.off()

#final exercício a)

# Início exercício b) moving average filter

# função para calcular as médias móveis, onde t é a entrada da série e j é índice "k" das médias móveis
maf <- function(t,j) {
	size <- length(t)-2*j # tamanho da nova série
	p <-j # atribuindo para nao dar problema
	#contr <- 2*j # talves eu use
	aux <- rep(0,size) # criando vetor da série filtrada
	for(k in 1:size){ # gerando a série filtrada
		o <- 1
		aux[k] <- t[k+p] # atribuindo valor central
		while(o <= j){
			aux[k] <- aux[k]+t[k+p-o] + t[k+p+o] #atribuindo valores com mesma distância
			o <- o+1
		}
		aux[k] <- aux[k]/(2*j+1) # dividindo pela quantidade de valores na média móvel
	}
	return(aux) # retornando a série filtrada
}

tam <- 2 # k das médias móveis

newseries <- maf(x,tam) # chamando a função que calcula as médias móveis

pdf(paste("time-series after moving average","mean=",as.character(aver),"var=",as.character(vari),"N=",as.character(N),".pdf"))
ts.plot(newseries,main="Time-series after moving average Bjundas")
dev.off()

# final exercício b)