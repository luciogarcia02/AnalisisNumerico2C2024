
set.seed(123)

integracion <- function(funcion, limiteSuperior, limiteInferior, n){
  uniforme <- limiteInferior + (limiteSuperior - limiteInferior) * runif(n = n)
  
  alturaPromedio <- 1/n * sum(funcion(uniforme))
  
  anchoBase <- limiteSuperior - limiteInferior
  
  desvioEstandar <- sqrt( 1/(n-1) * sum((funcion(uniforme) * (limiteSuperior - limiteInferior) - alturaPromedio * anchoBase)^2) )
  
  error <- desvioEstandar/sqrt(n)
  
  resultados <- list("error" = error, "alfa" =  alturaPromedio * anchoBase)
  return(resultados)
}

funcion<-function(x){
  return (sqrt(x+5)*sin(x))
}
result1a<-integracion(funcion,6,2,10000) 

##lo que vamos a sombrear
x <- seq(from = 2, to = 6, by = 0.01)
fx <- funcion(x)
##el margen extra
x1 <- seq(from = 0, to = 8, by = 0.01)
fx1 <- funcion(x1)
ggplot() +
  geom_area(aes(x = x, y = fx )) +
  geom_line(aes(x = x1, y = fx1))
library(ggplot2)








N <- 10000
resultado <- matrix(NA, nrow = N, ncol = 2)
set.seed(123)

for (i in 1:N) {
  # Genera un número aleatorio con distribucion binomial
  n <- rbinom(n = 1, size = 1200, prob = 0.7984)
  
  resultado[i, 1] <- n
  
  xi <- rchisq(df = 2, n = n)
  
  suma <- sum(xi)
  
  resultado[i,2] <- suma
}

n_esperanza <- mean(resultado[,1])
n_varianza <- var(resultado[,1])

suma_esperanza <- mean(resultado[,2])
suma_varianza <- var(resultado[,2])

## movimiento geometrico browniano

p0 <- 45 
mu <- 0.1
sigma <- 0.2
t <- 0.5
n <- 182
dt <- t / n # tamaño del time step

m <-1000
Pt <- matrix(NA, nrow = m, ncol = n + 1)
Pt[, 1] <- p0

for (i in 1:m) { # cuenta el numero de simulacion
  for (j in 2:(n + 1)) {
    Pt[i, j] <- Pt[i, j - 1] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(1)) # rnorm(1) genera un numero aleatorio (es el epsilon de la formula)
  }
}
mean(Pt[,182])


vec<-rep(0:n,m)
matr<-matrix(vec,nrow= m, ncol=n+1,byrow=TRUE)

plot(matr[1,],Pt[1,],type="l", ylim=c(min(Pt),max(Pt)))
for(i in 2:m){
  lines(matr[i,],Pt[i,], col=trunc(runif(1)*m))
}

matrizdemedias<-matrix(NA,nrow=1,ncol=n+1)
for(i in 2:n+1){
  matrizdemedias[1,i]<-mean(Pt[,i])
}

lines(matr[1,],matrizdemedias[1,], col="black",lwd = 5)


prob<-0.95 ## esto generaria un intervalo al 90%
ls<-matrix(NA,nrow=1,ncol=n+1)
for(i in 1:(n+1)){
  ls[i]<-quantile(Pt[,i],prob)
}
li<-matrix(NA,nrow=1,ncol=n+1)
for(i in 1:(n+1)){
  li[i]<-quantile(Pt[,i],1-prob)
}
lines(matr[1,],ls[1,], col="red",lwd = 3)
lines(matr[1,],li[1,], col="red",lwd = 3)


m=1000
e<-rnorm(m)
PT<-matrix(NA,nrow=m,ncol=1)
PT<-p0 * exp((mu - 0.5 * sigma^2) * 0.5 + sigma * sqrt(dt) * e)
hist(PT)

##simulacion de precios correlacionados
### e=z*CH ~N(0;Rho)
### arrancamos con una matriz de correlacion dada

library(quantmod)
library(dplyr)
set.seed(123)


# Simulacion
m <- 10000

# Matriz de camino de precios
pt <- matrix(NA, nrow = m, ncol = 2)

mu <- c(0.15, 0.12)
sigma <- c(0.2, 0.19)
t <- 1
n <- 365
dt <- t / n # tamaño del time step

p0 <- rep(NA, times = 2)

getSymbols("YPFD.BA", auto.assign = TRUE, src = "yahoo")

aux <- data.frame(YPFD.BA)
rm(YPFD.BA)

p0[1] <- aux['2020-11-06',]$YPFD.BA.Adjusted
rm(aux)

getSymbols("MELI.BA", auto.assign = TRUE, src = "yahoo")

aux <- data.frame(MELI.BA)
rm(MELI.BA)

p0[2] <- aux['2020-11-06',]$MELI.BA.Adjusted
rm(aux)

m=10000
e<-rnorm(m)
PT<-matrix(NA,nrow=m,ncol=2)
for(i in 1:2){
 PT[,i]<-p0[i] * exp((mu[i]  - 0.5 * sigma[i]^2) * 0.5 + sigma[i] * sqrt(dt) * e)  
}

hist(PT[,1])##histograma de ypf
hist(PT[,2])##histograma de meli

mean(PT[,1])
mean(PT[,2])


prob<-0.975 ## esto generaria un intervalo al 95%
limites_del_IC_YPF<-quantile(PT[,1],probs=c(prob, 1-prob))
limites_del_IC_MELI<-quantile(PT[,2],probs=c(prob, 1-prob))

rendimientoYPF<-log(mean(PT[,1])/p0[1])
rendimientoMELI<-log(mean(PT[,2])/p0[2])

##segunda parte del ejercicio

PT <- matrix(NA, nrow = m, ncol = 3)

mu <- c(0.15, 0.12,0.3)
sigma <- c(0.2, 0.19,0.42)
t <- 1
n <- 365
dt <- t / n # tamaño del time step

p0 <- rep(NA, times = 3)

getSymbols("YPFD.BA", auto.assign = TRUE, src = "yahoo")

aux <- data.frame(YPFD.BA)
rm(YPFD.BA)

p0[1] <- aux['2020-11-06',]$YPFD.BA.Adjusted
rm(aux)

getSymbols("MELI.BA", auto.assign = TRUE, src = "yahoo")

aux <- data.frame(MELI.BA)
rm(MELI.BA)

p0[2] <- aux['2020-11-06',]$MELI.BA.Adjusted
rm(aux)

p0[3]<-54 ##es dato

# Matriz de covarianzas
rho <- diag(3)
rho[1,2] <- rho[2,1] <- 0.9
rho[1,3] <- rho[3,1] <- 0.7
rho[2,3] <- rho[3,2] <- 0.6

# Matriz de cholesky
ch <- chol(rho)

set.seed(123)
z <- matrix(rnorm(3*m),nrow=m,ncol=3)

e <- z %*% ch

pt_cor <- matrix(NA, nrow = m, ncol = 3)

for (i in 1:m) {
  for (k in 1:3) {
    pt_cor[i,k] <- p0[k] * exp((mu[k] - 0.5 * sigma[k]^2) * t + sigma[k] * sqrt(t) * e[i,k]) 
  }
}

hist(pt_cor[,1])##histograma de ypf
hist(pt_cor[,2])##histograma de meli
hist(pt_cor[,3])

mean(pt_cor[,1])
mean(pt_cor[,2])
mean(pt_cor[,3])

cartera_p0<-c(200*p0[1],120*p0[2])
valor_esperado_de_la_cartera_independiente<-200*mean(PT[,1])+120*mean(PT[,1])
valor_esperado_de_la_cartera_correlacionada<-200*mean(pt_cor[,1])+120*mean(pt_cor[,1])

rendimiento_log_indep<-log(valor_esperado_de_la_cartera_independiente/cartera_p0)
rendimiento_log_cor<-log(valor_esperado_de_la_cartera_correlacionada/cartera_p0)