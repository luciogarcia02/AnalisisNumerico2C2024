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






