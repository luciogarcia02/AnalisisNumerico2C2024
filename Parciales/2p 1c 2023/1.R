##1)a)
###Cualquier función continua definida en un intervalo cerrado [a;b] puede aproximarse de manera uniforme por un polinomio con la precisión que se requiera.
#b)
x<-c(2.5,4.375,6.25,8.125,10.00,11.875,13.75,15.625,17.5) 
y<-c(0.0013,0.0122,0.0668,0.2266,0.50,0.7734,0.9332,0.9878,0.9987)

PolinomioInterpolanteNewton <- function(x, y){
  df <- DiferenciasDivididas(x = x, y = y)
  
  #Saco la primer columna del df
  df[,1] <- NULL
  
  n <- ncol(df)
  
  polinomio <- df[1,1]
  
  for (i in 2:n) {
    polinomio <- polinomio + glue::glue(" + ", df[i,i])
    for (j in 1:(i-1)) {
      polinomio <- polinomio + glue::glue(" * ( x - ", x[j], " )")
    }
  }
  return(polinomio)
}

polinomio<-PolinomioInterpolanteNewton(x,y)

fxPol<-function(x){
  return(0.0013 + 0.00581333333333333 * ( x - 2.5 ) + 0.00621511111111111 * ( x - 2.5 ) * ( x - 4.375 ) + 0.00155496296296296 * ( x - 2.5 ) * ( x - 4.375 ) * ( x - 6.25 ) + -0.00017901037037037 * ( x - 2.5 ) * ( x - 4.375 ) * ( x - 6.25 ) * ( x - 8.125 ) + -2.47760241426612e-05 * ( x - 2.5 ) * ( x - 4.375 ) * ( x - 6.25 ) * ( x - 8.125 ) * ( x - 10 ) + 6.10191002591068e-06 * ( x - 2.5 ) * ( x - 4.375 ) * ( x - 6.25 ) * ( x - 8.125 ) * ( x - 10 ) * ( x - 11.875 ) + -4.64907430545577e-07 * ( x - 2.5 ) * ( x - 4.375 ) * ( x - 6.25 ) * ( x - 8.125 ) * ( x - 10 ) * ( x - 11.875 ) * ( x - 13.75 ) + 2.5410988417629e-22 * ( x - 2.5 ) * ( x - 4.375 ) * ( x - 6.25 ) * ( x - 8.125 ) * ( x - 10 ) * ( x - 11.875 ) * ( x - 13.75 ) * ( x - 15.625 ))
}

fxPol(9.0)
fxPol(18.0)

TrazadorCubicoNatural = function(x,y){
  n = length(y)
  j = n - 1
  
  a = y
  b = c(rep(NA,n))
  c = c(rep(NA,n))
  d = c(rep(NA,n))
  
  A = c(rep(NA,n))
  h = c(rep(NA,n))
  l = c(rep(NA,n))
  u = c(rep(NA,n))
  z = c(rep(NA,n))
  
  #Paso 1
  for (i in 1:j) { 
    h[i] = x[i + 1] - x[i]
  }
  
  
  #Paso 2
  for (i in 1:j) { 
    if(i != 1){
      A[i] = (3 * (a[i + 1] - a[i])/(h[i])) - (3 * (a[i] - a[i - 1]) /h[i - 1])
    }
  }
  
  
  #Paso 3
  l[1] = 1
  u[1] = 0
  z[1] = 0
  
  #Paso 4
  for (i in 2:j) {
    l[i] = 2 * (x[i + 1] - x[i - 1]) - h[i - 1] * u[i - 1]
    u[i] = h[i]/l[i]
    z[i] = (A[i] - h[i - 1] * z[i - 1])/l[i]
  }
  
  #Paso 5
  l[n] = 1
  z[n] = 0
  c[n] = 0
  
  #Paso 6
  for (i in j:1) {
    c[i] = z[i] - u[i] * c[i + 1]
    b[i] = (a[i + 1] - a[i])/h[i] - h[i] * (c[i + 1] + 2*c[i])/3
    d[i] = (c[i + 1] - c[i])/(3*h[i])
  }
  
  #Paso 7
  results = matrix(rep(NA, 4*j), nrow = j, ncol = 4, byrow = FALSE)
  for (k in 1:j) {
    results[k, 1] = a[k]
    results[k, 2] = b[k]
    results[k, 3] = c[k]
    results[k, 4] = d[k]
  }
  
  #Construyo el polinomio
  polinomios <- rep(NA, times = nrow(results))
  for (i in 1:nrow(results)) {
    polinomios[i] <- glue::glue(results[i,1]) 
    for(j in 2:ncol(results)){
      polinomios[i] <- polinomios[i] + glue::glue(" + ", results[i,j], " * (x - ", x[i], ")^", (j-1)) 
    }
  }
  
  return(polinomios)
  
}
x<-c(2.5,17.5)
y<-c(0.0013,0.9987)
spline1<-TrazadorCubicoNatural(x,y)
spline1
fxspline_625_1375<-function(x){
  return(0.0668 + 0.0664933333333333 * (x - 6.25)^1 + 0.0196106666666667 * (x - 6.25)^2 + -0.00174317037037037 * (x - 6.25)^3)
}
##con solamente dos puntos estaríamos calculando una recta.
PolinomioInterpolanteNewton(x,y)
##la ecuacion que describe a la recta sería y = 0.0013 + 0.0664933333333333 * ( x - 2.5 )
poleval<-function(x){
  return(0.0013 + 0.0664933333333333 * ( x - 2.5 ))
}
poleval(9)##0.4335067
##lagrange en 9 = 0.3450514




##2)a)
polNewton<-PolinomioInterpolanteNewton(c(1,1.4,1.6),c(20.845,39.681,54.749))
spline2<-TrazadorCubicoNatural(c(1,1.4,1.6),c(20.845,39.681,54.749))
fxspline<-function(x){
  return(20.845 + 37.6733333333334 * (x - 1)^1 + 0 * (x - 1)^2 + 58.8541666666666 * (x - 1)^3)
}
fxspline(1.2)
fxpol<-function(x){
  return(20.845 + 47.09 * ( x - 1 ) + 47.0833333333333 * ( x - 1 ) * ( x - 1.4 ))
}
fxpol(1.2)


# Define las funciones
f1 <- function(x) {
  20.845 + 47.09 * (x - 1) + 47.0833333333333 * (x - 1) * (x - 1.4)
}

f2 <- function(x) {
  20.845 + 37.6733333333334 * (x - 1)^1 + 0 * (x - 1)^2 + 58.8541666666666 * (x - 1)^3
}

# Define el intervalo
x <- seq(0.9, 1.5, by = 0.01)

# Genera las y para cada función
y1 <- f1(x)
y2 <- f2(x)

# Grafica las funciones
plot(x, y1, type = "l", col = "blue", ylim = range(c(y1, y2)), ylab = "Y", xlab = "X", main = "Gráfico de Funciones")
lines(x, y2, col = "red")

# Añade leyenda
legend("topright", legend = c("f1", "f2"), col = c("blue", "red"), lty = 1)

##ejercicio 3

library(quantmod)
m=2000 #n de simulaciones
PT <- matrix(NA, nrow = m, ncol = 3)

mu <- c(0.11, 0.15,0.2)
sigma <- c(0.17, 0.2,0.24)
t <- 1.5
n <- 540
dt <- t / n # tamaño del time step

P0 <- rep(NA, times = 3)

getSymbols("KO", auto.assign = TRUE, src = "yahoo")

aux <- data.frame(KO)
rm(KO)

P0[1] <- aux['2023-06-05',]$KO.Adjusted
rm(aux)

getSymbols("PEP", auto.assign = TRUE, src = "yahoo")

aux <- data.frame(PEP)
rm(PEP)

P0[2] <- aux['2023-06-05',]$PEP.Adjusted
rm(aux)

getSymbols("JNJ", auto.assign = TRUE, src = "yahoo")

aux <- data.frame(JNJ)
rm(JNJ)

P0[3] <- aux['2023-06-05',]$JNJ.Adjusted
rm(aux)




p0 <- P0[2]
mu <- 0.15
sigma <- 0.2
t <- 1.5
n <- 540
dt <- t / n # tamaño del time step
m <-2000

Pt <- matrix(NA, nrow = m, ncol = n + 1)
Pt[, 1] <- p0

for (i in 1:m) { # cuenta el numero de simulacion
  for (j in 2:(n + 1)) {
    Pt[i, j] <- Pt[i, j - 1] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(1)) # rnorm(1) genera un numero aleatorio (es el epsilon de la formula)
  }
}

hist(Pt[,541])
mean(Pt[,541])
sd(Pt[,541])


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


prob<-0.975 ## esto generaria un intervalo al 90%
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

m=5000
e<-rnorm(m)
PT<-matrix(NA,nrow=m,ncol=3)
mu <- c(0.11, 0.15,0.2)
sigma <- c(0.17, 0.2,0.24)
for(i in 1:3){
  PT[,i]<-P0[i] * exp((mu[i]  - 0.5 * sigma[i]^2) * 0.5 + sigma[i] * sqrt(dt) * e)  
}

mean(PT[,1])
mean(PT[,2])
mean(PT[,3])

prob<-0.975 ## esto generaria un intervalo al 95%
limites_del_IC_KO<-quantile(PT[,1],probs=c(prob, 1-prob))
limites_del_IC_PEP<-quantile(PT[,2],probs=c(prob, 1-prob))
limites_del_IC_JNJ<-quantile(PT[,3],probs=c(1-prob, prob))

##ejercicio 4
mu=300
sigma=sqrt(675)


densidadNormal<-function(x){
  return((1/(sigma*srqt(2*pi)))*exp(-((x-mu)^2)/(2*(sigma^2))))
}


simpson_compuesto <- function(limiteInferior, limiteSuperior, funcion, n) {
  h <- (limiteSuperior - limiteInferior) / n
  suma <- 0
  for (i in 1:(n/2)) {
    xi <- limiteInferior + (2*i - 2)*h
    xi_1 <- limiteInferior + (2*i - 1)*h
    xi_2 <- limiteInferior + 2*i*h
    
    fx0 <- eval(funcion, list(x = xi))
    fx1 <- eval(funcion, list(x = xi_1))
    fx2 <- eval(funcion, list(x = xi_2))
    
    suma <- suma + (h / 3) * (fx0 + 4 * fx1 + fx2)
  }
  return(suma)
}
print(simpson_compuesto(240,10000,expression((1/(sigma*sqrt(2*pi)))*exp(-((x-mu)^2)/(2*(sigma^2)))),100000))