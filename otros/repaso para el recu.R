##clase de graficos
rm(list = ls())
graphics.off()

library(ggplot2)

vectordelasx<-seq(0,10 , by = 0.5)
funcion2x<-function(x){
  return (2*x)
}
misdatosxy<-data.frame(x=vectordelasx,y=funcion2x(vectordelasx))
misdatospunto<-data.frame(x=1,y=1)
migrafica<-ggplot(data = misdatosxy)  ##cargo los datos
migrafica=migrafica+  aes(x = misdatosxy$x, y = misdatosxy$y) ##agrego los ejes 
  migrafica=migrafica+  geom_line()## agrego la linea que une los puntos
migrafica=migrafica+ geom_point() ##agrego los puntos
migrafica=migrafica+geom_hline(yintercept = 0,linetype=1) #agrego el eje x en 0 
migrafica <- migrafica + geom_vline(xintercept=0)
migrafica = migrafica + scale_x_continuous(name = "x", breaks = seq(0,10, by = 1))
migrafica <- migrafica + scale_y_continuous(name = "valores de y", breaks = c(1,4,6,9))
migrafica

x<-seq(2.5,17.5,by=1.875)
fx<-c(0.0013,0.0122,0.0668,0.2266,0.5,0.7734,0.9332,0.9878,0.9987)
modelolineal<-lm(fx~x) ##modelolineal<-lm(fx~poly(x,1)) es lo mismo pero usando poly
#modelolineal<-lm(fx~x+  I(x^2)+  I(x^3)) seria la forma exlicita de ajustar con un polinomio de grado 3
summary(modelolineal)

ylineal=rep(NA,9)
ylineal=predict(modelolineal, newdata = data.frame(x=x))


datapuntos<-data.frame(x=x,y=fx)
datarecta<-data.frame(x=x,y=ylineal)
grafico<-ggplot(data=datapuntos,aes(x=x,y=y)) + geom_point() + geom_line(data=datarecta,color="green") +geom_point(x=9,y=0.41824,size=3,color="red")
splinerecta<-data.frame(x=c(2.5,17.5),y=c(0.0013,0.9987))
grafico<-grafico + geom_line(data=splinerecta, color="blue") + geom_line(data=datalagrange,color="purple")+geom_point(x=9,y=0.3450514,size=3,color="yellow")
grafico
PolinomioLagrange(x,fx,9)
PolinomioLagrange <- function(x, fx, y) {
  n <- length(x)  # Número de puntos
  resultado <- 0  # Inicializa el resultado
  
  # Construir y evaluar el polinomio de Lagrange
  for (i in 1:n) {
    # Calcula el término L_i(y)
    Li <- 1
    for (j in 1:n) {
      if (j != i) {
        Li <- Li * (y - x[j]) / (x[i] - x[j])
      }
    }
    # Suma el término correspondiente al polinomio
    resultado <- resultado + fx[i] * Li
  }
  
  return(resultado)  # Devuelve el valor aproximado
}
ylagrange=rep(NA,9)
for(i in 1:9){
  ylagrange[i]=PolinomioLagrange(x,fx,x[i]) 
}
datalagrange=data.frame(x=x,y=ylagrange)



traza<-TrazadorCubicoNatural(x,fx)
grafiquito<-graficarTrazador(traza,x,fx)
grafiquito<-grafiquito+ geom_line(data=splinerecta, color="blue") + geom_line(data=datalagrange,color="purple")+geom_point(x=9,y=0.3450514,size=3,color="yellow")
grafiquito
graficarTrazador <- function(trazador, x, fx) {
  polinomios <- trazador$polinomios
  a <- trazador$coeficientes$a
  b <- trazador$coeficientes$b
  c <- trazador$coeficientes$c
  d <- trazador$coeficientes$d
  n <- length(x) - 1
  
  # Crear un dataframe para graficar
  x_eval <- c()
  y_eval <- c()
  
  for (i in 1:n) {
    x_intervalo <- seq(x[i], x[i + 1], length.out = 100)
    y_intervalo <- a[i] + 
      b[i] * (x_intervalo - x[i]) + 
      c[i] * (x_intervalo - x[i])^2 + 
      d[i] * (x_intervalo - x[i])^3
    x_eval <- c(x_eval, x_intervalo)
    y_eval <- c(y_eval, y_intervalo)
  }
  
  # Graficar con ggplot2
  library(ggplot2)
  data <- data.frame(x = x_eval, y = y_eval)
  puntos <- data.frame(x = x, y = fx)
  
  plotgraficado<-ggplot(data, aes(x = x, y = y)) +
    geom_line(color = "brown") +
    labs(
      title = "Spline Cúbico Natural",
      x = "x",
      y = "f(x)"
    ) +
    theme_minimal()
  return(plotgraficado)
}
TrazadorCubicoNatural <- function(x, y) {
  # Verificar que x e y tengan la misma longitud
  if (length(x) != length(y)) {
    stop("Los vectores x e y deben tener la misma longitud.")
  }
  
  n <- length(y)
  j <- n - 1
  
  a <- y
  b <- numeric(n)
  c <- numeric(n)
  d <- numeric(n)
  
  A <- numeric(j)
  h <- numeric(j)
  l <- numeric(n)
  u <- numeric(n)
  z <- numeric(n)
  
  # Paso 1: Calcular los tamaños de los subintervalos
  for (i in 1:j) { 
    h[i] <- x[i + 1] - x[i]
  }
  
  # Paso 2: Calcular A[i]
  for (i in 2:j) { 
    A[i] <- (3 * (a[i + 1] - a[i]) / h[i]) - (3 * (a[i] - a[i - 1]) / h[i - 1])
  }
  
  # Paso 3: Condiciones iniciales
  l[1] <- 1
  u[1] <- 0
  z[1] <- 0
  
  # Paso 4: Cálculo de l, u y z
  for (i in 2:j) {
    l[i] <- 2 * (x[i + 1] - x[i - 1]) - h[i - 1] * u[i - 1]
    u[i] <- h[i] / l[i]
    z[i] <- (A[i] - h[i - 1] * z[i - 1]) / l[i]
  }
  
  # Paso 5: Condiciones finales
  l[n] <- 1
  z[n] <- 0
  c[n] <- 0
  
  # Paso 6: Backward substitution para c, b, d
  for (i in seq(j, 1)) {
    c[i] <- z[i] - u[i] * c[i + 1]
    b[i] <- (a[i + 1] - a[i]) / h[i] - h[i] * (c[i + 1] + 2 * c[i]) / 3
    d[i] <- (c[i + 1] - c[i]) / (3 * h[i])
  }
  
  # Paso 7: Construir los polinomios
  polinomios <- vector("character", j)
  for (i in 1:j) {
    polinomios[i] <- paste0(
      a[i], 
      " + ", b[i], " * (x - ", x[i], ")",
      " + ", c[i], " * (x - ", x[i], ")^2",
      " + ", d[i], " * (x - ", x[i], ")^3"
    )
  }
  
  return(list(polinomios = polinomios, coeficientes = data.frame(a, b, c, d)))
}


###nombrar una funcion e integrar el area con simson compuesto de 0 a 4, graficar marcando el area
##f(x)=2*x^4+x^3+2x+2
x<-seq(0,4,by=0.1)
y<-rep(NA,41)
for(i in 1:41){
  y[i]<-cos(x[i])+1.5
}
myframe<-data.frame(x=x,y=y)
library(ggplot2)
grafico<-ggplot(data=myframe,aes(x=x,y=y)) + geom_point() + geom_hline(yintercept=0)+geom_vline(xintercept=0) + geom_area()
grafico


funcionsimp<-function(x){
  return (cos(x)+1.5)
}
trapecio_compuesto(0,4,funcionsimp,4)
simpson_compuesto(0,4,funcionsimp,4)

simpson_compuesto <- function(limiteInferior, limiteSuperior, funcion, n) {
  # Verificar que n sea par
  if (n %% 2 != 0) {
    stop("El número de subintervalos 'n' debe ser par.")
  }
  
  h <- (limiteSuperior - limiteInferior) / n
  suma <- 0
  
  for (i in 1:(n/2)) {
    xi <- limiteInferior + (2*i - 2)*h
    xi_1 <- limiteInferior + (2*i - 1)*h
    xi_2 <- limiteInferior + 2*i*h
    
    # Evaluar la función directamente
    fx0 <- funcion(xi)
    fx1 <- funcion(xi_1)
    fx2 <- funcion(xi_2)
    
    suma <- suma + (h / 3) * (fx0 + 4 * fx1 + fx2)
  }
  return(suma)
}

trapecio_compuesto <- function(limiteInferior, limiteSuperior, funcion, n) {
  # Paso (h) basado en el número de intervalos
  h <- (limiteSuperior - limiteInferior) / n
  
  # Suma inicial con los extremos de la función
  suma <- funcion(limiteInferior) + funcion(limiteSuperior)
  
  # Sumar los puntos intermedios
  for (i in 1:(n-1)) {
    x <- limiteInferior + i * h
    suma <- suma + 2 * funcion(x)
  }
  
  # Multiplicar por h/2 para obtener el resultado
  resultado <- (h / 2) * suma
  return(resultado)
}
x<-c(1,3)
y<-c(NA,NA)
for(i in 1:2){
  y[i]<-funcioncuad(x[i])
}
funcioncuad<-function(x){
  return(-x^2+5*x-2)
}
x3<-seq(1,3,by=0.1)
y3<-rep(NA,21)
for(i in 1:21){
  y3[i]<-funcioncuad(x3[i])
}

myframe3<-data.frame(x=x3,y=y3)
myframe4<-data.frame(x=c(2,2),y=c(3,4))
myframe2<-data.frame(x=x,y=y)
library(ggplot2)
grafico<-ggplot() + geom_point(data=myframe2,aes(x=x,y=y)) + geom_hline(yintercept=0)+geom_vline(xintercept=0) 
grafico<- grafico + geom_line(data=myframe3,aes(x=x,y=y))
grafico<-grafico + geom_line(data=myframe4,aes(x=x,y=y),color="red",linetype = "dashed")
xl<-seq(1,3,by=0.1)
length(xl)
yl<-rep(NA,21)
for(i in 1:21){
  yl[i]<-PolinomioLagrange(x,y,xl[i])
}
myframe2<-data.frame(x=xl,y=yl)

##calculo de la cota
D(expression(log(sin(x)+1)+3*sin(x)),"x")
optimize(expression((-2/(3*2))*(x-1)*(x-3)),interval = c(1,3),maximum=TRUE)

x<-seq(5.075,7.91,by=0.405)
