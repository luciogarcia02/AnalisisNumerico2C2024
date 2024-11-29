####polinomio lagrange, devuelve el punto
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
####lagrange, devuelve la expresion del polinomio
polinomio_lagrange <- function(valores_x, valores_fx) {

  n <- length(valores_x)
  
  terminos <- vector("list", n)

  for (i in 1:n) {
    numerador <- "1"
    denominador <- 1
    for (j in 1:n) {
      if (i != j) {
        numerador <- paste0(numerador, " * (x - ", valores_x[j], ")")
        denominador <- denominador * (valores_x[i] - valores_x[j])
      }
    }
    termino <- paste0("(", valores_fx[i], ") * (", numerador, ") / ", denominador)
    terminos[[i]] <- termino
  }
  polinomio <- paste(terminos, collapse = " + ")
  
  return(polinomio)
}
####interpolante newton, devuelve el polinomio
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
####metodo Neville es un caso particular de lagrange,devuelve una tabla con la aproximacion usando punto 0, punto 1, punto 1/0...
Neville <- function(x, y, interpolar){
  #cantidad de iteraciones que voy a hacer
  n <- length(x)-1
  
  #Hago un vector vacio para llenar el df
  empty_vec <- rep(0, times = length(x))  
  
  df <- data.frame(x, y)
  
  for (i in 1:n) {
    df[glue::glue("Q",i)] <- empty_vec
    
    for (j in (i+1):(n+1)) {
      
      df[j, (i+2)] <- ( (interpolar-x[(j-i)]) * df[j,(i+1)] - (interpolar-x[j]) * df[(j-1),(i+1)] )  / (x[j]-x[j-i])
      
    }
  }
  
  return(df)
}
####metodo de dif divididas (lo necesito para newton)
DiferenciasDivididas <- function(x, y){
  n <- length(x)
  
  #Hago un vector vacio para llenar el df
  empty_vec <- rep(0, times = n)  
  
  df <- data.frame(x, y)
  
  for (i in 1:(n-1)) {
    
    df[glue::glue("Q",i)] <- empty_vec
    
    for (j in (i+1):n) {
      
      #print(paste("j: ", j, " i+2: ", i+2))
      df[j, (i+2)] <- ( df[j,(i+1)] - df[(j-1),(i+1)])/(x[j]-x[j-i])
    }
  }
  
  return(df)
}
#### trazador natural
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
####Graficador de trazador devuelve un ggplot
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
####simpson compuesto recibe function(no expression)
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
####trapecio compuesto recibe function(no expression)
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
####Definicion hacia adelante no puede calcular el ultimo hacia atras no puede calcular el primero
DerivadaPorDefinicion <- function(x, fx){
  fprima <- rep(NA, times = length(x))
  
  #Se asume que todos los valores estan separados por un h constante
  h <- x[2] - x[1]
  
  #Diferencia progresiva
  for (i in 1:(length(x)-1)){
    fprima[i] <- (fx[i+1] - fx[i]) / h
  }
  
  # Diferencia regresiva
  for (i in (length(x):2)) {
    fprimaReg <- (fx[i-1] - fx[i]) / (-h)
    
    if (!is.na(fprima[i])){
      if(fprimaReg != fprima[i]){
        aux <- fprima[i]
        fprima[i] <- glue::glue(aux, " (P)",
                                " o ",
                                fprimaReg, " (R)" )
      }  
    } else{
      fprima[i] <- fprimaReg
    }
    
  }
  
  resultado <- data.frame(x, fx, fprima)
  
  return(resultado)
}
####tres puntos
Tres_puntos <- function(x, fx){
  n <- length(x)
  
  fprima <- rep(NA, times = n)
  
  h <- x[2] - x[1]
  
  #Punto extremo
  fprima[1] <- (1/(2*h))*(-3*fx[1]+4*fx[2]-fx[3])
  fprima[n] <- (1/(2*(-h)))*(-3*fx[n]+4*fx[n-1]-fx[n-2])
  
  #Punto medio
  for (i in 2:(n-1)) {
    fprima[i] <- (1/(2*h))*(-fx[i-1]+fx[i+1])
  }
  
  tabla <- data.frame(x, fx, fprima)
  
  return(tabla)
}
####cinco puntos
Cinco_puntos <- function(x, fx){
  n <- length(x)
  
  fprima <- rep(NA, times = n)
  
  h <- x[2] - x[1]
  
  #Punto extremo
  fprima[1] <- (1/(12*h))*(-25*fx[1]+48*fx[2]-36*fx[3]+16*fx[4]-3*fx[5])
  fprima[n] <- (1/(12*(-h)))*(-25*fx[n]+48*fx[n-1]-36*fx[n-2]+16*fx[n-3]-3*fx[n-4])
  
  #Punto medio
  for (i in 3:(n-2)) {
    print(i)
    fprima[i] <- (1/(12*h))*(fx[i-2]-8*fx[i-1]+8*fx[i+1]-fx[i+2])
  }
  
  tabla <- data.frame(x, fx, fprima)
  
  return(tabla)
}
####segunda derivada
SegundaDerivada <- function (x, fx){
  n <- length(x)
  
  fprima <- rep(NA, times = n)
  
  h <- x[2] - x[1]
  
  #Punto medio
  for (i in 2:(n-1)) {
    fprima[i] <- (1/(h^2))*(fx[i-1]-2*fx[i]+fx[i+1])
  }
  
  tabla <- data.frame(x, fx, fprima)
  
  return(tabla)
  
}
####regresion y ajustamiento
#modelolineal<-lm(fx~x) ##modelolineal<-lm(fx~poly(x,1)) es lo mismo pero usando poly
#modelolineal<-lm(fx~x+  I(x^2)+  I(x^3)) seria la forma exlicita de ajustar con un polinomio de grado 3
#summary(modelolineal)
#ylineal=rep(NA,9)
#ylineal=predict(modelolineal, newdata = data.frame(x=x))