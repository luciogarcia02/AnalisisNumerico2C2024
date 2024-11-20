## parcial 1c2023
###1b) 
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

result<-PolinomioInterpolanteNewton(c(2.5, 4.375 ,6.25 ,8.125 ,10.00 ,11.875 ,13.75 ,15.625, 17.5),c(0.0013
                                                                                                     ,0.0122
                                                                                                     ,0.0668
                                                                                                     ,0.2266
                                                                                                     ,0.50
                                                                                                     ,0.7734
                                                                                                     ,0.9332                                                                                                     ,0.9878
                                                                                                     ,0.9987)) 



fx<-function(x){
  return(
    0.0013 + 0.00581333333333333 * ( x - 2.5 ) + 0.00621511111111111 * ( x - 2.5 ) * ( x - 4.375 ) + 0.00155496296296296 * ( x - 2.5 ) * ( x - 4.375 ) * ( x - 6.25 ) + -0.00017901037037037 * ( x - 2.5 ) * ( x - 4.375 ) * ( x - 6.25 ) * ( x - 8.125 ) + -2.47760241426612e-05 * ( x - 2.5 ) * ( x - 4.375 ) * ( x - 6.25 ) * ( x - 8.125 ) * ( x - 10 ) + 6.10191002591068e-06 * ( x - 2.5 ) * ( x - 4.375 ) * ( x - 6.25 ) * ( x - 8.125 ) * ( x - 10 ) * ( x - 11.875 ) + -4.64907430545577e-07 * ( x - 2.5 ) * ( x - 4.375 ) * ( x - 6.25 ) * ( x - 8.125 ) * ( x - 10 ) * ( x - 11.875 ) * ( x - 13.75 ) + 2.5410988417629e-22 * ( x - 2.5 ) * ( x - 4.375 ) * ( x - 6.25 ) * ( x - 8.125 ) * ( x - 10 ) * ( x - 11.875 ) * ( x - 13.75 ) * ( x - 15.625 ))
}

fx(9)##0.3450514
fx(18)##0.9732391

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

resultcubic<-TrazadorCubicoNatural(c(2.5, 4.375 ,6.25 ,8.125 ,10.00 ,11.875 ,13.75 ,15.625, 17.5),c(0.0013,0.0122,0.0668,0.2266,0.50,0.7734,0.9332,0.9878,0.9987))


