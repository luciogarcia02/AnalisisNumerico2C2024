##newton-cotes cerrado

newtonCotesCerradas <- function(limiteInferior, limiteSuperior, funcion, n){
  h <- (limiteSuperior - limiteInferior)/n
  
  fx <- rep(NA, times = (n+1))
  for (i in 1:(n+1)) {
    fx[i] <- eval(funcion, list(x = limiteInferior + (i-1)*h))
  }
  
  # Hay que cambiarlo para que quede solo con un 1
  if (n == 1){
    return((h/2) * (fx[1] + fx[2]))
  }
  else if (n == 2){
    return((h/3) * (fx[1] + 4*fx[2] + fx[3]))
  }
  else if(n == 3){
    return((3/8)*h*(fx[1] + 3*fx[2] + 3*fx[3] + fx[4]))
  }
  else if(n == 4){
    return((2/45) * h * ( 7 * fx[1] + 32 * fx[2] + 12 * fx[3] + 32 * fx[4] + 7 * fx[5]))
  }
  
}

# n = 1. Regla del trapecio.
# n = 2. Regla de Simpson.
# n = 3. Regla de tres octavos de Simpson.
# n = 4 regla de NC cerrada con n = 4.
# Poner la funcion con "x" como incognita
newtonCotesCerradas(limiteInferior = 0, limiteSuperior = 4, funcion = expression(exp(x)), n = 2)


## newton-cotes abierto

newtonCotesAbiertas <- function(limiteInferior, limiteSuperior, funcion, n){
  h <- (limiteSuperior - limiteInferior)/(n+2)
  
  fx <- rep(NA, times = (n+1))
  for (i in 1:(n+1)) {
    fx[i] <- eval(funcion, list(x = limiteInferior + i*h))
  }
  
  if (n == 0){
    return(2 * h * fx[1])
  }
  else if (n == 1){
    return((3/2)* h * (fx[1] + fx[2]))
  }
  else if(n == 2){
    return((4/3)*h*(2*fx[1] - fx[2] + 2*fx[3]))
  }
  else if(n == 3){
    return((5/24) * h * ( 11 * fx[1] + fx[2] + fx[3] + 11 * fx[4]))
  }
  
}
# n = 0. Regla del punto medio.
# n = 1. 
# n = 2. 
# n = 3.
# Poner la funcion con "x" como incognita
newtonCotesAbiertas(limiteInferior = 0, limiteSuperior = pi/4, funcion = expression(sin(x)), n = 3)

print(newtonCotesCerradas(0,0.5,expression(2/(x-4)),1))
newtonCotesCerradas(0,0.35,expression(2/((x^2)-4)),1)


newtonCotesCerradas(0,1,expression((x^2)*exp(-x)),2)
newtonCotesCerradas(0,pi/4,expression(exp(3*x)*sin(2*x)),2)

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

print(simpson_compuesto(12.68, 20.42, expression((1 / (x * 0.22 * sqrt(0.75))) * exp(-((log(x) - 2.77)^2) / (2 * 0.22 * 0.75))), 1000))
print(newtonCotesCerradas(12.68, 20.42, expression((1 / (x * 0.22 * sqrt(0.75))) * exp(-((log(x) - 2.77)^2) / (2 * 0.22 * 0.75))), 1))
print(newtonCotesCerradas(12.68, 20.42, expression((1 / (x * 0.22 * sqrt(0.75))) * exp(-((log(x) - 2.77)^2) / (2 * 0.22 * 0.75))), 2))
print(newtonCotesCerradas(12.68, 20.42, expression((1 / (x * 0.22 * sqrt(0.75))) * exp(-((log(x) - 2.77)^2) / (2 * 0.22 * 0.75))), 3))
