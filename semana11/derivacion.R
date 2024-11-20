## por definicion

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

## tres puntos

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

##cinco puntos

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

## cinco puntos modificado

Cinco_puntos_modificado <- function(x, fx){
  n <- length(x)
  
  fprima <- rep(NA, times = n)
  
  h <- x[2] - x[1]
  
  # Punto extremo progresivo
  for (i in 1:(n-4)) {
    fprima[i] <- (1/(12*h))*(-25*fx[i]+48*fx[i+1]-36*fx[i+2]+16*fx[i+3]-3*fx[i+4])
  }
  
  
  # Punto extremo regresivo
  for (i in n:5) {
    fprima[i] <- (1/(12*(-h)))*(-25*fx[i]+48*fx[i-1]-36*fx[i-2]+16*fx[i-3]-3*fx[i-4])
  }
  
  tabla <- data.frame(x, fx, fprima)
  
  return(tabla)
}

##segunda derivada

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
#1
fprimas<-DerivadaPorDefinicion(c(0.5,0.6,0.7),c(0.4794,0.5646,0.6442))
print(fprimas)

#2
tabla2a<-Tres_puntos(c(2.9,3.0,3.1,3.2),c(-4.827866,-4.240058,-3.496909,-2.596792))
print(tabla2a)

#3
tabla3a<-Cinco_puntos(c(0.2,0.4,0.6,0.8,1),c(0.9798652,0.9177710,0.8080348,0.6386093,0.3843735))
print(tabla3a)

tabla3a3p<-Cinco_puntos_modificado(c(0.2,0.4,0.6,0.8,1),c(0.9798652,0.9177710,0.8080348,0.6386093,0.3843735))
print(tabla3a3p)