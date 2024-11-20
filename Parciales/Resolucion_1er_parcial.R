##Ejercicio 1 - bisección
###A)
library(ggplot2)

Polinomio<-function(x){
  f<-1000*exp(100*x)+350/(x*(exp(100*x-1)))-800
  return(f)
}
x<-seq(from = -3, to = 3, by = 0.5)
y<-Polinomio(x)
datos<-data.frame(x=x,y=y)

graph<-ggplot(data=datos, aes(x=x,y=y))+geom_point()+
  geom_line()+
  geom_hline(yintercept = 0, color = "black") +  # Línea del eje x
  labs(title = "1000*exp(2*x)+350/x*(exp(2*x-1))")
graph

raiz_biseccion <- function(a,b,Tol,N){
  i <- 1
  FA <- Polinomio(a)
  while(i<=N){
    p <- a + (b-a)/2
    FP <- Polinomio(p)
    if(FP==0 | (b-a)/2<Tol){
      return(p)
    }
    i <- i+1
    if(FP*FA >0){
      a <- p
      FA <- FP
    } else {
      b <- p
    }
  }
  return(paste("El metodo fallo luego de", N, "iteraciones"))
}


raiz_biseccion(-3,3,10^(-5),10000) #-0.1287613

##punto D
raiz_biseccion <- function(a,b,Tol,N){
  i <- 1
  FA <- Polinomio(a)
  while(i<=N){
    p <- a + (b-a)/2
    FP <- Polinomio(p)
    if(FP==0 | (b-a)/2<Tol){
      paste("El resultado se encontro luego de ", i, " iteraciones")
      return(p)
      
    }
    i <- i+1
    if(FP*FA >0){
      a <- p
      FA <- FP
    } else {
      b <- p
    }
  }
  return(paste("El metodo fallo luego de", N, "iteraciones"))
}

#Regula Falsi respecto del método de Secante.
# en el metodo regula falsi el se elige xo y x1, se "unen" con una recta y x2 será el corte de la recta con el eje x
# luego se "unen" con una recta x1 y x2, x3 será el corte de la recta con el eje x
# para el metodo secante se eligen x0 y x1. Se unen y se toma la imagen de la raiz de al recta que los une (x2)
# se unen x2 y x1 y se realiza el mismo proceso

secante <- function(p0,p1,TOL,N){
  i <- 2
  q0 <- f_secante(p0)
  q1 <- f_secante(p1)
  while(i<=N){
    p <- p1 - q1*(p1-p0)/(q1-q0)
    if(abs(p-p1)<TOL){
      return(p)
    }
    i <- i+1
    p0 <- p1
    q0 <- q1
    p1 <- p
    q1 <- f_secante(p)
  }
  return(paste("El metodo fallo luego de",N,"iteraciones",p))
}
f_secante <- function(x){
  f <- (x^2)*cos(x)                       
  return(f)
}
#secante(0,2,0.01,3)

x_secante<-seq(from = 1, to = 4*pi, by = 0.1)
y_secante<-f_secante(x_secante)
datos_secante<-data.frame(x=x_secante,y=y_secante)

graph_secante<-ggplot(data=datos_secante, aes(x=x,y=y))+geom_point()+
  geom_line()+
  geom_hline(yintercept = 0, color = "black") +  # Línea del eje x
  labs(title = "(x^2)*cos(X)")
graph_secante


secante(1.5,2,0.001,100)1.570792
secante(4,5,0.001,100)4.712385
secante(7.5,8.5,0.001,100)7.853982
secante(10,11.5,0.001,100)10.99557

secante(4,6,0.001,4)

grafico<-ggplot(data=datos_secante, aes(x=x,y=y))+geom_point()+
  geom_line()+
  #geom_point(data=raices,aes(x=x,y=y),color="red",size= 3)+
  geom_hline(yintercept = 0, color = "black") +  # Línea del eje x
  #geom_vline(xintercept = 0, color = "black")+
  geom_point(aes(x=1.570792,y=0, color="red",size=3))+
  geom_point(aes(x=4.712385,y=0, color="blue",size=3))+
  geom_point(aes(x=7.853982,y=0, color="green",size=3))+
  geom_point(aes(x=10.99557,y=0, color="purple",size=3))+
  labs(title = "Grafico con raices")


#ejercicio3
resolver_sistema <- function(A) {
  n <- nrow(A)  
  
  for (i in 1:(n - 1)) {
    
    found_p <- FALSE
    for (p in i:n) {
      if (A[p, i] != 0) {
        found_p <- TRUE
        
        if (p != i) {
          A[c(i, p), ] <- A[c(p, i), ]
        }
        break
      }
    }
    
    if (!found_p) {
      return("no existe una solución única, hay una columna de ceros.")  
      }
    
   
    for (j in (i + 1):n) {
      
      m_ji <- A[j, i] / A[i, i]
      A[j, ] <- A[j, ] - m_ji * A[i, ]
    }
  }
  
  if (A[n, n] == 0) {
    return("no existe una solución única")  
  }
  
  x <- numeric(n)
  x[n] <- A[n, n + 1] / A[n, n]
  
  for (i in (n - 1):1) {
    suma <- A[i, n + 1]
    for (j in (i + 1):n) {
      suma <- suma - A[i, j] * x[j]
    }
    x[i] <- suma / A[i, i]
  }
  
  return(x)
}

A<-matrix(c(2,0,0,0,1,
            1,1.5,0,0,4.5,
            0,-3,0.5,0,-6.6,
            2,-2,1,1,0.8),nrow=4,byrow=T)
resolver_sistema(A)

factorizacionLU<-function(A){
  n<-nrow(A)
  L<-diag(1,n,n)
  U<-A
  for(i in 1:(n-1)){
    if(U[i,i]==0){
      stop("Factorizacion no posible se necesita pivoteo")
    }
    for(j in (i+1):n){
      L[j,i]<-U[j,i]/U[i,i]
      U[j,]<-U[j,]-L[j,i]*U[i,]
    }
  }
  return (list(L=L,U=U))
}
ALU <- matrix(c(2, 0, 0, 0,
                1, 1.5, 0, 0,
                0, -3, 0.5, 0,
                2, -2, 1, 1), nrow = 4, byrow = TRUE)

LU <- factorizacionLU(ALU)
L <- LU$L
U <- LU$U
b<-c(1,4.5,-6.6,0.8)
print(L)
print(U)
print("L*b*U")
print(L%*%U)
##ejercicio4
###SELECT p.ProductName, p.Unit, s.SupplierName, s.City 
###FROM Products p
###INNER JOIN Suppliers s ON p.SupplierID = s.SupplierID
###WHERE p.ProductName LIKE 'P%' AND s.Country='Australia'

###SELECT o.OrderID, o.OrderDate, p.ProductName, p.Price
###FROM (((Orders o
###        INNER JOIN OrderDetails od ON o.OrderID = od.OrderID)
###       INNER JOIN Products p ON od.ProductID = p.ProductID)
###      INNER JOIN Categories c ON p.CategoryID = c.CategoryID)
###WHERE c.CategoryName='Beverages' AND p.Price>35 

# Ejercicio de interes compuesto
C0<-50000
Cn<-100000
anos<-5

Polinomio_interes<-function(x){
  f<-50000*(1+x)^2-100000
  return(f)
}
x<-seq(from = 0.1, to = 0.25, by = 0.01)
y<-Polinomio_interes(x)
datos<-data.frame(x=x,y=y)

graph<-ggplot(data=datos, aes(x=x,y=y))+geom_point(size=0)+
  geom_line()+
  geom_hline(yintercept = 0, color = "black") +  # Línea del eje x
  geom_vline(xintercept = 0, color = "black") +  # Línea del eje x
  labs(title = "f(x)=50000*(1+x)^5-100000")
graph

raiz_biseccion_interes <- function(a,b,Tol,N){
  i <- 1
  FA <- Polinomio_interes(a)
  while(i<=N){
    p <- a + (b-a)/2
    FP <- Polinomio_interes(p)
    if(FP==0 | (b-a)/2<Tol){
      paste("La tolerancia 1e-20 se alcanzo en la iteracion: ", i)
      return(p)
    }
    i <- i+1
    if(FP*FA >0){
      a <- p
      FA <- FP
    } else {
      b <- p
    }
  }
  return(paste("El metodo fallo luego de", N, "iteraciones"))
}


raiz_biseccion_interes(0.1,1,10^(-20),100000000) #-0.1287613


