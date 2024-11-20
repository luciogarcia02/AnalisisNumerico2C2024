#biseccion
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

Polinomio<-function(x){
  devolver<-x-2^(-x)
  return (devolver)
}

###1a
raiz_biseccion(0,1,10^(-5),100)
###1b
Polinomio<-function(x){
  rev <- (exp(1)^x)-(x^2)+3*x-2
  return (rev)
  }
raiz_biseccion(0,1,10^(-5),100)


# COTA BISEECION
## en cuantos pasos llego al nivel de tolerancia requerido y cuales son esos valores

cota_biseccion <- function(a, b, tol, N) {
  li <- list()
  i <- 1
  FA <- polinomio(a)
  while (i <= N) {
    p <- a + (b-a)/2
    FP <- polinomio(p)
    li <- append(li,p)
    cat("Iteración", i, "- p =", p, "\n")
    if (FP == 0 || (b-a)/2<tol) {
      return(paste("El método converge a",p,"después de",i,"iteraciones."))
    }
    i <- i+1
    if (FP*FA > 0){
      a <- p
      FA <- FP
    } else {
      b <- p
    }
  }
  return(paste('El método falló luego de ', N, ' iteraciones.'))
}
Polinomio<-function(x){
  y<-x^3+x-4
    return(y)
}
cota_biseccion(1,4,10^(-3),100)

#Punto fijo 
##siempre se despeja la x de mayor grado
puntofijo = function(p0, Tol,N){
  i=1
  while(i<= N){
    p= Polinomio(p0)
    if(abs(p-p0) < Tol){
      return(p)
    }
    i= i+1
    p0=p
  }
  return(paste("El metodo fallo luego de ", N, "iteraciones "))
}

###3
Polinomio= function(x){
  f= (3*x^2 + 3)^(1/4)
  return(f) 
}
puntofijo(1,10^(-2),100)

###4
Polinomio<-function(x){
  y<-(x+1)^(1/3)
  return (y)
}
puntofijo(1,10^(-2),100)


#5 Punto fijo 
#A g(x)=pi+0.5*sen(x/2) tiene un unico p. fijo en (0,2pi) con 10^-2
#B Corolario para estimar las iteraciones con 10^-4

#A Usamos el codigo de punto fijo del punto 3 y 4

Polinomio= function(x){
  f= pi+0.5*sin(x/2)
  return(f) 
}

puntofijo(1,0.01,100)     #me difiere muy poco el ultimo decimal

#B
#Iteracion 1
PF = puntofijo(1, 0.0001, 1)
print(PF)
#Iteracion 2
PF = puntofijo(1, 0.0001, 2)
print(PF)
#Iteracion 3
PF = puntofijo(1, 0.0001, 3)
print
#Iteracion 4
PF = puntofijo(1, 0.0001, 4)
print(PF)
#Iteracion 5
PF = puntofijo(1, 0.0001, 5)
print(PF)                  #se termina ya que da el mismo rdo que el punto a




#6 Newton para f(x)=-x^3-cos(x) con p0=-1 y con p0=0

Polinomio_newton <- function(x){
  f <- -x^3-cos(x)
  return(f)
}

#CALCULAR LA DERIVADA
df <- D(expression(-x^3-cos(x)),"x")
df

dfnewton <- function(x){
  df <- -(3 * x^2 - sin(x))
  return(df)
}

newton_raphson <- function(p0, TOL,N){
  i <- 1
  while(i<=N){
    x <- p0
    p <- p0- Polinomio_newton(p0)/dfnewton(p0)
    print(p)
    if(abs(p-p0)<TOL){
      return(p)
    }
    i <- i+1
    p0 <- p
  }
  return(paste("El metodo fallo luego de", N,"iteraciones"))
}


newton_raphson(0,0.000000000001,10)

###8
####a x^3-2*x^2-5 respuesta->2.690647
####b x^3-3*x^2-1 respuesta->3.103803
####c x-cos(x) respuesta-> 0.7390851
####d x-0.8-0.2*sin(x) respuesta-> 0.9643339

Polinomio_newton <- function(x){
  f <- x-0.8-10*sin(x)
  return(f)
}

#CALCULAR LA DERIVADA
df <- D(expression(exp(-x*y) + 21 * z - 3 + (11/3) * pi),"z")
df

dfnewton <- function(x){
  df <- 1 - 10 * cos(x)
  return(df)
}
newton_raphson(1.4,0.0001,100)
raiz_biseccion(2.5,3.5,0.001,10)  ## Para calcular una aproximacion a p0 que este entre ciertos valores puedo usar biseccion


# secante y falsa posicion
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
  f <- x^2-6                       
  return(f)
}

secante(3,2,0.01,3) #uso N=3 para calcular p3

falsapos <- function(p0,p1,TOL,N){
  i <- 2
  q0 <- f_falsapos(p0)
  q1 <- f_falsapos(p1)
  while(i<=N){
    p <- p1 - q1*(p1-p0)/(q1-q0)
    if(abs(p-p1) < TOL){
      return(p)
    }
    i <- i+1
    q <- f_falsapos(p)
    if(q*q1 < 0){
      p0 <- p1
      q0 <- q1
    }
    p1 <- p
    q1 <- q
  }
  return(paste("El metodo fallo luego de",N,"iteraciones"))
}

f_falsapos <- function(x){
  f <- x^2-6                     
}

falsapos(3,2,0.1,100)   













