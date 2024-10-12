altura <- c(180, 175, 190, 189, 180, 179, 165, 158, 169, 175, 150, 170, 195)
peso <- c(80, 75, 85, 90, 87, 86, 65, 67, 70, 54, 68, 70, 90)




datos<-data.frame(peso<-peso,altura<-altura,dinero<-dinero)
ggplot(data=datos, aes(x=altura, y= peso,color="red")) + geom_point() + geom_line()
library(ggplot2)
##graficar la funcion y=x^5+x^3-x
polinomio<-function(x){
  y=x^2+x-5
  return (y)
}
x<-seq(from = 36, to = 46, by = 0.1)
y<-Polinomio_newton(x)
datos<-data.frame(x=x,y=y)

#raices<-data.frame(x=c(-2.791288,1.791288), y =c(0,0))

graph<-ggplot(data=datos, aes(x=x,y=y))+geom_point()+
  geom_line()+
  #geom_point(data=raices,aes(x=x,y=y),color="red",size= 3)+
  geom_hline(yintercept = 0, color = "black") +  # Línea del eje x
  #geom_vline(xintercept = 0, color = "black")+
  geom_point(aes(x=37.81062,y=0, color="red",size=3))+
  geom_point(aes(x=40.72923,y=0, color="blue",size=3))+
  geom_point(aes(x=44.09375,y=0, color="green",size=3))+
  labs(title = "grafica")
  
  


Polinomio_newton <- function(x){
  f <- 9*sin(x)-exp(83/(x^3))
  return(f)
}

#CALCULAR LA DERIVADA
df <- D(expression(9*sin(x)-exp(83/(x^3))),"x")
df

dfnewton <- function(x){
  df <- 9 * cos(x) + exp(83/(x^3)) * (83 * (3 * x^2)/(x^3)^2)
  return(df)
}
newton_raphson <- function(p0, TOL,N){
  i <- 1
  while(i<=N){
    x <- p0
    p <- p0- Polinomio_newton(p0)/dfnewton(p0)
    if(abs(p-p0)<TOL){
      return(p)
    }
    i <- i+1
    p0 <- p
  }
  return(paste("El metodo fallo luego de", N,"iteraciones"))
}
newton_raphson(38,0.00001,100)
newton_raphson(41,0.00001,100)
newton_raphson(44,0.00001,100)
raices<-data.frame(x=c(37.81062,40.72923,44.09375),y=c(0,0,0))

