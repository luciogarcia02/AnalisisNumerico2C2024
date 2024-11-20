#GUIA Lagrange, interpolacion y trazadores
# hacer el pol de lagrange para 
##a. f (0.43) si f (0) = 1, f (0.25) = 1.64872, f (0.5) = 2.71828, f (0.75) = 4.48169


PolinomioLagrange <- function(x, fx, y){
  
  n <- length(x)
  
  l <- rep("", times = n)
  
  resultado <- 0
  
  for (i in 1:n) {
    l[i] <- fx[i]
    for (j in 1:n) {
      if (j != i){
        l[i] <- l[i] + glue::glue("*(x-",x[j],")/(",x[i],"-",x[j],")")
      }
    }
  }
  
  for(i in 1:n){
    resultado <- resultado + eval(parse(text=l[i]), y)
  }
  return(paste("El resultado es: ", resultado))
}

polGra3 <- PolinomioLagrange(x= c(0,0.25,0.5,0.75),fx = c(1,1.64872,2.71828,4.481669), y = list(x = 0.43))
polGra2 <- PolinomioLagrange(x= c(0,0.25,0.5),fx = c(1,1.64872,2.71828), y = list(x = 0.43))
polGra1 <- PolinomioLagrange(x= c(0,0.25),fx = c(1,1.64872), y = list(x = 0.43))

# Metodo de diferencias divididas -----------------------------------------
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

difDiv<-DiferenciasDivididas(c(0,0.25,0.5,0.75),c(1,1.64872,2.71828,4.481669))
#con diferencias divididas podemos expresar el polinomio de la funcion 
#a) 1+2.59488*(x-0)+3.3667*(x-0)*(x-0.25)+2.911883*(x-0)*(x-0.25)*(x-0.5)



# Metodo de Neville -------------------------------------------------------

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
neville1<- Neville(c(0,0.25,0.5,0.75), c(1,1.64872,2.71828,4.481669),  0.43)
