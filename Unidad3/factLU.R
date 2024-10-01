## Clase Factorizacion
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

A<-matrix(c(4,-1,1,2,5,2,2,5,2),nrow=3,byrow=TRUE)
LU<-factorizacionLU(A)  
L<-LU$L
U<-LU$U
print(L)
print(U)
print("L*U")
print(paste(L%*%U))

##Cholesky















