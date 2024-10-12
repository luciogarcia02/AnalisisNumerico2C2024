#guia 2
##Ejercicio 1
###a.
A<-matrix(c(4,-1,1,2,5,2,2,5,2),nrow=3,byrow=TRUE)A
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
result<-factorizacionLU(A)
L<-result$L
U<-result$U
print(L)
print(U)
LU<-L%*%U
print(LU)

###b.
B<-matrix(c(4,1,2,2,4,-1,1,1,-3),nrow=3,byrow=TRUE)
result<-factorizacionLU(B)
L<-result$L
U<-result$U
print(L)
print(U)
LU<-L%*%U
print(LU)

###E.
E<-matrix(c(1.012,-2.132,3.104,-2.132,4.906,-7.013,3.104,-7.013,0.014),nrow=3,byrow=TRUE)
result<-factorizacionLU(E)
L<-result$L
U<-result$U
print(L)
print(U)
LU<-L%*%U
print(LU)


## Factorizacion Cholesky
###a.
A<-matrix(c(2,-1,0,-1,2,-1,0,-1,2),nrow =3, byrow=T)
cholesky<-function(A){
  n<-nrow(A)
  L<-matrix(0,n,n)
  for(i in 1:n){
    L[i,i]<-sqrt(A[i,i]-sum(L[i,1:(i-1)]^2))
    if(i<n){
      for(j in (i+1):n){
        L[j,i]<-(A[j,i]-sum(L[j,1:(i-1)]*L[i,1:(i-1)]))/L[i,i]
      }
    }
    
  }
  return(L)
}
L<-cholesky(A)
Lt<-t(L)
comp<-L%*%Lt
comp

###D
D<-matrix(c(1,2,4,7,2,13,23,38,4,13,77,122,7,38,122,294),nrow =4, byrow=T)
cholesky(D)
L<-cholesky(D)
Lt<-t(L)
comp<-L%*%Lt
comp