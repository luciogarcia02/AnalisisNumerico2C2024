##Cholesky

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

A<-matrix(c(2,-1,0,-1,2,-1,0,-1,2), nrow=3,byrow=TRUE)
L<-cholesky(A)
print(L)
Ltrans<-t(L)
print(Ltrans)
print(L%*%Ltrans)



