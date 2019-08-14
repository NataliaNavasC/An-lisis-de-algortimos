# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

gauss = function(A, b){
  if(det(A) != 0){
    n = nrow(A) # = ncol(A) para que sea cuadrada
    # matriz ampliada
    Ab = cbind(A,b)
    # Eliminación
    for (k in 1:(n-1)){
      # desde columna k=1 hasta k=n-1
      if(Ab[k,k]==0){
        # intercambio de fila
        fila = which(Ab[k, ]!=0)[1]
        Ab[c(k, fila),] = Ab[c(fila, k),]
      }
      # Eliminación columna k
      for (i in (k+1):n){# debajo de la diagonal
        # Fi = Fi - a_ik/a_kk * Fk, i=k+1,...,n
        Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
      }
    }
    # Sustitución hacia atrás-------------------------
    # b(i) = A[i, n+1]
    x = rep(NA, times=n)
    x[n] = Ab[n, n+1]/Ab[n,n]   # xn = bn/a_nn
    for(i in (n-1):1 ){
      x[i]= (Ab[i, n+1] -sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i]
    }
    return(x)
  } else {
    return("Error la matriz no tiene inversa.")
  }
}
#--- Pruebas
#A = matrix(sample(0:1,100,replace=T), nrow = 10, byrow = TRUE)
#b = c(sample(0:1,10,replace=T))
#A = matrix(sample(0:1,10000,replace=T), nrow = 100, byrow = TRUE)
#b = c(sample(0:1,100,replace=T))
A = matrix(sample(0:1,1000000,replace=T), nrow = 1000, byrow = TRUE)
b = c(sample(0:1,1000,replace=T))

gauss(A,b)
