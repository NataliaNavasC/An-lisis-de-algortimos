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
        Ab[c(k, fila),
           ] = Ab[c(fila, k),
                  ]
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
    x[n] =
      Ab[n, n+1]/Ab[n,n]
    # xn = bn/a_nn
    for(i in (n-1):1 ){
      x[i]= (Ab[i, n+1] -sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i]
    }
    return(x)
  } else {
    return("Error la matriz no tiene inversa.")
  }
}
#--- Pruebas
A = matrix(c(3.5, 4.2,
             4.5, 1/3), nrow = 2, byrow = TRUE)
b = matrix(c(1.3, 7/4), nrow = 2, byrow = FALSE)

#deltaA = matrix(c(3.5, 4.2,
 #            0.5, 1/3), nrow = 2, byrow = TRUE)
#deltab = matrix(c(1.3, 7/4), nrow = 2, byrow = FALSE)
##
gauss(A,b) # [1] 7.410714 -5.866071
#gauss(deltaA,deltab) # [2] 0.39003759 -0.01550752
rcond(A)
