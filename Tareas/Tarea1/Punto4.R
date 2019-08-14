# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

library(pracma)
#valores de Prueba
A = matrix(c(  3.5,  4.2,  
               1/2,   1/3  
), nrow=2, byrow=TRUE)
k = 5
#valores de prueba termina
mp <- A 

#eigen(mp)

#Obtiene los valores propios para hacer D
valoresPropios <- eigen(mp)$values
#Obtiene los vectores propios para hacer P
vectoresPropios <-  eigen(mp)$vectors
#largo de nuevas matrices
valoresPropiosTam <- length(valoresPropios)
vectoresPropiosTam <- valoresPropiosTam
#creacion de D, la diagonal
D <- Diag(valoresPropios)
#creacion de P, los vectores probios
P <-matrix(vectoresPropios, vectoresPropiosTam, vectoresPropiosTam)
#creacion de inverso P, con error si la matriz no tiene inverso
pInverso <- tryCatch({
  solve(P)
}, error = function(e){print("La matriz de P no tiene inverso")})

#Mostrar en pantalla los parametros
print("La matriz A definida por ")
mp
print("Tiene la siguiente digonal D: ")
D

print("Una Matriz de vectores propios P de: ")
P

print("Y el inverso de P es: ")
pInverso
print("Y la multiplicacion de P*D*Pinverso es")
P %*% D %*% pInverso
print("Y esto equivale a A")
#for loop para multiplicar D
dim = seq(0,k,1)
for(val in dim){
  D = D %*% D
}
cat("Y D a un número k, donde k = ", k, " es igual a")
D
cat("Finalmente (P)(D)^k(Pinverso) es igual a A^k, y eso es representada en la matriz: ")
P %*% D %*% pInverso
