library(pracma)
library(Matrix)

#---------------------Punto 1 extra ----------------------

#Punto 1 
n = 6
rrr =sample(0:20, 36, replace = TRUE)
A = matrix( rrr, nrow=6, byrow=TRUE)
print(A)

condicional = 0
while(condicional< 1000 )
{
  A = matrix( sample(10:20, 36, replace = TRUE), nrow=n, byrow=TRUE)
  condicional = cond(A)
}
print("A")
print(A)

condicional = (cond(A))
print(condicional)
b = matrix( c(1,2,3,4,5,6), nrow = 1, byrow=TRUE)
print("b")
print(b)

if(condicional > 1000)
{
  print(condicional)  
  diagonal <- function(M) 
  {
    M[col(M)!=row(M)] <- 0
    return(M)
  }
  
  
  #T = -D^-1(L + U)
  D = diagonal(A)
  L = tril(A,k=-1)
  U = triu(A,k=1)
  
  T = (-solve(D))%*%(L+U)
  print("Matriz de transición")
  print(T)
  print("Norma")
  norma <- norm(T,"F")
  print(norma)
  print("Radio espectral de la matriz")
  
  ev <- eig(A)
  abev <- abs(ev)
  radioExp <- max(abev)
  print( radioExp)
  
  
  # Matriz diagonal de dimension 3
  I=diag(1,nrow = nrow(A))
  # Matriz inversa de A
  D1 <- solve(D,I)
  T1 = D1 %*% U
  T2 = (I + (L %*% D1))
  # Matriz inversa de A
  T2<- solve(T2,I)
  
  #Analisi de convergencia
  MatTG = T1+T2
  normaG = norm(MatTG, type = c( "I"))
  print("Norma/convergencia de Gauss")
  print(normaG)
  print("Matriz de trancision de Gauss")
  print(MatTG)
  
  MatTJ = (-D1)%*%(L+U)
  normaJ = norm(MatTJ, type = c("I"))
  print("Norma/convergencia de Jacobi")
  print(normaJ)
  print("Matriz de trancision de Jacobi")
  print(MatTJ)
  
  
}
#------------------------- PUNTO 2  extra -------------------

# Matrices necesarias
n = 3
A = matrix(c(8,9,2,
             2,7,2,
             2,8,5), nrow=n, byrow=TRUE)
print(A)

b = matrix(c(69,47,88), nrow=3, byrow=TRUE)
print(b)



U = A
L = A
L[lower.tri(L,diag=TRUE)] <- 0
U[upper.tri(U, diag = TRUE)] <- 0


D = diag(diag(A))
# Matriz diagonal de dimension 3
I=diag(1,nrow = nrow(A))
# Matriz inversa de A
D1 <- solve(D,I)
T1 = D1 %*% U
T2 = (I + (L %*% D1))
# Matriz inversa de A
T2<- solve(T2,I)

MatTG = T1+T2
normaG = norm(MatTG, type = c( "I"))
print("Norma de Gauss")
print(normaG)
print("Matriz de trancision de Gauss")
print(MatTG)

MatTJ = (-D1)%*%(L+U)
normaJ = norm(MatTJ, type = c("I"))
print("Norma de Jacobi")
print(normaJ)
print("Matriz de trancision de Jacobi")
print(MatTJ)





#---------------PUNTO 1---------------
# 1. Para el siguiente ejercico, instale el paquete "pracma"

#-- a --
#Revise las siguientes funciones con la matriz del ejercicio 2


#Tamaño de la matriz
n = 4
A = matrix(c(-2, 5, 6, 19,
             15, 12,8, 2,
             0, 1, 5, 6,
             1, 7, 1, 2), nrow=n, byrow=TRUE)
print("A")
print(A)

b = matrix(c(1.45,3,5.12,-4), nrow=n, byrow=TRUE)
print("b")
print(b)


D1 <- eye(n, m = n)
D2 <- ones(n, m = n)
D3 <- zeros(n, m = n)

print("D1")
D1
print("D2")
D2
print("D3")
D3

#El comando eye(n, m = n) permite obtener la matriz identidad de tamaño n, la cual está compuesta de ceros a excepción de la diagonal principal que contiene unos.
#El comando ones(n, m = n) permite obtener una matriz de tamaño n contenida únicamnete por unos.
#El comando zeros(n, m = n) permite obtener una matriz de tamaño n compuesta de ceros.



#-- b --
# b. Evalue la matriz de transicionn para el metodo SOR

U = A
L = A

#Aca se asignan las respectivas matrices diagonalizadas a cada variable
U[upper.tri(U, diag = TRUE)] <- 0
L[lower.tri(L,diag=TRUE)] <- 0
print (A)

#Obtienee la diagonal principal de la matriz
D1 = diag(diag(A^-1))

#Multiplicación de la matriz
T1 = D1 %*% U
I = D1
T2 = (I + (L %*% D1))
MatT = T1+T2
print(MatT)





#---------------PUNTO 2---------------
#2. Dada la siguiente matriz, utilice las funciones del paquete para descomponer la matriz $A=L+D+U$ (Jacobi)
#----- a -----


U = A
L = A
U[lower.tri(U,diag=TRUE)] <- 0
L[upper.tri(L, diag = TRUE)] <- 0
print (A)
D = diag(diag(A))
#Imprimiendo las tres matrices obtenidas
D
U
L

#------ b ------
#b. Utilice la funcion  itersolve(A, b, tol , method = "Gauss-Seidel") y solucionar el sistema asociado a la matriz $A$ con $b=[1.45,3,5.12,-4]^{t}$ con una tolerancia de $1e^-9$

print("Método de Gauss-Seidel:")
tol = 1e-9
sol = itersolve(A, b, x0=c(1,2,1,1), tol=1e-9 , method = "Gauss-Seidel")
print(sol)


#------ c ------
#c. Genere 5 iteraciones del metodo de Jacobi, calcular error relativo para cada iteracion

x0 <- c(0,0,0,0)
#For para validar las 5 iteraciones
for (i in 1:5)
{
  x <- itersolve(A, b, x0 ,nmax=i, method = "Jacobi")
  x1 <- itersolve(A, b, x0 ,nmax=i+1, method = "Jacobi")
  error = x1[[1]] - x[[1]]
  cat ("Error en la iteracion ",i," es ", error,"\n")
}



#---------------PUNTO 3---------------
#a. Implemente una funcion en R para que evalue las raices del polinomio caracteristico asociado a la matriz $A$

poli = charpoly(A, info = TRUE)
print(poli)

# b. Use el teorema de convergencia para determinar cual metodo iterativo es mas favorable.

# Matrices necesarias

U = A
L = A
L[lower.tri(L,diag=TRUE)] <- 0
U[upper.tri(U, diag = TRUE)] <- 0


D = diag(diag(A))
# Matriz diagonal de dimension 3
I=diag(1,nrow = nrow(A))
# Matriz inversa de A
D1 <- solve(D,I)
T1 = D1 %*% U
T2 = (I + (L %*% D1))
# Matriz inversa de A
T2<- solve(T2,I)

#Analisi de convergencia
MatTG = T1+T2
normaG = norm(MatTG, type = c( "I"))
print("Norma/convergencia de Gauss")
print(normaG)
print("Matriz de trancision de Gauss")
print(MatTG)

MatTJ = (-D1)%*%(L+U)
normaJ = norm(MatTJ, type = c("I"))
print("Norma/convergencia de Jacobi")
print(normaJ)
print("Matriz de trancision de Jacobi")
print(MatTJ)


# c. Evalue la matriz de trancision para cada caso y en el caso del metodo de relajacion determine el valor optimo de $\omega$

print("Matriz transicion Gauss")
print(MatTG)
print("Matriz transicion Jacobi")
print (MatTJ)

# d. Teniendo en cuenta lo anterio resolver el sistema y comparar con la solucion por defecto
#Sol por jacobi
X <- itersolve(A, b, method = "Jacobi")
print(X)
#Sol por Gauss-Seide
X <- itersolve(A, b, tol = 1e-9 , method = "Gauss-Seidel")
print(X)
#Sol por defecto
solucion<- solve(A,b)
print(solucion)

#------------------ PUNTO 3' ------------------
#a. Pruebe el siguiente algoritmo con una matriz $A_{3}$, modifiquelo para quue $a_{ii}=0$ para todo $i$

tril1 <- function(M, k)
{
  if (k != 0) {
    M[!lower.tri(M, diag = TRUE)] <- 0
    M[!upper.tri(M, diag = TRUE)] <- 0
  } else {
    M[col(M) == row(M) + k ] <- 0
  }
  return(M)
}
M = matrix(c(10,9,5,
             9,8,3,
             6,4,7), nrow = 3, byrow = TRUE)
ttt<- tril1(M, 0)
print(ttt)

#b. Implemente una funcion en R para que dada una matriz $A$ se obtenga una matriz diagonal $D$ donde en la diagonal estan los mismo elementos de A

#Funcionobetener la  diagonal principal de una matriz
diagonal <- function(M)
{
  M[col(M)!=row(M)] <- 0
  return(M)
}
M = matrix(c(2,3,4,1,2,3,5,6,7),nrow=3)
print(M)
#Imprime la diagonal principal de la matriz
print(diagonal(M))



#-------------------------  PUNTO 4 -----------------------

#Punto 4
#Cree una funcion que cuente el numero de multiplicaciones 
#en el metodo directo de Gauss Jordan, para resolver un sistema 
#de nn ecuaciones y pruebelo para n=5

numMult = function(A, b){ # Se supone det(A) != 0
  mult = 0
  n = nrow(A) # = ncol(A) para que sea cuadrada
  
  # matriz ampliada
  Ab = cbind(A,b)
  print(Ab)
  # Eliminación
  for (k in 1:(n-1)){ # desde columna k=1 hasta k=n-1
    if(Ab[k,k]==0){ # intercambio de fila
      fila = which(Ab[k, ]!=0)[1]
      Ab[c(k, fila), ] = Ab[c(fila, k), ]
    }
    
    # Eliminación columna k
    for (i in (k+1):n){# debajo de la diagonal
      # Fi = Fi - a_ik/a_kk * Fk, i=k+1,...,n
      Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
      mult = mult + 2*(ncol(Ab))
    }
  }
  
  # Sustitución hacia atrás-------------------------
  # b(i) = A[i, n+1]
  x = rep(NA, times=n)
  x[n] = Ab[n, n+1]/Ab[n,n] # xn = bn/a_nn
  mult = mult + n+1
  
  for(i in (n-1):1 ){
    x[i]= (Ab[i, n+1] - sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i]
    mult = mult + 2*(n-2)
  }
  #cat(x, "\n")
  cat("Numero de multiplicaciones:", mult, "\n")
  return(x)
}

A = matrix(c( 0, 2, 3, 3, 3,
              -5, -4, 1, 4, 5,
              0, 0, 0, 3, 7,
              -4, -7, -8, 9,7,
              3, 4, 5, 5, 6), nrow=5, byrow=TRUE)
b = matrix(c(1,0,0,0,1), nrow=5, byrow=TRUE)
cat("Punto 4: ","\n")
numMult(A,b)


#-------------------- PUNTO 5 --------------------
#Punto a. Encuentre el valor de $\alpha$ y $\beta$ para asegura la convergencia por el mÃ©todo de Jacobi
#Se llega a los valores de alpha y beta por las operaciones de +
# alpha > 1+1 
# beta < 3
# de acuerdo a su posiscion en  la matrix

# b. Genere una tabla que tenga 10 iteraciones del mÃ©todo de Jacobi con vector inicial $x_{0}=[1,2,3]^t

beta = 0
alpha = 3

A = matrix(c(2, 0, 1,
             beta,2 , -1,
             -1, 1, alpha), nrow=3, byrow=TRUE)
B = matrix (c(1,2,1),nrow=3, byrow=TRUE)
Ab = cbind(A,B)

print(Ab)




#----------------------- PUNTO 6-----------------------
A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
print("A")
print(A)
b = matrix(c(1.45,3,5.12,-4), nrow=4, byrow=TRUE)
print("b")
print(b)

Ab = cbind(A,b)
print(Ab)

#matrices diagonales
L = tril(A,k=-1)
U = triu(A,k=1)
print(L) 
print(U)

#factorizacion QR
gs <- gramSchmidt(A)
(Q <- gs$Q); (R <- gs$R)
print(Q)
print(R)
print(Q %*% R)  # = A

#----------------------- PUNTO 7 -----------------------
#a
library(BB)
ecuaciones = function(x) {
  n = length(x)
  F = rep(NA, n)
  F[1] = x[1] - x[2]
  F[2] = x[1]^2 + x[2]^2 -1
  F
}
p0 = c(1,1) # n initial starting guess
sol = BBsolve(par=p0, fn=ecuaciones)
sol$par

plot(sol$par)
plot(ecuaciones)

#b
trigexp = function(x) {
  
  #se obtiene el numero de variables del sistema
  n = length(x)
  #se llena F con n elementos vacíos
  F = rep(NA, n)
  #Se ingresan las ecuaciones del sistema
  #Primera ecuación: F[1] = 3(x_1)^2 + 2(x_2) - 5 + sin( (x_1)-(x_2) )( sin( (x_1)+(x_2) ) )
  F[1] = 3*x[1]^2 + 2*x[2] - 5 + sin(x[1] - x[2]) * sin(x[1] + x[2])
  #tn1 es una secuencia de 2 hasta n-1
  tn1 = 2:(n-1)
  #Se ingresan |tn1| ecuaciones, es decir desde la ecuación 2 hasta la n-1
  F[tn1] = -x[tn1-1] * exp(x[tn1-1] - x[tn1]) + x[tn1] *
    ( 4 + 3*x[tn1]^2) + 2 * x[tn1 + 1] + sin(x[tn1] -
                                               x[tn1 + 1]) * sin(x[tn1] + x[tn1 + 1]) - 8
  #Se ingresa la ecuación n
  F[n] = -x[n-1] * exp(x[n-1] - x[n]) + 4*x[n] - 3
  F
}
n = 10000
p0 = runif(n) # n initial random starting guesses
#se resuelve el sistema trigexp con BBsolve, utilizando n valores iniciales
sol = BBsolve(par=p0, fn=trigexp)
#Muestra por pantalla la solución del sistema para cada uno de los n valores iniciales
sol$par
#----------------------- Punto 8 -----------------------

N <- 3
A <- Diag(rep(3,N)) + Diag(rep(-2, N-1), k=-1) + Diag(rep(-1, N-1), k=1)
x0 <- rep(0, N)
b = c(4,5,6)
itersolve(A, b, tol=1e-9 , method = "Gauss-Seidel")