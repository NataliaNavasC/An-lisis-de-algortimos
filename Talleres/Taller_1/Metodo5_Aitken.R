# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

f <- function(x){
  return ((exp(1)^x) - (pi*x))
}

vectorAux <-0
biseccion <- function(a,b,i){
  e <- b-a
  error <- 1e-8
  x <- 0
  iteraciones <- 0
  vectorAux <- 0
  while (error < e & iteraciones < i ){
    x <- (a+b)/2
    if(f(x)*f(a) < 0 )
      b = x
    if(f(x)*f(b) < 0)
      a = x
    
    e <- (b-a)/2
    vectorAux <- c(vectorAux,x)
    iteraciones <- iteraciones+1
  }
  vectorAux <<- vectorAux
  e <<- e
  return(x)
}
aitken <- function(x,xPos1,xPos2){
  
  resultado = xPos2 - (((xPos2 - xPos1)^2)/(xPos2 -2*xPos1+x)) 
  
  return(resultado)
}
i <-18
iteracion <- 3
biseccion(0,2,i)
while(iteracion < i ){
  cat("i= ",iteracion," x=", aitken(vectorAux[iteracion-2],vectorAux[iteracion-1],vectorAux[iteracion]),"\n")
  iteracion <- iteracion +1
}
cat("Resultado sin aceleracion: ", biseccion(0,1,i), "\n")
cat("Resultado con aceleracion: ", aitken(vectorAux[i-3],vectorAux[i-2],vectorAux[i-1]), "\n")
cat("Valor real de la solucion: 0.55382701")
cat("Resultado sin aceleracion: ", biseccion(1,2,i), "\n")
cat("Resultado con aceleracion: ", aitken(vectorAux[i-3],vectorAux[i-2],vectorAux[i-1]), "\n")
cat("Valor real de la solucion: 1.63853")
