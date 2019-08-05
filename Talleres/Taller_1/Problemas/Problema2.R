# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

raizCuadrada <- function(n, e, x){
  eActual <- c()
  eAnterior <- c()
  k <- 0
  y <- (1/2)*(x+(n/x))
  k <- abs(x-y)
  while(k > e){
    x <- y
    y <- (1/2)*(x+(n/x))
    eAnterior <- c(eAnterior, k)
    k <- abs(x-y)
    eActual <- c(eActual, k)
  }
  # Inserción de puntos y líneas en la gráfica
  points(eActual, eAnterior, col = "red")
  lines(eActual, eAnterior, col = "blue")
  return(cat("El resultado es: ", y, " con error de ", e))
}
f = function(x) (x)
plot(f, xlim=c(0,60), ylim=c(0,60), col = "white", xlab="Error actual",ylab="Error anterior ", main= "Error actual vs Error anterior")
abline(h=0,col="black")
raizCuadrada(7, 0.00000001, 100)