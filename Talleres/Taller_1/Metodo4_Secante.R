# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------
rm(list=ls())

Fx <- function(x) ((exp(1)^x) - (pi*x))
F1x <- function(x) ((exp(1)^x)-pi)
# Metodo de la Secante

secante <- function(x0,x1, err) {
  x = (Fx(x1)*x0-Fx(x0)*x1)/(Fx(x1)-Fx(x0))
  eActual <- c()
  eAnterior <- c()
  error = 1
  it = 0
  while (error > err) {
    it = it + 1
    x0 = x1
    x1 = x
    x = (Fx(x1)*x0-Fx(x0)*x1)/(Fx(x1)-Fx(x0))
    if (Fx(x) == 0) break
    if(it > 1){
      eActual <- c(eActual, error)
      eAnterior <- c(eAnterior, aux)
    }
    aux = error
    error = abs(Fx(x)/F1x(x))
    cat("X=",x,"\t","E=",error,"\t","Iteracion=",it,"\n")
  }
  # Insercion de puntos y lineas en la gráfica
  points(eActual, eAnterior, col = "red")
  lines(eActual, eAnterior, col = "blue")
}
plot(Fx, xlim=c(0,0.25), ylim=c(0,1), col = "white", xlab="Error actual",ylab="Error anterior ", main= "Error actual vs Error anterior")
abline(h=0,col="black")
secante(0, 1, 10e-8)
secante(1, 2, 10e-8)