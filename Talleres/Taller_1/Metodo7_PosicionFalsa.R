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

falsaPosicion <- function(a, b, err) {
  eActual <- c()
  eAnterior <- c()
  x = seq(a,b,0.1)
  #plot(x,Fx(x),type="l",col="blue")
  #abline(h=0,col="blue")
  it = 0
  error = 1
  aux <- 1
  while (error > err) {
    it = it + 1
    x = (Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
    
    if (Fx(x) == 0) {
      break
    }
    
    if (Fx(x)*Fx(a) < 0) {
      b = x
    }
    else {
      a = x
    }
    if(it > 1){
      eAnterior <- c(eAnterior, aux)
      eActual <- c(eActual, error)
    }
    aux <- error
    error = abs(Fx(x)/F1x(x))
    
    #points(rbind(c(x,0)),pch=19,cex=0.7,col="red")
    
    cat("X=",x,"\t","E=",error,"\t","Iteracion=",it,"\n")
  }
  # Insercion de puntos y lineas en la gráfica
  points(eActual, eAnterior, col = "red")
  lines(eActual, eAnterior, col = "blue")
}
plot(Fx, xlim=c(0,0.92), ylim=c(0,0.92), col = "white", xlab="Error actual",ylab="Error anterior ", main= "Error actual vs Error anterior")
abline(h=0,col="black")
falsaPosicion(0, 1, 10e-8)
falsaPosicion(1, 2, 10e-8)