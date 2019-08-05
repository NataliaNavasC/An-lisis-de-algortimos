Fx <- function(x) exp(x) - x*pi
Gx <- function(x) exp(x) / pi



puntoFijo <- function(a,b) 
{
  
  # Listas que contendrán los valores a graficar
  listaErrorAnt = c()
  listaErrorAct = c()
  
  # Imprimir estado
  cat("---------------------------------------------------------------------------\n")
  cat(formatC( c("Iteración","G(x)","x","Error est."), width = -20, format = "f", flag = " "), "\n")
  cat("---------------------------------------------------------------------------\n")
  
  if((Gx(a)-a)*(Gx(b)-b) < 0)
  {
    x<-(a+b)/2
    i<-0
    dx = 0
    tol = 1e-8
    x2 = 1.63848876953125
    while (Gx(x) != x ) 
    {    
      eAnterior = dx
      dx<-abs(a-b)/2
      
      if(dx > tol)
      {
        if (Gx(x) < x)
        {
          b <- x
        }
        else {a <- x}
      }
      else 
      {
        break
      }  
      x<-(a+b)/2
        
      i<-i+1
      #cat("I=",i,"\tG(x) =",Gx(x),"\tX=",,"\tE=",dx,"\n")
      cat(formatC( c(Gx(x),signif(x, digits = 5),dx,eAnterior), digits=15, width = -15, format = "f", flag = " "), "\n")
      if(i>1)
      {
        listaErrorAnt = c(listaErrorAnt , eAnterior)
        listaErrorAct = c(listaErrorAct, dx)
      }
        
    }
    #imprime la relación de error -> Convergencia lineal
    points(listaErrorAnt, listaErrorAct, col = "blue")
    lines(listaErrorAnt, listaErrorAct, col = "blue")
    
    cat("I = ", i, " Raíces = ", x," y  ", x2, " Error estimado <= ", dx)
  }
  else
  {
    cat("No tiene raíz la funcion en ese intervalo\n")
  }
}
options(digits = 10)
#graficando relación de error
plot.function(Gx, xlim=c(0,0.5), ylim=c(0,1),  main = "Relación error",xlab = " Error i ",ylab = " Error i+1 ",col ="white")
abline(h = 0, v = 0:2/2, lty = 3, col = "gray")
options(digits = 10)
puntoFijo(0,1)