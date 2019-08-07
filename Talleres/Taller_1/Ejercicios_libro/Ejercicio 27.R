# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

##Código para grafica polar
#-0.2803467164  y  1.63852842
#install.packages("pracma")

#install.packages("rootSolve")
library(pracma)
library(rootSolve)
#Codigo de biseccion del paquete pracma. Esta versión permite cambiar la tolerancia

f = function(x) (3*(sin(x))^3-1)-(4*sin(x)*cos(x))
print(uniroot.all(f, c(0, pi)))
bisectPracma <- function(f, a, b, maxiter=100, tol=10^-5)
  # Bisection search for zero of a univariate function in a bounded interval
{
  if (f(a)*f(b) > 0) stop("f(a) and f(b) must have different signs.")
  x1 <- min(a, b); x2 <- max(a,b)
  xm <- (x1+x2)/2.0
  n <- 0
  while (abs(x1-x2)/2.0 > tol) {
    n <- n+1
    if (abs(f(xm)) <= tol) break
    if (f(x1)*f(xm) < 0) {
      x2 <- xm
    } else {
      x1 <- xm
    }
    xm <- (x1+x2)/2.0  # xm <- x1 - f(x1) * (x2-x1) / (f(x2)-f(x1))
    if (n >= maxiter) break
  }
  return(list(root=xm, f.root=f(xm), iter=n, estim.prec=abs(x1-x2)/2.0))
}
respuestaBiseccion = bisectPracma(f, 0, 2, 100, 10^-5)


#multiroot emplea el metodo de newton ralpson segund la documentaciónn.
respuestaNewton = newton(f, 1,NULL,100, 10^-5)


print(respuestaNewton)
print(respuestaBiseccion)
#funciones para graficar los puntos
graficarPuntoPolar <- function(angulo, f, color, forma){
  r <- f(angulo)
  y <- r*sin(angulo)
  x <- r*cos(angulo)
  points(x,y,pch=forma,cex=1, col=color)
}



dim <- seq(0, pi/2, by=pi/300) 
x =(3*(sin(dim))^3-1)
y = (4*sin(dim)*cos(dim))
fPolar = (3*(sin(dim))^3-1)-(4*sin(dim)*cos(dim))
#graficar las funciones x y y
#función polar del paquete pracma
polar(dim, x, "l", "blue", main= "X - Y")
polar(dim, y, "l", "green", add=TRUE)
#polar(dim, fPolar, "l", "red", add=TRUE)
graficarPuntoPolar(respuestaBiseccion$root, f, "red", 23)
graficarPuntoPolar(respuestaNewton$root, f, "yellow", 1)
cat("La respuesta del algoritmo de Newton fue: ", respuestaNewton$root, " con valor en la ecuación f(respuestaNewton) de: ", respuestaNewton$f.root, ". A este resultado se llegó en ", respuestaNewton$niter , " iteraciones. Error estimado de : ", respuestaNewton$estim.prec)

cat("La respuesta del algoritmo de Biseccion fue: ", respuestaBiseccion$root, " con valor en la ecuación f(respuestaBiseccion) de: ", respuestaBiseccion$f.root, ". A este resultado se llegó en ", respuestaBiseccion$iter , " iteraciones. Error estimado de : ", respuestaBiseccion$estim.prec)
