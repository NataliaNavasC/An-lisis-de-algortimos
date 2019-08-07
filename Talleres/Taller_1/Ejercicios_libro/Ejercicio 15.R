# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------


#riemman sum
suma <- 0
xDelta <- 0
xAsterisco <- 0
xAnterior <- 5 - exp(0)
i = 1
f = function(q) 5 - exp(q)
integral = 0
z <- 0
pasos = c()


valorEnZ = f(z)

x <- 0:1
y <- 5 - exp(x)
plot(x, y)
lines(x, y)
polygon(cbind(c(min(x), x, max(x)), c(min(y), y, min(y))), col="#00CC66")
while(integral<2.00){
  integral = integrate(f, 0, z)$value
  pasos = c(pasos, z)
  valorEnZ<-f(z)
  
  segments(x0=z,y0=0,x1=z,y1=valorEnZ,col="red")
  z = z + 0.01
  
}

cat("Cuando X = ", z-1, " la integral es igual a 2 con un error menor a ", 1e-2, " este número fue obtenido al usar la suma de Riemann")





