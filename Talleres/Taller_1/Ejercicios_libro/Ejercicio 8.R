# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

#determine la raiz real de sin(x)=ln(x)

Fx = function(x) sin(x)-log(x);
respuesta <- uniroot(Fx, c(2,3))
cat("La raiz real de la funcion sin(x)=ln(x) es: ", respuesta$root)

#Algoritmo de biseccion

f <- function (x)
{
  return (sin(x)-log(x))
}

#Intervalo [0,2]
a = 1
b = 4
ai = a
bi = b
#Error
E = (b-a)

#Tolerancia
error <- 1e-8

#Inicialización de variables
m<-0
i <-0

#Listas para graficar
listaErrorAnt = c()
listaIteraciones = c()
listaErrorAct = c()

if( a == 0)
{
  m2 = 1.63848876953125 
}
if( a == 1 )
{
  m2 = 0.55377197265625
}

while(E > error)
{
  
  m<-(a+b)/2
  eAnterior = E
  E<-E/2
  
  if (f(a)*f(m) < 0)
  {
    b = m
  }
  
  if (f(b)*f(m) < 0)
  {
    a = m
  }
  
  if(i>0)
  {
    listaErrorAnt = c(listaErrorAnt , eAnterior)
    listaIteraciones = c(listaIteraciones, i)
    listaErrorAct = c(listaErrorAct, E)
  }
  
  if(i == 0)
  {
    #Par imprimir estado
    cat("---------------------------------------------------------------------------\n")
    cat(formatC( c("a","b","m","Error est.","Error ant."), width = -15, format = "f", flag = " "), "\n")
    cat("---------------------------------------------------------------------------\n")
  }
  
  #Imprimir el estado
  cat(formatC( c(a,b,m,E,eAnterior), digits=7, width = -15, format = "f", flag = " "), "\n")
  
  i = i+1
}

#graficando h(x) vs x
#plot.function(f,0,2, pch =19, main = "h(x) en función de x",xlab = " X ",ylab = " h(x) ",col ="black")


#graficando relación de error
plot.function(f, xlim=c(0,1), ylim=c(0,1),  main = "Relación error",xlab = " Error i ",ylab = " Error i+1 ",col ="white")

#imprime la relación de error -> Convergencia lineal
points(listaErrorAnt, listaErrorAct, col = "blue")
lines(listaErrorAnt, listaErrorAct, col = "blue")

lstRaices= c(m,m2)
lst0 = c(0,0)
#points(lstRaices,lst0,col="red")
abline(h = 0, v = 0:2/2, lty = 3, col = "gray")
cat("Cero de f en [",ai,",",bi,"] las raíces son: ", m ,  "con error <=", E, " El numero de iteraciones es: ", i)