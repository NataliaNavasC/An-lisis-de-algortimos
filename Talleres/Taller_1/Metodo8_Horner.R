# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

horner <- function(coeficiente, x){
  resultado <- coeficiente[1]
  iteraciones <-0
  
  for(k in coeficiente[2:length(coeficiente)]){
    resultado <- x*resultado + k
    iteraciones <- iteraciones + 2
  }
  return(cat("El resultado es: ", resultado, "\nEl numero minimo de operaciones es: ", iteraciones,"\nSon ",iteraciones/2," multiplicaciones y ",iteraciones/2, " sumas"))
}
x0 <- -2
coeficiente <- c(2,0,-3,3,-4)
horner(coeficiente,x0)