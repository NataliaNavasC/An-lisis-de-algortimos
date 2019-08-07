# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------
raiz <- function(n, val, x_0, err) {
  x_k = x_0
  deltax_k = 1
  it = 0
  while (abs(deltax_k) > err){
    deltax_k = ( ( val/(x_k**(n-1)) ) - x_k )/n
    x_k = x_k + deltax_k
    it = it + 1
  }
  cat("Iteraciones ",it, "\n")
  return (x_k)
}
n = 5
val = 789
x_0 = 9
err = 1e-8
cat("El resultado es", raiz(n,val,x_0,err))
#val es el valor dentro de la raiz y n corresponde al índice, partiendo de un
#valor x_0 y con un error estimado de 1e-8.
#Para hacer una expresión equivalente, se calcula (val)^(1/n).