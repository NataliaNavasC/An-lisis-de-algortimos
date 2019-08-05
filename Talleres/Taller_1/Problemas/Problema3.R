# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

DD <- function(expr, name, order = 1) {
  if(order < 1) stop("’order’ must be >= 1")
  if(order == 1) D(expr, name)
  else DD(D(expr, name), name, order - 1)
}

taylor = function(f, x0, a, n){ # f es tira
  # parse devuelve una expresión
  g = parse(text=f)
  # convertir en función
  fx = function(x){eval(g[[1]])}
  # almacenar los sumandos
  sumds = rep(NA, length=n+1)
  for(k in 1:n){
    g. = DD(g,"x", k)
    fp = function(x) eval(g.)
    sumds[k]=1/factorial(k)*(x0-a)^k *fp(a)
  }
  sumds[n+1] = fx(a)
  sum(sumds)
}
cat("El resultado es ", round(taylor("exp(x)", 0.5, 1, 6),4))