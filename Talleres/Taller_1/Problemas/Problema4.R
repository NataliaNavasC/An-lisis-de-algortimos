# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

calculoError = function(vel, tiempo, eVel, eTiempo){
  errorAbsoluto <- (vel*eVel) + (tiempo*eTiempo)
  distancia <- vel*tiempo
  errorRelativo <- (eVel/vel)+(eTiempo/tiempo)
  cat("La distancia recorrida es ", distancia, " con un rango de error ",
      distancia-errorAbsoluto, " <= ", distancia, " <= ", distancia+errorAbsoluto,
      " \nEl error relativo es ", errorRelativo*100, "%")
}

calculoError(4,5,0.1,0.1)