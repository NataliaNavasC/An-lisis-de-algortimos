# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

errorTruncamiento = function(maxDigitos, numero){
  temp = numero
  contador = 0
  #normalizar
  while(temp>1){
    temp = temp/10
    
    contador = contador+1
  }
  tempTruncado = trunc(temp*10^maxDigitos)/10^maxDigitos
  respuesta = (temp - tempTruncado)*10^(contador-1)
  print(respuesta)
  
}
errorTruncamiento(4, 5367.8)
