### get the PID-5 score

puntaje_pid <- function (cant_sujetos,cant_componentes_por_sujetos,ubicacion_comp1_pid){
  # cant_sujetos = cantidad de sujetos; 
  # cant_componentes_por_sujetos = cantidad de componentes en un sujeto;
  # ubicacion_comp_pid = la ubicacion del primer componente pid en el primer sujeto
  
  # res[[4]] primera parte
  # res[[5]] segunda parte
  # 1 = Muy falso o a menudo falso
  # 2 = A veces o algo falso
  # 3 = A veces o algo verdadero
  # 4 = Muy cierto o a menudo verdadero
  
  puntaje.sujetos <- rep(NaN, cant_sujetos)
  
  # puntar un solo sujeto
  for (i in 1:220) {
    parte1 <- res[[ubicacion_comp1_pid]]$value
    parte2 <- res[[5]]$value
  }

}

