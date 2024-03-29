### descartando sujetos por apretar X % la misma tecla de confianza

discard_by_x_same_confidence_new <- function(percent,df_total){

  Conf1 <- rep(NaN, length(unique(df_total$sujetos)))
  Conf2 <- rep(NaN, length(unique(df_total$sujetos)))
  Conf3 <- rep(NaN, length(unique(df_total$sujetos)))
  Conf4 <- rep(NaN, length(unique(df_total$sujetos)))
  existing_subject <- unique(df_total$sujetos)
  
  for (i in 1:length(existing_subject)) {
    Conf1[i] <- unique(df_total[df_total$sujetos == existing_subject[i],'confidence_key_1'])
    Conf2[i] <- unique(df_total[df_total$sujetos == existing_subject[i],'confidence_key_2'])
    Conf3[i] <- unique(df_total[df_total$sujetos == existing_subject[i],'confidence_key_3'])
    Conf4[i] <- unique(df_total[df_total$sujetos == existing_subject[i],'confidence_key_4'])
  }
  
  df_conf <- data_frame(Participantes = existing_subject,
                        Conf1 = Conf1,
                        Conf2 = Conf2,
                        Conf3 = Conf3,
                        Conf4 = Conf4)
  
  
  existing_subject <- unique(df_total$sujetos)
  
  # saco el X % del total
  filtro <- round((percent*130)/100)
  sujetos_a_descartar <- c()
  j <-1
  for (i in 1:nrow(df_conf)) {
    se_descarta <- FALSE
    df_subj <- df_conf[df_conf$Participantes == existing_subject[i],]
    if(df_subj$Conf1 > filtro){se_descarta <- TRUE}
    if(df_subj$Conf2 > filtro){se_descarta <- TRUE}
    if(df_subj$Conf3 > filtro){se_descarta <- TRUE}
    if(df_subj$Conf4 > filtro){se_descarta <- TRUE}
    
    if(se_descarta == TRUE){
      sujetos_a_descartar <- c(sujetos_a_descartar, existing_subject[i])
      j <- j+1}
  }
  
  return(sujetos_a_descartar)
}
