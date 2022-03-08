######################
## Preprocesamiento ##
######################

####### Data Preprocessing:

# Read the .txt results from JATOS and perform the data preprocessing.

# (To read the .txt results from JATOS it was necesary to add an "enter" before the curly brackets
# that open the component "sincericidio").

library(jsonlite)

# Read the .txt result
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to read the .txt results from JATOS and create a dataframe
source(root$find_file("Analysis/AuxiliaryFunctions/initial_df.R"))

# experiment complete
content<-readLines(root$find_file("Data/Experiment_Complete/jatos_results_20210406141549.txt"))
res<-lapply(content,fromJSON)
df_list <- initial_df(res)

# df_DatosUnicos: for data of each subject.
# df_exp: save each trial of metacognition exp (already created in previous loop)
df_DatosUnicos <- df_list$a
df_exp <- df_list$b
pid1 <- df_list$c
pid2 <- df_list$d

# only survey
content<-readLines(root$find_file("Data/Experiment_OnlySurvey/datos_PIDOnlySurv+Metacog.txt"))
res<-lapply(content,fromJSON)
df_list <- initial_df(res)
df <- df_list$a 
df$sujetos <- df$sujetos + 1000
df_DatosUnicos <- rbind(df_DatosUnicos, df) 
df_exp <- rbind(df_exp, df_list$b)
pid1 <- c(pid1,df_list$c)
pid2 <- c(pid2,df_list$d)

# pid sorteo
content<-readLines(root$find_file("Data/PID5_sorteo/jatos_results_20210826172451.txt"))
res<-lapply(content,fromJSON)
df_list <- initial_df(res)
df <- df_list$a 
df$sujetos <- df$sujetos + 5000
df_DatosUnicos <- rbind(df_DatosUnicos, df) 
df_exp <- rbind(df_exp, df_list$b)
pid1 <- c(pid1,df_list$c)
pid2 <- c(pid2,df_list$d)

# pid sorteo
content<-readLines(root$find_file("Data/PID5_sorteo_final/jatos_results_20211112152407.txt"))
res<-lapply(content,fromJSON)
df_list <- initial_df(res)
df <- df_list$a 
df$sujetos <- df$sujetos + 8000
df_DatosUnicos <- rbind(df_DatosUnicos, df) 
df_exp <- rbind(df_exp, df_list$b)
pid1 <- c(pid1,df_list$c)
pid2 <- c(pid2,df_list$d)

####### add subjects and trials to df_exp

# get the number of trials per subject
cant_trials <- 130

# prepare subject column
sujetos <- rep(df_DatosUnicos$sujetos, each = cant_trials)

# prepare trials column
col_trials <- 1:cant_trials
trials <- rep(col_trials, times = length(df_DatosUnicos$sujetos))

# add columns to df_exp
df_exp$sujetos <- sujetos
df_exp$trials <- trials

####### get the PID-5 scores

# where are the responses values of a the first subject
ubicacion_respuestas_pid1 <- 2
ubicacion_respuestas_pid2 <- 2

# number of subject 
cant_sujetos <- length(df_DatosUnicos$sujetos)

# location of the sublist where are the first part of pid-5 of the first subject
ubicacion_comp1_pid <- 4

# load the function to get the PID5 quotient  
source(root$find_file("Analysis/AuxiliaryFunctions/pid-5.R"))

# get the pid score
puntaje_pid.5 <- puntaje_pid(cant_sujetos,
                             ubicacion_respuestas_pid1,
                             ubicacion_respuestas_pid2,
                             ubicacion_comp1_pid,
                             pid1,
                             pid2)

# add to df_DatosUnicos
df_DatosUnicos <- cbind(df_DatosUnicos,puntaje_pid.5)

######### Adding columns of reaction times

df_exp$t_ensayo_discriminacion <- df_exp$discrimination_t_keydown - 
  df_exp$discrimination_t_onset
df_exp$t_ensayo_confianza <- df_exp$confidence_t_keydown -
  df_exp$confidence_t_onset

## get percentage of correct answers
PC  <- rep(NA, nrow(df_DatosUnicos)) 
existing_subjects <- unique(df_DatosUnicos$sujetos)
for (s in 1:nrow(df_DatosUnicos)){
  PC[s]   <- mean(df_exp$discrimination_is_correct[df_exp$sujetos== existing_subjects[s]])
}

# add to df_DatosUnicos
df_DatosUnicos$PC <- PC

# add difference in dots in every trial to df_exp
df_exp$diferencia_puntitos <- abs(df_exp$dots_num_left- df_exp$dots_num_right)

####### Unifying the format of columns values
# (horaSueno, medicacion affeccionPsico, TeEscuchamos from df_DatosUnicos)

# If the script does not recognize a certain value, it asks the user for help with it 
# (keep an eye on the console). In this case, the user must write the corresponding value 
# that is requested.
# The script saves the response in a df that the script will review the next time it is run.
# The script returns the df_DatosUnicos_mod that will have all the column values in the same format.
# In addition, in this part, the age column of the participants is created and filled
# in from their date of birth.
# It is recommended to run the script by column to unify.

## column: horaSueno

# df that will have the data in a unified and readable format.
# The values in the hoursSleep column are converted to numeric. If not possible in NA
df_DatosUnicos_mod <- transform(df_DatosUnicos, 
                                horasSueno = as.numeric(as.character(horasSueno)))

# the corresponding function is loaded
source(root$find_file("Analysis/AuxiliaryFunctions/numeriza_col_horasSuenos.R"))

df_DatosUnicos_mod <- numeriza_col_horasSuenos(df_DatosUnicos_mod,df_DatosUnicos)

## column: edad

# save the age of the subjects in df_DatosUnicos_mod
library(eeptools)
is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x),
                                                     tz = 'UTC', format = '%Y-%m-%d'))

edad <- rep(NA, nrow(df_DatosUnicos_mod)) 

for(i in 1:nrow(df_DatosUnicos_mod)){
  if(sapply(df_DatosUnicos_mod$fechaNac[i], is.convertible.to.date)){
    a <- as.Date( df_DatosUnicos_mod$fechaNac[i])
    if (a < "2004-01-01"){
      age <- as.integer( age_calc(a, units='years') )
      edad[i] <- age
    } else{
      edad[i] <- NA
    }

  }else{ # if it is not possible to obtain the age it is also a NA
    edad[i] <- NA
  }
}

df_DatosUnicos_mod$edad <- edad

## column: medicacion

# the corresponding function is loaded
source(root$find_file("Analysis/AuxiliaryFunctions/unifica_col_medicacion.R"))

# converts the medication values in: Si, No , noSabe
df_DatosUnicos_mod <- unifica_col_medicacion(df_DatosUnicos_mod,df_DatosUnicos)

## column: affeccionPsico

# the corresponding function is loaded
source(root$find_file("Analysis/AuxiliaryFunctions/unifica_col_affeccionPsico.R"))

# converts the affeccionPsico values in Si, No , noSabe
df_DatosUnicos_mod <- unifica_col_affeccionPsico(df_DatosUnicos_mod,df_DatosUnicos)

## columna: TeEscuchamos

# the corresponding function is loaded
source(root$find_file("Analysis/AuxiliaryFunctions/unifica_col_TeEscuchamos.R"))

# converts the TeEscuchamos values in Si, No , noSabe
df_DatosUnicos_mod <- unifica_col_TeEscuchamos(df_DatosUnicos_mod,df_DatosUnicos) 


### Add the confidence columns to df_DatosUnicos_mod

# Confidence columns for all the subjects
confidence_key_1 <- rep(NA, nrow(df_DatosUnicos_mod))
confidence_key_2 <- rep(NA, nrow(df_DatosUnicos_mod))
confidence_key_3 <- rep(NA, nrow(df_DatosUnicos_mod))
confidence_key_4 <- rep(NA, nrow(df_DatosUnicos_mod))

ExistingSubjects <- unique(df_exp$sujetos)

for(i in 1:nrow(df_DatosUnicos_mod)){
  # confidence columns are created to iterate by subject
  confidence_key_1_total <- 0
  confidence_key_2_total <- 0
  confidence_key_3_total <- 0
  confidence_key_4_total <- 0
  
  df_prueba <- df_exp[df_exp$confidence_key =='1',]
  confidence_key_1_total <- nrow(df_prueba[df_prueba$sujetos== ExistingSubjects[i],])
  confidence_key_1[i] <- confidence_key_1_total
  
  df_prueba <- df_exp[df_exp$confidence_key =='2',]
  confidence_key_2_total <- nrow(df_prueba[df_prueba$sujetos==ExistingSubjects[i],])
  confidence_key_2[i] <- confidence_key_2_total
  
  df_prueba <- df_exp[df_exp$confidence_key =='3',]
  confidence_key_3_total <- nrow(df_prueba[df_prueba$sujetos==ExistingSubjects[i],])
  confidence_key_3[i] <- confidence_key_3_total
  
  df_prueba <- df_exp[df_exp$confidence_key =='4',]
  confidence_key_4_total <- nrow(df_prueba[df_prueba$sujetos==ExistingSubjects[i],])
  confidence_key_4[i] <- confidence_key_4_total
}

# Add the columns to df_DatosUnicos_mod
df_DatosUnicos_mod$confidence_key_1 <- confidence_key_1
df_DatosUnicos_mod$confidence_key_2 <- confidence_key_2
df_DatosUnicos_mod$confidence_key_3 <- confidence_key_3
df_DatosUnicos_mod$confidence_key_4 <- confidence_key_4

## Get the sd and mean of confidence by subject
media_confidence <- rep(NA, nrow(df_DatosUnicos_mod))
sd_confidence <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  media_confidence[i] <- mean(df_exp[df_exp$sujetos==ExistingSubjects[i],"confidence_key"])
  sd_confidence[i] <- sd(df_exp[df_exp$sujetos==ExistingSubjects[i],"confidence_key"])
}

df_DatosUnicos_mod$media_confidence <- media_confidence
df_DatosUnicos_mod$sd_confidence <- sd_confidence

## Get the sd and mean of reaction times by subject in the discrimination task
media_tr_discri <- rep(NA, nrow(df_DatosUnicos_mod))
sd_tr_discri <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  media_tr_discri[i] <- mean(df_exp[df_exp$sujetos==ExistingSubjects[i],"t_ensayo_discriminacion"])
  sd_tr_discri[i] <- sd(df_exp[df_exp$sujetos==ExistingSubjects[i],"t_ensayo_discriminacion"])
}

df_DatosUnicos_mod$media_tr_discri <- media_tr_discri
df_DatosUnicos_mod$sd_tr_discri <- sd_tr_discri

## get the sd and mean of reaction times by subject in the confidence task
media_tr_confi <- rep(NA, nrow(df_DatosUnicos_mod))
sd_tr_confi <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  media_tr_confi[i] <- mean(df_exp[df_exp$sujetos==ExistingSubjects[i],"t_ensayo_confianza"])
  sd_tr_confi[i] <- sd(df_exp[df_exp$sujetos==ExistingSubjects[i],"t_ensayo_confianza"])
}

df_DatosUnicos_mod$media_tr_confi <- media_tr_confi
df_DatosUnicos_mod$sd_tr_confi <- sd_tr_confi

####### Inclusion criteria, data is not included in future analysis
## Comment / uncomment or modify filters as required

cat("Cantidad de sujetos antes de todo filtro: ", nrow(df_DatosUnicos_mod))

##Filter for hours of sleep, leaving me only with > 4
df_DatosUnicos_mod <- df_DatosUnicos_mod[df_DatosUnicos_mod$horasSueno > 4,] 

cat("Cantidad de sujetos luego de filtrar por horas sueno: ", nrow(df_DatosUnicos_mod))

# ## Filter by psychological disorder, staying only with those who do not have.
# df_DatosUnicos_mod2 <- df_DatosUnicos_mod[df_DatosUnicos_mod$affeccionPsico ==
#                                             'No',]

cat("Cantidad de sujetos luego de filtrar por trastorno psi: ", nrow(df_DatosUnicos_mod))

## Filter by medication, leaving only with those who do not take.
# df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$medicacion ==
#                                              'No',]

cat("Cantidad de sujetos luego de filtrar por medicacion: ", nrow(df_DatosUnicos_mod))

## Filter by age, leaving only those who are age > 17, < 60, and are not NA
df_DatosUnicos_mod <- df_DatosUnicos_mod[df_DatosUnicos_mod$edad > 17,]
df_DatosUnicos_mod <- df_DatosUnicos_mod[df_DatosUnicos_mod$edad < 60,]
df_DatosUnicos_mod <- df_DatosUnicos_mod[!is.na(df_DatosUnicos_mod$edad),]

cat("Cantidad de sujetos luego de filtrar por edad: ", nrow(df_DatosUnicos_mod))
## filter in df_exp those who survived inclusion criteria applied to 
## df_DatosUnicos_mod2
library(dplyr)
df_exp <- df_exp %>% 
  filter(df_exp$sujetos %in% df_DatosUnicos_mod$sujetos)

####### putting it all together 

df_total <- df_DatosUnicos_mod[0,]

#  sujetos que quedaron
ExistingSubjects <- unique(df_exp$sujetos)

# iterar por sujeto existente
for (i in 1:length(ExistingSubjects)){
  # saco la cantidad de trials del sujeto
  sujeto_df_exp <- df_exp[df_exp$sujetos== ExistingSubjects[i],]
  cant_trials <- nrow(sujeto_df_exp)
  
  # repito cada fila del sujeto segun la cantidad de trials que le quedaron
  sujeto_df_DatosUnicos_mod <- df_DatosUnicos_mod[df_DatosUnicos_mod$sujetos== ExistingSubjects[i],]
  df <- as.data.frame(lapply(sujeto_df_DatosUnicos_mod, rep, cant_trials))
  
  # lo agrego al df_total
  df_total <- rbind(df_total, df)
}

# combino las columnas de df_exp que me interesan con el df_total
df_total <- cbind(df_total, 
                  discrimination_is_correct = df_exp$discrimination_is_correct,
                  confidence_key = df_exp$confidence_key, 
                  trials = df_exp$trials,
                  diferencia_puntitos = df_exp$diferencia_puntitos, 
                  t_ensayo_discriminacion = df_exp$t_ensayo_discriminacion,
                  t_ensayo_confianza = df_exp$t_ensayo_confianza)

####### save the df_total

# # RESULTS_EXP
filepath <- root$find_file("Data/All_exp_inclusion_criteria/df_total.Rda")
save(df_total,file = filepath)

####### Exclusion criteria, data is excluded of future analysis

cat("Cantidad de sujetos antes de filtrar por criterios de exclusion: ", length(unique(df_total$sujetos)))

## Filter by sincericide, leaving only those who tell us that we can count on their answers.
library (stringr)
library (tidyverse)
df_total <- df_total %>% 
  filter(str_detect(df_total$sincericidio, "Pueden")) # if start with "Pueden"
#                                                                  # it stays

cat("Cantidad de sujetos luego de filtrar por sincericidio: ", length(unique(df_total$sujetos)))

## Filter by TeEscuchamos leaving only those who did not interrup the 
# task drastically (= ok)
df_total <- df_total[df_total$TeEscuchamos == 'ok',] 

cat("Cantidad de sujetos luego de filtrar por te escuchamos: ", length(unique(df_total$sujetos)))

## Filter by performance, leaving only those who have PC > 60 
df_total <- df_total[df_total$PC > 0.60,]

cat("Cantidad de sujetos luego de filtrar por desempeno: ", length(unique(df_total$sujetos)))

## sujetos que tienen un 85 % de trials en una misma respuesta de confianza
source(root$find_file("Analysis/AuxiliaryFunctions/discard_by_x_same_confidence_new.R"))
sujetos_a_descartar <- discard_by_x_same_confidence_new(85,df_total)  
df_total <- df_total[! df_total$sujetos %in% sujetos_a_descartar,]

cat("Cantidad de sujetos luego de filtrar por X% de trials con la misma confianza: ", length(unique(df_total$sujetos)))

cat("Cantidad de trials antes de filtrar por RT: ", nrow(df_total))
## Filter by reaction times
df_total <- df_total[df_total$t_ensayo_discriminacion <= 5000,]
cat("Cantidad de trials luego de filtrar por <5000 en tarea t1: ", nrow(df_total))
df_total <- df_total[df_total$t_ensayo_discriminacion >= 200,]
cat("Cantidad de trials luego de filtrar por >200 en tarea t1: ", nrow(df_total))
df_total <- df_total[df_total$t_ensayo_confianza <=5000,]
cat("Cantidad de trials luego de filtrar por <5000 en tarea t2: ", nrow(df_total))
df_total <- df_total[df_total$t_ensayo_confianza >=0,] 
cat("Cantidad de trials luego de filtrar por >0 en tarea t2: ", nrow(df_total))

## burning the first 20 trials of each subject
df_total <- df_total[df_total$trials > 20,]
cat("Cantidad de trials luego de quemar los primeros 20 trials: ", nrow(df_total))

## Filter by trails needed to calculate AUROC2
## discarding because very few trials
cant_trials_por_sujeto <- rep(NaN, length(unique(df_total$sujetos)))
existing_subject <- unique(df_total$sujetos)

for (i in 1:length(cant_trials_por_sujeto)) {
  cant_trials_por_sujeto[i] <- nrow(df_total[df_total$sujetos == existing_subject[i],])
}

# veo quienes son los que tienen menos trials que X
indices_cant_trials <- which(cant_trials_por_sujeto < 70) # en el de tea esta en 90 este valor
subj_pocos_trials<- existing_subject[indices_cant_trials]

# los descarto
df_total <- df_total[! df_total$sujetos %in% subj_pocos_trials,]

cat("Cantidad de sujetos luego de filtrar por trials insuficientes para calcular AUROC2: ", length(unique(df_total$sujetos)))

####### AUROC2
## get metacognitive sensivity

# load the type 2 ROC analysis function
source(root$find_file("Analysis/AuxiliaryFunctions/auroc2.R"))
library(dplyr)

Nsuj <- length(unique(df_total$sujetos))
# saving metacog = mc for each RT discarded
mc <- rep(NA, Nsuj)
ExistingSubjects <- unique(df_total$sujetos)

for (i in 1:Nsuj){
  mc[i] <- type2roc(correct = df_total$discrimination_is_correct[df_total$sujetos==ExistingSubjects[i]],
                    conf = df_total$confidence_key[df_total$sujetos==ExistingSubjects[i]], 
                    Nratings = 4)}

## adding column mc to df_total

todos_sujetos_mc <- c()

# iterar por sujeto existente
for (i in 1:length(ExistingSubjects)) {
  # saco la cantidad de trials del sujeto
  sujeto_df_exp <- df_total[df_total$sujetos == ExistingSubjects[i],]
  cant_trials <- nrow(sujeto_df_exp)
  
  # repito cada valor de mc segun la cantidad de trials que le quedaron al sujeto
  sujeto_mc <-rep(mc[i],cant_trials)
  
  # lo agrego a un vector que tenga los mc de todos los sujetos por cantidad de trials
  todos_sujetos_mc <- c(todos_sujetos_mc,sujeto_mc)
}

# lo agrego al df_total
df_total$mc <- todos_sujetos_mc

####### filter by mc
# filtro para los que tienen metacog menores a 0.5
mean_mc <- mean(mc)
sd_mc <-sd(mc)
df_total <- df_total[df_total$mc >= mean_mc - sd_mc* 1.5,]
# a partir de cuanto quiero dejar de metacog (otra forma de filtrar)
# d.sin.normalizar.mc.filter <- d.sin.normalizar[d.sin.normalizar$mc >= 0.4,] 

cat("Cantidad de sujetos luego de filtrar por AUROC2: ", length(unique(df_total$sujetos)))

####### save the df_total

# # RESULTS all exp
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
save(df_total,file = filepath)


# # RESULTS_EXP
# filepath <- root$find_file("Data/Experiment_Complete/df_total.Rda")
# save(df_total,file = filepath)
# 
# 
# # RESULTS_EXP
# filepath <- root$find_file("Data/Experiment_OnlySurvey/df_total_SIN_DESCARTAR.Rda")
# save(df_total,file = filepath)
# 
# # RESULTS_EXP
# filepath <- root$find_file("Data/PID5_sorteo/df_total.Rda")
# save(df_total,file = filepath)
# 
# # save the df in .txt format, it is saved in the mail folder
# write.table(df_total, file= 'df_total.txt')




