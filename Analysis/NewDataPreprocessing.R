######################
## Preprocesamiento ##
######################

####### Data Preprocessing:

# Read the .txt results from JATOS and perform the data preprocessing.

# (To reado the .txt results from JATOS it was necesary to add an "enter" before the curly brackets
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

# load the function to get the AQ quotient  
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
PC   <- rep(NA, max(df_exp$sujetos)) 

for (s in 1:max(df_exp$sujetos)){
  PC[s]   <- mean(df_exp$discrimination_is_correct[df_exp$sujetos==s])}

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


################# HASTA ACA REVISAMOS ################## HACER FUNCION

### Add the confidence columns to df_DatosUnicos_mod

# Confidence columns for all the subjects
confidence_key_1 <- rep(NA, nrow(df_DatosUnicos_mod))
confidence_key_2 <- rep(NA, nrow(df_DatosUnicos_mod))
confidence_key_3 <- rep(NA, nrow(df_DatosUnicos_mod))
confidence_key_4 <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  # confidence columns are created to iterate by subject
  confidence_key_1_total <- 0
  confidence_key_2_total <- 0
  confidence_key_3_total <- 0
  confidence_key_4_total <- 0
  
  df_prueba <- df_exp[df_exp$confidence_key =='1',]
  confidence_key_1_total <- nrow(df_prueba[df_prueba$sujetos==i,])
  confidence_key_1[i] <- confidence_key_1_total
  
  df_prueba <- df_exp[df_exp$confidence_key =='2',]
  confidence_key_2_total <- nrow(df_prueba[df_prueba$sujetos==i,])
  confidence_key_2[i] <- confidence_key_2_total
  
  df_prueba <- df_exp[df_exp$confidence_key =='3',]
  confidence_key_3_total <- nrow(df_prueba[df_prueba$sujetos==i,])
  confidence_key_3[i] <- confidence_key_3_total
  
  df_prueba <- df_exp[df_exp$confidence_key =='4',]
  confidence_key_4_total <- nrow(df_prueba[df_prueba$sujetos==i,])
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
  media_confidence[i] <- mean(df_exp[df_exp$sujetos==i,"confidence_key"])
  sd_confidence[i] <- sd(df_exp[df_exp$sujetos==i,"confidence_key"])
}

df_DatosUnicos_mod$media_confidence <- media_confidence
df_DatosUnicos_mod$sd_confidence <- sd_confidence

## Get the sd and mean of reaction times by subject in the discrimination task
media_tr_discri <- rep(NA, nrow(df_DatosUnicos_mod))
sd_tr_discri <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  media_tr_discri[i] <- mean(df_exp[df_exp$sujetos==i,"t_ensayo_discriminacion"])
  sd_tr_discri[i] <- sd(df_exp[df_exp$sujetos==i,"t_ensayo_discriminacion"])
}

df_DatosUnicos_mod$media_tr_discri <- media_tr_discri
df_DatosUnicos_mod$sd_tr_discri <- sd_tr_discri

## get the sd and mean of reaction times by subject in the confidence task
media_tr_confi <- rep(NA, nrow(df_DatosUnicos_mod))
sd_tr_confi <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  media_tr_confi[i] <- mean(df_exp[df_exp$sujetos==i,"t_ensayo_confianza"])
  sd_tr_confi[i] <- sd(df_exp[df_exp$sujetos==i,"t_ensayo_confianza"])
}

df_DatosUnicos_mod$media_tr_confi <- media_tr_confi
df_DatosUnicos_mod$sd_tr_confi <- sd_tr_confi

####### Exclusion criteria, data from future analysis is discarded
## Comment / uncomment or modify filters as required

## Filter for hours of sleep, leaving me only with > 4
#df_DatosUnicos_mod2 <- df_DatosUnicos_mod[df_DatosUnicos_mod$horasSueno > 4,] 

# ## Filter by psychological disorder, staying only with those who do not have.
# df_DatosUnicos_mod2 <- df_DatosUnicos_mod[df_DatosUnicos_mod$affeccionPsico ==
#                                             'No',]
# ## Filter by medication, leaving only with those who do not take.
# df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$medicacion ==
#                                              'No',]

df_DatosUnicos_mod2 <- df_DatosUnicos_mod
## Filter by sincericide, leaving only those who tell us that we can count on their answers.
library (stringr)
library (tidyverse)
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2 %>% 
  filter(str_detect(df_DatosUnicos_mod2$sincericidio, "Pueden")) # if start with "Pueden"
                                                                # it stays

## Filter by TeEscuchamos leaving only those who didnt interrup the task drastically (= ok)
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$TeEscuchamos ==
                                            'ok',] 

## Filter by performance, leaving only those who have PC > 60 
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$PC > 0.60,] 

## Filter by age, leaving only those who are age > 17 and are not NA
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$edad > 17,] 
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[!is.na(df_DatosUnicos_mod2$edad),]

## filter in df_exp those who survived the filters applied to df_DatosUnicos_mod2
df_exp_mod2 <- df_exp %>% 
  filter(df_exp_mod$sujetos %in% df_DatosUnicos_mod2$sujetos)

####### Prepare the df for the regression analysis

df_total <- df_DatosUnicos_mod2[0,]

#  sujetos que quedaron
ExistingSubjects <- unique(df_exp_mod2$sujetos)

# iterar por sujeto existente
for (i in ExistingSubjects) {
  # saco la cantidad de trials del sujeto
  sujeto_df_exp <- df_exp_mod2[df_exp_mod2$sujetos== i,]
  cant_trials <- nrow(sujeto_df_exp)
  
  # repito cada fila del sujeto segun la cantidad de trials que le quedaron
  sujeto_df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$sujetos== i,]
  df <- as.data.frame(lapply(sujeto_df_DatosUnicos_mod2, rep, cant_trials))
  
  # lo agrago al df_total
  df_total <- rbind(df_total, df)
}

# combino las columnas de df_exp_mod2 que me interesan con el df_total
df_total <- cbind(df_total, discrimination_is_correct = df_exp_mod2$discrimination_is_correct,
                  confidence_key = df_exp_mod2$confidence_key, trials = df_exp_mod2$trials,
                  diferencia_puntitos = df_exp_mod2$diferencia_puntitos,
                  t_ensayo_discriminacion = df_exp_mod2$t_ensayo_discriminacion,
                  t_ensayo_confianza = df_exp_mod2$t_ensayo_confianza)

## save the df_total


# RESULTS_EXP
filepath <- root$find_file("Data/Experiment_Complete/df_total.Rda")
save(df_total,file = filepath)


# RESULTS_EXP
filepath <- root$find_file("Data/Experiment_OnlySurvey/df_total_SIN_DESCARTAR.Rda")
save(df_total,file = filepath)

# RESULTS_EXP
filepath <- root$find_file("Data/PID5_sorteo/df_total.Rda")
save(df_total,file = filepath)

# save the df in .txt format, it is saved in the mail folder
write.table(df_total, file= 'df_total.txt')




