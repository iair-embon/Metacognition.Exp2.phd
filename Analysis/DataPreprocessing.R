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
# REEMPLAZAR:
#read each line and convert 
content<-readLines(root$find_file("Data/jatos_results_20210406141549.txt"))
res<-lapply(content,fromJSON)

# each subject has 6 lists in order of arrival and by subjects.
# res[[1]] are the demographic data of subject 1
# res[[2]] are the data of the practice of the experiment of subject 1
# res[[3]] are the data from subject 1's experiment
# res[[4]] are the first part of personality test of subject 1
# res[[5]] are the second part of personality test of subject 1 (new subject)
# res[[6]] are the browser of subject 1
# res[[7]] are sincericidio and the email data of subject 1
# res[[8]] are the demographic data of subject 2
# res[[9]] are the data of the practice of the experiment of subject 2
# ....

iSub      <- 0
horasSuen <- c()
fechaNac  <- c()
pais      <- c()
genero    <- c()
estudio   <- c()
affeccionPsico <- c()
medicacion <- c()
pid1      <- c()
pid2      <- c()
Browser <- c()
Sinc    <- c()
TeEscuchamos <- c()

# Experiment data frame
df_exp <- data.frame(t0 =character(), 
                     t_offset =character(), 
                     dots_num_left =character(), 
                     dots_num_right =character(), 
                     discrimination_is_correct =character(), 
                     discrimination_t_onset =character(), 
                     discrimination_t_keydown =character(), 
                     confidence_key =character(), 
                     confidence_t_onset=character(), 
                     confidence_t_keydown=character(), 
                     stringsAsFactors=FALSE) 

for (s in 1:(length(res)-4)){# xq un -4?
  
  ind_suenio <- NaN  
  ind_fecha <- NaN  
  ind_pais <- NaN  
  ind_genero <- NaN  
  ind_estudio <- NaN  
  ind_affeccion <- NaN  
  ind_medicacion <- NaN  
  
  for (item in 1:length(res[[s]])){
    if (is.null(res[[s]][item]$sueno)           ==FALSE){   ind_suenio <- item   }
    if (is.null(res[[s]][item]$Cumpleanos)      ==FALSE){   ind_fecha  <- item   }
    if (is.null(res[[s]][item]$Pais)            ==FALSE){   ind_pais   <- item   }
    if (is.null(res[[s]][item]$Genero)          ==FALSE){   ind_genero <- item   }
    if (is.null(res[[s]][item]$Estudio)         ==FALSE){   ind_estudio <- item   }
    if (is.null(res[[s]][item]$AffeccionPsico)  ==FALSE){   ind_affeccion <- item   }
    if (is.null(res[[s]][item]$medicacion)      ==FALSE){   ind_medicacion <- item   }
  }
  
  # Condition 1 will be TRUE if there is a response to the first component of demographic data
  condicion1 <-  is.nan(ind_suenio) == FALSE
  # Condition 2 will be TRUE if there is an answer to the second part PID questions (component 4) 
  condicion2 <-  is.null(res[[s+4]]$question) ==FALSE    
  
  if(condicion1 & condicion2 ){ # new participant
    iSub <- iSub + 1;
    # I take data from component 1 (demographic)
    horasSuen <- c(horasSuen,res[[s]][ind_suenio]$sueno)
    fechaNac  <- c(fechaNac,res[[s]][ind_fecha]$Cumpleanos)
    pais <- c(pais, res[[s]][ind_pais]$Pais)
    genero <- c(genero,res[[s]][ind_genero]$Genero)
    estudio <- c(estudio,res[[s]][ind_estudio]$Estudio)
    affeccionPsico <- c(affeccionPsico,res[[s]][ind_affeccion]$AffeccionPsico)
    medicacion <- c(medicacion,res[[s]][ind_medicacion]$medicacion)
    
    # Experiment data 
    df_exp <- rbind(df_exp, res[[s+2]])
    
    # pid1 data
    pid1 <- c(pid1, res[[s+3]])  
    
    # pid2 data
    pid2 <- c(pid2, res[[s+4]])  
    
    if(is.null(res[[s+5]][1]$browser) ==FALSE){
      Browser <- c(Browser, res[[s+5]][1]$browser)
    }else{
      Browser <- c(Browser, NaN)}
    
    if(length(res)-s >= 6 ){
      
      if(is.null(res[[s+6]][1]$sincericidio) ==FALSE){
        Sinc <- c(Sinc, res[[s+6]][1]$sincericidio)
      }else{
        Sinc <- c(Sinc, NaN)}
      if(length(res) - s >= 7 ){
        if(is.null(res[[s+7]]$TeEscuchamos) ==FALSE){
          TeEscuchamos <- c(TeEscuchamos, res[[s+7]]$TeEscuchamos)
        }else{
          TeEscuchamos <- c(TeEscuchamos, NaN)}
        
      }
    }
  }
}

### save df
filepath <- root$find_file("Data/Results_Exp2(replica)/df_total.Rda")
save(df_total,file = filepath)


####### df 
