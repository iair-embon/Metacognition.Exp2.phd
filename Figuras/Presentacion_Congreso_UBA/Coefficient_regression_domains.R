###############
### library ###
###############
library(tidyverse)
library(ggridges)
library(matrixStats)
library(arm)
library(jtools)
library(broom.mixed)
library(TMB)
library(sjPlot)
library(dotwhisker)
library(reshape2)
library(ggExtra)
library(dplyr)

##################
### data frame ###
##################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

####### data frames with filters already applied
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered_already_applied.R"))
DF_list <- DataFrame_Filtered_already_applied(df_total)

# DF_list:
# a = d (df normalizado)
# b = d.sin.normalizar

d <- DF_list$a
d.sin.normalizar <- DF_list$b

# only male and female genders
d <- d[d$Im == "Femenino" | d$Im == "Masculino",]
d2 <- d.sin.normalizar[d.sin.normalizar$Im == "Femenino" | 
                                       d.sin.normalizar$Im == "Masculino",]

d[d == "Masculino"] <- "1"
d[d == "Femenino"] <- "0"
d$Im <- as.integer(d$Im)

d2[d2 == "Masculino"] <- "1"
d2[d2 == "Femenino"] <- "0"
d2$Im <- as.integer(d2$Im)

###########################
### Regression Analysis ###
###########################

### lineas para hacer regresion 

vec_variables_values <- list(d$DomainPsychoticism,
                             d$DomainDisinhibition,
                             d$DomainAntagonism,
                             d$DomainDetachment,
                             d$DomainNegativeAffect)


vec_variables_string <- c("DomainPsychoticism",
                          'DomainDisinhibition',
                          'DomainAntagonism',
                          'DomainDetachment',
                          'DomainNegativeAffect')

a <- lm(d2$mc ~ vec_variables_values[[1]] +
          vec_variables_values[[2]] +
          vec_variables_values[[3]] +
          vec_variables_values[[4]] +
          vec_variables_values[[5]] +
          d$edad +
          d$Im)

print(summary(a))

############
### Plot ###
############

plot_summs(a, coefs = c('Psychoticism'='vec_variables_values[[1]]' ,
                        'Disinhibition' = 'vec_variables_values[[2]]',
                        'Antagonism' = 'vec_variables_values[[3]]',
                        'Detachment' = 'vec_variables_values[[4]]',
                        'Negative Affect' = 'vec_variables_values[[5]]',
                        'Age' = 'd$edad',
                        'Gender' = 'd$Im'),
           plot.distributions = FALSE, colors = "black")+
  ylab("") +
  xlab("") +#xlab("Regression coefficient") +
  scale_x_continuous(breaks=seq(-0.03,0.03,0.02))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),#element_text(size = 20, angle = (45)),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 25))
