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

vec_variables_values <- list(d$Anhedonia,
                             d$Anxiousness,
                             d$AttentionSeeking,
                             d$Callousness,
                             d$Deceitfulness,
                             d$Depressivity,
                             d$Distractivility,
                             d$Excentricity,
                             d$EmotionalLability,
                             d$Grandiosity,
                             d$Hostility,
                             d$Impulsivity,
                             d$IntimacyAvoidance,
                             d$Irresponsibility,
                             d$Manipulativeness,
                             d$PerceptualDysregulation,
                             d$Perseveration,
                             d$RestrictedAffectivity,
                             d$RigidPerfeccionism,
                             d$RiskTaking,
                             d$SeparationInsecurity,
                             d$Submissiveness,
                             d$Suspiciousness,
                             d$UnusualBeliefsAndExperiences,
                             d$Withdrawal)


vec_variables_string <- c("Anhedonia",
                          "Anxiousness",
                          "AttentionSeeking",
                          "Callousness",
                          "Deceitfulness",
                          "Depressivity",
                          "Distractivility",
                          "Excentricity",
                          "EmotionalLability",
                          "Grandiosity",
                          "Hostility",
                          "Impulsivity",
                          "IntimacyAvoidance",
                          "Irresponsibility",
                          "Manipulativeness",
                          "PerceptualDysregulation",
                          "Perseveration",
                          "RestrictedAffectivity",
                          "RigidPerfeccionism",
                          "RiskTaking",
                          "SeparationInsecurity",
                          "Submissiveness",
                          "Suspiciousness",
                          "UnusualBeliefsAndExperiences",
                          "Withdrawal")

## para todos los regresores juntos
######
a <- lm(d2$mc ~ vec_variables_values[[1]] +
          vec_variables_values[[2]] +
          vec_variables_values[[3]] +
          vec_variables_values[[4]] +
          vec_variables_values[[5]] +
          vec_variables_values[[6]] +
          vec_variables_values[[7]] +
          vec_variables_values[[8]] +
          vec_variables_values[[9]] +
          vec_variables_values[[10]] +
          vec_variables_values[[11]] +
          vec_variables_values[[12]] +
          vec_variables_values[[13]] +
          vec_variables_values[[14]] +
          vec_variables_values[[15]] +
          vec_variables_values[[16]] +
          vec_variables_values[[17]] +
          vec_variables_values[[18]] +
          vec_variables_values[[19]] +
          vec_variables_values[[20]] +
          vec_variables_values[[21]] +
          vec_variables_values[[22]] +
          vec_variables_values[[23]] +
          vec_variables_values[[24]] +
          vec_variables_values[[25]] +
          d$edad +
          d$Im)

## muestro los resultados
######
print(summary(a))

############
### Plot ###
############

# first part
plot_summs(a, coefs = c('Anhedonia'='vec_variables_values[[1]]' ,
                        'Anxiousness' = 'vec_variables_values[[2]]',
                        'Attention Seeking' = 'vec_variables_values[[3]]',
                        'Callousness' = 'vec_variables_values[[4]]',
                        'Deceitfulness' = 'vec_variables_values[[5]]',
                        'Depressivity' = 'vec_variables_values[[6]]',
                        'Distractivility' = 'vec_variables_values[[7]]'),
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
        axis.text.y = element_blank(),# element_text(size = 20, angle = (45)),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 25))

# second part
plot_summs(a, coefs = c('Excentricity' = 'vec_variables_values[[8]]',
                        'Emotional Lability' = 'vec_variables_values[[9]]',
                        'Grandiosity' = 'vec_variables_values[[10]]',
                        'Hostility' = 'vec_variables_values[[11]]',
                        'Impulsivity' = 'vec_variables_values[[12]]',
                        'Intimacy Avoidance' = 'vec_variables_values[[13]]',
                        'Irresponsibility' = 'vec_variables_values[[14]]'),
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
        axis.text.y = element_blank(), # element_text(size = 20, angle = (45)),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 25))

# third part
plot_summs(a, coefs = c('Manipulativeness' = 'vec_variables_values[[15]]',
                        'Perceptual Dysregulation' = 'vec_variables_values[[16]]',
                        'Perseveration' = 'vec_variables_values[[17]]',
                        'Restricted Affectivity' = 'vec_variables_values[[18]]',
                        'Separation Insecurity' = 'vec_variables_values[[21]]',
                        'Rigid Perfeccionism' = 'vec_variables_values[[19]]',
                        'Risk Taking' = 'vec_variables_values[[20]]'),
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
        axis.text.y = element_blank(), # element_text(size = 20, angle = (45)),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 25))

# fourth part
plot_summs(a, coefs = c('Submissiveness' = 'vec_variables_values[[22]]',
                        'Suspiciousness' = 'vec_variables_values[[23]]',
                        'Unusual Beliefs And Experiences' = 'vec_variables_values[[24]]',
                        'Withdrawal' = 'vec_variables_values[[25]]',
                        "edad" = "d$edad",
                        "Im" = "d$Im"),
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
        axis.text.y = element_blank(), # element_text(size = 20, angle = (45)),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 25))