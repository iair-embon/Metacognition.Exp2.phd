###############
### library ###
###############
library(tidyverse)
library(ggridges)
library(matrixStats)
library(dplyr)
library(arm)
library(jtools)
library(broom.mixed)
library(TMB)
library(sjPlot)
library(dotwhisker)

#######################
### EDAD histograms ###
#######################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered.R"))

# get the df list
# experimento = completo,survey,sorteo, todos
DF_list <- DataFrame_Filtered(experimento = "todos", 
                              filtroRT_Disc_Sup = 5000,
                              filtroRT_Disc_Inf = 200,
                              filtroRT_Conf_Sup = 5000,
                              filtroRT_Conf_Inf = 0,
                              filtroTrial = 20,
                              cant_trial_filter = 70)

# DF_list:
# a = df_total
# b = d.sin.normalizar
# c = d.sin.normalizar.mc.filter
# d = d.mc.filter

df_total <- DF_list$a
d.sin.normalizar <- DF_list$b
d.sin.normalizar.mc.filter <- DF_list$c
d.mc.filter <- DF_list$d 
d <- d.mc.filter

d1 = d.sin.normalizar.mc.filter

# sujetos que tienen un 85 % de trials en una misma respuesta de confianza

source(root$find_file("Analysis/AuxiliaryFunctions/discard_by_x_same_confidence.R"))
sujetos_a_descartar <- discard_by_x_same_confidence(85)
d2 <- d1[! d1$sujetos %in% sujetos_a_descartar,]



# hist
ggplot(d2,aes(edad))+
  geom_bar(#data=subset(d3, Im == 'Male'),
           fill = "black", alpha = 0.8)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Age") +
  ylab("Participants")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 30))

##########################
### AUROC2-Performance ###
##########################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered.R"))

# get the df list
# experimento = completo,survey,sorteo, todos
DF_list <- DataFrame_Filtered(experimento = "todos", 
                              filtroRT_Disc_Sup = 5000,
                              filtroRT_Disc_Inf = 200,
                              filtroRT_Conf_Sup = 5000,
                              filtroRT_Conf_Inf = 0,
                              filtroTrial = 20,
                              cant_trial_filter = 70)

# DF_list:
# a = df_total
# b = d.sin.normalizar
# c = d.sin.normalizar.mc.filter
# d = d.mc.filter

df_total <- DF_list$a
d.sin.normalizar <- DF_list$b
d.sin.normalizar.mc.filter <- DF_list$c
d.mc.filter <- DF_list$d 
d <- d.mc.filter

d1 = d.sin.normalizar.mc.filter

# sujetos que tienen un 85 % de trials en una misma respuesta de confianza

source(root$find_file("Analysis/AuxiliaryFunctions/discard_by_x_same_confidence.R"))
sujetos_a_descartar <- discard_by_x_same_confidence(85)
d2 <- d1[! d1$sujetos %in% sujetos_a_descartar,]

d2 <- d2[d2$Im == "Masculino" | d2$Im == "Femenino",]

mc.sorted <-  d2[order(d2$mc),]
subjects <- 1:nrow(mc.sorted)
mc.sorted$s <- subjects

ggplot(mc.sorted, aes(s)) +                   
  geom_point(aes(x = s, y=mc, colour="Metacognition")) +  
  geom_point(aes(x = s, y=pc, colour="Performance")) +  
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  scale_x_continuous(expand = c(.009, 0)) +
  labs(x="Participants", y="", color = "") +
  #scale_color_manual(values = c("mc" = "black", "pc" = "red"))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        #legend.title = element_text(size = 20),
        legend.text = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 

##########################
### density plot ######### https://www.datasciencemadesimple.com/dot-plot-in-r/
##########################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered.R"))

# get the df list
# experimento = completo,survey,sorteo, todos
DF_list <- DataFrame_Filtered(experimento = "todos", 
                              filtroRT_Disc_Sup = 5000,
                              filtroRT_Disc_Inf = 200,
                              filtroRT_Conf_Sup = 5000,
                              filtroRT_Conf_Inf = 0,
                              filtroTrial = 20,
                              cant_trial_filter = 70)

# DF_list:
# a = df_total
# b = d.sin.normalizar
# c = d.sin.normalizar.mc.filter
# d = d.mc.filter

df_total <- DF_list$a
d.sin.normalizar <- DF_list$b
d.sin.normalizar.mc.filter <- DF_list$c
d.mc.filter <- DF_list$d 
d <- d.mc.filter

d1 = d.sin.normalizar.mc.filter

# sujetos que tienen un 85 % de trials en una misma respuesta de confianza

source(root$find_file("Analysis/AuxiliaryFunctions/discard_by_x_same_confidence.R"))
sujetos_a_descartar <- discard_by_x_same_confidence(85)
d2 <- d1[! d1$sujetos %in% sujetos_a_descartar,]

DomainPsychoticism <- d2$DomainPsychoticism
DomainDisinhibition  <- d2$DomainDisinhibition
DomainAntagonism <- d2$DomainAntagonism
DomainDetachment <- d2$DomainDetachment
DomainNegativeAffect <- d2$DomainNegativeAffect

DomainValues <- c(DomainPsychoticism,DomainDisinhibition,DomainAntagonism,
                  DomainDetachment,DomainNegativeAffect)

DomainPsychoticism_label <- rep("Psychoticism",length(DomainPsychoticism))
DomainDisinhibition_label <- rep("Disinhibition", length(DomainDisinhibition)) 
DomainAntagonism_label <- rep("Antagonism", length(DomainAntagonism))
DomainDetachment_label <- rep("Detachment", length(DomainDetachment))
DomainNegativeAffect_label <- rep("Negative Affect", length(DomainNegativeAffect))

DomainLabels <- c(DomainPsychoticism_label,DomainDisinhibition_label,
                  DomainAntagonism_label, DomainDetachment_label,
                  DomainNegativeAffect_label)

d1 <- data.frame(DomainValues = DomainValues,
                 DomainLabels = DomainLabels)

ggplot(d1, aes(x = DomainValues, color = DomainLabels)) + 
  geom_density(alpha=0.3,size=1.5)+
  scale_x_continuous(expand = c(.0, 0),limits = c(0.1, 2.0)) +
  labs(colour = "Domain", x = "Domain Values")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.title = element_text(size = 20),
        #legend.title = element_blank(),
        legend.text = element_text(size = 20),
        #legend.text = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 


############################## intento del fake density plot
## Dot chart in R for groups

# subset and assign colour
dm <- 

pg$color[pg$group=="ctrl"] <- "red"
pg$color[pg$group=="trt1"] <- "Violet"
pg$color[pg$group=="trt2"] <- "blue"

dotchart(PlantGrowth$weight,
         labels=PlantGrowth$group,
         cex=0.8,
         groups= PlantGrowth$group,
         main="group vs weight",
         xlab="weight",
         gcolor="black",
         color=pg$color)


#############################
### Regression plot ######### 
#############################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered.R"))

# get the df list
# experimento = completo,survey,sorteo, todos
DF_list <- DataFrame_Filtered(experimento = "todos", 
                              filtroRT_Disc_Sup = 5000,
                              filtroRT_Disc_Inf = 200,
                              filtroRT_Conf_Sup = 5000,
                              filtroRT_Conf_Inf = 0,
                              filtroTrial = 20,
                              cant_trial_filter = 70)

# DF_list:
# a = df_total
# b = d.sin.normalizar
# c = d.sin.normalizar.mc.filter
# d = d.mc.filter

df_total <- DF_list$a
d.sin.normalizar <- DF_list$b
d.sin.normalizar.mc.filter <- DF_list$c
d.mc.filter <- DF_list$d 
d <- d.mc.filter
###############
### library ###
###############
library(arm)
library(jtools)
library(broom.mixed)
library(TMB)
library(sjPlot)
library(dotwhisker)
library(tidyverse)
library(dplyr)

###########################
### Regression Analysis ###
###########################

### lineas para hacer regresion 

d1 = d.sin.normalizar.mc.filter
d1= d1 <- d1[d1$Im == "Masculino" | d1$Im == "Femenino",]

# sujetos que tienen un 85 % de trials en una misma respuesta de confianza

source(root$find_file("Analysis/AuxiliaryFunctions/discard_by_x_same_confidence.R"))
sujetos_a_descartar <- discard_by_x_same_confidence(85)
d2 <- d1[! d1$sujetos %in% sujetos_a_descartar,]


d <- subset(d, d$sujetos %in% d2$sujetos)

# preparo para correr regesiones de todas las facetas y dominios
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


# corro regresiones de cada faceta
for (i in 1:length(vec_variables_string)) {
  a <- lm(d2$mc ~ vec_variables_values[[i]] + 
            d$edad + 
            d$Im +
            d$edad:vec_variables_values[[i]]) #+
  #d$Im: vec_variables_values[[i]])
  print(vec_variables_string[i])
  print(summary(a))
}


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


# corro regresiones de cada dominio
for (i in 1:length(vec_variables_string)) {
  a <- lm(d2$mc ~ vec_variables_values[[i]] + 
            d$edad + 
            d$Im +
            d$edad:vec_variables_values[[i]]) #+
  #d$Im: vec_variables_values[[i]])
  print(vec_variables_string[i])
  print(summary(a))
}


# preparo para plotear un modelo de regresion
a <- lm(d$mc ~ d$Distractivility + 
          d$edad + 
          d$Im +
          d$edad:d$Distractivility)

summary(a)
display(a)

############################# grafico 1 modelo de regresion individual

plot_summs(a, coefs = c('Distractibility' = 'd$Distractivility',
                        'Age'='d$edad','Gender-Male' = 'd$ImMasculino',
                        'Gender-Non binary'='d$ImNo Binario',
                        'Distractibility:Age'='d$Distractivility:d$edad') ,
           plot.distributions = FALSE)+
  ylab("") +
  xlab("Regression coefficient") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25))

############################# grafico 2 bar plot de coef, 1 solo modelo

# Distractibility # *
a <- lm(d2$mc ~ d$Distractivility + 
          d$edad + 
          d$Im +
          d$edad:d$Distractivility)

coeff <- coefficients(a)
coeff <- coeff[2]
sd_coef <- unname(summary(a)$coefficients[2,2])
names.coef <- 'Distractibility'

# Irresponsibility
a <- lm(d2$mc ~ d$Irresponsibility + 
          d$edad + 
          d$Im +
          d$edad:d$Irresponsibility)

coe <- coefficients(a)
coeff <- c(coeff,coe[2])
sd_coef <- c(sd_coef,unname(summary(a)$coefficients[2,2]))
names.coef <-c(names.coef,'Irresponsibility')

# Impulsivity
a <- lm(d2$mc ~ d$Impulsivity + 
          d$edad + 
          d$Im +
          d$edad:d$Impulsivity)

coe <- coefficients(a)
coeff <- c(coeff,coe[2])
sd_coef <- c(sd_coef,unname(summary(a)$coefficients[2,2]))
names.coef <-c(names.coef,'Impulsivity')

# Impulsivity
a <- lm(d2$mc ~ d$Impulsivity + 
          d$edad + 
          d$Im +
          d$edad:d$Impulsivity)

coe <- coefficients(a)
coeff <- c(coeff,coe[2])
sd_coef <- c(sd_coef,unname(summary(a)$coefficients[2,2]))
names.coef <-c(names.coef,'Impulsivity')



# Submissiveness # *
a <- lm(d2$mc ~ d$Submissiveness + 
          d$edad + 
          d$Im +
          d$edad:d$Submissiveness)

coe <- coefficients(a)
coeff <- c(coeff,coe[2])
sd_coef <- c(sd_coef,unname(summary(a)$coefficients[2,2]))
names.coef <-c(names.coef,'Submissiveness')

# Anxiousness
a <- lm(d2$mc ~ d$Anxiousness + 
          d$edad + 
          d$Im +
          d$edad:d$Anxiousness)

coe <- coefficients(a)
coeff <- c(coeff,coe[2])
sd_coef <- c(sd_coef,unname(summary(a)$coefficients[2,2]))
names.coef <-c(names.coef,'Anxiousness')

# RigidPerfeccionism
a <- lm(d2$mc ~ d$RigidPerfeccionism + 
          d$edad + 
          d$Im +
          d$edad:d$RigidPerfeccionism)

coe <- coefficients(a)
coeff <- c(coeff,coe[2])
sd_coef <- c(sd_coef,unname(summary(a)$coefficients[2,2]))
names.coef <-c(names.coef,'RigidPerfeccionism')

# Depressivity
a <- lm(d2$mc ~ d$Depressivity + 
          d$edad + 
          d$Im +
          d$edad:d$Depressivity)

coe <- coefficients(a)
coeff <- c(coeff,coe[2])
sd_coef <- c(sd_coef,unname(summary(a)$coefficients[2,2]))
names.coef <-c(names.coef,'Depressivity')

# Perseveration
a <- lm(d2$mc ~ d$Perseveration + 
          d$edad + 
          d$Im +
          d$edad:d$Perseveration)

coe <- coefficients(a)
coeff <- c(coeff,coe[2])
sd_coef <- c(sd_coef,unname(summary(a)$coefficients[2,2]))
names.coef <-c(names.coef,'Perseveration')

# DomainPsychoticism
a <- lm(d2$mc ~ d$DomainPsychoticism + 
          d$edad + 
          d$Im +
          d$edad:d$DomainPsychoticism)

coe <- coefficients(a)
coeff <- c(coeff,coe[2])
sd_coef <- c(sd_coef,unname(summary(a)$coefficients[2,2]))
names.coef <-c(names.coef,'Domain Psychoticism')


# DomainDisinhibition # *
a <- lm(d2$mc ~ d$DomainDisinhibition + 
          d$edad + 
          d$Im +
          d$edad:d$DomainDisinhibition)

coe <- coefficients(a)
coeff <- c(coeff,coe[2])
sd_coef <- c(sd_coef,unname(summary(a)$coefficients[2,2]))
names.coef <-c(names.coef,'Domain Disinhibition')


dtf1 <- data.frame(Predictor = names.coef,
                   y = coeff,
                   sd= sd_coef)

row.names(dtf1) <- NULL


ggplot(dtf1, aes(Predictor, y)) +
  geom_bar(stat = "identity", aes(fill = Predictor), width = 0.9) +
  #facet_grid(. ~model)+ 
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=.2,
                position=position_dodge(.9)) +
  geom_hline(yintercept=0) +
  theme_bw() + xlab("") + ylab("Regression coefficient") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        legend.title =element_text(size = 30),#element_blank(),
        legend.text = element_text(size = 30),#element_blank(),
        legend.position = "left",
        aspect.ratio = 2/1.5,#2/0.7,
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30))


############################# scatter plots
# Distractibility
a <- lm(mc ~ Distractivility + 
          edad + 
          Im +
          edad:Distractivility, data = d)

ggplot(d, aes(x = Distractivility, y = mc)) + 
  geom_point() + 
  geom_abline(slope = coef(a)[[2]], intercept = coef(a)[[1]])+
  ylab('Metacognition')+
  xlab('Distractibility')+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 

# DomainDisinhibition
a <- lm(mc ~ DomainDisinhibition + 
          edad + 
          Im +
          edad:DomainDisinhibition, data = d)

ggplot(d, aes(x = DomainDisinhibition, y = mc)) + 
  geom_point() + 
  geom_abline(slope = coef(a)[[2]], intercept = coef(a)[[1]])+
  ylab('Metacognition')+
  xlab('Domain Disinhibition')+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 

############################# grafico 4 bar plot de coef, dos modelos en un graf 
############################# (EN CONSTRUCCION)
# Modelo 1
a <- lm(d$mc ~ d$Distractivility + 
          d$edad + 
          d$Im +
          d$edad:d$Distractivility)

coeff <- coefficients(a1)
coeff <- coeff[2:4]
sd_coef <- unname(summary(a1)$coefficients[2:4,2])
names.coef <- names(coeff)
model <- rep(1, length(coeff))

dtf1 <- data.frame(Predictor = names.coef,
                   y = coeff,
                   sd= sd_coef,
                   model = model)

row.names(dtf1) <- NULL

# model 2
a2=lm(mc ~ AQ.norm + Gender + edad + es + AQ.norm: Gender , data = df.plot)
summary(a2)
display(a2)

coeff <- coefficients(a2)
coeff <- coeff[2:10]
sd_coef <- unname(summary(a2)$coefficients[2:10,2])
names.coef <- names(coeff)
# names.coef[1] <- "AQ2"
# names.coef[2] <-"GenderMale2"
# names.coef[3] <-"edad2"
# names.coef[4] <-"esPosgrado incompleto2"
# names.coef[5] <-"esSecundaria completa2"
# names.coef[6] <-"esSecundaria incompleta2"
# names.coef[7] <-"esUniversidad completa2"
# names.coef[8] <-"esUniversidad incompleta2"
# names.coef[9] <-"AQ:GenderMale2"   
model <- rep(2, length(coeff))

dtf2 <- data.frame(Predictor = names.coef,
                   y = coeff,
                   sd= sd_coef,
                   model=model)
row.names(dtf2) <- NULL

dtf.plot <- dtf2


ggplot(dtf.plot, aes(Predictor, y)) +                                  # VA ESTE PARA REGRESION
  geom_bar(stat = "identity", aes(fill = Predictor), width = 0.9) +
  #facet_grid(. ~model)+ 
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=.2,
                position=position_dodge(.9)) +
  geom_hline(yintercept=0) +
  theme_bw() + xlab("") + ylab("Regression coefficient") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        legend.title =element_text(size = 30),#element_blank(),
        legend.text = element_text(size = 20),#element_blank(),
        legend.position = "left",
        aspect.ratio = 2/0.7,
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30))


####### scatter plot 2 (va este para SAN)

d$mc.sin.normalizar <-d2$mc

# DomainDisinhibition
a <- lm(mc.sin.normalizar ~ DomainDisinhibition, data = d)

p <- plot_model(a, type = "pred", terms = "DomainDisinhibition",
                axis.labels = c('DomainDisinhibition','Metacognition'),
                legend.title = '',
                title = '', 
                axis.title = c('Domain Disinhibition','Metacognition'),
                show.data = FALSE)
p +theme_sjplot(base_size = 25)


# Submissiveness
a <- lm(mc.sin.normalizar ~ Submissiveness, data = d)

p <- plot_model(a, type = "pred", terms = "Submissiveness",
                axis.labels = c('Submissiveness','Metacognition'),
                legend.title = '',
                title = '', 
                axis.title = c('Submissiveness','Metacognition'),
                show.data = FALSE)
p +theme_sjplot(base_size = 25)

# Distractibility
a <- lm(mc.sin.normalizar ~ Distractivility, data = d)

p <- plot_model(a, type = "pred", terms = "Distractivility",
                axis.labels = c('Distractivility','Metacognition'),
                legend.title = '',
                title = '', 
                axis.title = c('Distractibility','Metacognition'),
                show.data = FALSE)
p +theme_sjplot(base_size = 25)

