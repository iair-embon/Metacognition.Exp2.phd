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

DomainPsychoticism_label <- rep("DomainPsychoticism",length(DomainPsychoticism))
DomainDisinhibition_label <- rep("DomainDisinhibition", length(DomainDisinhibition)) 
DomainAntagonism_label <- rep("DomainAntagonism", length(DomainAntagonism))
DomainDetachment_label <- rep("DomainDetachment", length(DomainDetachment))
DomainNegativeAffect_label <- rep("DomainNegativeAffect", length(DomainNegativeAffect))

DomainLabels <- c(DomainPsychoticism_label,DomainDisinhibition_label,
                  DomainAntagonism_label, DomainDetachment_label,
                  DomainNegativeAffect_label)

d1 <- data.frame(DomainValues = DomainValues,
                 DomainLabels = DomainLabels)

ggplot(d1, aes(x = DomainValues, fill = DomainLabels)) + 
  geom_density(alpha = 0.1)+
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

