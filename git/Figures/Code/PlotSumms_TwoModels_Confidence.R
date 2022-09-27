### Modelos de dominios para predecir confianza media por sujeto

# set root
root <- rprojroot::is_rstudio_project
basename(getwd())  

# library
library(jtools)
library(tidyverse)

filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_domain_linear_model.RData")
load(file= filepath)

Linear_Model <- a

### Regresion lineal de dominios para predecir mc por sujeto

filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_domain_Beta_linear_model.RData")
load(file= filepath)

Beta_Model <- a

## plot both models

plot_summs(Linear_Model,
           Beta_Model, 
           coefs = c('Afecto Negativo' = 'DomainNegativeAffect.norm',
                     'Desapego'='DomainDetachment.norm',
                     'Antagonísmo' = 'DomainAntagonism.norm',
                     'Desinhibición'='DomainDisinhibition.norm',
                     'Psicoticísmo'='DomainPsychoticism.norm',
                     'género' = 'gender',
                     'edad' = 'age.norm') ,
           model.names = c('normal', 'beta'),
           plot.distributions = FALSE)+
  ylab("") +
  xlab("Coeficientes de la regresión") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.text = element_text(size=20),
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30))

ggsave("git/Figures/Figures/TwoModels_Confidence.png", 
       width = 10, height = 6)
