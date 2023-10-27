### Figure2

# set root
root <- rprojroot::is_rstudio_project
basename(getwd())  

# library
library(jtools)
library(tidyverse)
library(glmnet)
library(boot)

### reg beta

filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_facets_Beta_linear_model.RData")
load(file= filepath)

sum_a <- summary(a)
term <- c("Grandiosity", "Hostility", "Impulsivity", "PerceptualDysregulation", "RestrictedAffectivity", "SeparationInsecurity", "Submissiveness")
coeff <- c(unname(sum_a$coefficients$mean[11,"Estimate"]),unname(sum_a$coefficients$mean[12,"Estimate"]), unname(sum_a$coefficients$mean[13,"Estimate"]), unname(sum_a$coefficients$mean[17,"Estimate"]),unname(sum_a$coefficients$mean[19,"Estimate"]),unname(sum_a$coefficients$mean[22,"Estimate"]),unname(sum_a$coefficients$mean[23,"Estimate"]))
se <- c(unname(sum_a$coefficients$mean[11,"Std. Error"]), unname(sum_a$coefficients$mean[12,"Std. Error"]), unname(sum_a$coefficients$mean[13,"Std. Error"]), unname(sum_a$coefficients$mean[17,"Std. Error"]),unname(sum_a$coefficients$mean[19,"Std. Error"]),unname(sum_a$coefficients$mean[22,"Std. Error"]),unname(sum_a$coefficients$mean[23,"Std. Error"]))
model <- rep("beta multivariate",7)

 
df.models <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)


### reg univariate beta

# Grandiosity 

filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/Grandiosity.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

sum_a <- summary(a)
term <- "Grandiosity"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)


df.models <- rbind(df.models, df.beta)

# Hostility 

filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/Hostility.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

sum_a <- summary(a)
term <- "Hostility"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

df.models <- rbind(df.models, df.beta)

# Impulsivity 

filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/Impulsivity.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

sum_a <- summary(a)
term <- "Impulsivity"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

df.models <- rbind(df.models, df.beta)

# PerceptualDysregulation 

filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/PerceptualDysregulation.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

sum_a <- summary(a)
term <- "PerceptualDysregulation"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

df.models <- rbind(df.models, df.beta)

# RestrictedAffectivity 

filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/RestrictedAffectivity.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

sum_a <- summary(a)
term <- "RestrictedAffectivity"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

df.models <- rbind(df.models, df.beta)

# SeparationInsecurity 

filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/SeparationInsecurity.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

sum_a <- summary(a)
term <- "SeparationInsecurity"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

df.models <- rbind(df.models, df.beta)

# Submissiveness 

filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/Submissiveness.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

sum_a <- summary(a)
term <- "Submissiveness"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

df.models <- rbind(df.models, df.beta)

#### elastic net 
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_facets_fit_elasticNet_alpha_lambda_loocv.RData")
load(file= filepath)

term <- term <- term <- c("Grandiosity", "Hostility", "Impulsivity", "PerceptualDysregulation", "RestrictedAffectivity", "SeparationInsecurity", "Submissiveness")
coeff <- c(a[12],a[13],a[14],a[18], a[20],a[23],a[24])
se <- rep(0,7) ## sd is not usefull in regularized regressions, could cause misinterpretations
model <- rep("elastic-net",7)

df.elastic <- data.frame(terms = term,
                         coeff = coeff,
                         se = se,
                         model = model)


df.models <- rbind(df.models, df.elastic)


##### preprocessing to plot

df.models <- df.models %>%
  mutate(model = fct_relevel(model,
                             "beta multivariate",
                             "beta univariate",
                             "elastic-net"
                             ))


## plot models

ggplot(df.models , aes(coeff, terms, color=model)) + 
  geom_point(aes(shape=model),size=4, 
             position=position_dodge(width=0.9)) +
  geom_vline(xintercept= 0, linetype='dashed', color= "black")+
  scale_color_manual(name="model",
                     values=c("darkred", "darkblue", "darkgreen")) +
  scale_shape_manual(name="model",values=c(17,18,19)) + 
  scale_x_continuous("regression coefficients") +
  scale_y_discrete(labels= c("Grandiosity",
                             "Hostility",
                             "Impulsivity",
                             "Perceptual Dysregulation",
                             "Restricted Affectivity",
                             "Separation Insecurity",
                             "Submissiveness"))+
  geom_errorbar(aes(xmin= coeff - 2* se,xmax= coeff + 2* se),
                width=0.1,
                position=position_dodge(width=0.9), size = 1)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(1, 1,1, 1, "cm"),
      legend.text =  element_text(size = 15),
      legend.title =  element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 15))

ggsave("git/Figures/FiguresPaper/Figure2.png", 
       width = 10, height = 6)
