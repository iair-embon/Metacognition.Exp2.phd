### Modelos de grandiosidad para predecir confianza
### y  para predecir metaco

# set root
root <- rprojroot::is_rstudio_project
basename(getwd())  

# library
library(jtools)
library(tidyverse)
library(glmnet)
library(boot)

### reg normal

# cargo datos
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo la info
sum_a <- summary(a)
term <- c("Anxiousness", "EmotionalLability", "Impulsivity", "PerceptualDysregulation")
coeff <- c(unname(sum_a$coefficients[3,"Estimate"]),unname(sum_a$coefficients[10,"Estimate"]), unname(sum_a$coefficients[13,"Estimate"]), unname(sum_a$coefficients[17,"Estimate"]))
se <- c(unname(sum_a$coefficients[3,"Std. Error"]), unname(sum_a$coefficients[10,"Std. Error"]), unname(sum_a$coefficients[13,"Std. Error"]), unname(sum_a$coefficients[17,"Std. Error"]))
model <- rep("normal multivariado", 4)

# creo el df que va a guardar todo
df.models <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

### reg univariate normal

# anxiety

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_linear_model/Anxiousness.norm_mc_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Anxiousness"
coeff <- unname(sum_a$coefficients[2,"Estimate"])
se <- unname(sum_a$coefficients[2,"Std. Error"])
model <- "normal univariado"

df.normal <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)

# EmotionalLability

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_linear_model/EmotionalLability.norm_mc_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "EmotionalLability"
coeff <- unname(sum_a$coefficients[2,"Estimate"])
se <- unname(sum_a$coefficients[2,"Std. Error"])
model <- "normal univariado"

df.normal <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)

# Impulsivity

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_linear_model/Impulsivity.norm_mc_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Impulsivity"
coeff <- unname(sum_a$coefficients[2,"Estimate"])
se <- unname(sum_a$coefficients[2,"Std. Error"])
model <- "normal univariado"

df.normal <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)

# PerceptualDysregulation

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_linear_model/PerceptualDysregulation.norm_mc_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "PerceptualDysregulation"
coeff <- unname(sum_a$coefficients[2,"Estimate"])
se <- unname(sum_a$coefficients[2,"Std. Error"])
model <- "normal univariado"

df.normal <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)




### reg beta

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_facets_Beta_linear_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- term <- c("Anxiousness", "EmotionalLability", "Impulsivity", "PerceptualDysregulation")
coeff <- c(unname(sum_a$coefficients$mean[3,"Estimate"]),unname(sum_a$coefficients$mean[10,"Estimate"]), unname(sum_a$coefficients$mean[13,"Estimate"]), unname(sum_a$coefficients$mean[17,"Estimate"]))
se <- c(unname(sum_a$coefficients$mean[3,"Std. Error"]), unname(sum_a$coefficients$mean[10,"Std. Error"]), unname(sum_a$coefficients$mean[13,"Std. Error"]), unname(sum_a$coefficients$mean[17,"Std. Error"]))
model <- rep("beta multivariado",4)

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

### reg univariate beta

# Anxiety 

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_beta_model/escalada/Anxiousness.norm_mc_PID_facets_beta_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Anxiousness"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariado"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

# EmotionalLability 

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_beta_model/escalada/EmotionalLability.norm_mc_PID_facets_beta_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "EmotionalLability"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariado"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

# Impulsivity 

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_beta_model/escalada/Impulsivity.norm_mc_PID_facets_beta_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Impulsivity"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariado"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

# PerceptualDysregulation 

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_beta_model/escalada/PerceptualDysregulation.norm_mc_PID_facets_beta_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "PerceptualDysregulation"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariado"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)


### reg log mixta

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_facets_logistic_mixed_model.RData")
load(file= filepath)

# extraigo info
term <- term <- c("Anxiousness", "EmotionalLability", "Impulsivity", "PerceptualDysregulation")
coeff <- c(a_log@beta[4],a_log@beta[11],a_log@beta[14],a_log@beta[18])
sum_a_log <- summary(a_log)
se <- c(unname(sum_a_log$coefficients[4,"Std. Error"]),unname(sum_a_log$coefficients[11,"Std. Error"]),unname(sum_a_log$coefficients[14,"Std. Error"]),unname(sum_a_log$coefficients[18,"Std. Error"]))
model <- rep("mixto logístico multivariado",4)

# guardo en el df
df.log_mix <- data.frame(terms = term,
                         coeff = coeff,
                         se = se,
                         model = model)


# uno con el df principal
df.models <- rbind(df.models, df.log_mix)

### reg univariate log mix

# Anxioussness
# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_mixed_logistic_model/Anxiousness_mc_PID_facets_mixed_logistic_model.RData")
load(file= filepath)

# extraigo info
term <- "Anxiousness"
coeff <- a_log@beta[3]
sum_a_log <- summary(a_log)
se <- unname(sum_a_log$coefficients[3,"Std. Error"])
model <- "mixto logístico univariado"

df.log <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.log)

# EmotionalLability
# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_mixed_logistic_model/EmotionalLability_mc_PID_facets_mixed_logistic_model.RData")
load(file= filepath)

# extraigo info
term <- "EmotionalLability"
coeff <- a_log@beta[3]
sum_a_log <- summary(a_log)
se <- unname(sum_a_log$coefficients[3,"Std. Error"])
model <- "mixto logístico univariado"

df.log <- data.frame(terms = term,
                     coeff = coeff,
                     se = se,
                     model = model)

# uno con el df principal
df.models <- rbind(df.models, df.log)

# Impulsivity
# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_mixed_logistic_model/Impulsivity_mc_PID_facets_mixed_logistic_model.RData")
load(file= filepath)

# extraigo info
term <- "Impulsivity"
coeff <- a_log@beta[3]
sum_a_log <- summary(a_log)
se <- unname(sum_a_log$coefficients[3,"Std. Error"])
model <- "mixto logístico univariado"

df.log <- data.frame(terms = term,
                     coeff = coeff,
                     se = se,
                     model = model)

# uno con el df principal
df.models <- rbind(df.models, df.log)

# PerceptualDysregulation
# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_mixed_logistic_model/PerceptualDysregulation_mc_PID_facets_mixed_logistic_model.RData")
load(file= filepath)

# extraigo info
term <- "PerceptualDysregulation"
coeff <- a_log@beta[3]
sum_a_log <- summary(a_log)
se <- unname(sum_a_log$coefficients[3,"Std. Error"])
model <- "mixto logístico univariado"

df.log <- data.frame(terms = term,
                     coeff = coeff,
                     se = se,
                     model = model)

# uno con el df principal
df.models <- rbind(df.models, df.log)

# elastic net 
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_facets_fit_elasticNet_alpha_lambda_loocv.RData")
load(file= filepath)

# extraigo info
term <- term <- c("Anxiousness", "EmotionalLability", "Impulsivity", "PerceptualDysregulation")
coeff <- c(a[4],a[11],a[14],a[18])
se <- rep(0,4) ## sd is not usefull in regularized regressions, could cause misinterpretations
model <- rep("elastic-net",4)

# guardo en el df
df.elastic <- data.frame(terms = term,
                         coeff = coeff,
                         se = se,
                         model = model)


# uno con el df principal
df.models <- rbind(df.models, df.elastic)


##### se preprocesa para plotear

# saco la intercept que no es intepretable
df.models <- df.models %>%
  mutate(model = fct_relevel(model, "normal multivariado",
                             "normal univariado",
                             "beta multivariado",
                             "beta univariado",
                             "mixto logístico multivariado",
                             "mixto logístico univariado",
                             "elastic-net"
                             ))


## plot models

ggplot(df.models , aes(coeff, model, color=terms)) + # fct_rev(model)
  geom_point(aes(shape=terms),size=4, 
             position=position_dodge(width=0.8)) +
  geom_vline(xintercept= 0, linetype='dashed', color= "black")+
  scale_color_manual(name="terms",
                     values=c("red", "blue", "green", "orange")) +
  scale_shape_manual(name="terms",values=c(17,18,19,20)) + 
  scale_x_continuous("regression coefficients") +
  scale_y_discrete(labels= c("normal multivariate",
                             "normal univariate",
                             "beta multivariate",
                             "beta univariate",
                             "mixed logistic multivariate",
                             "mixed logistic univariate",
                             "elastic-net"))+
  geom_errorbar(aes(xmin= coeff - 2* se,xmax= coeff + 2* se),
                width=0.1,
                position=position_dodge(width=0.8), size = 1)+
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

ggsave("git/Figures/Figures/Figure3a.png", 
       width = 10, height = 6)
