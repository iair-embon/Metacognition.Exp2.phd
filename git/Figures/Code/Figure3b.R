### Figure3b

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
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo la info
sum_a <- summary(a)
term <- c("Grandiosity", "Hostility", "Impulsivity", "PerceptualDysregulation", "RestrictedAffectivity", "SeparationInsecurity", "Submissiveness")
coeff <- c(unname(sum_a$coefficients[11,"Estimate"]),unname(sum_a$coefficients[12,"Estimate"]), unname(sum_a$coefficients[13,"Estimate"]), unname(sum_a$coefficients[17,"Estimate"]),unname(sum_a$coefficients[19,"Estimate"]),unname(sum_a$coefficients[22,"Estimate"]),unname(sum_a$coefficients[23,"Estimate"]))
se <- c(unname(sum_a$coefficients[11,"Std. Error"]), unname(sum_a$coefficients[12,"Std. Error"]), unname(sum_a$coefficients[13,"Std. Error"]), unname(sum_a$coefficients[17,"Std. Error"]),unname(sum_a$coefficients[19,"Std. Error"]),unname(sum_a$coefficients[22,"Std. Error"]),unname(sum_a$coefficients[23,"Std. Error"]))
model <- rep("normal multivariate", 7)

# creo el df que va a guardar todo
df.models <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

### reg univariate normal

# Grandiosity

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_linear_model/Grandiosity.norm_Conf_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Grandiosity"
coeff <- unname(sum_a$coefficients[2,"Estimate"])
se <- unname(sum_a$coefficients[2,"Std. Error"])
model <- "normal univariate"

df.normal <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)

# Hostility

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_linear_model/Hostility.norm_Conf_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Hostility"
coeff <- unname(sum_a$coefficients[2,"Estimate"])
se <- unname(sum_a$coefficients[2,"Std. Error"])
model <- "normal univariate"

df.normal <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)

# Impulsivity

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_linear_model/Impulsivity.norm_Conf_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Impulsivity"
coeff <- unname(sum_a$coefficients[2,"Estimate"])
se <- unname(sum_a$coefficients[2,"Std. Error"])
model <- "normal univariate"

df.normal <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)

# PerceptualDysregulation

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_linear_model/PerceptualDysregulation.norm_Conf_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "PerceptualDysregulation"
coeff <- unname(sum_a$coefficients[2,"Estimate"])
se <- unname(sum_a$coefficients[2,"Std. Error"])
model <- "normal univariate"

df.normal <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)

# RestrictedAffectivity

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_linear_model/RestrictedAffectivity.norm_Conf_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "RestrictedAffectivity"
coeff <- unname(sum_a$coefficients[2,"Estimate"])
se <- unname(sum_a$coefficients[2,"Std. Error"])
model <- "normal univariate"

df.normal <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)

# SeparationInsecurity

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_linear_model/SeparationInsecurity.norm_Conf_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "SeparationInsecurity"
coeff <- unname(sum_a$coefficients[2,"Estimate"])
se <- unname(sum_a$coefficients[2,"Std. Error"])
model <- "normal univariate"

df.normal <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)

# Submissiveness

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_linear_model/Submissiveness.norm_Conf_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Submissiveness"
coeff <- unname(sum_a$coefficients[2,"Estimate"])
se <- unname(sum_a$coefficients[2,"Std. Error"])
model <- "normal univariate"

df.normal <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)


### reg beta

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_facets_Beta_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- c("Grandiosity", "Hostility", "Impulsivity", "PerceptualDysregulation", "RestrictedAffectivity", "SeparationInsecurity", "Submissiveness")
coeff <- c(unname(sum_a$coefficients$mean[11,"Estimate"]),unname(sum_a$coefficients$mean[12,"Estimate"]), unname(sum_a$coefficients$mean[13,"Estimate"]), unname(sum_a$coefficients$mean[17,"Estimate"]),unname(sum_a$coefficients$mean[19,"Estimate"]),unname(sum_a$coefficients$mean[22,"Estimate"]),unname(sum_a$coefficients$mean[23,"Estimate"]))
se <- c(unname(sum_a$coefficients$mean[11,"Std. Error"]), unname(sum_a$coefficients$mean[12,"Std. Error"]), unname(sum_a$coefficients$mean[13,"Std. Error"]), unname(sum_a$coefficients$mean[17,"Std. Error"]),unname(sum_a$coefficients$mean[19,"Std. Error"]),unname(sum_a$coefficients$mean[22,"Std. Error"]),unname(sum_a$coefficients$mean[23,"Std. Error"]))
model <- rep("beta multivariate",7)

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

### reg univariate beta

# Grandiosity 

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/Grandiosity.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Grandiosity"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

# Hostility 

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/Hostility.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Hostility"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

# Impulsivity 

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/Impulsivity.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Impulsivity"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

# PerceptualDysregulation 

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/PerceptualDysregulation.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "PerceptualDysregulation"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

# RestrictedAffectivity 

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/RestrictedAffectivity.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "RestrictedAffectivity"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

# SeparationInsecurity 

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/SeparationInsecurity.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "SeparationInsecurity"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

# Submissiveness 

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_facet_beta_model/Submissiveness.norm_Conf_PID_facets_beta_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Submissiveness"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

#### elastic net 
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_facets_fit_elasticNet_alpha_lambda_loocv.RData")
load(file= filepath)

# extraigo info
term <- term <- term <- c("Grandiosity", "Hostility", "Impulsivity", "PerceptualDysregulation", "RestrictedAffectivity", "SeparationInsecurity", "Submissiveness")
coeff <- c(a[12],a[13],a[14],a[18], a[20],a[23],a[24])
se <- rep(0,7) ## sd is not usefull in regularized regressions, could cause misinterpretations
model <- rep("elastic-net",7)

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
  mutate(model = fct_relevel(model, "normal multivariate",
                             "normal univariate",
                             "beta multivariate",
                             "beta univariate",
                             "elastic-net"
                             ))


## plot models

ggplot(df.models , aes(coeff, model, color=terms)) + # fct_rev(model)
  geom_point(aes(shape=terms),size=4, 
             position=position_dodge(width=0.9)) +
  geom_vline(xintercept= 0, linetype='dashed', color= "black")+
  scale_color_manual(name="terms",
                     values=c("red", "blue", "green", "orange", "darkred","darkgreen","gray")) +
  scale_shape_manual(name="terms",values=c(17,18,19,20,21,22,23)) + 
  scale_x_continuous("regression coefficients") +
  scale_y_discrete(labels= c("normal multivariate",
                             "normal univariate",
                             "beta multivariate",
                             "beta univariate",
                             "elastic-net"))+
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

ggsave("git/Figures/Figures/Figure3b.png", 
       width = 10, height = 6)
