### Modelos de grandiosidad para predecir confianza
### y ansiedad para predecir metaco

# set root
root <- rprojroot::is_rstudio_project
basename(getwd())  

# library
library(jtools)
library(tidyverse)
library(glmnet)
library(boot)

##### vamos con grandiosidad

### reg normal

# cargo datos
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo la info
sum_a <- summary(a)
term <- "Grandiosity"
coeff <- unname(sum_a$coefficients[11,"Estimate"])
se <- unname(sum_a$coefficients[11,"Std. Error"])
model <- "normal"

# creo el df que va a guardar todo
df.models <- data.frame(terms = term,
                 coeff = coeff,
                 se = se,
                 model = model)

### reg univariate normal

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

  
### reg beta

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_facets_Beta_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Grandiosity"
coeff <- unname(sum_a$coefficients$mean[11,"Estimate"])
se <- unname(sum_a$coefficients$mean[11,"Std. Error"])
model <- "beta"

df.beta <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

### reg univariate beta

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

### reg normal regularizada con elastic net

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_domain_fit_elasticNet.RData")
load(file= filepath)
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_domain_Boot_elasticNet.RData")
load(file= filepath)

# extraigo info
sum_a <- coef(fit)
term <- "Grandiosity"
coeff <- sum_a[12]
se <- sd(rep_boot$t[,12])
model <- "elastic-net"

df.elastic <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.elastic)

##### Vamos con ansiedad

### reg normal

# cargo datos
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo la info
sum_a <- summary(a)
term <- "Anxiousness"
coeff <- unname(sum_a$coefficients[3,"Estimate"])
se <- unname(sum_a$coefficients[3,"Std. Error"])
model <- "normal"

df.normal <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)

### reg univariate normal

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_linear_model/Anxiousness.norm_mc_PID_facets_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Anxiousness"
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
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_facets_Beta_linear_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Anxiousness"
coeff <- unname(sum_a$coefficients$mean[3,"Estimate"])
se <- unname(sum_a$coefficients$mean[3,"Std. Error"])
model <- "beta"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

### reg univariate beta

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_beta_model/Anxiousness.norm_mc_PID_facets_beta_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- "Anxiousness"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "beta univariate"

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
term <- "Anxiousness"
coeff <- a_log@beta[4]
sum_a_log <- summary(a_log)
se <- unname(sum_a_log$coefficients[4,"Std. Error"])
model <- "logistic mixed univariate"

# guardo en el df
df.log_mix <- data.frame(terms = term,
                         coeff = coeff,
                         se = se,
                         model = model)


# uno con el df principal
df.models <- rbind(df.models, df.log_mix)

### reg univariate log mix

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_facets_mixed_logistic_model/Anxiousness_mc_PID_facets_mixed_logistic_model.RData")
load(file= filepath)

# extraigo info
term <- "Anxiousness"
coeff <- a_log@beta[3]
sum_a_log <- summary(a_log)
se <- unname(sum_a_log$coefficients[3,"Std. Error"])
model <- "logistic mixed"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

### reg normal regularizada con elastic net

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_domain_fit_elasticNet.RData")
load(file= filepath)
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_domain_Boot_elasticNet.RData")
load(file= filepath)

# extraigo info
sum_a <- coef(fit)
term <- "Anxiousness"
coeff <- sum_a[4]
se <- sd(rep_boot$t[,4])
model <- "elastic-net"

df.elastic <- data.frame(terms = term,
                         coeff = coeff,
                         se = se,
                         model = model)

# uno con el df principal
df.models <- rbind(df.models, df.elastic)

##### se preprocesa para plotear

# saco la intercept que no es intepretable
df.models <- df.models %>%
  mutate(model = fct_relevel(model, "normal",
                             "normal univariate",
                             "beta",
                             "beta univariate",
                             "logistic mixed",
                             "logistic mixed univariate",
                             "elastic-net"
                             ))


## plot models

ggplot(df.models , aes(coeff, model, color=terms)) + # fct_rev(model)
  geom_point(aes(shape=terms),size=4, 
             position=position_dodge(width=0.4)) +
  geom_vline(xintercept= 0, linetype='dashed', color= "black")+
  scale_color_manual(name="terms",
                     values=c("red", "blue")) +
  scale_shape_manual(name="terms",values=c(17, 19)) + 
  scale_x_continuous("Regression coefficient") +
  scale_y_discrete(labels= c("normal",
                             "normal univariate",
                             "beta",
                             "beta univariate",
                             "logistic mixed",
                             "logistic mixed univariate",
                             "elastic-net"))+
  geom_errorbar(aes(xmin= coeff - 2* se,xmax= coeff + 2* se),
                width=0.1,
                position=position_dodge(width=0.4), size = 1)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(1, 1,1, 1, "cm"),
      legend.text =  element_text(size = 20),
      legend.title =  element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(size = 30),
      axis.text.y = element_text(size = 30),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 30))

ggsave("git/Figures/Figures/severalModels_Grandiosity_Anxiousness.png", 
       width = 10, height = 6)
