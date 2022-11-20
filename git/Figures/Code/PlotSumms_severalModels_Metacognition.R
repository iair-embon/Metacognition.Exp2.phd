### Modelos de dominios para predecir metacognicion

# set root
root <- rprojroot::is_rstudio_project
basename(getwd())  

# library
library(jtools)
library(tidyverse)
library(lme4)

### reg normal

# cargo datos
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_domain_linear_model.RData")
load(file= filepath)

# extraigo la info
sum_a <- summary(a)
terms <-rownames(sum_a$coefficients)
coeff <- unname(sum_a$coefficients[,"Estimate"])
se <- unname(sum_a$coefficients[,"Std. Error"])
model <- rep("normal", length(terms))

# creo el df que va a guardar todo
df.models <- data.frame(terms = terms,
                 coeff = coeff,
                 se = se,
                 model = model)

### reg univariate normal

## Domain Negative Affect

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_linear_model/DomainNegativeAffect.norm_mc_PID_domain_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a_NegAff <- summary(a)
terms_NegAff <- "DomainNegativeAffect.norm"
coeff_NegAff <- unname(sum_a_NegAff$coefficients[2,"Estimate"])
se_NegAff <- unname(sum_a_NegAff$coefficients[2,"Std. Error"])

## Domain Detachment

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_linear_model/DomainDetachment.norm_mc_PID_domain_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a_Det <- summary(a)
terms_Det <- "DomainDetachment.norm"
coeff_Det <- unname(sum_a_Det$coefficients[2,"Estimate"])
se_Det <- unname(sum_a_Det$coefficients[2,"Std. Error"])

## Domain Antagonism

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_linear_model/DomainAntagonism.norm_mc_PID_domain_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a_Ant <- summary(a)
terms_Ant <- "DomainAntagonism.norm"
coeff_Ant <- unname(sum_a_Ant$coefficients[2,"Estimate"])
se_Ant <- unname(sum_a_Ant$coefficients[2,"Std. Error"])

## Domain Disinhibition

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_linear_model/DomainDisinhibition.norm_mc_PID_domain_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a_Dis <- summary(a)
terms_Dis <- "DomainDisinhibition.norm"
coeff_Dis <- unname(sum_a_Dis$coefficients[2,"Estimate"])
se_Dis <- unname(sum_a_Dis$coefficients[2,"Std. Error"])

## Domain Psychoticism

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_linear_model/DomainPsychoticism.norm_mc_PID_domain_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a_Psy <- summary(a)
terms_Psy <- "DomainPsychoticism.norm"
coeff_Psy <- unname(sum_a_Psy$coefficients[2,"Estimate"])
se_Psy <- unname(sum_a_Psy$coefficients[2,"Std. Error"])

# uno toda la info
terms <- c(terms_NegAff, terms_Det, terms_Ant, terms_Dis, terms_Psy)
coeff <- c(coeff_NegAff, coeff_Det, coeff_Ant, coeff_Dis, coeff_Psy)
se <- c(se_NegAff, se_Det, se_Ant, se_Dis, se_Psy)
model <- rep("normal univariate", length(terms))

df.normal <- data.frame(terms = terms,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.normal)

  
### reg beta

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_domain_Beta_linear_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
terms <- rownames(sum_a$coefficients$mean)
coeff <- unname(sum_a$coefficients$mean[,"Estimate"])
se <- unname(sum_a$coefficients$mean[,"Std. Error"])
model <- rep("beta", length(terms))

df.beta <- data.frame(terms = terms,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

### reg univariate beta

## Domain Negative Affect

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_beta_model/DomainNegativeAffect.norm_mc_PID_domain_beta_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a_NegAff <- summary(a)
terms_NegAff <- "DomainNegativeAffect.norm"
coeff_NegAff <- unname(sum_a_NegAff$coefficients$mean[2,"Estimate"])
se_NegAff <- unname(sum_a_NegAff$coefficients$mean[2,"Std. Error"])

## Domain Detachment

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_beta_model/DomainDetachment.norm_mc_PID_domain_beta_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a_Det <- summary(a)
terms_Det <- "DomainDetachment.norm"
coeff_Det <- unname(sum_a_Det$coefficients$mean[2,"Estimate"])
se_Det <- unname(sum_a_Det$coefficients$mean[2,"Std. Error"])

## Domain Antagonism

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_beta_model/DomainAntagonism.norm_mc_PID_domain_beta_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a_Ant <- summary(a)
terms_Ant <- "DomainAntagonism.norm"
coeff_Ant <- unname(sum_a_Ant$coefficients$mean[2,"Estimate"])
se_Ant <- unname(sum_a_Ant$coefficients$mean[2,"Std. Error"])

## Domain Disinhibition

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_beta_model/DomainDisinhibition.norm_mc_PID_domain_beta_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a_Dis <- summary(a)
terms_Dis <- "DomainDisinhibition.norm"
coeff_Dis <- unname(sum_a_Dis$coefficients$mean[2,"Estimate"])
se_Dis <- unname(sum_a_Dis$coefficients$mean[2,"Std. Error"])

## Domain Psychoticism

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_beta_model/DomainPsychoticism.norm_mc_PID_domain_beta_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a_Psy <- summary(a)
terms_Psy <- "DomainPsychoticism.norm"
coeff_Psy <- unname(sum_a_Psy$coefficients$mean[2,"Estimate"])
se_Psy <- unname(sum_a_Psy$coefficients$mean[2,"Std. Error"])

# uno toda la info
terms <- c(terms_NegAff, terms_Det, terms_Ant, terms_Dis, terms_Psy)
coeff <- c(coeff_NegAff, coeff_Det, coeff_Ant, coeff_Dis, coeff_Psy)
se <- c(se_NegAff, se_Det, se_Ant, se_Dis, se_Psy)
model <- rep("beta univariate", length(terms))

df.beta <- data.frame(terms = terms,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

### reg log mixta

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_domain_logistic_mixed_model.RData")
load(file= filepath)

# extraigo info
terms <-attr(a_log@pp$X, "dimnames")[[2]]
terms <- c("(Intercept)","confidence_key.norm"  ,"DomainNegativeAffect.norm", "DomainDetachment.norm", "DomainAntagonism.norm", "DomainDisinhibition.norm", "DomainPsychoticism.norm", "gender", "age.norm")
coeff <- a_log@beta
sum_a_log <- summary(a_log)
se <- unname(sum_a_log$coefficients[,"Std. Error"])
model <- rep("logistic mixed", length(terms))

# guardo en el df
df.log_mix <- data.frame(terms = terms,
                      coeff = coeff,
                      se = se,
                      model = model)

# saco el predictor aislado de confianza
df.log_mix <-  df.log_mix %>%
  filter(!row_number() == 2)

# uno con el df principal
df.models <- rbind(df.models, df.log_mix)

### reg univariate logistica mixta

## Domain Negative Affect

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_mixed_logistic_model/DomainNegativeAffect_mc_PID_domain_mixed_logistic_model.RData")
load(file= filepath)

# extraigo info
sum_a_log_NegAff <- summary(a_log)
terms_NegAff <- "DomainNegativeAffect.norm"
coeff_NegAff <- a_log@beta[3]
se_NegAff <- unname(sum_a_log_NegAff$coefficients[,"Std. Error"])[3]

## Domain Detachment

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_mixed_logistic_model/DomainDetachment_mc_PID_domain_mixed_logistic_model.RData")
load(file= filepath)

# extraigo info
sum_a_log_Det <- summary(a_log)
terms_Det <- "DomainDetachment.norm"
coeff_Det <- a_log@beta[3]
se_Det <- unname(sum_a_log_Det$coefficients[,"Std. Error"])[3]

## Domain Antagonism

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_mixed_logistic_model/DomainAntagonism_mc_PID_domain_mixed_logistic_model.RData")
load(file= filepath)

# extraigo info
sum_a_log_Ant <- summary(a_log)
terms_Ant <- "DomainAntagonism.norm"
coeff_Ant <- a_log@beta[3]
se_Ant <- unname(sum_a_log_Ant$coefficients[,"Std. Error"])[3]

## Domain Disinhibition

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_mixed_logistic_model/DomainDisinhibition_mc_PID_domain_mixed_logistic_model.RData")
load(file= filepath)

# extraigo info
sum_a_Dis <- summary(a_log)
terms_Dis <- "DomainDisinhibition.norm"
coeff_Dis <- a_log@beta[3]
se_Dis <- unname(sum_a_Dis$coefficients[,"Std. Error"])[3]

## Domain Psychoticism

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_mc_PID_domain_mixed_logistic_model/DomainPsychoticism_mc_PID_domain_mixed_logistic_model.RData")
load(file= filepath)

# extraigo info
sum_a_log_Psy <- summary(a_log)
terms_Psy <- "DomainPsychoticism.norm"
coeff_Psy <- a_log@beta[3]
se_Psy <- unname(sum_a_log_Psy$coefficients[,"Std. Error"])[3]

# uno toda la info
terms <- c(terms_NegAff, terms_Det, terms_Ant, terms_Dis, terms_Psy)
coeff <- c(coeff_NegAff, coeff_Det, coeff_Ant, coeff_Dis, coeff_Psy)
se <- c(se_NegAff, se_Det, se_Ant, se_Dis, se_Psy)
model <- rep("logistic mixed univariate", length(terms))

df.log_mix <- data.frame(terms = terms,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.log_mix)

# saco la intercept que no es intepretable
df.models <- df.models %>%
  filter(terms != "(Intercept)" &
           terms != "age.norm" &
           terms != "gender") %>%
  mutate(terms = fct_relevel(terms, "DomainNegativeAffect.norm",
                             "DomainDetachment.norm",
                             "DomainPsychoticism.norm",
                             "DomainDisinhibition.norm",
                             "DomainAntagonism.norm"#,
                             #"gender",
                             #"age.norm"
                             ))


## plot models

ggplot(df.models , aes(coeff,fct_rev(terms), color=model)) +
  geom_point(aes(shape=model),size=4, 
             position=position_dodge(width=0.8)) +
  geom_vline(xintercept= 0, linetype='dashed', color= "black")+
  scale_color_manual(name="Modelos",
                     values=c("green","orange", "black", "red", "blue", "brown")) +
  scale_shape_manual(name="Modelos",values=c(17,19, 18, 16, 20, 15)) +
  scale_x_continuous("Regression coefficient") +
  scale_y_discrete(labels= c(#"age",
                             #"gender",
                             "Antagonism",
                             "Disinhibition",
                             "Psychoticism",
                             "Detachment",
                             "Negative Affect"))+
  geom_errorbar(aes(xmin= coeff - 2* se,xmax= coeff + 2* se),
                width=0.1,
                position=position_dodge(width=0.8), size = 1)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(1, 1,1, 1, "cm"),
      legend.text =  element_text(size = 20),
      legend.title =  element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 20))


ggsave("git/Figures/Figures/severalModels_Metacognicion.png", 
       width = 10, height = 6)
