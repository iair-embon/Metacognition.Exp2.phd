### Models for predict confidence from domains - Figure6b

# set root
root <- rprojroot::is_rstudio_project
basename(getwd())  

# library
library(jtools)
library(tidyverse)

### reg normal

# cargo datos
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_domain_linear_model.RData")
load(file= filepath)

# extraigo la info
sum_a <- summary(a)
terms <-rownames(sum_a$coefficients)
coeff <- unname(sum_a$coefficients[,"Estimate"])
se <- unname(sum_a$coefficients[,"Std. Error"])
model <- rep("normal multivariate", length(terms))

# creo el df que va a guardar todo
df.models <- data.frame(terms = terms,
                 coeff = coeff,
                 se = se,
                 model = model)

### reg univariate normal

## Domain Negative Affect

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_domain_linear_model/DomainNegativeAffect.norm_Conf_PID_domain_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a_NegAff <- summary(a)
terms_NegAff <- "DomainNegativeAffect.norm"
coeff_NegAff <- unname(sum_a_NegAff$coefficients[2,"Estimate"])
se_NegAff <- unname(sum_a_NegAff$coefficients[2,"Std. Error"])

## Domain Detachment

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_domain_linear_model/DomainDetachment.norm_Conf_PID_domain_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a_Det <- summary(a)
terms_Det <- "DomainDetachment.norm"
coeff_Det <- unname(sum_a_Det$coefficients[2,"Estimate"])
se_Det <- unname(sum_a_Det$coefficients[2,"Std. Error"])

## Domain Antagonism

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_domain_linear_model/DomainAntagonism.norm_Conf_PID_domain_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a_Ant <- summary(a)
terms_Ant <- "DomainAntagonism.norm"
coeff_Ant <- unname(sum_a_Ant$coefficients[2,"Estimate"])
se_Ant <- unname(sum_a_Ant$coefficients[2,"Std. Error"])

## Domain Disinhibition

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_domain_linear_model/DomainDisinhibition.norm_Conf_PID_domain_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a_Dis <- summary(a)
terms_Dis <- "DomainDisinhibition.norm"
coeff_Dis <- unname(sum_a_Dis$coefficients[2,"Estimate"])
se_Dis <- unname(sum_a_Dis$coefficients[2,"Std. Error"])

## Domain Psychoticism

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_domain_linear_model/DomainPsychoticism.norm_Conf_PID_domain_linear_model.RData")
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
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_domain_Beta_linear_model.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
terms <- rownames(sum_a$coefficients$mean)
coeff <- unname(sum_a$coefficients$mean[,"Estimate"])
se <- unname(sum_a$coefficients$mean[,"Std. Error"])
model <- rep("beta multivariate", length(terms))

df.beta <- data.frame(terms = terms,
                        coeff = coeff,
                        se = se,
                        model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

### reg univariate beta

## Domain Negative Affect

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_domain_beta_model/DomainNegativeAffect.norm_Conf_PID_domain_beta_model.RData")
load(file= filepath)

# extraigo info
sum_a_NegAff <- summary(a)
terms_NegAff <- "DomainNegativeAffect.norm"
coeff_NegAff <- unname(sum_a_NegAff$coefficients$mean[2,"Estimate"])
se_NegAff <- unname(sum_a_NegAff$coefficients$mean[2,"Std. Error"])

## Domain Detachment

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_domain_beta_model/DomainDetachment.norm_Conf_PID_domain_beta_model.RData")
load(file= filepath)

# extraigo info
sum_a_Det <- summary(a)
terms_Det <- "DomainDetachment.norm"
coeff_Det <- unname(sum_a_Det$coefficients$mean[2,"Estimate"])
se_Det <- unname(sum_a_Det$coefficients$mean[2,"Std. Error"])

## Domain Antagonism

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_domain_beta_model/DomainAntagonism.norm_Conf_PID_domain_beta_model.RData")
load(file= filepath)

# extraigo info
sum_a_Ant <- summary(a)
terms_Ant <- "DomainAntagonism.norm"
coeff_Ant <- unname(sum_a_Ant$coefficients$mean[2,"Estimate"])
se_Ant <- unname(sum_a_Ant$coefficients$mean[2,"Std. Error"])

## Domain Disinhibition

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_domain_beta_model/DomainDisinhibition.norm_Conf_PID_domain_beta_model.RData")
load(file= filepath)

# extraigo info
sum_a_Dis <- summary(a)
terms_Dis <- "DomainDisinhibition.norm"
coeff_Dis <- unname(sum_a_Dis$coefficients$mean[2,"Estimate"])
se_Dis <- unname(sum_a_Dis$coefficients$mean[2,"Std. Error"])

## Domain Psychoticism

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/individual_Conf_PID_domain_beta_model/DomainPsychoticism.norm_Conf_PID_domain_beta_model.RData")
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
                     values=c("green","orange",  "blue", "brown")) +
  scale_shape_manual(name="Modelos",values=c(17,19, 20, 15)) + 
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
      legend.text =  element_text(size = 19),
      legend.title =  element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(size = 19),
      axis.text.y = element_text(size = 19),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 19))


ggsave("git/Figures/Figures/Figure6b.png", 
       width = 10, height = 6)
