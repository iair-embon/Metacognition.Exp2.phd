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

Linear_Model <- a

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
  
### reg beta

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_domain_Beta_linear_model.RData")
load(file= filepath)

Beta_Model <- a

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

### reg log mixta

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_domain_logistic_mixed_model.RData")
load(file= filepath)

log_mixed_Model <- a_log

# extraigo info
terms <-attr(a_log@pp$X, "dimnames")[[2]]
terms <- c("(Intercept)","confidence_key.norm"  ,"DomainNegativeAffect.norm", "DomainDetachment.norm", "DomainAntagonism.norm", "DomainDisinhibition.norm", "DomainPsychoticism.norm", "gender", "age.norm")
coeff <- a_log@beta
sum_a_log <- summary(a_log)
se <- unname(sum_a_log$coefficients[,"Std. Error"])
model <- rep("log_mix", length(terms))

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



# saco la intercept que no es intepretable
df.models <- df.models %>%
  filter(terms != "(Intercept)") %>%
  mutate(terms = fct_relevel(terms, "DomainNegativeAffect.norm",
                             "DomainDetachment.norm",
                             "DomainAntagonism.norm",
                             "DomainDisinhibition.norm",
                             "DomainPsychoticism.norm",
                             "gender",
                             "age.norm"))


## plot both models

############### intento 1

ggplot(df.models, aes(x = terms, y = coeff)) + 
  geom_pointrange(aes(ymax = coeff + se*2 , ymin = coeff - se*2, color = model)) +
  geom_text(aes(label = coeff), nudge_x = 0.15) + 
  scale_x_discrete("") + 
  geom_hline(yintercept = 0, color = "red") + 
  theme_bw() + 
  theme(text = element_text(size=10)) +
  ylab(NULL) + 
  coord_flip() 
#  facet_grid(~model)

############### intento 2

ggplot(df.models , aes(coeff,terms, color=model)) +
  geom_point(aes(shape=model),size=4, 
             position=position_dodge(width=0.6)) +
  geom_vline(xintercept= 0, linetype='dashed', color= "black")+
  scale_color_manual(name="Model",
                     values=c("blue","orange", "black")) +
  scale_shape_manual(name="Model",values=c(17,19, 18)) +
  scale_x_continuous("Coefficent") +
#  scale_y_discrete(labels = c("Genero")) +
  geom_errorbar(aes(xmin= coeff - 2* se,xmax= coeff + 2* se),
                width=0.1,
                position=position_dodge(width=0.6), size = 1)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(1, 1,1, 1, "cm"),
      legend.text =  element_text(size = 15),
      panel.background = element_blank(),
      axis.text.y = element_text(size = 25), #,angle = 45, hjust=1
      axis.text.x = element_text(size = 25),
      axis.title.x = element_blank(),
      axis.title.y = element_blank())


ggsave("git/Figures/Figures/severalModels_Metacognicion.png", 
       width = 10, height = 6)
