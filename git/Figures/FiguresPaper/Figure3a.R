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

### reg beta

# cargo los datos
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_facets_Beta_linear_model_escalada.RData")
load(file= filepath)

# extraigo info
sum_a <- summary(a)
term <- term <- c("Anxiousness", "EmotionalLability")
coeff <- c(unname(sum_a$coefficients$mean[3,"Estimate"]),unname(sum_a$coefficients$mean[10,"Estimate"]))
se <- c(unname(sum_a$coefficients$mean[3,"Std. Error"]), unname(sum_a$coefficients$mean[10,"Std. Error"]))
model <- rep("beta multivariate",2)


# creo el df que va a guardar todo
df.models <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

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
model <- "beta univariate"

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
model <- "beta univariate"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

# uno con el df principal
df.models <- rbind(df.models, df.beta)

#### elastic net 
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_facets_fit_elasticNet_alpha_lambda_loocv.RData")
load(file= filepath)

# extraigo info
term <- term <- c("Anxiousness", "EmotionalLability")
coeff <- c(a[4],a[11])
se <- rep(0,2) ## sd is not usefull in regularized regressions, could cause misinterpretations
model <- rep("elastic-net",2)

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
  mutate(model = fct_relevel(model,
                             "beta multivariate",
                             "beta univariate",
                             "elastic-net"
                             ))


# Define el orden deseado
#order_new <- c("beta multivariate", "beta univariate", "elastic-net")

# Crea una variable auxiliar para establecer el orden de las filas
#df.models <- df.models %>%
#  mutate(model_order = factor(model, levels = order_new))


## plot models
ggplot(df.models , aes(coeff, model, color=terms)) + 
  geom_point(aes(shape=terms),size=4, 
             position=position_dodge(width=0.3)) +
  geom_vline(xintercept= 0, linetype='dashed', color= "black")+
  scale_color_manual(name="terms",
                     values=c("darkred", "darkblue")) +
  scale_shape_manual(name="terms",values=c(17,18)) + 
  scale_x_continuous("regression coefficients") +
  scale_y_discrete(labels = c("beta multivariate", "beta univariate", "elastic-net"), expand = c(0.5, 0)) + # Ajusta el expand
  geom_errorbar(aes(xmin= coeff - 2* se,xmax= coeff + 2* se),
                width=0.1,
                position=position_dodge(width=0.3), size = 1)+
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

ggsave("git/Figures/FiguresPaper/Figure3a.png", 
       width = 10, height = 6)
