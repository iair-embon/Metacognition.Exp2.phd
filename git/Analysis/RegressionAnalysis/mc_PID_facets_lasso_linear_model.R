###################################################################
### Lasso Linear Regression Analysis Metacognition - PID facets ### 
###################################################################

### Lasso regresion model
library(glmnet)
library(dplyr)
library(HDCI)


root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("git/Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

### preprocessing
d$gender <- ifelse(d$gender == "Masculino",1,0)

# transforming the df to a matrix, without a few variables that don't matter
d_mat <- d %>%
  select(!c(Participant,
            DomainNegativeAffect,
            DomainDetachment,
            DomainAntagonism,
            DomainDisinhibition,
            DomainPsychoticism,
            PC,
            ConfMean,
            ConfSD,
            ReacTimeMean_DiscTask,
            ReacTimeSD_DiscTask,
            ReacTimeMean_ConfTask,
            ReacTimeSD_ConfTask,
            mc)) %>%
  data.matrix()

# corro el modelo
fit <- glmnet(d_mat, d$mc, alpha = 1, standardize = T)

print(fit)

plot(fit)
plot(fit, xvar = "lambda", label = TRUE)
plot(fit, xvar = "dev", label = TRUE)

# hago leave one out cross validation con lasso regression
#set.seed(1010)
cvfit <- cv.glmnet(d_mat, 
                   d$mc, 
                   family = "gaussian", 
                   type.measure = "mse", 
                   nfolds = nrow(d), 
                   alpha = 1)
print(cvfit)
# ploteo los errores cuadraticos
plot(cvfit)

# elijo el que minimiza el error
cvfit$lambda.min

# veo el modelo con ese lambda
a <- coef(cvfit, s = "lambda.min")

save(a, file = "git/Data/Regression_Results/mc_PID_facets_lasso_linear_model.RData")

### hago boostrap para este fiteo con 1000 samples

a_boot <- bootLasso(d_mat, 
          d$mc,
          B = 1000,
          type.boot = "residual",
          type.measure = "mse", 
          family = "gaussian", 
          cv.method = "cv", 
          nfolds = nrow(d))

print(a_boot)

save(a_boot, file = "git/Data/Regression_Results/mc_PID_facets_lasso_linear_model.RData")

### Hago boostrap con libreria boot

library(boot)

lasso_fun <- function(data, indices, parametro){
  boot_data <- data[indices,]
  boot_matrix <- boot_data %>%
    select(!c(Participant,
              DomainNegativeAffect,
              DomainDetachment,
              DomainAntagonism,
              DomainDisinhibition,
              DomainPsychoticism,
              PC,
              ConfMean,
              ConfSD,
              ReacTimeMean_DiscTask,
              ReacTimeSD_DiscTask,
              ReacTimeMean_ConfTask,
              ReacTimeSD_ConfTask,
              mc)) %>%
    data.matrix()
  
  cvfit <- cv.glmnet(boot_matrix, d$mc, family = "gaussian", 
                     type.measure = "mse", nfolds = parametro, 
                     alpha= 1)
  a <- coef(cvfit, s = "lambda.min")
  return(c(a))
}

rep <- boot(
  data = d,
  statistic = lasso_fun,
  R = 5, 
  parametro = nrow(d)
)

plot(rep)

boot.ci(rep, type="bca")
