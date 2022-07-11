#############################################################
### Linear Regression Analysis Metacognition - PID domain ### 
#############################################################

### Elastic Net regresion model
library(glmnet)
library(dplyr)

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
fit <- glmnet(d_mat, d$mc, alpha = 0.5, standardize = T)

print(fit)

plot(fit)
plot(fit, xvar = "lambda", label = TRUE)
plot(fit, xvar = "dev", label = TRUE)

# hago cross validation usando los valores por defectos
cvfit <- cv.glmnet(d_mat, d$mc, type.measure = "mse", nfolds = 10)
print(cvfit)
# ploteo los errores cuadraticos
plot(cvfit)

# elijo el que minimiza el error
cvfit$lambda.min

# veo el modelo con ese lambda
coef(cvfit, s = "lambda.min")

save(fit, file = "git/Data/Regression_Results/mc_PID_facets_elasticNet_linear_model.RData")
