### ElasticNet linear model with Confidence facets - alpha selection

# libraries
library(glmnet)
library(dplyr)
library(caret)
library(boot)


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
d_caret <- d %>%
  select(!c(Participant,
            DomainNegativeAffect,
            DomainDetachment,
            DomainAntagonism,
            DomainDisinhibition,
            DomainPsychoticism,
            PC,
            ConfSD,
            ReacTimeMean_DiscTask,
            ReacTimeSD_DiscTask,
            ReacTimeMean_ConfTask,
            ReacTimeSD_ConfTask,
            mc))

### selecting the alpha and the lambda using caret
loocv = trainControl(method = "LOOCV")

m_elnet  = train(
  ConfMean ~ ., data = d_caret,
  method = "glmnet",
  trControl = loocv,
  tuneLength = 50
)

alpha_selected <- m_elnet$bestTune[[1]]
lambda_selected <- m_elnet$bestTune[[2]]

## runing the model regression with the selected alpha and lambda

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



fit <- glmnet(d_mat, d$ConfMean, 
              family = "gaussian",
              alpha = alpha_selected,
              lambda = lambda_selected)

print(fit)

a <- coef(fit)


### now, I perform boostrap to know the p-value of the not zero
# predictors coefficients

non_zero_predict <- a[a[,1]!=0,]

boot_fun <- function(data, indices, alpha_selected , lambda_selected, ConfMean){
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
  
  fit_boot <- glmnet(boot_matrix, ConfMean, 
         family = "gaussian",
         alpha = alpha_selected,
         lambda = lambda_selected)
  
  coef_fit_boot <- coef(fit_boot)[12]
  
  return(coef_fit_boot)
  }


list_rep_boot <- list()
i<- length(a[a[,1]!=0,])
while(i > 0) {
  
  rep_boot <- boot(
    data = d,
    statistic = boot_fun,
    R = 2000, 
    alpha_selected = alpha_selected,
    lambda_selected = lambda_selected,
    ConfMean = d$ConfMean,
    predictor = non_zero_predict[[i]]
  )
  
  
  list_rep_boot <- list(list_rep_boot, rep_boot)
  
  i <- i - 1
}


rep_boot

plot(rep_boot)

boot.ci(rep_boot, type="bca")
