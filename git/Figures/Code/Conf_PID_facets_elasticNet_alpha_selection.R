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

alpha_selected <- m_elnet$bestTune[[1]] # 0.2285714
lambda_selected <- m_elnet$bestTune[[2]] # 0.1555395

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


######## sigo probando ahora mas manual VA ESTE!!!
fit <- glmnet(d_mat, d$ConfMean, 
              family = "gaussian",
              alpha = alpha_selected)


library(reshape)

beta <- coef(fit)

tmp <- as.data.frame(as.matrix(beta))
tmp$coef <- row.names(tmp)
tmp <- reshape::melt(tmp, id = "coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- fit$lambda[tmp$variable+1] # extract the lambda values
tmp$norm <- apply(abs(beta[-1,]), 2, sum)[tmp$variable+1] # compute L1 norm

## aca si no es alguno de los siguientes poner gris en una col
## sino poner algun otro color aleatorio
## los que van en color:
## Grandiosity, Hostility, Impulsivity, RestrictedAffectivity,
## SeparationInsecurity, Submissiveness
tmp_lala <- tmp %>%
  mutate(color = (coef ==  "Grandiosity" |
                    coef ==  "Hostility" |
                    coef == "Impulsivity"|
                    coef == "RestrictedAffectivity" |
                    coef == "SeparationInsecurity" |
                    coef == "Submissiveness")) ##%>%
  ##mutate(color = (coef == "Grandiosity", "red")) #### CORREGIR ACA


### the plot without legends
ggplot(tmp_lala[tmp_lala$coef != "(Intercept)",], 
       aes(lambda, value, colour = color, group = coef)) + 
  geom_line(size = 1) + 
  scale_x_log10() +
  ylab("Coeficientes") +
  xlab("Lambda (escala log)") + 
  guides(color = guide_legend(title = ""), 
         linetype = guide_legend(title = "")) +
  geom_vline(xintercept= lambda_selected,
             linetype='dashed', color='black', size=1) +
  geom_hline(yintercept= 0,
             linetype='dashed', color='black', size=1) +
  scale_colour_manual(values = c("FALSE" = 'grey', "TRUE" = 'black')) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.text =  element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))#+ 
  #theme(legend.key.width = unit(3,"lines"))


ggsave("git/Figures/Figures/Conf_Facets_elasticNet_withoutLabels.png", 
       width = 10, height = 6)

### the plot with legends
ggplot(tmp[tmp$coef != "(Intercept)",], 
       aes(lambda, value, colour = coef)) + 
  geom_line(size = 1) + 
  scale_x_log10() +
  #ylab("Coeficientes") +
  #xlab("Lambda (escala log)") + 
  guides(color = guide_legend(title = ""), 
         linetype = guide_legend(title = "")) +
  geom_vline(xintercept= lambda_selected,
             linetype='dashed', color='black', size=1) +
  geom_hline(yintercept= 0,
             linetype='dashed', color='black', size=1) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.text =  element_text(size = 10),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank())#+ 
#theme(legend.key.width = unit(3,"lines"))


ggsave("git/Figures/Figures/Conf_Facets_elasticNet_withLabels.png", 
       width = 10, height = 6)





####################### HASTA ACA ESTA BIEN
##### intento plotear como caen las variables en base al lambda
library(plotmo)

fit <- glmnet(d_mat, d$ConfMean, 
              family = "gaussian",
              alpha = alpha_selected)
#,lambda = lambda_selected)

plotres(fit, which = 1, id.n = 1)

library(arm)
library(coefplot)
coefplot(fit, lambda=lambda_selected, sort='magnitude')
coefpath(fit, showLegend = "never",
         colorMin = "black",
         strokePatternMin = "dotted") ############# WOW VA POR ACA


plot(fit, xvar='lambda', label=TRUE)


plot(fit)


### now, I perform boostrap to know the CI and the proportion of times that the 
### predictor was exactly 0.

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
  
  coef_fit_boot <- coef(fit_boot) 
  c <- c(coef_fit_boot[1:length(coef_fit_boot)]) 
  
  return(c) 
  }

rep_boot <- boot(
    data = d,
    statistic = boot_fun,
    R = 5000, 
    alpha_selected = alpha_selected,
    lambda_selected = lambda_selected,
    ConfMean = d$ConfMean
  )

boot.ci(rep_boot, index = 12) # se puede ir variando el index

# probability of 0 (page 154, Statistical learning with sparsity... Hastie)
sum(rep_boot$t[,12] == 0)/5000
sum(rep_boot$t[,13] == 0)/5000
sum(rep_boot$t[,14] == 0)/5000
sum(rep_boot$t[,20] == 0)/5000
sum(rep_boot$t[,23] == 0)/5000
sum(rep_boot$t[,24] == 0)/5000

save(rep_boot, file = "git/Data/Regression_Results/Conf_PID_domain_Boot_elasticNet.RData")

hist(rep_boot$t[,13])
mean(rep_boot$t[,5])
plot(rep_boot, index = 2)
plot(rep_boot, index = 12)
plot(rep_boot, index = 13)
plot(rep_boot, index = 14)
