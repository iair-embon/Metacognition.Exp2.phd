#####################################################################
### Individual linear Regression Analysis Confidence - PID domain ### 
#####################################################################

### linear regression model 
root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("git/Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

### preprocessing
d$age.norm <- (d$age - mean(d$age))/ sd(d$age)
d$gender <- ifelse(d$gender == "Masculino",1,0)
d$DomainNegativeAffect.norm <- (d$DomainNegativeAffect - mean(d$DomainNegativeAffect))/ sd(d$DomainNegativeAffect)
d$DomainDetachment.norm <- (d$DomainDetachment - mean(d$DomainDetachment))/ sd(d$DomainDetachment)
d$DomainAntagonism.norm <- (d$DomainAntagonism - mean(d$DomainAntagonism))/ sd(d$DomainAntagonism)
d$DomainDisinhibition.norm <- (d$DomainDisinhibition - mean(d$DomainDisinhibition))/ sd(d$DomainDisinhibition)
d$DomainPsychoticism.norm <- (d$DomainPsychoticism - mean(d$DomainPsychoticism))/ sd(d$DomainPsychoticism)

vec_variables_string <- c("DomainNegativeAffect.norm",
                          "DomainDetachment.norm",
                          "DomainAntagonism.norm",
                          "DomainDisinhibition.norm",
                          "DomainPsychoticism.norm")


vec_variables_values <- list(d$DomainNegativeAffect.norm,
                             d$DomainDetachment.norm,
                             d$DomainAntagonism.norm,
                             d$DomainDisinhibition.norm,
                             d$DomainPsychoticism.norm)



# corro el modelo
for (i in 1:length(vec_variables_string)) {
  a <- lm(ConfMean ~ vec_variables_values[[i]] + 
            age.norm + 
            gender +
            age.norm:vec_variables_values[[i]] +
            gender: vec_variables_values[[i]],
          data = d)
  
  print(vec_variables_string[i])
  print(summary(a))
  name <- paste('git/Data/Regression_Results/individual_Conf_PID_domain_linear_model/',
                vec_variables_string[i],
                '_Conf_PID_domain_linear_model.RData', 
                sep = "")
  save(a, file = name)
}


