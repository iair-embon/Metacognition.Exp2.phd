#####################################################################
### Mixed logistic Regression Analysis Metacognition - PID Domain ### 
#####################################################################

require(lme4)

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

### preprocessing
df_total$gender <- ifelse(df_total$gender == "Masculino",1,0)
df_total$discrimination_is_correct <- ifelse(df_total$discrimination_is_correct == TRUE, 1, 0)
df_total$Participant <- factor(df_total$Participant)
df_total$confidence_key.norm <- (df_total$confidence_key - 1) / 3 
df_total$age.norm <- (df_total$age - mean(df_total$age))/sd(df_total$age)
df_total$DomainNegativeAffect.norm <- (df_total$DomainNegativeAffect - mean(df_total$DomainNegativeAffect))/ sd(df_total$DomainNegativeAffect)
df_total$DomainDetachment.norm <- (df_total$DomainDetachment - mean(df_total$DomainDetachment))/ sd(df_total$DomainDetachment)
df_total$DomainAntagonism.norm <- (df_total$DomainAntagonism - mean(df_total$DomainAntagonism))/ sd(df_total$DomainAntagonism)
df_total$DomainDisinhibition.norm <- (df_total$DomainDisinhibition - mean(df_total$DomainDisinhibition))/ sd(df_total$DomainDisinhibition)
df_total$DomainPsychoticism.norm <- (df_total$DomainPsychoticism - mean(df_total$DomainPsychoticism))/ sd(df_total$DomainPsychoticism)


vec_variables_string <- colnames(df_total[30:34])
vec_variables_values <-df_total[30:34]

# corro el modelo
for (i in 1:length(vec_variables_string)) {
  a_log <- glmer(discrimination_is_correct ~ confidence_key.norm +
                   confidence_key.norm:vec_variables_values[[i]]  +
                   confidence_key.norm:gender +
                   confidence_key.norm:age.norm +
                   confidence_key.norm:gender:vec_variables_values[[i]] +
                   confidence_key.norm:age.norm:vec_variables_values[[i]] +
                   (1|Participant),
                 data = df_total,
                 family = binomial,
                 control=glmerControl(optimizer="bobyqa",
                                      optCtrl=list(maxfun=2e5)))
  print(vec_variables_string[[i]])
  print(summary(a_log))
  name <- paste('git/Data/Regression_Results/individual_mc_PID_domain_mixed_logistic_model/',
                vec_variables_string[[i]],
                '_mc_PID_domain_mixed_logistic_model.RData', 
                sep = "")
  save(a_log, file = name)
}
