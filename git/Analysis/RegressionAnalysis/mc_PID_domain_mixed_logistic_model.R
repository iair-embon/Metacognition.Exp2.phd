#####################################################################  
### Logistic Mixed Regression Analysis Metacognition - PID domain ### 
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

a_log <- glmer(discrimination_is_correct ~ confidence_key.norm +
                 confidence_key.norm:DomainNegativeAffect.norm +
                 confidence_key.norm:DomainDetachment.norm +
                 confidence_key.norm:DomainAntagonism.norm +
                 confidence_key.norm:DomainDisinhibition.norm +
                 confidence_key.norm:DomainPsychoticism.norm +
                 confidence_key.norm:gender +
                 confidence_key.norm:age.norm +
                 (1|Participant),
               data = df_total,
               family = binomial,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))

summary(a_log)

save(a_log, file = "git/Data/Regression_Results/mc_PID_domain_logistic_mixed_model.RData")
