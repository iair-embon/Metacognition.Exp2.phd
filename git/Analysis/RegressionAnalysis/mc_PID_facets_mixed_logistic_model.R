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

normalized_fun <- function(vec){
  vec.norm <- (vec - mean(vec) )/ sd(vec)
  return(vec.norm)
}

df_total$age.norm <- normalized_fun(df_total$age)
df_total$Anhedonia.norm <- normalized_fun(df_total$Anhedonia)
df_total$Anxiousness.norm <- normalized_fun(df_total$Anxiousness)
df_total$AttentionSeeking.norm <- normalized_fun(df_total$AttentionSeeking)
df_total$Callousness.norm <- normalized_fun(df_total$Callousness)
df_total$Deceitfulness.norm <- normalized_fun(df_total$Deceitfulness)
df_total$Depressivity.norm <- normalized_fun(df_total$Depressivity)
df_total$Distractivility.norm <- normalized_fun(df_total$Distractivility)
df_total$Excentricity.norm <- normalized_fun(df_total$Excentricity)
df_total$EmotionalLability.norm <- normalized_fun(df_total$EmotionalLability)
df_total$Grandiosity.norm <- normalized_fun(df_total$Grandiosity)
df_total$Hostility.norm <- normalized_fun(df_total$Hostility)
df_total$Impulsivity.norm <- normalized_fun(df_total$Impulsivity)
df_total$IntimacyAvoidance.norm <- normalized_fun(df_total$IntimacyAvoidance)
df_total$Irresponsibility.norm <- normalized_fun(df_total$Irresponsibility)
df_total$Manipulativeness.norm <- normalized_fun(df_total$Manipulativeness)
df_total$PerceptualDysregulation.norm <- normalized_fun(df_total$PerceptualDysregulation)
df_total$Perseveration.norm <- normalized_fun(df_total$Perseveration)
df_total$RestrictedAffectivity.norm <- normalized_fun(df_total$RestrictedAffectivity)
df_total$RigidPerfeccionism.norm <- normalized_fun(df_total$RigidPerfeccionism)
df_total$RiskTaking.norm <- normalized_fun(df_total$RiskTaking)
df_total$SeparationInsecurity.norm <- normalized_fun(df_total$SeparationInsecurity)
df_total$Submissiveness.norm <- normalized_fun(df_total$Submissiveness)
df_total$Suspiciousness.norm <- normalized_fun(df_total$Suspiciousness)
df_total$UnusualBeliefsAndExperiences.norm <- normalized_fun(df_total$UnusualBeliefsAndExperiences)
df_total$Withdrawal.norm <- normalized_fun(df_total$Withdrawal)

a_log <- glmer(discrimination_is_correct ~ confidence_key.norm +
                 confidence_key.norm:Anhedonia.norm +
                 confidence_key.norm:Anxiousness.norm +
                 confidence_key.norm:AttentionSeeking.norm +
                 confidence_key.norm:Callousness.norm +
                 confidence_key.norm:Deceitfulness.norm +
                 confidence_key.norm:Depressivity.norm +
                 confidence_key.norm:Distractivility.norm +
                 confidence_key.norm:Excentricity.norm +
                 confidence_key.norm:EmotionalLability.norm +
                 confidence_key.norm:Grandiosity.norm +
                 confidence_key.norm:Hostility.norm +
                 confidence_key.norm:Impulsivity.norm +
                 confidence_key.norm:IntimacyAvoidance.norm +
                 confidence_key.norm:Irresponsibility.norm +
                 confidence_key.norm:Manipulativeness.norm +
                 confidence_key.norm:PerceptualDysregulation.norm +
                 confidence_key.norm:Perseveration.norm +
                 confidence_key.norm:Perseveration.norm +
                 confidence_key.norm:RestrictedAffectivity.norm +
                 confidence_key.norm:RigidPerfeccionism.norm +
                 confidence_key.norm:RiskTaking.norm +
                 confidence_key.norm:SeparationInsecurity.norm +
                 confidence_key.norm:Submissiveness.norm +
                 confidence_key.norm:Suspiciousness.norm +
                 confidence_key.norm:UnusualBeliefsAndExperiences.norm +
                 confidence_key.norm:Withdrawal.norm +
                 confidence_key.norm:gender +
                 confidence_key.norm:age.norm +
                 (1|Participant),
               data = df_total,
               family = binomial,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))

summary(a_log)

save(a_log, file = "git/Data/Regression_Results/mc_PID_facets_logistic_mixed_model.RData")
