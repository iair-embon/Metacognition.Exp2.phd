###########################################################
### Beta Regression Analysis Metacognition - PID facets ### 
###########################################################

library(betareg)

### linear regression model 

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("git/Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

### preprocessing
d$gender <- ifelse(d$gender == "Male",1,0)

normalized_fun <- function(vec){
  vec.norm <- (vec - mean(vec) )/ sd(vec)
  return(vec.norm)
}

d$age.norm <- normalized_fun(d$age)
d$Anhedonia.norm <- normalized_fun(d$Anhedonia)
d$Anxiousness.norm <- normalized_fun(d$Anxiousness)
d$AttentionSeeking.norm <- normalized_fun(d$AttentionSeeking)
d$Callousness.norm <- normalized_fun(d$Callousness)
d$Deceitfulness.norm <- normalized_fun(d$Deceitfulness)
d$Depressivity.norm <- normalized_fun(d$Depressivity)
d$Distractivility.norm <- normalized_fun(d$Distractivility)
d$Excentricity.norm <- normalized_fun(d$Excentricity)
d$EmotionalLability.norm <- normalized_fun(d$EmotionalLability)
d$Grandiosity.norm <- normalized_fun(d$Grandiosity)
d$Hostility.norm <- normalized_fun(d$Hostility)
d$Impulsivity.norm <- normalized_fun(d$Impulsivity)
d$IntimacyAvoidance.norm <- normalized_fun(d$IntimacyAvoidance)
d$Irresponsibility.norm <- normalized_fun(d$Irresponsibility)
d$Manipulativeness.norm <- normalized_fun(d$Manipulativeness)
d$PerceptualDysregulation.norm <- normalized_fun(d$PerceptualDysregulation)
d$Perseveration.norm <- normalized_fun(d$Perseveration)
d$RestrictedAffectivity.norm <- normalized_fun(d$RestrictedAffectivity)
d$RigidPerfeccionism.norm <- normalized_fun(d$RigidPerfeccionism)
d$RiskTaking.norm <- normalized_fun(d$RiskTaking)
d$SeparationInsecurity.norm <- normalized_fun(d$SeparationInsecurity)
d$Submissiveness.norm <- normalized_fun(d$Submissiveness)
d$Suspiciousness.norm <- normalized_fun(d$Suspiciousness)
d$UnusualBeliefsAndExperiences.norm <- normalized_fun(d$UnusualBeliefsAndExperiences)
d$Withdrawal.norm <- normalized_fun(d$Withdrawal)
d$mc.norm <- (d$mc - 0.5) * 2 


vec_variables_string <- colnames(d[43:67])
vec_variables_values <-d[43:67]

# run model
for (i in 1:length(vec_variables_string)) {
  a <- betareg(mc.norm ~ vec_variables_values[[i]] + 
            age.norm + 
            gender +
            age.norm:vec_variables_values[[i]] +
            gender:vec_variables_values[[i]],
          data = d)
  print(vec_variables_string[[i]])
  print(summary(a))
  name <- paste('git/Data/Regression_Results/individual_mc_PID_facets_beta_model/',
                vec_variables_string[[i]],
                '_mc_PID_facets_beta_model_escalada.RData', 
                sep = "")
  save(a, file = name)
  print(summary(a))
}
