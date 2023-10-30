########################################################################
### Beta Multiple Linear Regression Analysis Confidence - PID facets ### 
########################################################################

library (betareg)

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("git/Final_Git/Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("git/Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total_filtered)

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
d$ConfMean.norm <- (d$ConfMean - 1)/3

# run model
a=betareg(ConfMean.norm ~ Anhedonia.norm +
       Anxiousness.norm +
       AttentionSeeking.norm +
       Callousness.norm +
       Deceitfulness.norm +
       Depressivity.norm +
       Distractivility.norm +
       Excentricity.norm +
       EmotionalLability.norm +
       Grandiosity.norm +
       Hostility.norm +
       Impulsivity.norm +
       IntimacyAvoidance.norm +
       Irresponsibility.norm +
       Manipulativeness.norm +
       PerceptualDysregulation.norm +
       Perseveration.norm +
       Perseveration.norm +
       RestrictedAffectivity.norm +
       RigidPerfeccionism.norm +
       RiskTaking.norm +
       SeparationInsecurity.norm +
       Submissiveness.norm +
       Suspiciousness.norm +
       UnusualBeliefsAndExperiences.norm +
       Withdrawal.norm +
       gender +
       age.norm,
     data = d) 
summary(a)

save(a, file = "git/Data/Regression_Results/Conf_PID_facets_Beta_linear_model.RData")


### power analysis based on simulations

# Define parameters for simulation
n_simulations <- 1000  # Number of simulations
effect_size <- 0.1  # Specify the effect size
sample_size <- 224

response_data <- function(sample_size = 224){
  # Set the true coefficients
  true_coefficients <- c(intercept = 0.5, Anhedonia.norm = -0.1)
  
  # Simulate predictor variable(s)
  Anhedonia.norm <- rnorm(sample_size)
  
  # Generate a linear predictor
  linear_predictor <- true_coefficients["intercept"] + true_coefficients["Anhedonia.norm"] * Anhedonia.norm
  
  # Transform the linear predictor to ensure it's within (0, 1)
  mu_sim <- plogis(linear_predictor)
  var_sim <- 5 ################ CAMBIAR LA VARIANZA
  alpha <- ((1 - mu_sim) / var_sim - 1 / mu_sim) * mu_sim ^ 2
  beta <- alpha * (1 / mu_sim - 1)
  # Simulate response variable (the dependent variable)
  response_variable <- rbeta(sample_size, 
                             shape1 = alpha,
                             shape2 = beta)
  
  # Create a data frame for the simulated data
  simulated_data <- data.frame(Anhedonia.norm, response_variable)
  
  return(simulated_data)
} 

# Initialize a vector to store results
power_results <- numeric(n_simulations)

for (i in 1:n_simulations) {
  # Simulate data based on the DGP with the specified effect size
  
  
  # Fit the beta regression model to the simulated data
  simulated_model <- betareg(response_variable ~ Anhedonia.norm, data = simulated_data)
  
  # Test for significance and store the result in the power_results vector
  power_results[i] <- ifelse(p_value < 0.05, 1, 0)
}

# Calculate power as the proportion of significant results
power <- mean(power_results)


