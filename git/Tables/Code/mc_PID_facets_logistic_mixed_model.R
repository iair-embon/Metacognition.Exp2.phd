### Regresion logistica mixta de facetas para predecir mc por sujeto

# set root
root <- rprojroot::is_rstudio_project
basename(getwd())  

# library
require(gtsummary)
require(dplyr)

filepath <- root$find_file("git/Data/Regression_Results/mc_PID_facets_logistic_mixed_model.RData")
load(file= filepath)

tab <- a_log %>%
  tbl_regression(
    intercept = T,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun =  ~style_ratio (.x, digits = 3),
    tidy_fun = broom.mixed::tidy,
    label = list(
      "(Intercept)" ~ "Intercept",
      "confidence_key.norm" ~ "confidence_key",
      "confidence_key.norm:Anhedonia.norm" ~ "Anhedonia",
      "confidence_key.norm:Anxiousness.norm" ~ "Anxiousness",
      "confidence_key.norm:AttentionSeeking.norm" ~ "AttentionSeeking",
      "confidence_key.norm:Callousness.norm" ~ "Callousness",
      "confidence_key.norm:Deceitfulness.norm" ~ "Deceitfulness",
      "confidence_key.norm:Depressivity.norm" ~ "Depressivity",
      "confidence_key.norm:Distractivility.norm" ~ "Distractivility",
      "confidence_key.norm:Excentricity.norm" ~ "Excentricity",
      "confidence_key.norm:EmotionalLability.norm" ~ "EmotionalLability",
      "confidence_key.norm:Grandiosity.norm" ~ "Grandiosity",
      "confidence_key.norm:Hostility.norm" ~ "Hostility",
      "confidence_key.norm:Impulsivity.norm" ~ "Impulsivity",
      "confidence_key.norm:IntimacyAvoidance.norm" ~ "IntimacyAvoidance",
      "confidence_key.norm:Irresponsibility.norm" ~ "Irresponsibility",
      "confidence_key.norm:Manipulativeness.norm" ~ "Manipulativeness",
      "confidence_key.norm:PerceptualDysregulation.norm" ~ "PerceptualDysregulation",
      "confidence_key.norm:Perseveration.norm" ~ "Perseveration",      
      "confidence_key.norm:RestrictedAffectivity.norm" ~ "RestrictedAffectivity",   
      "confidence_key.norm:RigidPerfeccionism.norm" ~ "RigidPerfeccionism",         
      "confidence_key.norm:RiskTaking.norm" ~ "RiskTaking",         
      "confidence_key.norm:SeparationInsecurity.norm" ~ "SeparationInsecurity",
      "confidence_key.norm:Submissiveness.norm" ~ "Submissiveness",    
      "confidence_key.norm:Suspiciousness.norm" ~ "Suspiciousness",
      "confidence_key.norm:UnusualBeliefsAndExperiences.norm" ~ "UnusualBeliefsAndExperiences",  
      "confidence_key.norm:Withdrawal.norm" ~ "Withdrawal",         
      "confidence_key.norm:gender" ~ "Gender[m]",
      "confidence_key.norm:age.norm" ~ "Age")) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  bold_p(t = 0.05) %>%
  add_glance_table(include = deviance)

gt::gtsave(as_gt(tab), file = "git/Tables/Tables/mc_PID_facets_logistic_mixed_model.png")
save(tab, file = "git/Tables/Code/mc_PID_facets_logistic_mixed_model.RData")

