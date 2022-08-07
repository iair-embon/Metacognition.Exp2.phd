### Regresion lineal de facetas para predecir mc por sujeto

# set root
root <- rprojroot::is_rstudio_project
basename(getwd())  

# library
require(gtsummary)
require(dplyr)

filepath <- root$find_file("git/Data/Regression_Results/mc_PID_facets_linear_model.RData")
load(file= filepath)

tab <- a %>%
  tbl_regression(
    intercept = T,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun =  ~style_number (.x, digits = 3),
    label = list(
      "(Intercept)" ~ "Intercept",
      "Anhedonia.norm" ~ "Anhedonia",
      "Anxiousness.norm" ~ "Anxiousness",
      "AttentionSeeking.norm" ~ "AttentionSeeking",
      "Callousness.norm" ~ "Callousness",
      "Deceitfulness.norm" ~ "Deceitfulness",
      "Depressivity.norm" ~ "Depressivity",
      "Distractivility.norm" ~ "Distractivility",
      "Excentricity.norm" ~ "Excentricity",
      "EmotionalLability.norm" ~ "EmotionalLability",
      "Grandiosity.norm" ~ "Grandiosity",
      "Hostility.norm" ~ "Hostility",
      "Impulsivity.norm" ~ "Impulsivity",
      "IntimacyAvoidance.norm" ~ "IntimacyAvoidance",
      "Irresponsibility.norm" ~ "Irresponsibility",         
      "Manipulativeness.norm" ~ "Manipulativeness",         
      "PerceptualDysregulation.norm" ~ "PerceptualDysregulation",
      "Perseveration.norm" ~ "Perseveration",         
      "RestrictedAffectivity.norm" ~ "RestrictedAffectivity",   
      "RigidPerfeccionism.norm" ~ "RigidPerfeccionism",         
      "RiskTaking.norm" ~ "RiskTaking",         
      "SeparationInsecurity.norm" ~ "SeparationInsecurity",
      "Submissiveness.norm" ~ "Submissiveness",         
      "Suspiciousness.norm" ~ "Suspiciousness",         
      "UnusualBeliefsAndExperiences.norm" ~ "UnusualBeliefsAndExperiences",  
      "Withdrawal.norm" ~ "Withdrawal",         
      "gender" ~ "Gender[m]",
      "age.norm" ~ "Age")
  ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  bold_p(t = 0.05) %>%
  add_glance_table(include = c(r.squared, adj.r.squared))

gt::gtsave(as_gt(tab), file = "git/Tables/Tables/mc_PID_facets_linear_model.png")
save(tab, file = "git/Tables/Code/mc_PID_facets_linear_model.RData")

