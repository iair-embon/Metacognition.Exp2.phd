#################################################
### Mixed Logistic Regression Analysis AUROC2 ### TAB 3.5
#################################################

require(gtsummary)
require(dplyr)
library(webshot2)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_facets_logistic_mixed_model.RData")
load(file= filepath)

table1 <- a_log %>%
  tbl_regression(
    intercept = T,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun =  ~style_number (.x, digits = 3),
    label = list(
      "(Intercept)" ~ "Intercept",
      "confidence_key.norm" ~ "confidence_key.norm",
      "confidence_key.norm:Anhedonia.norm" ~ "Anhedonia.std:Confidence_key.norm",
      "confidence_key.norm:Anxiousness.norm" ~ "Anxiousness.std:Confidence_key.norm",
      "confidence_key.norm:AttentionSeeking.norm" ~ "AttentionSeeking.std:Confidence_key.norm",
      "confidence_key.norm:Callousness.norm" ~ "Callousness.std:Confidence_key.norm",
      "confidence_key.norm:Deceitfulness.norm" ~ "Deceitfulness.std:Confidence_key.norm",
      "confidence_key.norm:Depressivity.norm" ~ "Depressivity.std:Confidence_key.norm",
      "confidence_key.norm:Distractivility.norm" ~ "Distractivility.std:Confidence_key.norm",
      "confidence_key.norm:Excentricity.norm" ~ "Excentricity.std:Confidence_key.norm",
      "confidence_key.norm:EmotionalLability.norm" ~ "EmotionalLability.std:Confidence_key.norm",
      "confidence_key.norm:Grandiosity.norm" ~ "Grandiosity.std:Confidence_key.norm",
      "confidence_key.norm:Hostility.norm" ~ "Hostility.std:Confidence_key.norm",
      "confidence_key.norm:Impulsivity.norm" ~ "Impulsivity.std:Confidence_key.norm",
      "confidence_key.norm:IntimacyAvoidance.norm" ~ "IntimacyAvoidance.std:Confidence_key.norm",
      "confidence_key.norm:Irresponsibility.norm" ~ "Irresponsibility.std:Confidence_key.norm",
      "confidence_key.norm:Manipulativeness.norm" ~ "Manipulativeness.std:Confidence_key.norm",
      "confidence_key.norm:PerceptualDysregulation.norm" ~ "PerceptualDysregulation.std:Confidence_key.norm",
      "confidence_key.norm:Perseveration.norm" ~ "Perseveration.std:Confidence_key.norm",
      "confidence_key.norm:RestrictedAffectivity.norm" ~ "RestrictedAffectivity.std:Confidence_key.norm",
      "confidence_key.norm:RigidPerfeccionism.norm" ~ "RigidPerfeccionism.std:Confidence_key.norm",
      "confidence_key.norm:RiskTaking.norm" ~ "RiskTaking.std:Confidence_key.norm",
      "confidence_key.norm:SeparationInsecurity.norm" ~ "SeparationInsecurity.std:Confidence_key.norm",
      "confidence_key.norm:Submissiveness.norm" ~ "Submissiveness.std:Confidence_key.norm",
      "confidence_key.norm:Suspiciousness.norm" ~ "Suspiciousness.std:Confidence_key.norm",
      "confidence_key.norm:UnusualBeliefsAndExperiences.norm" ~ "UnusualBeliefsAndExperiences.std:Confidence_key.norm",
      "confidence_key.norm:Withdrawal.norm" ~ "Withdrawal.std:Confidence_key.norm",
      "confidence_key.norm:gender" ~ "Gender[m]:Confidence_key.norm",
      "confidence_key.norm:age.norm" ~ "Age.std:Confidence_key.norm")
  ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  add_q() %>%
  bold_p(t = 0.05, q = TRUE) %>%
  add_glance_table(include = deviance)

gt::gtsave(as_gt(table1), file = "git/Tables/Tables/table_3.5.png")
