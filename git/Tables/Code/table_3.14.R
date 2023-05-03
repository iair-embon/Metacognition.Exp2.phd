####################################################
### normal Regression Analysis confidence facets ### TAB 3.14
####################################################

require(gtsummary)
require(dplyr)
library(webshot2)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_facets_linear_model.RData")
load(file= filepath)

table1 <- a %>%
  tbl_regression(
    intercept = T,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun =  ~style_number (.x, digits = 3),
    label = list(
      "(Intercept)" ~ "Intercept",
      "Anhedonia.norm" ~ "Anhedonia.std",
      "Anxiousness.norm" ~ "Anxiousness.std",
      "AttentionSeeking.norm" ~ "AttentionSeeking.std",
      "Callousness.norm" ~ "Callousness.std",
      "Deceitfulness.norm" ~ "Deceitfulness.std",
      "Depressivity.norm" ~ "Depressivity.std",
      "Distractivility.norm" ~ "Distractivility.std",
      "Excentricity.norm" ~ "Excentricity.std",
      "EmotionalLability.norm" ~ "EmotionalLability.std",
      "Grandiosity.norm" ~ "Grandiosity.std",
      "Hostility.norm" ~ "Hostility.std",
      "Impulsivity.norm" ~ "Impulsivity.std",
      "IntimacyAvoidance.norm" ~ "IntimacyAvoidance.std",
      "Irresponsibility.norm" ~ "Irresponsibility.std",
      "Manipulativeness.norm" ~ "Manipulativeness.std",
      "PerceptualDysregulation.norm" ~ "PerceptualDysregulation.std",
      "Perseveration.norm" ~ "Perseveration.std",
      "RestrictedAffectivity.norm" ~ "RestrictedAffectivity.std",
      "RigidPerfeccionism.norm" ~ "RigidPerfeccionism.std",
      "RiskTaking.norm" ~ "RiskTaking.std",
      "SeparationInsecurity.norm" ~ "SeparationInsecurity.std",
      "Submissiveness.norm" ~ "Submissiveness.std",
      "Suspiciousness.norm" ~ "Suspiciousness.std",
      "UnusualBeliefsAndExperiences.norm" ~ "UnusualBeliefsAndExperiences.std",
      "Withdrawal.norm" ~ "Withdrawal.std",
      "gender" ~ "Gender[m]",
      "age.norm" ~ "Age.std")
  ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  add_q() %>%
  bold_p(t = 0.05, q = TRUE) %>%
  add_glance_table(include = c(r.squared, adj.r.squared))

gt::gtsave(as_gt(table1), file = "D:/Windows/Descargas/Git_Metacog_Personalidad/Pubilico/Metacognition.PersonalityTraits/git/Tables/Tables/table_3.14.png")
