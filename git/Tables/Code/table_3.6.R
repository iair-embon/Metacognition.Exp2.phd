#################################################
### Mixed Logistic Regression Analysis AUROC2 ### TAB 3.6
#################################################

require(gtsummary)
require(dplyr)
library(webshot2)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_domain_logistic_mixed_model.RData")
load(file= filepath)

table1 <- a_log %>%
  tbl_regression(
    intercept = T,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun =  ~style_number (.x, digits = 3),
    label = list(
      "(Intercept)" ~ "Intercept",
      "confidence_key.norm" ~ "confidence_key.norm",
      "confidence_key.norm:DomainNegativeAffect.norm" ~ "DomainNegativeAffect.std:Confidence_key.norm",
      "confidence_key.norm:DomainDetachment.norm" ~ "DomainDetachment.std:Confidence_key.norm",
      "confidence_key.norm:DomainAntagonism.norm" ~ "DomainAntagonism.std:Confidence_key.norm",
      "confidence_key.norm:DomainDisinhibition.norm" ~ "DomainDisinhibition.std:Confidence_key.norm",
      "confidence_key.norm:DomainPsychoticism.norm" ~ "DomainPsychoticism.std:Confidence_key.norm",
      "confidence_key.norm:gender" ~ "Gender[m]:Confidence_key.norm",
      "confidence_key.norm:age.norm" ~ "Age.std:Confidence_key.norm")
  ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  add_q() %>%
  bold_p(t = 0.05, q = TRUE) %>%
  add_glance_table(include = deviance)

gt::gtsave(as_gt(table1), file = "git/Tables/Tables/table_3.6.png")
