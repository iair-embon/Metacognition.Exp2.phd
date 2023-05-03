##################################################
### beta Regression Analysis confidence facets ### TAB 3.17
##################################################

require(gtsummary)
require(dplyr)
library(webshot2)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_domain_Beta_linear_model.RData")
load(file= filepath)

table1 <- a %>%
  tbl_regression(
    intercept = T,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun =  ~style_number (.x, digits = 3),
    label = list(
      "(Intercept)" ~ "Intercept",
      "DomainNegativeAffect.norm" ~ "DomainNegativeAffect.std",
      "DomainDetachment.norm" ~ "DomainDetachment.std",
      "DomainAntagonism.norm" ~ "DomainAntagonism.std",
      "DomainDisinhibition.norm" ~ "DomainDisinhibition.std",
      "DomainPsychoticism.norm" ~ "DomainPsychoticism.std",
      "gender" ~ "Gender[m]",
      "age.norm" ~ "Age.std")
  ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  add_q() %>%
  bold_p(t = 0.05, q = TRUE)

gt::gtsave(as_gt(table1), file = "git/Tables/Tables/table_3.17.png")
