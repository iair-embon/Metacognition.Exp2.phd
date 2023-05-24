########################################
### beta Regression Analysis AUROC2 ### TAB 3.3
########################################

require(gtsummary)
require(dplyr)
library(webshot2)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("git/Data/Regression_Results/mc_PID_domain_Beta_linear_model_escalada.RData")
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
  bold_p(t = 0.05) #%>%
  #add_glance_table(include = c(r.squared, adj.r.squared))

gt::gtsave(as_gt(table1), file = "git/Tables/Tables/table_3.3.png")
