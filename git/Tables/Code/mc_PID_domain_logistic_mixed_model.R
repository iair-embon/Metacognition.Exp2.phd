### Regresion logistica mixta de dominios para predecir mc por sujeto

# set root
root <- rprojroot::is_rstudio_project
basename(getwd())  

# library
require(gtsummary)
require(dplyr)

filepath <- root$find_file("git/Data/Regression_Results/mc_PID_domain_logistic_mixed_model.RData")
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
      "confidence_key.norm:DomainNegativeAffect.norm" ~ "DomainNegativeAffect",
      "confidence_key.norm:DomainDetachment.norm" ~ "DomainDetachment",
      "confidence_key.norm:DomainAntagonism.norm" ~ "DomaiAntagonism",
      "confidence_key.norm:DomainDisinhibition.norm" ~ "DomainDisinhibition",
      "confidence_key.norm:DomainPsychoticism.norm" ~ "DomainPsychoticism",
      "confidence_key.norm:gender" ~ "Gender[m]",
      "confidence_key.norm:age.norm" ~ "Age")) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  bold_p(t = 0.05) %>%
  add_glance_table(include = deviance)

gt::gtsave(as_gt(tab), file = "git/Tables/Tables/mc_PID_domain_logistic_mixed_model.png")
save(tab, file = "git/Tables/Code/mc_PID_domain_logistic_mixed_model.RData")

