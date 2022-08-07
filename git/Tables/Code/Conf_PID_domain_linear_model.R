### Regresion lineal de dominios para predecir confianza media por sujeto

# set root
root <- rprojroot::is_rstudio_project
basename(getwd())  

# library
require(gtsummary)
require(dplyr)

filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_domain_linear_model.RData")
load(file= filepath)

# tabla
tab <- a %>%
  tbl_regression(
    intercept = T,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun =  ~style_number (.x, digits = 3),
    label = list(
      "(Intercept)" ~ "Intercept",
      "DomainNegativeAffect.norm" ~ "DomainNegativeAffect",
      "DomainDetachment.norm" ~ "DomainDetachment",
      "DomainAntagonism.norm" ~ "DomaiAntagonism",
      "DomainDisinhibition.norm" ~ "DomainDisinhibition",
      "DomainPsychoticism.norm" ~ "DomainPsychoticism",
      "gender" ~ "Gender[m]",
      "age.norm" ~ "Age")
  ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  bold_p(t = 0.05) %>%
  add_glance_table(include = c(r.squared, adj.r.squared))

gt::gtsave(as_gt(tab), file = "git/Tables/Tables/Conf_PID_domain_linear_model.png")
save(tab, file = "git/Tables/Code/Conf_PID_domain_linear_model.RData")

