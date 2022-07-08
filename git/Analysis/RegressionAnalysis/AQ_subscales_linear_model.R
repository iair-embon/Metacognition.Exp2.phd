###############################################
### Linear Regression Analysis AQ Subscales ### 
###############################################

### linear regression model 

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

### preprocessing
d$mc.norm <- (d$mc - mean(d$mc))/ sd(d$mc)
d$age.norm <- (d$age - mean(d$age))/ sd(d$age)
d[d == "Masculino"] <- "1"
d[d == "Femenino"] <- "0"
d$gender <- as.integer(d$gender)
d$AQ_social.norm <- (d$AQ_social - mean(d$AQ_social))/ sd(d$AQ_social)
d$AQ_AttentionSwitch.norm <- (d$AQ_AttentionSwitch - mean(d$AQ_AttentionSwitch))/ sd(d$AQ_AttentionSwitch)
d$AQ_AttentionDetail.norm <- (d$AQ_AttentionDetail - mean(d$AQ_AttentionDetail))/ sd(d$AQ_AttentionDetail)
d$AQ_communication.norm <- (d$AQ_communication - mean(d$AQ_communication))/ sd(d$AQ_communication)
d$AQ_imagination.norm <- (d$AQ_imagination - mean(d$AQ_imagination))/ sd(d$AQ_imagination)

# corro el modelo
a=lm(mc ~ AQ_social.norm+
       AQ_AttentionSwitch.norm+
       AQ_AttentionDetail.norm+
       AQ_communication.norm+
       AQ_imagination.norm+
       gender +
       age.norm,
     data = d) 
summary(a)

save(a, file = "Data/Regression_Results/AQ_subscales_linear_model.RData")
