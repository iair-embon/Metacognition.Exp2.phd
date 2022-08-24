#### regression line and scatter plot

library(tidyverse)

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

filepath <- root$find_file("git/Data/Regression_Results/conf_PID_facets_linear_model.RData")
load(file= filepath)

# data frame with filters already applied
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("git/Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

# convert the normalized Grandiosity scores to the original scores
intercept <- coefficients(a)[[1]]
slope <- coefficients(a)[[11]]

converted_intercept <- intercept-slope*mean(d$Grandiosity)/sd(d$Grandiosity)
converted_slope <- slope/sd(d$Grandiosity)

# figure
ggplot(d, aes(x=Grandiosity, y=ConfMean)) + 
  geom_point()+
  geom_abline(intercept = converted_intercept, 
              slope = converted_slope)+
  ylab("Confidence mean") +
  xlab("Grandiosity") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 20),
        axis.text.x=element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20))
