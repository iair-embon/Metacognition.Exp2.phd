#### regression line and scatter plot

library(tidyverse)

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

filepath <- root$find_file("git/Data/Regression_Results/mc_PID_facets_linear_model.RData")
load(file= filepath)

# data frame with filters already applied
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("git/Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

# convert the normalized Grandiosity scores to the original scores
intercept <- coefficients(a)[[1]]
slope <- coefficients(a)[[3]]

converted_intercept <- intercept-slope*mean(d$Anxiousness)/sd(d$Anxiousness)
converted_slope <- slope/sd(d$Anxiousness)

# figure
ggplot(d, aes(x=Anxiousness, y=mc)) + 
  geom_point()+
  geom_abline(intercept = converted_intercept, 
              slope = converted_slope)+
  ylab("Metacognicion") +
  xlab("Ansiedad") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 30),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30))
