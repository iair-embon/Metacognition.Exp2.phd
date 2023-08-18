#### regression line and scatter plot

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("git/Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

# figure
ggplot(d, aes(x=SeparationInsecurity, y=ConfMean)) + 
  geom_point()+
  geom_smooth(method = "lm",se = FALSE, color = "darkred")+
  geom_ribbon(
    aes(ymin = predict(lm(ConfMean ~ SeparationInsecurity, data = d), newdata = data.frame(SeparationInsecurity = SeparationInsecurity), interval = "confidence")[, "lwr"],
        ymax = predict(lm(ConfMean ~ SeparationInsecurity, data = d), newdata = data.frame(SeparationInsecurity = SeparationInsecurity), interval = "confidence")[, "upr"]),
    fill = "lightpink", alpha = 0.5)+
  ylab("Confidence") +
  xlab("Separation Insecurity") +
  scale_x_continuous(breaks = seq(0, max(d$SeparationInsecurity), 1), labels = scales::comma_format()) +  # Ajustar formato de los números en el eje x
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


# ggplot(d, aes(x=SeparationInsecurity, y=ConfMean)) + 
#   geom_point()+
#   geom_smooth(method = "lm")+
#   ylab("Confidence") +
#   xlab("SeparationInsecurity") +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         plot.margin = margin(1, 1,1, 1, "cm"),
#         panel.background = element_blank(),
#         axis.title.x=element_text(size = 30),
#         axis.text.x=element_text(size = 30),
#         axis.text.y = element_text(size = 30),
#         axis.title.y = element_text(size = 30))

ggsave("git/Figures/Figures/scatterPlotSeparationInsecurity.png", 
       width = 10, height = 6)
