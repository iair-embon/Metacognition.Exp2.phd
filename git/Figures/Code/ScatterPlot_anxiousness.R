#### regression line and scatter plot
library(tidyverse)

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("git/Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

# figure

## Retocado
ggplot(d, aes(x=Anxiousness, y=mc)) + 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  geom_ribbon(
    aes(ymin = predict(lm(mc ~ Anxiousness, data = d), newdata = data.frame(Anxiousness = Anxiousness), interval = "confidence")[, "lwr"],
        ymax = predict(lm(mc ~ Anxiousness, data = d), newdata = data.frame(Anxiousness = Anxiousness), interval = "confidence")[, "upr"]),
    fill = "lightblue", alpha = 0.5)+ 
  ylab("Metacognition") +
  xlab("Anxiousness") +
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


# ## verdadero:
# 
# ggplot(d, aes(x=Anxiousness, y=mc)) + 
#   geom_point()+
#   geom_smooth(method = "lm")+
#   ylab("Metacognition") +
#   xlab("Anxiousness") +
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


ggsave("git/Figures/Figures/scatterPlotAnxiousness.png", 
       width = 10, height = 6)
