#############################################
### Density plot about domains - Figure 2 ### 
#############################################

library(tidyverse)
library(jtools)
library(boot)


root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("git/Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

DomainPsychoticism <- d$DomainPsychoticism
DomainDisinhibition  <- d$DomainDisinhibition
DomainAntagonism <- d$DomainAntagonism
DomainDetachment <- d$DomainDetachment
DomainNegativeAffect <- d$DomainNegativeAffect

DomainValues <- c(DomainPsychoticism,DomainDisinhibition,DomainAntagonism,
                  DomainDetachment,DomainNegativeAffect)

DomainPsychoticism_label <- rep("Psychoticism",length(DomainPsychoticism))
DomainDisinhibition_label <- rep("Disinhibition", length(DomainDisinhibition)) 
DomainAntagonism_label <- rep("Antagonism", length(DomainAntagonism))
DomainDetachment_label <- rep("Detachment", length(DomainDetachment))
DomainNegativeAffect_label <- rep("Negative Affect", length(DomainNegativeAffect))

DomainLabels <- c(DomainPsychoticism_label,DomainDisinhibition_label,
                  DomainAntagonism_label, DomainDetachment_label,
                  DomainNegativeAffect_label)

d1 <- data.frame(DomainValues = DomainValues,
                 DomainLabels = DomainLabels)

ggplot(d1, aes(x = DomainValues, color = DomainLabels)) + 
  geom_density(alpha=0.3,size=1.5)+
  scale_x_continuous(expand = c(.0, 0),limits = c(0.1, 2.0)) +
  labs(colour = "Domain", x = "Domain Values")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.title = element_text(size = 20),
        #legend.title = element_blank(),
        legend.text = element_text(size = 20),
        #legend.text = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 


ggsave("git/Figures/Figures/Figure_2.png",width = 10, height = 6)
