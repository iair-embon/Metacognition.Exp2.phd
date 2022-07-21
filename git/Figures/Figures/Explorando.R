# Exploring relationship and graphics
library(tidyverse)
library(ggExtra)

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("git/Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

# exploring psychoticism domain in relation with the metacognition and age
p <- ggplot(d, aes(x=DomainPsychoticism, y=mc, color=age, size=age)) +
  geom_point() +
  theme(legend.position="none")

p2 <- ggMarginal(p, type="density")
# no pareciera ser normal

# testeo normalidad usando el test adecuado
library(dgof)
x <- rnorm(nrow(d))
ks.test(x, d$DomainPsychoticism)
# no es normal la distribucion. Puebo en los otros dominios.


ks.test(x, d$DomainAntagonism)
ks.test(x, d$DomainDetachment)
ks.test(x, d$DomainDisinhibition)
ks.test(x, d$DomainNegativeAffect)

# ninguna es normal...

# pruebo normalizar una y luego polotear para ver como se ve

DomainPsychoticism.norm <- (d$DomainPsychoticism- mean(d$DomainPsychoticism)) / 
  sd(d$DomainPsychoticism)

# ploteo el resultado
plot(density(DomainPsychoticism.norm))

# pruebo hacer el test de normalidad devuelta
ks.test(x, DomainPsychoticism.norm)
# ahora si es normal por poco...

# bueno, sigo explorando
p <- ggplot(d, aes(x=PerceptualDysregulation, y=mc, color=age, size=age)) +
  geom_point() +
  geom_smooth()+
  #xlim(0,3000) +
  theme(legend.position="none")

p2 <- ggMarginal(p, type="density")
