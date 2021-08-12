#################
### DataFrame ###
#################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/Graphics/DataFrames_ForGraphics.R"))

# get the df list
# experimento = 1,2,ambos
DF_list <- DataFrame_ForGraphics(experimento = "ambos", 
                                 filtroRT_Disc_Sup = 5000,
                                 filtroRT_Disc_Inf = 200,
                                 filtroRT_Conf_Sup = 5000,
                                 filtroRT_Conf_Inf = 0,
                                 filtroTrial = 20)

# DF_list:
# a df_total
# b d.sin.normalizar
# c d.sin.normalizar.mc.filter
# d d.mc.filter
# e d.sin.normalizar.solo.FyM
# f d.sin.normalizar.solo.FyM.mc.filter
# g d.solo.FyM.mc.filter
# h df_total.sin.normalizar.solo.FyM.mc.filter

d.sin.normalizar.solo.FyM.mc.filter <- DF_list$g
d.mc.filter <- DF_list$d
d.sin.normalizar <- DF_list$b

###############
### library ###
###############
library(arm)
library(jtools)
library(broom.mixed)
library(TMB)
library(sjPlot)
library(dotwhisker)
library(tidyverse)
library(dplyr)

#library(tidyverse)
#library(dplyr)
#library(forcats)
#library(hrbrthemes)
#library(viridis)
#library(ggridges)
#library(plyr)
#library(ggstance)
#library(sjmisc)
#library(ggeffects)
#library(scales)
#library(coefplot)

###########################
### Regression Analysis ###
###########################

### lineas para hacer regresion 

d.sin.normalizar.solo.FyM.mc.filter$aq.norm <- (d.sin.normalizar.solo.FyM.mc.filter$aq 
                                                - mean(d.sin.normalizar.solo.FyM.mc.filter$aq)
                                                ) / sd(d.sin.normalizar.solo.FyM.mc.filter$aq)


d1 = d.sin.normalizar.solo.FyM.mc.filter
d1[d1 == "Masculino"] <- "1"
d1[d1 == "Femenino"] <- "0"
d1$Im <- as.integer(d1$Im)

d1

a=lm(mc ~aq.norm +
       Im +
       edad +
       es +
       aq.norm: Im ,
     data = d1) 
summary(a)
display(a)

a=lm(mc ~aq +
       Im +
       #edad +
       #es +
       aq: Im ,
     data = d1) 
summary(a)
display(a)


a=lm(sd_c ~aq +  aq: Im , data = d.sin.normalizar.solo.FyM.mc.filter) # sin normalizar no da interaccion con sexo
summary(a)
display(a)

a=lm(mc ~aq + Im  + aq: Im , data = d.solo.FyM.mc.filter)
summary(a)
display(a)


res <- resid(a)
plot(fitted(a), res)
abline(0,0)
hist(res)

a=lm(mc ~ aq +Im + aq:Im  , data = d.solo.FyM.mc.filter)
summary(a)
display(a)

# plot metacog AQ regresion
par(mar = c(5, 5, 5, 5))
plot(d.mc.filter$aq, d.mc.filter$mc, pch = 16, cex = 1, col = "black",
     xlab = "AQ", ylab = "AUROC2", cex.axis = 1.7, cex.lab = 1.8)
abline(lm(d.mc.filter$mc ~ d.mc.filter$aq),col="black", lwd=3)


# plot metacog AQ regresion for males

l <- d.mc.filter[d.mc.filter$Im=='Masculino',]
# regresion only for males
a=lm(mc ~ aq  , data = l)
summary(a)
display(a)

par(mar = c(5, 5, 5, 5))
plot(l$aq, l$mc, pch = 16, cex = 1, col = "grey", main = "Males",
     xlab = "AQ", ylab = "AUROC2", cex.axis = 1.7, cex.lab = 1.8, cex.main = 1.8)
abline(lm(l$mc ~ l$aq),col="grey", lwd=3)

# plot metacog AQ regresion for females

l <- d.mc.filter[d.mc.filter$Im=='Femenino',]
# regresion only for females
a=lm(mc ~ aq  , data = l)
summary(a)
display(a)

par(mar = c(5, 5, 5, 5))
plot(l$aq, l$mc, pch = 16, cex = 1, col = "grey53", main = "Females",
     xlab = "AQ", ylab = "AUROC2", cex.axis = 1.7, cex.lab = 1.8, cex.main = 1.8)
abline(lm(l$mc ~ l$aq),col="grey53", lwd=3)

# Trying other models
a=lm(tr_c ~ aq  , data = d)
summary(a)
display(a)

a=lm(tr_c ~ aq + Im + aq:Im   , data = d)
summary(a)
display(a)

a=lm(tr_c ~ aq + Im + mc + aq:mc   , data = d)
summary(a)
display(a)

a=lm(aq~ mc*es  , data = d)
summary(a)

a=lm(mc ~ aq+ aq:Im, data = d)
summary(a)

# solo f
a=lm(mc~ aq, data = solo.f)
summary(a)
display(a)

plot (solo.f$aq, solo.f$mc, xlab="aq", ylab="mc")
curve (coef(a)[1] + coef(a)[2]*x, add=TRUE)

# solo m
a=lm(mc~ aq, data = d.sin.normalizar)
summary(a)
display(a)

plot (solo.m$aq, solo.m$mc, xlab="aq", ylab="mc")
curve (coef(a)[1] + coef(a)[2]*x, add=TRUE)

plot (d.sin.normalizar$aq, d.sin.normalizar$mc, xlab="aq", ylab="mc")
curve (coef(a)[1] + coef(a)[2]*x, add=TRUE)

# pruebo hacer algunas regresiones luego de sacar los que tienen metacog menor a 0.5

a=lm(mc ~  aq + Im + aq:Im, data = d.sin.normalizar.solo.FyM.mc.filter) ## DA SIGNIFICATIVO AQ, Y LA INTERACCION
summary(a)
display(a)

res <- resid(a)
plot(fitted(a), res)
abline(0,0)
hist(res)

a=lm(m_c ~ aq  , data = d.solo.FyM.mc.filter)
summary(a)
display(a)

a=lm(tr_c ~ aq  + mc, data = d.solo.FyM.mc.filter)
summary(a)
display(a)

a=lm(tr_c ~ aq + Im + aq:Im   , data = d.solo.FyM.mc.filter)
summary(a)
display(a)

a=lm(tr_c ~ aq + Im + mc + aq:mc   , data = d.solo.FyM.mc.filter)
summary(a)
display(a)

a=lm(aq~ mc*es  , data = d.solo.FyM.mc.filter)
summary(a)

a=lm(mc ~ aq+ aq:Im, data = d.solo.FyM.mc.filter)
summary(a)


# ploteo los coeficientes con plot_summs 
df.plot <- d.sin.normalizar.solo.FyM.mc.filter
#df.plot$AQ <- df.plot$aq
df.plot$Gender <- df.plot$Im
df.plot$Gender[df.plot$Gender == "Masculino"] <- "Male"
df.plot$Gender[df.plot$Gender == "Femenino"] <- "Female"

a.1=lm(mc ~ AQ+ Gender +AQ:Gender, data = df.plot) 
display(a.1)
summary(a.1)
plot_summs(a.1, plot.distributions = FALSE)+
  #ylab("Model") +
  ylab("") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25))

a=lm(mc ~aq.norm +
       Im +
       edad +
       es +
       aq.norm: Im ,
     data = d1) 
summary(a)

plot_summs(a, plot.distributions = FALSE)+
  ylab("Model") +
  ylab("") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25))


a.2=lm(mc ~ aq+ aq:Im, data = d.sin.normalizar.solo.FyM.mc.filter)
display(a.2)
plot_summs(a.1, a.2, plot.distributions = TRUE)

# ploteo los coeficientes con dotwhisker
a.1=lm(mc ~ aq+ aq:Im, data = d.solo.FyM.mc.filter)
display(a.1)

dwplot((a.1),       
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c(aq = "AQ",                       
                       Im = "Sex:")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25))


dwplot((a),       
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c(aq = "AQ",                       
                       Im = "Sex:")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25))



# ploteo con plot model   
df <- d.solo.FyM.mc.filter

# make categorical
df$Im <- to_factor(df$Im)

# fit model with interaction
fit <- lm(mc ~ aq + aq * Im, data = df)

plot_model(fit, type = "pred", terms = c("aq", "Im"))

# plot model!!

a=lm(mc ~aq.norm +
       Im +
       edad +
       es +
       aq.norm: Im ,
     data = d1) 
summary(a)


p <- plot_model(a, type = "pred", terms = c("aq.norm", "Im"),
           axis.labels = c('AUROC2','AQ standarized'),
           title = 'Predicted AUROC2 for AQ by Sex', 
           axis.title = c('AUROC2','AQ standarized'),
           wrap.title = 50,
           show.data = TRUE,
           wrap.labels = 50)
#p + theme_sjplot()
#p + theme_sjplot(base_size = 12, base_family = "")
#p + theme_sjplot2(base_size = 12, base_family = "")
#p + theme_blank(base_size = 12, base_family = "")
#p + theme_538(base_size = 12, base_family = "")
#p + label_angle(angle.x, angle.y,
#                base.theme = theme_blank(base_size = 12, base_family = ""))
p + font_size(title, axis_title.x, axis_title.y, labels.x, labels.y, offset.x,
          offset.y, base.theme= theme_blank(base_size = 12, base_family = ""))

# ploteo los coeficientes con bar plot

# intento 1
df.plot <- d.solo.FyM.mc.filter
df.plot$AQ <- df.plot$aq
df.plot$Gender <- df.plot$Im
df.plot$Gender[df.plot$Gender == "Masculino"] <- "Male"
df.plot$Gender[df.plot$Gender == "Femenino"] <- "Female"

a1=lm(mc ~ AQ + Gender + AQ: Gender , data = df.plot)
summary(a1)
display(a1)

coeff <- coefficients(a1)
coeff <- coeff[2:4]
sd_coef <- unname(summary(a1)$coefficients[2:4,2])
names.coef <- names(coeff)
model <- rep(1, length(coeff))

dtf1 <- data.frame(Predictor = names.coef,
                   y = coeff,
                   sd= sd_coef,
                   model = model)

row.names(dtf1) <- NULL

a2=lm(mc ~ AQ + Gender + edad + es + AQ: Gender , data = df.plot)
summary(a2)
display(a2)

coeff <- coefficients(a2)
coeff <- coeff[2:10]
sd_coef <- unname(summary(a2)$coefficients[2:10,2])
names.coef <- names(coeff)
# names.coef[1] <- "AQ2"
# names.coef[2] <-"GenderMale2"
# names.coef[3] <-"edad2"
# names.coef[4] <-"esPosgrado incompleto2"
# names.coef[5] <-"esSecundaria completa2"
# names.coef[6] <-"esSecundaria incompleta2"
# names.coef[7] <-"esUniversidad completa2"
# names.coef[8] <-"esUniversidad incompleta2"
# names.coef[9] <-"AQ:GenderMale2"   
model <- rep(2, length(coeff))

dtf2 <- data.frame(Predictor = names.coef,
                   y = coeff,
                   sd= sd_coef,
                   model=model)
row.names(dtf2) <- NULL

dtf.plot <- rbind(dtf1,dtf2)


ggplot(dtf.plot, aes(Predictor, y)) +                                  # VA ESTE PARA REGRESION
  geom_bar(stat = "identity", aes(fill = Predictor), width = 0.9) +
  facet_grid(. ~model)+ 
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=.2,
                position=position_dodge(.9)) +
  geom_hline(yintercept=0) +
  theme_bw() + xlab("") + ylab("Regression coefficient") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text =element_blank(),
        legend.position = "none",
        aspect.ratio = 2/0.7,
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30))

# intento 2

plot_coeffs <- function(mlr_model) {
  coeffs <- coefficients(mlr_model)
  mp <- barplot(coeffs, col="#3F97D0", xaxt='n', main="Regression Coefficients")
  lablist <- names(coeffs)
  text(mp, par("usr")[3], labels = lablist, srt = 45, 
       adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
}

a=lm(mc ~aq + edad + es + Im + aq: Im , data = d.sin.normalizar.solo.FyM.mc.filter)
summary(a)

a=lm(mc ~aq + Im + aq: Im , data = d.sin.normalizar.solo.FyM.mc.filter)
summary(a)


plot_coeffs(a)


# probando coefplot
a=lm(mc ~ AQ + Gender + AQ: Gender , data = df.plot)
summary(a)

coefplot (a, CI=2,)

# pruebas con cod de guille

m5 <- glmer(PC ~ confidence_key*AQ*genero + (1 + confidence_key + confidence_key:genero|sujetos), 
            data = df_total.sin.normalizar.solo.FyM.mc.filter,
            family = binomial,
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m5)


a <- lm(mc ~ #DomainPsychoticism +
          #DomainDisinhibition +
          #DomainAntagonism+
          #DomainDetachment+
          DomainNegativeAffect + 
          edad:DomainNegativeAffect + 
          Im:DomainNegativeAffect , data = d.mc.filter)
summary(a)

a <- lm(mc ~ #Anhedonia+
        #Anxiousness+
        #AttentionSeeking+
        #Callousness +
        #Deceitfulness+
        #Depressivity +
        #Distractivility +
        #Excentricity +
        #EmotionalLability+
        #Grandiosity +
        #Hostility +
        #Impulsivity +
        #IntimacyAvoidance +
        #Irresponsibility +
        #Manipulativeness +
        #PerceptualDysregulation +
        #Perseveration +
        #RestrictedAffectivity +
        #RigidPerfeccionism +
        RiskTaking +
        #SeparationInsecurity+
        #Submissiveness +
        #Suspiciousness +
        #UnusualBeliefsAndExperiences +
        #Withdrawal+
        edad + Im + edad:RiskTaking+
        Im: RiskTaking, data = d.mc.filter)

summary(a)
plot_model(a)

d <- d.mc.filter
vec_variables_string <- c("Anhedonia",
                   "Anxiousness",
                   "AttentionSeeking",
                   "Callousness",
                   "Deceitfulness",
                   "Depressivity",
                   "Distractivility",
                   "Excentricity",
                   "EmotionalLability",
                   "Grandiosity",
                   "Hostility",
                   "Impulsivity",
                   "IntimacyAvoidance",
                   "Irresponsibility",
                   "Manipulativeness",
                   "PerceptualDysregulation",
                   "Perseveration",
                   "RestrictedAffectivity",
                   "RigidPerfeccionism",
                   "RiskTaking",
                   "SeparationInsecurity",
                   "Submissiveness",
                   "Suspiciousness",
                   "UnusualBeliefsAndExperiences",
                   "Withdrawal")


vec_variables_values <- list(d$Anhedonia,
                          d$Anxiousness,
                          d$AttentionSeeking,
                          d$Callousness,
                          d$Deceitfulness,
                    d$Depressivity,
                   d$Distractivility,
                   d$Excentricity,
                   d$EmotionalLability,
                   d$Grandiosity,
                   d$Hostility,
                   d$Impulsivity,
                   d$IntimacyAvoidance,
                   d$Irresponsibility,
                   d$Manipulativeness,
                   d$PerceptualDysregulation,
                   d$Perseveration,
                   d$RestrictedAffectivity,
                   d$RigidPerfeccionism,
                   d$RiskTaking,
                   d$SeparationInsecurity,
                   d$Submissiveness,
                   d$Suspiciousness,
                   d$UnusualBeliefsAndExperiences,
                   d$Withdrawal)

for (i in 1:length(vec_variables_string)) {
  a <- lm(d2$mc ~ vec_variables_values[[i]] + 
            d$edad + d$Im +
            d$edad:vec_variables_values[[i]] +
            d$Im: vec_variables_values[[i]])
  print(vec_variables_string[i])
  print(summary(a))
}
