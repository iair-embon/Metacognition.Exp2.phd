# jugando con graficos del libro https://r4ds.had.co.nz/data-visualisation.html 
# y los datos del exp de metacog

library(tidyverse)

### ggplot 1
ggplot(data = df_DatosUnicos_mod2) + 
  geom_point(mapping = aes(x = AQ, y =auc2))

### ggplot 2 

# segun genero
ggplot(data = df_DatosUnicos_mod2) + 
  geom_point(mapping = aes(x = auc2, y = AQ, color = genero))

# segun estudio
ggplot(data = df_DatosUnicos_mod2) + 
  geom_point(mapping = aes(x = auc2, y = AQ, color = estudio))

### ggplot 3 

# con transparencias
ggplot(data = df_DatosUnicos_mod7) + 
  geom_point(mapping = aes(x = auc2, y = AQ, alpha = estudio))

# con formitas
ggplot(data = df_DatosUnicos_mod7) + 
  geom_point(mapping = aes(x = auc2, y = AQ, shape = estudio))

### ggplot 4

# facet estudios
ggplot(data = df_DatosUnicos_mod7) + 
  geom_point(mapping = aes(x = auc2, y = AQ)) + 
  facet_wrap(~ estudio, nrow = 2)

# facet genero
ggplot(data = df_DatosUnicos_mod2) + 
  geom_point(mapping = aes(x = AQ, y = media_tr_discri)) + 
  facet_wrap(~ genero, nrow = 2)


### ggplot 5

# geom

# 1
ggplot(data = df_DatosUnicos_mod7) + 
  geom_smooth(mapping = aes(x = auc2, y = AQ))


### ggplot 6

# histogramas

ggplot(data = df_DatosUnicos_mod2) + 
  geom_bar(mapping = aes(x = AQ))

# con estadisticos para una discreta
ggplot(data = df_DatosUnicos_mod2) + 
  stat_summary(
    mapping = aes(x = genero, y = auc2),
    fun.min = min,
    fun.max = max,
    fun = median
  )


### ggplot 7

# boxplot estudio

ggplot(data = df_DatosUnicos_mod2, mapping = aes(x = estudio, y = auc2)) + 
  geom_boxplot()

# boxplot genero

ggplot(data = df_DatosUnicos_mod2, mapping = aes(x = genero, y = sd_tr_confi)) + 
  geom_boxplot()

######## PARTE DE ACA / CORRER DESDE ACA
### lineas para normalizar variables y hacer regresion 

library(arm)

root <- rprojroot::is_rstudio_project
basename(getwd())

filepath <- (root$find_file("Data/df_total.Rda"))

load(file= filepath)

# tomo las variables de interes
auc2 <- rep(NaN, length(unique(df_total$sujetos)))
horasSuen <- rep(NaN, length(unique(df_total$sujetos)))
PC <- rep(NaN, length(unique(df_total$sujetos)))
genero <- rep(NaN, length(unique(df_total$sujetos)))
Anhedonia <- rep(NaN, length(unique(df_total$sujetos)))
Anxiousness <- rep(NaN, length(unique(df_total$sujetos)))
AttentionSeeking <- rep(NaN, length(unique(df_total$sujetos)))
Callousness <- rep(NaN, length(unique(df_total$sujetos)))
Deceitfulness <- rep(NaN, length(unique(df_total$sujetos)))
Depressivity <- rep(NaN, length(unique(df_total$sujetos)))
Distractivility <- rep(NaN, length(unique(df_total$sujetos)))
Excentricity <- rep(NaN, length(unique(df_total$sujetos)))
EmotionalLability <- rep(NaN, length(unique(df_total$sujetos)))
Grandiosity <- rep(NaN, length(unique(df_total$sujetos)))
Hostility <- rep(NaN, length(unique(df_total$sujetos)))
Impulsivity <- rep(NaN, length(unique(df_total$sujetos)))
IntimacyAvoidance <- rep(NaN, length(unique(df_total$sujetos)))
Irresponsibility <- rep(NaN, length(unique(df_total$sujetos)))
Manipulativeness <- rep(NaN, length(unique(df_total$sujetos)))
PerceptualDysregulation <- rep(NaN, length(unique(df_total$sujetos)))
Perseveration <- rep(NaN, length(unique(df_total$sujetos)))
RestrictedAffectivity <- rep(NaN, length(unique(df_total$sujetos)))
RigidPerfeccionism <- rep(NaN, length(unique(df_total$sujetos)))
RiskTaking <- rep(NaN, length(unique(df_total$sujetos)))
SeparationInsecurity <- rep(NaN, length(unique(df_total$sujetos)))
Submissiveness <- rep(NaN, length(unique(df_total$sujetos)))
Suspiciousness <- rep(NaN, length(unique(df_total$sujetos)))
UnusualBeliefsAndExperiences <- rep(NaN, length(unique(df_total$sujetos)))
Withdrawal <- rep(NaN, length(unique(df_total$sujetos)))
DomainNegativeAffect <- rep(NaN, length(unique(df_total$sujetos)))
DomainDetachment <- rep(NaN, length(unique(df_total$sujetos)))
DomainAntagonism <- rep(NaN, length(unique(df_total$sujetos)))
DomainDisinhibition <- rep(NaN, length(unique(df_total$sujetos)))
DomainPsychoticism <- rep(NaN, length(unique(df_total$sujetos)))
horasSueno <- rep(NaN, length(unique(df_total$sujetos)))
edad <- rep(NaN, length(unique(df_total$sujetos)))
estudio <- rep(NaN, length(unique(df_total$sujetos)))
media_tr_discri <- rep(NaN, length(unique(df_total$sujetos)))
media_tr_confi <- rep(NaN, length(unique(df_total$sujetos)))

# sujetos que quedaron
ExistingSubjects <- unique(df_total$sujetos)

for (i in 1:length(unique(df_total$sujetos))) {
  
  auc2[i] <- unique(df_total[df_total$sujetos == ExistingSubjects[i],"auc2"])
  PC[i] <- unique(df_total[df_total$sujetos == ExistingSubjects[i],"PC"])
  genero[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"genero"])
  horasSueno[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"horasSueno"])
  edad[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"edad"])
  estudio[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"estudio"])
  media_tr_discri[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"media_tr_discri"])
  media_tr_confi[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"media_tr_confi"])
  
  Anhedonia [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Anhedonia"])
  Anxiousness [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Anxiousness"])
  AttentionSeeking [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"AttentionSeeking"])
  Callousness [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Callousness"])
  Deceitfulness [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Deceitfulness"])
  Depressivity [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Depressivity"])
  Distractivility [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Distractivility"])
  Excentricity [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Excentricity"])
  EmotionalLability [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"EmotionalLability"])
  Grandiosity [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Grandiosity"])
  Hostility [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Hostility"])
  Impulsivity [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Impulsivity"])
  IntimacyAvoidance [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"IntimacyAvoidance"])
  Irresponsibility [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Irresponsibility"])
  Manipulativeness [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Manipulativeness"])
  PerceptualDysregulation [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"PerceptualDysregulation"])
  Perseveration [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Perseveration"])
  RestrictedAffectivity [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"RestrictedAffectivity"])
  RigidPerfeccionism [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"RigidPerfeccionism"])
  RiskTaking [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"RiskTaking"])
  SeparationInsecurity [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"SeparationInsecurity"])
  Submissiveness [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Submissiveness"])
  Suspiciousness [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Suspiciousness"])
  UnusualBeliefsAndExperiences [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"UnusualBeliefsAndExperiences"])
  Withdrawal  [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"Withdrawal"])
  DomainNegativeAffect  [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"DomainNegativeAffect"])
  DomainDetachment  [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"DomainDetachment"])
  DomainAntagonism  [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"DomainAntagonism"])
  DomainDisinhibition  [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"DomainDisinhibition"])
  DomainPsychoticism  [i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"DomainPsychoticism"])
  }


d = data.frame(mc  = auc2,
               Im = genero, 
               pc  = PC,
               hs = horasSueno,
               edad = edad,
               es = estudio,
               tr_d = media_tr_discri,
               tr_c = media_tr_confi,
               
               Anhedonia = Anhedonia,
               Anxiousness = Anxiousness ,
               AttentionSeeking = AttentionSeeking,
               Callousness = Callousness,
               Deceitfulness = Deceitfulness,
               Depressivity = Depressivity,
               Distractivility = Distractivility,
               Excentricity = Excentricity,
               EmotionalLability = EmotionalLability,
               Grandiosity = Grandiosity,
               Hostility = Hostility,
               Impulsivity = Impulsivity,
               IntimacyAvoidance = IntimacyAvoidance,
               Irresponsibility = Irresponsibility,
               Manipulativeness = Manipulativeness,
               PerceptualDysregulation = PerceptualDysregulation,
               Perseveration = Perseveration,
               RestrictedAffectivity = RestrictedAffectivity,
               RigidPerfeccionism = RigidPerfeccionism,
               RiskTaking = RiskTaking,
               SeparationInsecurity = SeparationInsecurity,
               Submissiveness = Submissiveness,
               Suspiciousness = Suspiciousness,
               UnusualBeliefsAndExperiences = UnusualBeliefsAndExperiences,
               Withdrawal = Withdrawal,
               DomainNegativeAffect = DomainNegativeAffect,
               DomainDetachment = DomainDetachment,
               DomainAntagonism = DomainAntagonism,
               DomainDisinhibition = DomainDisinhibition,
               DomainPsychoticism = DomainPsychoticism 
               )

d$pc <- (d$pc - mean(d$pc)) / sd(d$pc)
#d$hs <- (d$hs - mean(d$hs)) / sd(d$hs)
d$mc <- (d$mc - mean(d$mc)) / sd(d$mc)
d$tr_d <- (d$tr_d - mean(d$tr_d)) / sd(d$tr_d)
d$tr_c <- (d$tr_c - mean(d$tr_c)) / sd(d$tr_c)

d$Anhedonia <- (d$Anhedonia - mean(d$Anhedonia)) / sd(d$Anhedonia)
d$Anxiousness <- (d$Anxiousness - mean(d$Anxiousness)) / sd(d$Anxiousness)
d$AttentionSeeking <- (d$AttentionSeeking - mean(d$AttentionSeeking)) / sd(d$AttentionSeeking)
d$Callousness <- (d$Callousness - mean(d$Callousness)) / sd(d$Callousness)
d$Deceitfulness <- (d$Deceitfulness - mean(d$Deceitfulness)) / sd(d$Deceitfulness)
d$Depressivity <- (d$Depressivity - mean(d$Depressivity)) / sd(d$Depressivity)
d$Distractivility <- (d$Distractivility - mean(d$Distractivility)) / sd(d$Distractivility)
d$Excentricity <- (d$Excentricity - mean(d$Excentricity)) / sd(d$Excentricity)
d$EmotionalLability <- (d$EmotionalLability - mean(d$EmotionalLability)) / sd(d$EmotionalLability)
d$Grandiosity <- (d$Grandiosity - mean(d$Grandiosity)) / sd(d$Grandiosity)
d$Hostility <- (d$Hostility - mean(d$Hostility)) / sd(d$Hostility)
d$Impulsivity <- (d$Impulsivity - mean(d$Impulsivity)) / sd(d$Impulsivity)
d$IntimacyAvoidance <- (d$IntimacyAvoidance - mean(d$IntimacyAvoidance)) / sd(d$IntimacyAvoidance)
d$Irresponsibility <- (d$Irresponsibility - mean(d$Irresponsibility)) / sd(d$Irresponsibility)
d$Manipulativeness <- (d$Manipulativeness - mean(d$Manipulativeness)) / sd(d$Manipulativeness)
d$PerceptualDysregulation <- (d$PerceptualDysregulation - mean(d$PerceptualDysregulation))/sd(d$PerceptualDysregulation)
d$Perseveration <- (d$Perseveration - mean(d$Perseveration)) / sd(d$Perseveration)
d$RestrictedAffectivity <- (d$RestrictedAffectivity - mean(d$RestrictedAffectivity)) / sd(d$RestrictedAffectivity)
d$RigidPerfeccionism <- (d$RigidPerfeccionism - mean(d$RigidPerfeccionism)) / sd(d$RigidPerfeccionism)
d$RiskTaking <- (d$RiskTaking - mean(d$RiskTaking)) / sd(d$RiskTaking)
d$SeparationInsecurity <- (d$SeparationInsecurity - mean(d$SeparationInsecurity)) / sd(d$SeparationInsecurity)
d$Submissiveness <- (d$Submissiveness - mean(d$Submissiveness)) / sd(d$Submissiveness)
d$Suspiciousness <- (d$Suspiciousness - mean(d$Suspiciousness)) / sd(d$Suspiciousness)
d$UnusualBeliefsAndExperiences <- (d$UnusualBeliefsAndExperiences - mean(d$UnusualBeliefsAndExperiences)) / sd(d$UnusualBeliefsAndExperiences)
d$Withdrawal <- (d$Withdrawal - mean(d$Withdrawal)) / sd(d$Withdrawal)
d$DomainNegativeAffect <- (d$DomainNegativeAffect - mean(d$DomainNegativeAffect)) / sd(d$DomainNegativeAffect)
d$DomainDetachment <- (d$DomainDetachment - mean(d$DomainDetachment)) / sd(d$DomainDetachment)
d$DomainAntagonism <- (d$DomainAntagonism - mean(d$DomainAntagonism)) / sd(d$DomainAntagonism)
d$DomainDisinhibition <- (d$DomainDisinhibition - mean(d$DomainDisinhibition)) /  sd(d$DomainDisinhibition)
d$DomainPsychoticism <- (d$DomainPsychoticism - mean(d$DomainPsychoticism)) / sd(d$DomainPsychoticism)


a=lm(mc~ as.factor(d$Im) + d$DomainPsychoticism + Im:d$DomainPsychoticism, data = d)
summary(a)
display(a)

res <- resid(a)
plot(fitted(a), res)
abline(0,0)
hist(res)



##


