# despues de correr hasta antes de sacar el puntaje pid en el script principal
pid2.test <- pid2[1:4] 
pid1.test <- pid1[1:4]

# where are the responses values of a the first subject
ubicacion_respuestas_pid1 <- 2
ubicacion_respuestas_pid2 <- 2

# number of subject 
cant_sujetos <- 2

# location of the sublist where are the first part of pid-5 of the first subject
ubicacion_comp1_pid <- 4

# load the function to get the AQ quotient  
#source(root$find_file("Analysis/AuxiliaryFunctions/pid-5.R"))

# get the pid score
#puntaje_pid.5 <- puntaje_pid(cant_sujetos,ubicacion_respuestas_pid1,ubicacion_respuestas_pid2
#                             ,ubicacion_comp1_pid)


# cant_sujetos = cantidad de sujetos; 
# cant_componentes_por_sujetos = cantidad de componentes en un sujeto;
# ubicacion_comp_pid = la ubicacion del primer componente pid en el primer sujeto

# res[[4]] primera parte
# res[[5]] segunda parte
# 1 = Muy falso o a menudo falso
# 2 = A veces o algo falso
# 3 = A veces o algo verdadero
# 4 = Muy cierto o a menudo verdadero

# creamos variables que van a guardar puntaje de todos los sujetos
scoreAnhedonia.Completo                       <- rep(NaN,cant_sujetos)
scoreAnxiousness.Completo                     <- rep(NaN,cant_sujetos)
scoreAttentionSeeking.Completo                <- rep(NaN,cant_sujetos)
scoreCallousness.Completo                     <- rep(NaN,cant_sujetos)
scoreDeceitfulness.Completo                   <- rep(NaN,cant_sujetos)
scoreDepressivity.Completo                    <- rep(NaN,cant_sujetos)
scoreDistractivility.Completo                 <- rep(NaN,cant_sujetos)
scoreExcentricity.Completo                    <- rep(NaN,cant_sujetos)
scoreEmotionalLability.Completo               <- rep(NaN,cant_sujetos)
scoreGrandiosity.Completo                     <- rep(NaN,cant_sujetos)
scoreHostility.Completo                       <- rep(NaN,cant_sujetos)
scoreImpulsivity.Completo                     <- rep(NaN,cant_sujetos)
scoreIntimacyAvoidance.Completo               <- rep(NaN,cant_sujetos)
scoreIrresponsibility.Completo                <- rep(NaN,cant_sujetos)
scoreManipulativeness.Completo                <- rep(NaN,cant_sujetos)
scorePerceptualDysregulation.Completo         <- rep(NaN,cant_sujetos)
scorePerseveration.Completo                   <- rep(NaN,cant_sujetos)
scoreRestrictedAffectivity.Completo           <- rep(NaN,cant_sujetos)
scoreRigidPerfeccionism.Completo              <- rep(NaN,cant_sujetos)
scoreRiskTaking.Completo                      <- rep(NaN,cant_sujetos)
scoreSeparationInsecurity.Completo            <- rep(NaN,cant_sujetos)
scoreSubmissiveness.Completo                  <- rep(NaN,cant_sujetos)
scoreSuspiciousness.Completo                  <- rep(NaN,cant_sujetos)
scoreUnusualBeliefsAndExperiences.Completo    <- rep(NaN,cant_sujetos)
scoreWithdrawal.Completo                      <- rep(NaN,cant_sujetos)


ScoreDomainNegativeAffect.Completo <- rep(NaN,cant_sujetos)
ScoreDomainDetachment.Completo     <- rep(NaN,cant_sujetos)
ScoreDomainAntagonism.Completo     <- rep(NaN,cant_sujetos)
ScoreDomainDisinhibition.Completo  <- rep(NaN,cant_sujetos)
ScoreDomainPsychoticism.Completo   <- rep(NaN,cant_sujetos)


# valores que puntuan inversos
inversos <- c(7,30,35,58,87,90,96,97,98,131,142,155,164,177,210,215)

# get the indices
indsAnhedonia                       <- c(1, 23, 26, 30, 124, 155, 157, 189)
indsAnxiousness                     <- c(79, 93, 95, 96, 109, 110, 130, 141, 174)
indsAttentionSeeking                <- c(14, 43, 74, 111, 113, 173, 191,211)
indsCallousness                     <- c(11, 13, 19, 54, 72, 73, 90, 153, 166, 183, 198, 200, 207, 208)
indsDeceitfulness                   <- c(41, 53, 56, 76, 126, 134, 142, 206, 214, 218)
indsDepressivity                    <- c(27, 61, 66, 81, 86, 104, 119, 148, 151, 163, 168, 169, 178, 212)
indsDistractivility                 <- c(6, 29, 47, 68, 88, 118, 132, 144, 199)
indsExcentricity                    <- c(5, 21, 24, 25, 33, 52, 55, 70, 71, 152, 172,185, 205)
indsEmotionalLability               <- c(18, 62, 102, 122, 138, 165, 181)
indsGrandiosity                     <- c(40, 65, 114, 179, 187, 197)
indsHostility                       <- c(28, 32, 38, 85, 92, 116, 158, 170, 188, 216)
indsImpulsivity                     <- c(4, 16, 17, 22, 58, 204)
indsIntimacyAvoidance               <- c(89, 97, 108, 120, 145, 203)
indsIrresponsibility                <- c(31, 129, 156, 160, 171, 201, 210)
indsManipulativeness                <- c(107, 125, 162, 180, 219)
indsPerceptualDysregulation         <- c(36, 37, 42, 44, 59, 77, 83, 154, 192, 193, 213, 217)
indsPerseveration                   <- c(46, 51, 60, 78, 80, 100, 121, 128, 137)
indsRestrictedAffectivity           <- c(8, 45, 84, 91, 101, 167, 184)
indsRigidPerfeccionism              <- c(34, 49, 105, 115, 123, 135, 140, 176, 196, 220)
indsRiskTaking                      <- c(3, 7, 35, 39, 48, 67, 69, 87, 98, 112, 159, 164, 195, 215)
indsSeparationInsecurity            <- c(12, 50, 57,64, 127, 149, 175)
indsSubmissiveness                  <- c(9, 15, 63, 202)
indsSuspiciousness                  <- c(2, 103, 117, 131, 133, 177, 190)
indsUnusualBeliefsAndExperiences    <- c(94, 99, 106, 139, 143, 150, 194, 209)
indsWithdrawal                      <- c(10, 20, 75, 82, 136, 146, 147, 161, 182, 186)



# PRUEBA 1 puedo poner hacer que un indice tenga todo 1 en este caso anhedonia
for (i in 1:length(pid1.test[4]$value)) {
  if (i %in% indsEmotionalLability){
    pid1.test[4]$value[i] <- 1
  }
}

for (i in 1:length(pid1.test[4]$value)) {
  if ((i+110) %in% indsEmotionalLability){
    pid2.test[4]$value[i] <- 1
  }
}

lala <- c(pid1.test[4]$value,pid2.test[4]$value)
lala[indsEmotionalLability]

# hago que las variables que son parte de un dominio sean punteadas todas por 1
for (i in 1:length(pid1.test[4]$value)) {
  if (i %in% indsAnxiousness){
    pid1.test[4]$value[i] <- 1
  }
}

for (i in 1:length(pid1.test[4]$value)) {
  if ((i+110) %in% indsAnxiousness){
    pid2.test[4]$value[i] <- 1
  }
}

lala <- c(pid1.test[4]$value,pid2.test[4]$value)
lala[indsAnxiousness]

for (i in 1:length(pid1.test[4]$value)) {
  if (i %in% indsSeparationInsecurity){
    pid1.test[4]$value[i] <- 1
  }
}

for (i in 1:length(pid1.test[4]$value)) {
  if ((i+110) %in% indsSeparationInsecurity){
    pid2.test[4]$value[i] <- 1
  }
}

lala <- c(pid1.test[4]$value,pid2.test[4]$value)
lala[indsSeparationInsecurity]



# puntar por sujeto
for (i in 1:cant_sujetos) {
  
  # guarde los puntajes del sujeto en cuestion
  puntaje.sujeto.cuestion <- rep(NaN, (length(pid1.test[ubicacion_respuestas_pid1]$value)
                                       + length(pid2.test[ubicacion_respuestas_pid2]$value)))
  
  # unimos ambas partes en una sola
  parte1 <- pid1.test[ubicacion_respuestas_pid1]$value
  parte2 <- pid2.test[ubicacion_respuestas_pid2]$value
  test.completo <- c(parte1, parte2)
  # convertimos sus valores en numericos
  test.completo <- as.numeric(test.completo)
  
  
  # puntuar cada valor de cada pregunta
  for (j in 1:length(test.completo)) {
    if(j %in% inversos){
      puntaje.sujeto.cuestion[j] <- abs(test.completo[j]-3)
    } else {
      puntaje.sujeto.cuestion[j] <- test.completo[j]-1
    }
    
  }
  
  # sacar puntuacion para cada indice
  scoreAnhedonia                       <- mean(puntaje.sujeto.cuestion[indsAnhedonia])
  scoreAnxiousness                     <- mean(puntaje.sujeto.cuestion[indsAnxiousness])
  scoreAttentionSeeking                <- mean(puntaje.sujeto.cuestion[indsAttentionSeeking])
  scoreCallousness                     <- mean(puntaje.sujeto.cuestion[indsCallousness])
  scoreDeceitfulness                   <- mean(puntaje.sujeto.cuestion[indsDeceitfulness])
  scoreDepressivity                    <- mean(puntaje.sujeto.cuestion[indsDepressivity])
  scoreDistractivility                 <- mean(puntaje.sujeto.cuestion[indsDistractivility])
  scoreExcentricity                    <- mean(puntaje.sujeto.cuestion[indsExcentricity])
  scoreEmotionalLability               <- mean(puntaje.sujeto.cuestion[indsEmotionalLability])
  scoreGrandiosity                     <- mean(puntaje.sujeto.cuestion[indsGrandiosity])
  scoreHostility                       <- mean(puntaje.sujeto.cuestion[indsHostility])
  scoreImpulsivity                     <- mean(puntaje.sujeto.cuestion[indsImpulsivity])
  scoreIntimacyAvoidance               <- mean(puntaje.sujeto.cuestion[indsIntimacyAvoidance])
  scoreIrresponsibility                <- mean(puntaje.sujeto.cuestion[indsIrresponsibility])
  scoreManipulativeness                <- mean(puntaje.sujeto.cuestion[indsManipulativeness])
  scorePerceptualDysregulation         <- mean(puntaje.sujeto.cuestion[indsPerceptualDysregulation])
  scorePerseveration                   <- mean(puntaje.sujeto.cuestion[indsPerseveration])
  scoreRestrictedAffectivity           <- mean(puntaje.sujeto.cuestion[indsRestrictedAffectivity])
  scoreRigidPerfeccionism              <- mean(puntaje.sujeto.cuestion[indsRigidPerfeccionism])
  scoreRiskTaking                      <- mean(puntaje.sujeto.cuestion[indsRiskTaking])
  scoreSeparationInsecurity            <- mean(puntaje.sujeto.cuestion[indsSeparationInsecurity])
  scoreSubmissiveness                  <- mean(puntaje.sujeto.cuestion[indsSubmissiveness])
  scoreSuspiciousness                  <- mean(puntaje.sujeto.cuestion[indsSuspiciousness])
  scoreUnusualBeliefsAndExperiences    <- mean(puntaje.sujeto.cuestion[indsUnusualBeliefsAndExperiences])
  scoreWithdrawal                      <- mean(puntaje.sujeto.cuestion[indsWithdrawal])
  
  
  ScoreDomainNegativeAffect <- mean(c(scoreEmotionalLability,scoreAnxiousness, scoreSeparationInsecurity))
  ScoreDomainDetachment     <- mean(c(scoreWithdrawal, scoreAnhedonia, scoreIntimacyAvoidance))
  ScoreDomainAntagonism     <- mean(c(scoreManipulativeness, scoreDeceitfulness, scoreGrandiosity))
  ScoreDomainDisinhibition  <- mean(c(scoreIrresponsibility, scoreImpulsivity, scoreDistractivility))
  ScoreDomainPsychoticism   <- mean(c(scoreUnusualBeliefsAndExperiences, scoreExcentricity, scorePerceptualDysregulation))
  
  # guarda puntaje de todos los sujetos
  scoreAnhedonia.Completo[i]                       <- scoreAnhedonia
  scoreAnxiousness.Completo[i]                     <- scoreAnxiousness
  scoreAttentionSeeking.Completo[i]                <- scoreAttentionSeeking
  scoreCallousness.Completo[i]                     <- scoreCallousness
  scoreDeceitfulness.Completo[i]                   <- scoreDeceitfulness
  scoreDepressivity.Completo[i]                    <- scoreDepressivity
  scoreDistractivility.Completo[i]                 <- scoreDistractivility
  scoreExcentricity.Completo[i]                    <- scoreExcentricity
  scoreEmotionalLability.Completo[i]               <- scoreEmotionalLability
  scoreGrandiosity.Completo[i]                     <- scoreGrandiosity
  scoreHostility.Completo[i]                       <- scoreHostility
  scoreImpulsivity.Completo[i]                     <- scoreImpulsivity
  scoreIntimacyAvoidance.Completo[i]               <- scoreIntimacyAvoidance
  scoreIrresponsibility.Completo[i]                <- scoreIrresponsibility
  scoreManipulativeness.Completo[i]                <- scoreManipulativeness
  scorePerceptualDysregulation.Completo[i]         <- scorePerceptualDysregulation
  scorePerseveration.Completo[i]                   <- scorePerseveration
  scoreRestrictedAffectivity.Completo[i]           <- scoreRestrictedAffectivity
  scoreRigidPerfeccionism.Completo[i]              <- scoreRigidPerfeccionism
  scoreRiskTaking.Completo[i]                      <- scoreRiskTaking
  scoreSeparationInsecurity.Completo[i]            <- scoreSeparationInsecurity
  scoreSubmissiveness.Completo[i]                  <- scoreSubmissiveness
  scoreSuspiciousness.Completo[i]                  <- scoreSuspiciousness
  scoreUnusualBeliefsAndExperiences.Completo[i]    <- scoreUnusualBeliefsAndExperiences
  scoreWithdrawal.Completo[i]                      <- scoreWithdrawal
  
  
  ScoreDomainNegativeAffect.Completo[i] <- ScoreDomainNegativeAffect
  ScoreDomainDetachment.Completo[i]     <- ScoreDomainDetachment
  ScoreDomainAntagonism.Completo[i]     <- ScoreDomainAntagonism
  ScoreDomainDisinhibition.Completo[i]  <- ScoreDomainDisinhibition
  ScoreDomainPsychoticism.Completo[i]   <- ScoreDomainPsychoticism
  
  
  # incremento ubicacion_respuestas_pid para pasar al siguiente sujeto
  ubicacion_respuestas_pid1 <- ubicacion_respuestas_pid1 + 2
  ubicacion_respuestas_pid2 <- ubicacion_respuestas_pid2 + 2
  
}

# creo un df donde meta todas las variables de personalidad
df_pib5 <- data.frame(
  Anhedonia                       = scoreAnhedonia.Completo,
  Anxiousness                     = scoreAnxiousness.Completo,
  AttentionSeeking                = scoreAttentionSeeking.Completo,
  Callousness                     = scoreCallousness.Completo,
  Deceitfulness                   = scoreDeceitfulness.Completo,
  Depressivity                    = scoreDepressivity.Completo,
  Distractivility                 = scoreDistractivility.Completo,
  Excentricity                    = scoreExcentricity.Completo,
  EmotionalLability               = scoreEmotionalLability.Completo,
  Grandiosity                     = scoreGrandiosity.Completo,
  Hostility                       = scoreHostility.Completo,
  Impulsivity                     = scoreImpulsivity.Completo,
  IntimacyAvoidance               = scoreIntimacyAvoidance.Completo,
  Irresponsibility                = scoreIrresponsibility.Completo,
  Manipulativeness                = scoreManipulativeness.Completo,
  PerceptualDysregulation         = scorePerceptualDysregulation.Completo,
  Perseveration                   = scorePerseveration.Completo,
  RestrictedAffectivity           = scoreRestrictedAffectivity.Completo,
  RigidPerfeccionism              = scoreRigidPerfeccionism.Completo,
  RiskTaking                      = scoreRiskTaking.Completo,
  SeparationInsecurity            = scoreSeparationInsecurity.Completo,
  Submissiveness                  = scoreSubmissiveness.Completo,
  Suspiciousness                  = scoreSuspiciousness.Completo,
  UnusualBeliefsAndExperiences    = scoreUnusualBeliefsAndExperiences.Completo,
  Withdrawal                      = scoreWithdrawal.Completo,
  
  DomainNegativeAffect = ScoreDomainNegativeAffect.Completo,
  DomainDetachment     = ScoreDomainDetachment.Completo,
  DomainAntagonism     = ScoreDomainAntagonism.Completo,
  DomainDisinhibition  = ScoreDomainDisinhibition.Completo,
  DomainPsychoticism   = ScoreDomainPsychoticism.Completo,
  
  stringsAsFactors = FALSE
)

