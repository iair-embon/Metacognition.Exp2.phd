DataFrame_Filtered <- function(experimento, 
                                  filtroRT_Disc_Sup,
                                  filtroRT_Disc_Inf,
                                  filtroRT_Conf_Sup,
                                  filtroRT_Conf_Inf,
                                  filtroTrial = 0){ 
  
  # experimento = completo,survey,sorteo, todos
  # Superior and inferior filtro Reaction Time Discrimination task 
  # Superior and inferior filtro Reaction Time Confidence task 
  

  # voy a la carpeta del proyecto
  root <- rprojroot::is_rstudio_project
  basename(getwd())
  
  # elijo que exp voy a utilizar, 1, 2 (replica), o ambos
  if (experimento == 'completo'){
    filepath <- (root$find_file("Data/Experiment_Complete/df_total.Rda"))
    df_total <- load(file= filepath)} 
  if (experimento == 'survey'){
    filepath <- (root$find_file("Data/Experiment_OnlySurvey/df_total.Rda"))
    df_total <- load(file= filepath)}
  if (experimento == 'sorteo'){
    filepath <- (root$find_file("Data/PID5_sorteo/df_total.Rda"))
    df_total <- load(file= filepath)}
  if (experimento == 'todos'){
    ## ambos df_total:
    filepath <- (root$find_file("Data/Experiment_Complete/df_total.Rda"))
    load(file= filepath)
    a <- df_total
    
    filepath <- (root$find_file("Data/Experiment_OnlySurvey/df_total.Rda"))
    load(file= filepath)
    b <- df_total
    # sumo 100 a la columna sujetos, para que no se pisen los nros y este nro sea unico
    b$sujetos <- b$sujetos + 1000 
    
    filepath <- (root$find_file("Data/PID5_sorteo/df_total.Rda"))
    load(file= filepath)
    c <- df_total
    # sumo 100 a la columna sujetos, para que no se pisen los nros y este nro sea unico
    c$sujetos <- c$sujetos + 2000 
    
    
    ### cambio la columna sujetos por una nueva, para que no se pisen los nros y este nro sea unico
    # uno los df
    df_total <- rbind(a,b,c)
  }
  
  ## Filter by reaction times 
  source(root$find_file("Analysis/AuxiliaryFunctions/auroc2_by_ReactionTimeFilter.R"))
  list_exp <- Auroc2_by_RT_filter(d = df_total,
                                filtroRT_Disc_Sup = filtroRT_Disc_Sup,
                                filtroRT_Disc_Inf = filtroRT_Disc_Inf,
                                filtroRT_Conf_Sup = filtroRT_Conf_Sup,
                                filtroRT_Conf_Inf = filtroRT_Conf_Inf,
                                filtroTrial = filtroTrial)
  auc2 <- list_exp$mc_Rt_Discarded
  df_total <- list_exp$df_total
  
  # tomo las variables de interes
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
  
  
  d.sin.normalizar = data.frame(sujetos = ExistingSubjects,
                                mc  = auc2,
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
  
  d.sin.normalizar.mc.filter <- d.sin.normalizar[d.sin.normalizar$mc >= 0.5,]
  
  d <- d.sin.normalizar.mc.filter
  
  d$pc <- (d$pc - mean(d$pc)) / sd(d$pc)
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
  
  
  d.mc.filter <- d
  
  DF_list <- list(a = df_total, 
                  b = d.sin.normalizar, 
                  c= d.sin.normalizar.mc.filter,
                  d = d.mc.filter)
  return(DF_list)
  }