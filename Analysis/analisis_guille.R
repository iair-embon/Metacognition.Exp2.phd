#### Datos Exp 1

## variables:
# - id: numero que identifica sujeto
# - trial: numero de trial del sujeto
# - stim: abs(puntos.izquierda - puntos.derecha)
# - correct: respuesta correcta (1/0)
# - confianza: confianza (0, 1/3, 2/3, 3/3)
# - RT1: tiempo de respuesta para respuesta tipo 1
# - RT2: tiempo de respuesta para respuesta tipo 2
# - sexo: F/M
# - edad: en años
# - AQ: score rasgo-autismo

library(arm)

## Cargo los datos
load("~/Dropbox/work/2020/2020 - proyectos/Metacog_TEA_exp_online/Resultados/ResultadosUltimos/df_exp_mod2.Rda")
load("~/Dropbox/work/2020/2020 - proyectos/Metacog_TEA_exp_online/Resultados/ResultadosUltimos/df_DatosUnicos_mod2.Rda")

## Construyo la tabla con las variables que me interesan
d <- data.frame(id = rep(NA,nrow(df_exp_mod2)), trial = NA, stim = NA, correct = NA, confianza = NA, 
                RT1 = NA, RT2 = NA, sexo = NA, edad = NA, edad.c = NA, edad.s = NA, AQ = NA, AQ.s = NA, sd.stim = NA)

d$id        <- factor( df_exp_mod2$sujetos )
d$trial     <- df_exp_mod2$trials
d$stim      <- df_exp_mod2$diferencia_puntitos
d$correct   <- ifelse( df_exp_mod2$discrimination_is_correct == TRUE, 1, 0 )
d$confianza <- (df_exp_mod2$confidence_key - 1) / 3
d$RT1       <- df_exp_mod2$t_ensayo_discriminacion
d$RT2       <- df_exp_mod2$t_ensayo_confianza

u.id <- unique(d$id)
n.id <- length(u.id)
for (i in 1:n.id)
  df_DatosUnicos_mod2$sd.stim[i] <- sd(d$stim[d$id==u.id[i]])


edad.median <- median(df_DatosUnicos_mod2$edad)
edad.mean   <- mean(df_DatosUnicos_mod2$edad)
edad.sd     <- sd(df_DatosUnicos_mod2$edad)
AQ.mean     <- mean(df_DatosUnicos_mod2$AQ)
AQ.sd       <- sd(df_DatosUnicos_mod2$AQ)
for (i in 1:nrow(d)){
  
  suj          <- d$id[i]
  sexo         <- df_DatosUnicos_mod2$genero[df_DatosUnicos_mod2$sujetos==suj]
  d$sexo[i]    <- ifelse(sexo=="Femenino", 1, ifelse(sexo=="Masculino", 0, -1) )
  d$edad[i]    <- df_DatosUnicos_mod2$edad[df_DatosUnicos_mod2$sujetos==suj]
  d$AQ[i]      <- df_DatosUnicos_mod2$AQ[df_DatosUnicos_mod2$sujetos==suj]   
  d$sd.stim[i] <- df_DatosUnicos_mod2$sd.stim[df_DatosUnicos_mod2$sujetos==suj]   
  
  d$edad.c[i] <-  d$edad[i] - edad.median
  d$edad.s[i] <- (d$edad[i] - edad.mean) / (2*edad.sd)
  d$AQ.s[i]   <- (d$AQ[i] - AQ.mean) / (2*AQ.sd)
  
}



## Modelos NO jerarquicos. en todos los casos, p = p(correct)

summary( glm(correct ~ confianza + sexo + edad + AQ, data = d) )
summary( glm(correct ~ confianza + sexo + edad + AQ + trial, data = d) )

summary( glm(correct ~ confianza + sexo + edad + AQ + trial + sd.stim, data = d) )
summary( glm(correct ~ confianza + sexo + edad + AQ + trial + sd.stim + confianza:sd.stim, data = d) )

modelo <- correct ~ sexo + edad.c + AQ.s + trial + sd.stim + confianza + confianza:edad.c + confianza:edad.c:sexo
summary( glm(modelo, data = d, family = binomial(link = "logit")) )

modelo <- correct ~ sexo + edad.c + AQ.s + trial + sd.stim + confianza + confianza:edad.c + confianza:edad.c:sexo + confianza:AQ.s + confianza:AQ.s:sexo
summary( glm(modelo, data = d, family = binomial(link = "logit")) )

d$confianza2lev <- ifelse(d$confianza>0.5, 1, 0)
modelo <- confianza2lev ~ sexo + edad.c + AQ.s + trial + sd.stim + AQ.s:sexo
summary( glm(modelo, data = d, family = binomial(link = "logit")) )

modelo <- correct ~ sexo + edad.c + AQ.s + trial + sd.stim + AQ.s:sexo
summary( glm(modelo, data = d, family = binomial(link = "logit")) )


summary( glm(correct ~ confianza + sexo + edad + AQ.s + trial + sd.stim + confianza:sd.stim, data = d, family = binomial(link = "logit")) )
summary( glm(correct ~ confianza + trial + sd.stim + confianza:sd.stim, data = d, family = binomial(link = "logit")) )
summary( glm(correct ~ confianza + trial + sd.stim + confianza:AQ + confianza:AQ:sexo, data = d, family = binomial(link = "logit")) )

summary( glm(correct ~ confianza + trial + sd.stim + confianza:AQ.s + confianza:AQ.s:sexo, data = d, family = binomial(link = "logit")) )

# m1: logit(p) = a + b * confianza + error 
#display( glm(correct ~ confianza, data = d) )
#mean(d$correct[d$confianza==0])
# b = 0.08

# m2: logit(p) = a + (b + b1*AQ) * confianza + c * AQ + error 
# summary( glm(correct ~ confianza*AQ, data = d), digits = 3 )

# m3: logit(p) = a + (b + bs*sexo + (baq + baqs*sexo) *AQ) * confianza
#              = 1 + confianza + sexo:confianza + AQ:confianza + sexo:AQ:confianza
#m3 <- glm(correct ~  1 + confianza + sexo:confianza + AQ.s:confianza + sexo:AQ.s:confianza, data = d)
m4 <- glm(correct ~  sexo*AQ.s*confianza, data = d)
summary(m4)

# m3: logit(p) = a + (b + b1*AQ + b2*sexo + b3*AQ*sexo) * confianza + c * AQ + error 
# summary( glm(correct ~ confianza*AQ.s*sexo, data = d), digits = 3 )


# glmer(correct ~ confianza*AQ.s*sexo + (confianza|trial) + (AQ.s|id) + (sexo|id), data = d)
m5 <- glmer(correct ~ confianza*AQ.s*sexo + (1 + confianza + confianza:sexo|id), 
      data = d,
      family = binomial,
      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m5)

# control = lmerControl( optimizer ='optimx', optCtrl=list(method='L-BFGS-B'))
# library(optimx)
  
