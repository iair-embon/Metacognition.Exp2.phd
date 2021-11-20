## GRÁFICOS PARA EL CONGRESO DE PSICOLOGÍA, UNIVERSIDAD DE BUENOS AIRES, 2021 ##

## PROYECTO: METACOGNICIÓN Y RASGOS DE PERSONALIDAD. LAB. DE CIENCIAS COGNITIVAS, IPSI ##

library(ggplot2)

# GRAFICO DE BARRAS PARA GENERO #
ggplot(data= df_total, mapping = aes(x=genero))+
  geom_bar(color="darkred", fill="pink", bins = 100)+
  ylab("Sujetos")+
  xlab("Género")+
  scale_x_discrete() + 
  scale_y_discrete() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25))
  

# HISTOGRAMAS #

# HISTOGRAMA PARA LA EDAD #

ggplot(data= df_total, aes(x=edad))+
    geom_histogram(color="darkred", fill="orange", bins = 100)+
  ylab("Sujetos")+
  xlab("Edad")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  xlim(15,70)+
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(1, 1,1, 1, "cm"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          axis.title.x = element_text(size = 25)) 

# HISTOGRAMA PARA TIEMPOS DE REACCION #

ggplot(data= df_total, aes(x=media_tr_discri))+
  geom_histogram(color="darkred", fill="red", bins = 100)+
  ylab("Sujetos")+
  xlab("TR de discriminación")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25)) 

# HISTOGRAMA PARA LA MEDIA DE CONFIANZA #

ggplot(data= df_total, aes(x= media_confidence))+
  geom_histogram(color="darkred", fill="pink", bins = 30)+
  ylab("Sujetos")+
  xlab("Confianza")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  xlim(1,4)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25)) 

# GRAFICO DE TORTAS #
grafico_tortas <- ggplot(df_total, aes(x="", y=value, fill=affeccionPsico))+
pie <- grafico_tortas + coord_polar("y", start=0)
pie

# DENSITY PLOTS #

# DENSITY DEL BAREMOS # ESTE AUN NO SALIO ### 

Dominio_Psicoticismo <- unique(percentiles$Dominio_psicoticismo)
Dominio_Desinhibicion  <- unique(percentiles$Dominio_Desinhibicion)
Dominio_Antagonismo <- unique(percentiles$Dominio_Antagonismo)
Dominio_Desapego <- unique(percentiles$Dominio_Desapego)
Dominio_AfectividadNegativa <- unique(percentiles$Dominio_AfectividadNegativa)

Valor_Dominios<- c(Dominio_AfectividadNegativa, Dominio_Antagonismo, Dominio_Desapego,
                   Dominio_Desinhibicion, Dominio_Psicoticismo)

Dominio_Psicoticismo_label <- rep("DominioPsicoticismo",length(Dominio_Psicoticismo))
Dominio_Desinhibicion_label <- rep("DominioDessinhibicion", length(Dominio_Desinhibicion)) 
Dominio_Antagonismo_label <- rep("DominioAntagonismo", length(Dominio_Antagonismo))
Dominio_Desapego_label <- rep("DeminioDesapego", length(Dominio_Desapego))
Dominio_AfectividadNegativa_label <- rep("DominioAfectividadNegativa", length(Dominio_AfectividadNegativa))

Dominios_labels <- c(Dominio_AfectividadNegativa_label,Dominio_Antagonismo_label,
                  Dominio_Psicoticismo_label, Dominio_Desapego_label,
                  Dominio_Desinhibicion_label)

d1 <- data.frame(Valor_Dominios = Valor_Dominios,
                 Dominios_labels = Dominios_labels)

ggplot(percentiles, aes(x=Valor_Dominios, Color= Dominios_labels)) + 
  geom_density(alpha=0.3,size=1.5)+
  scale_x_continuous(expand = c(.0, 0),limits = c(0.1, 2)) +
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