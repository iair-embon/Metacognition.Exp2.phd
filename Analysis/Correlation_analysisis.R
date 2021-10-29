# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered.R"))

# get the df list
# experimento = completo,survey,sorteo, todos
DF_list <- DataFrame_Filtered(experimento = "todos", 
                              filtroRT_Disc_Sup = 5000,
                              filtroRT_Disc_Inf = 200,
                              filtroRT_Conf_Sup = 5000,
                              filtroRT_Conf_Inf = 0,
                              filtroTrial = 20,
                              cant_trial_filter = 70)

# DF_list:
# a = df_total
# b = d.sin.normalizar
# c = d.sin.normalizar.mc.filter
# d = d.mc.filter

df_total <- DF_list$a
d.sin.normalizar <- DF_list$b
d.sin.normalizar.mc.filter <- DF_list$c
d.mc.filter <- DF_list$d 
d <- d.mc.filter


d1 = d.sin.normalizar.mc.filter

# sujetos que tienen un 85 % de trials en una misma respuesta de confianza

source(root$find_file("Analysis/AuxiliaryFunctions/discard_by_x_same_confidence.R"))
sujetos_a_descartar <- discard_by_x_same_confidence(85)
d2 <- d1[! d1$sujetos %in% sujetos_a_descartar,]


d <- subset(d, d$sujetos %in% d2$sujetos)

###############
### library ###
###############
library(stats)
library(corrplot)

############################
### correlation Analysis ###
############################

# me quedo solo con variables de interes
dnew <- d[,c(2,6,10,12:41)]
dnew.only.facets <-dnew[,c(4:28)]
dnew.only.facets.mc <- dnew[,c(1,4:28)]
# ploteo una correlacion 
corrplot(cor(dnew), method = "ellipse")

# seleccionamos la cantidad de factores usando el criterio Cattell-Nelson-Gorsush
library(nFactors)
fac.criterio.1 <- nCng(dnew.only.facets)
fac.criterio.2 <- nCng(dnew.only.facets.mc)

# corremos el analisis factorial
fa.1 <- factanal(dnew.only.facets, factors = 3)
fa.2 <- factanal(dnew.only.facets.mc, factors = 3)
