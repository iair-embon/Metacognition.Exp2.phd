df_string_y_stringMod$id.value <- 1:nrow(df_string_y_stringMod)


filas_descartar <- c(35,36,41,42,49,51,57,61)


df_string_y_stringMod <- df_string_y_stringMod[! df_string_y_stringMod$id.value %in% filas_descartar,]
df_string_y_stringMod <- df_string_y_stringMod[,-3]



# where you will save it
root <- rprojroot::is_rstudio_project
basename(getwd())
otro_filepath <- (root$find_file("Analysis/AuxiliaryFunctions/df_string_y_stringMod_medicacion.Rda"))

# save it
save(df_string_y_stringMod,file = otro_filepath)
