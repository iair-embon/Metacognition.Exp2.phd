DataFrame_subset <- function(df_total){ 
  
  library(dplyr)
  
  d <- df_total %>%
    select(!c(RelyOn,
              Problems,
              ConfKey1,
              ConfKey2,
              ConfKey3,
              ConfKey4)) %>%
    distinct(Participant,.keep_all = TRUE)
  
  return(d)
}