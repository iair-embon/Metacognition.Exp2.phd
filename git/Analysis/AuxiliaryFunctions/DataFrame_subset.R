DataFrame_subset <- function(df_total){ 
  
  library(dplyr)
  
  d <- df_total %>%
    select(!c(RelyOn,
              Problems,
              ConfKey1,
              ConfKey2,
              ConfKey3,
              ConfKey4,
              discrimination_is_correct,
              confidence_key,
              trials,
              PointDifference,
              ReacTime_DiscTask,
              ReacTime_ConfTask)) %>%
    distinct(Participant,.keep_all = TRUE)
  
  return(d)
}