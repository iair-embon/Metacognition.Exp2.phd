# having pressed the same confidence key more than X% of trials
discard_by_x_same_confidence <- function(percent,df_total){
  require(dplyr)
  d <- df_total %>%
    select(Participant, ConfKey1, ConfKey2, ConfKey3, ConfKey4) %>%
    group_by_all%>%
    count
  filter <- round((percent*130)/100)
  discard_participant <- c()

  for (i in 1:nrow(d)) {
    state <- F
    if(d$ConfKey1[i] > filter){state <- T}
    if(d$ConfKey2[i] > filter){state <- T}
    if(d$ConfKey3[i] > filter){state <- T}
    if(d$ConfKey4[i] > filter){state <- T}
    if(state == T){discard_participant <- c(discard_participant, d$Participant[i])}
  }
  return(discard_participant)
}

