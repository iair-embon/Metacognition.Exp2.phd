library(tidyverse)
#library(knitr)
#library(kableExtra)
library(htmlTable)
#library(magick)
#library(webshot)
#library(htmlwidgets)

# calculate mean and sd from each facet
means <- df_total %>%
  select(5:29) %>%
  sapply(mean) %>%
  round(digits = 3)

sds <- df_total %>%
  select(5:29) %>%
  sapply(sd) %>%
  round(digits = 3)

table_2.1 <- data.frame(facet = names(df_total)[5:29], mean = means, sd = sds)


rownames(table_2.1) <- NULL

tabla <- htmlTable(table_2.1, align = "c", rnames = FALSE, 
                   css.cell = "padding: 10px; font-size: 14px; border: 1px solid black;") 

tabla
