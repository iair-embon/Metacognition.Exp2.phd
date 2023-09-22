#########################################
### Descriptive statistics  domains ##### TAB 2.2
#########################################

library(dplyr)
library(kableExtra)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

# Calculate the mean and standard deviation for each domain
means <- df_total %>%
  select(30:34) %>%
  sapply(mean) %>%
  round(digits = 3)

sds <- df_total %>%
  select(30:34) %>%
  sapply(sd) %>%
  round(digits = 3)

table_2.2 <- data.frame(mean = means, sd = sds)

# Define the domain names
domains <- c("Negative Affect", "Detachment", "Antagonism", "Disinhibition", "Psychoticism")  
rownames(table_2.2) <- domains

# Create the table using kableExtra 
table_2.2 <- table_2.2 %>%
  kable(format = "html") %>%
  kable_styling(full_width = FALSE, font_size = 15, position = "center") %>%
  column_spec(1, bold = TRUE)  # Set the first column to bold

# Specify the file path and name
file_path <- "~/Documents/Investigación/Metacog.Personality/Metacognition.PersonalityTraits/git/Tables/Tables/table_2.2.png"

