########################################
### Descriptive statistics  facets ##### TAB 2.1
########################################

library(dplyr)
library(kableExtra)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

# calculate mean and sd from each facet
means <- df_total %>%
  select(5:29) %>%
  sapply(mean) %>%
  round(digits = 3)

sds <- df_total %>%
  select(5:29) %>%
  sapply(sd) %>%
  round(digits = 3)

table_2.1 <- data.frame(mean = means, sd = sds)

# Define the domain names
domains <- c("Anhedonia", 
             "Anxiousness", 
             "Attention Seeking", 
             "Callousness",
             "Deceitfulness",
             "Depressivity",
             "Distractibility",
             "Eccentricity",
             "Emotional Lability",
             "Grandiosity",
             "Hostility",
             "Impulsivity",
             "Intimacy Avoidance",
             "Irresponsibility",
             "Manipulativeness",
             "Perceptual Dysregulation",
             "Perseveration",
             "Restricted Affectivity",
             "Rigid Perfectionism",
             "Risk Taking",
             "Separation Insecurity",
             "Submissiveness",
             "Suspiciousness",
             "Unusual Beliefs And Experiences",
             "Withdrawal")

rownames(table_2.1) <- domains

# Create the table using kableExtra 
table_2.1 <- table_2.1 %>%
  kable(format = "html") %>%
  kable_styling(full_width = FALSE, font_size = 15, position = "center") %>%
  column_spec(1, bold = TRUE)  # Set the first column to bold


# Set the file path and name
file_path <- "~/Documents/Investigación/Metacog.Personality/Metacognition.PersonalityTraits/git/Tables/Tables/table_2.1.png"

