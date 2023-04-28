#########################################
### Descriptive statistics  domains ##### TAB 2.2
#########################################

library(dplyr)
library(ggplot2)
library(gridExtra)
library(data.table)
library(grid)

# data
### load mixed logistic regression model 
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("git/Data/df_total_filtered.Rda")
load(file= filepath)

# calculate mean and sd from each facet
means <- df_total %>%
  select(30:34) %>%
  sapply(mean) %>%
  round(digits = 3)

sds <- df_total %>%
  select(30:34) %>%
  sapply(sd) %>%
  round(digits = 3)

table_2.2 <- data.frame(mean = means, sd = sds)

### save the table as png

# Set the file path and name
file_path <- "~/Documents/Investigación/Metacog.Personality/Metacognition.PersonalityTraits/git/Tables/Tables/table_2.2.png"
# Create the directory if it doesn't exist
dir.create(dirname(file_path), showWarnings = FALSE)

# Open the PNG device and specify the file path
png(file_path, width = 6, height = 5, units = "in", res = 72)

# Create the table grob
p <- tableGrob(table_2.2)

# Draw the table grob
grid.draw(p)

# Close the device
dev.off()
