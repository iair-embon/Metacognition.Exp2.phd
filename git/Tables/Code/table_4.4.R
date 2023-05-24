################################################################
### univariate beta Regression Analysis confidence domains ##### TAB 4.4
################################################################

library(ggplot2)
library(gridExtra)
library(data.table)
library(grid)

# data
# set the directory where your RData files are located
setwd("D:/Windows/Descargas/Git_Metacog_Personalidad/Pubilico/Metacognition.PersonalityTraits/git/Data/Regression_Results/individual_Conf_PID_domain_beta_model")

# list all files in the directory with .RData extension
files <- list.files(pattern = "\\.RData$")

# create an empty data frame to store the summary statistics
summary_df <- data.frame()

# loop through each file and load the RData object
for (file in files) {
  
  # load the RData object into memory
  load(file)
  
  # run the summary() function and extract the relevant statistics
  model_summary <- summary(a)
  beta_coef <- model_summary$coefficients$mean[2, 1]
  std_error <- model_summary$coefficients$mean[2, 2]
  p_value <- model_summary$coefficients$mean[2, 4]
  conf_int_lower <- model_summary$coefficients$mean[2, 1] - (1.96 * model_summary$coefficients$mean[2, 2])
  conf_int_upper <- model_summary$coefficients$mean[2, 1] + (1.96 * model_summary$coefficients$mean[2, 2])
  pseudo.r.squared <- model_summary$pseudo.r.squared
  
  # create a data frame with the summary statistics
  model_summary_df <- data.frame(
    beta_coef = beta_coef,
    std_error = std_error,
    p_value = p_value,
    conf_int_lower = conf_int_lower,
    conf_int_upper = conf_int_upper,
    pseudo.r.squared = pseudo.r.squared
  )
  
  # set the row name of the data frame to the file name, minus the .RData extension
  row.names(model_summary_df) <- sub("\\.RData", "", file)
  
  # append the data frame to the summary_df data frame
  summary_df <- rbind(summary_df, model_summary_df)
}

# Remove the substring ".norm_mc_PID_facets_linear_model" from the row names
rownames(summary_df) <- gsub("\\.norm_Conf_PID_domain_beta_model", "", rownames(summary_df))

# adjusted p values for fdr
p_values <- summary_df$p_value
summary_df$p_adjusted_fdr <- p.adjust(p_values, method = "fdr")

# Print the resulting data frame
summary_df

# round the values 
summary_df <- round(summary_df, digits = 3)


### save the data frame as png

# Set the file path and name
file_path <- "D:/Windows/Descargas/Git_Metacog_Personalidad/Pubilico/Metacognition.PersonalityTraits/git/Tables/Tables/table_4.4.png"
# Create the directory if it doesn't exist
dir.create(dirname(file_path), showWarnings = FALSE)

# Open the PNG device and specify the file path
png(file_path, width = 14, height = 5, units = "in", res = 72)

# Create the table grob
p <- tableGrob(summary_df)

# Draw the table grob
grid.draw(p)

# Close the device
dev.off()