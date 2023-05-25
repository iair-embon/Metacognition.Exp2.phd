########################################################################
### ElasticNet linear model with Confidence facets - alpha selection ### TAB 3.22
########################################################################

library(ggplot2)
library(gridExtra)
library(data.table)
library(grid)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("git/Data/Regression_Results/Conf_PID_domain_fit_elasticNet.RData")
load(file= filepath)

index_cov <- a@i
names_cov <- a@Dimnames[[1]][index_cov+1]
coef_cov <- a@x


model <- data.frame(Beta = coef_cov)
row.names(model) <- names_cov

# round the values 
model <- round(model, digits = 3)


### save the data frame as png

# Set the file path and name
file_path <- "D:/Windows/Descargas/Git_Metacog_Personalidad/Pubilico/Metacognition.PersonalityTraits/git/Tables/Tables/table_3.22.png"
# Create the directory if it doesn't exist
dir.create(dirname(file_path), showWarnings = FALSE)

# Open the PNG device and specify the file path
png(file_path, width = 7, height = 3, units = "in", res = 200)

# Create the table grob
p <- tableGrob(model)

# Draw the table grob
grid.draw(p)

# Close the device
dev.off()

