##########################
### Correlation Matrix ### FIG
##########################

### library
library(corrplot)

### load correlation resutls
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("git/Data/Regression_Results/Correlation_Matrix_PID_domain_facets.RData")
load(file= filepath)

### FIG
# Insignificant correlations are leaved blank
corrplot(a$r, type="upper",  tl.col = "black", tl.srt = 45,
         p.mat = a$P, sig.level = 0.01, insig = "blank")
