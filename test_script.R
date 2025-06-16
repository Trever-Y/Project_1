# =============================================
# test_script.R - For validating functions.R
# =============================================

library(tidyverse)
library(readr)
library(ggplot2)

# Load your custom functions
source("functions.R")

# ---------------------------------------------
# 1) EDU Data Processing
# ---------------------------------------------
edu1 <- my_wrapper("https://www4.stat.ncsu.edu/~online/datasets/EDU01a.csv", value = "Enrollment Value")
edu2 <- my_wrapper("https://www4.stat.ncsu.edu/~online/datasets/EDU01b.csv", value = "Enrollment Value")

edu_combined <- combine_wrapper_results(edu1, edu2)

# Check output
head(edu_combined$county)
head(edu_combined$noncounty)

# ---------------------------------------------
# 2) EDU State Plot
# ---------------------------------------------
plot(edu_combined$noncounty, var_name = "Enrollment Value")

# ---------------------------------------------
# 3) EDU County Plots
# ---------------------------------------------
plot(edu_combined$county, var_name = "Enrollment Value", state = "NC", top_or_bottom = "top", n = 20)
plot(edu_combined$county, var_name = "Enrollment Value", state = "SC", top_or_bottom = "bottom", n = 7)
plot(edu_combined$county, var_name = "Enrollment Value") # default (NC, top 5)
plot(edu_combined$county, var_name = "Enrollment Value", state = "PA", top_or_bottom = "top", n = 8)

# ---------------------------------------------
# 4) PST Data Processing
# ---------------------------------------------
pst1 <- my_wrapper("https://www4.stat.ncsu.edu/~online/datasets/PST01a.csv", value = "Enrollment Value")
pst2 <- my_wrapper("https://www4.stat.ncsu.edu/~online/datasets/PST01b.csv", value = "Enrollment Value")
pst3 <- my_wrapper("https://www4.stat.ncsu.edu/~online/datasets/PST01c.csv", value = "Enrollment Value")
pst4 <- my_wrapper("https://www4.stat.ncsu.edu/~online/datasets/PST01d.csv", value = "Enrollment Value")

pst12 <- combine_wrapper_results(pst1, pst2)
pst34 <- combine_wrapper_results(pst3, pst4)
pst_combined <- combine_wrapper_results(pst12, pst34)

# ---------------------------------------------
# 5) PST State Plot
# ---------------------------------------------
plot(pst_combined$noncounty, var_name = "Enrollment Value")

# ---------------------------------------------
# 6) PST County Plots
# ---------------------------------------------
plot(pst_combined$county, var_name = "Enrollment Value", state = "CA", top_or_bottom = "top", n = 15)
plot(pst_combined$county, var_name = "Enrollment Value", state = "TX", top_or_bottom = "top", n = 4)
plot(pst_combined$county, var_name = "Enrollment Value") # default (NC, top 5)
plot(pst_combined$county, var_name = "Enrollment Value", state = "NY", top_or_bottom = "top", n = 10)
