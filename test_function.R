# ============================================
# test_function.R 
# ============================================

library(tidyverse)
library(readr)
library(ggplot2)

# ----------------------------------------------------------
# 0) Load data
# ----------------------------------------------------------
df <- read_csv("https://www4.stat.ncsu.edu/~online/datasets/EDU01a.csv", show_col_types = FALSE)
glimpse(df)

# ----------------------------------------------------------
# 1) long_format_conversion
# ----------------------------------------------------------
df_selected <- df %>%
  select(Area_name, STCOU, ends_with("D")) %>%
  rename(area_name = Area_name)

df_long <- pivot_longer(
  df_selected,
  cols = ends_with("D"),
  names_to = "Survey",
  values_to = "Enrollment Value"
)

head(df_long)

# ----------------------------------------------------------
# 2) survey_function
# ----------------------------------------------------------
long_updated <- df_long %>%
  mutate(
    Year = as.numeric(substr(Survey, 8, 9)),
    Year = ifelse(Year > 25, Year + 1900, Year + 2000),
    Measurement = substr(Survey, 1, 7)
  )

head(long_updated)

# ----------------------------------------------------------
# 3) state_function
# (do after splitting county)
# ----------------------------------------------------------

# ----------------------------------------------------------
# 4) division_function
# (do after splitting noncounty)
# ----------------------------------------------------------

# ----------------------------------------------------------
# 5) create_datasets equivalent
# ----------------------------------------------------------
county_idx <- grep(", \\w\\w", long_updated$area_name)
county <- long_updated[county_idx, ]
noncounty <- long_updated[-county_idx, ]

class(county) <- c("county", class(county))
class(noncounty) <- c("state", class(noncounty))

# Add State to county (state_function)
county <- county %>%
  mutate(State = substr(area_name, nchar(area_name) - 1, nchar(area_name)))

head(county)

# Add Division to noncounty (division_function)
noncounty <- noncounty %>%
  mutate(area_name = toupper(area_name)) %>%
  mutate(
    Division = case_when(
      area_name %in% c("CONNECTICUT", "MAINE", "MASSACHUSETTS", "NEW HAMPSHIRE", "RHODE ISLAND", "VERMONT") ~ "New England",
      area_name %in% c("NEW JERSEY", "NEW YORK", "PENNSYLVANIA") ~ "Middle Atlantic",
      area_name %in% c("ILLINOIS", "INDIANA", "MICHIGAN", "OHIO", "WISCONSIN") ~ "East North Central",
      area_name %in% c("IOWA", "KANSAS", "MINNESOTA", "MISSOURI", "NEBRASKA", "NORTH DAKOTA", "SOUTH DAKOTA") ~ "West North Central",
      area_name %in% c("DELAWARE", "MARYLAND", "DISTRICT OF COLUMBIA", "VIRGINIA", "WEST VIRGINIA", "NORTH CAROLINA", "SOUTH CAROLINA", "GEORGIA", "FLORIDA") ~ "South Atlantic",
      area_name %in% c("ALABAMA", "KENTUCKY", "MISSISSIPPI", "TENNESSEE") ~ "East South Central",
      area_name %in% c("ARKANSAS", "LOUISIANA", "OKLAHOMA", "TEXAS") ~ "West South Central",
      area_name %in% c("ARIZONA", "COLORADO", "IDAHO", "MONTANA", "NEVADA", "NEW MEXICO", "UTAH", "WYOMING") ~ "Mountain",
      area_name %in% c("ALASKA", "CALIFORNIA", "HAWAII", "OREGON", "WASHINGTON") ~ "Pacific",
      TRUE ~ "ERROR"
    )
  )

# Check Division table (no zeros!)
noncounty %>% count(Division, area_name) %>% print(n = 52)

# ----------------------------------------------------------
# 6) Wrapper function equivalent test
# (use actual wrapper)
# ----------------------------------------------------------
wrapped <- my_wrapper("https://www4.stat.ncsu.edu/~online/datasets/EDU01a.csv", value = "Enrollment Value")

head(wrapped$county)
head(wrapped$noncounty)

# ----------------------------------------------------------
# 7) Combine function test
# ----------------------------------------------------------
wrapped2 <- my_wrapper("https://www4.stat.ncsu.edu/~online/datasets/EDU01b.csv", value = "Enrollment Value")
combined <- combine_wrapper_results(wrapped, wrapped2)

head(combined$county)
head(combined$noncounty)

# ----------------------------------------------------------
# 8) plot.state test
# ----------------------------------------------------------
plot(combined$noncounty, var_name = "Enrollment Value")

# ----------------------------------------------------------
# 9) plot.county test
# ----------------------------------------------------------
plot(combined$county, var_name = "Enrollment Value", state = "NC", top_or_bottom = "top", n = 10)
plot(combined$county, var_name = "Enrollment Value", state = "SC", top_or_bottom = "bottom", n = 5)
plot(combined$county, var_name = "Enrollment Value")  # Default
plot(combined$county, var_name = "Enrollment Value", state = "PA", top_or_bottom = "top", n = 8)
