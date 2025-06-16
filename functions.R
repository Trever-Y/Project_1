# ============================================
# functions.R
# ============================================

library(tidyverse)
library(readr)

# --------------------------------------------
# 1) Convert wide format to long format
# --------------------------------------------
long_format_conversion <- function(df, value = "Enrollment Value") {
  selected_data <- df %>%
    # Select required columns
    select(Area_name, STCOU, ends_with("D")) %>%
    # Rename Area_name to area_name
    rename(area_name = Area_name)
  
  # Convert to long format
  long_data <- pivot_longer(
    selected_data,
    cols = ends_with("D"),
    names_to = "Survey",
    values_to = value
  )
  return(long_data)
}

# --------------------------------------------
# 2) Extract Year and Measurement
# --------------------------------------------
survey_function <- function(long_data) {
  long_data_updated <- long_data %>%
    # Extract the last two digits as Year and convert to numeric
    mutate(Year = as.numeric(substr(Survey, start = 8, stop = 9))) %>%
    # Convert two-digit Year to four-digit Year
    mutate(Year = ifelse(Year > 25, Year + 1900, Year + 2000)) %>%
    # Extract the first 7 characters as Measurement
    mutate(Measurement = substr(Survey, start = 1, stop = 7))
  
  return(long_data_updated)
}

# --------------------------------------------
# 3) Add State column to county data
# --------------------------------------------
state_function <- function(county_tibble) {
  new_county_tibble <- county_tibble %>%
    mutate(State = substr(area_name, start = nchar(area_name) - 1, stop = nchar(area_name)))
  return(new_county_tibble)
}

# --------------------------------------------
# 4) Add Division column to non-county data (US Census standard)
# --------------------------------------------
division_function <- function(noncounty_tibble) {
  noncounty_tibble_updated <- noncounty_tibble %>%
    mutate(area_name = toupper(area_name)) %>%  # Ensure uppercase for matching
    mutate(
      Division = case_when(
        area_name %in% c(
          "CONNECTICUT", "MAINE", "MASSACHUSETTS", 
          "NEW HAMPSHIRE", "RHODE ISLAND", "VERMONT"
        ) ~ "New England",
        area_name %in% c(
          "NEW JERSEY", "NEW YORK", "PENNSYLVANIA"
        ) ~ "Middle Atlantic",
        area_name %in% c(
          "ILLINOIS", "INDIANA", "MICHIGAN", "OHIO", "WISCONSIN"
        ) ~ "East North Central",
        area_name %in% c(
          "IOWA", "KANSAS", "MINNESOTA", "MISSOURI",
          "NEBRASKA", "NORTH DAKOTA", "SOUTH DAKOTA"
        ) ~ "West North Central",
        area_name %in% c(
          "DELAWARE", "MARYLAND", "DISTRICT OF COLUMBIA",
          "VIRGINIA", "WEST VIRGINIA", "NORTH CAROLINA",
          "SOUTH CAROLINA", "GEORGIA", "FLORIDA"
        ) ~ "South Atlantic",
        area_name %in% c(
          "ALABAMA", "KENTUCKY", "MISSISSIPPI", "TENNESSEE"
        ) ~ "East South Central",
        area_name %in% c(
          "ARKANSAS", "LOUISIANA", "OKLAHOMA", "TEXAS"
        ) ~ "West South Central",
        area_name %in% c(
          "ARIZONA", "COLORADO", "IDAHO", "MONTANA",
          "NEVADA", "NEW MEXICO", "UTAH", "WYOMING"
        ) ~ "Mountain",
        area_name %in% c(
          "ALASKA", "CALIFORNIA", "HAWAII", "OREGON", "WASHINGTON"
        ) ~ "Pacific",
        TRUE ~ "ERROR"
      )
    )
  return(noncounty_tibble_updated)
}

# --------------------------------------------
# 5) Create county and non-county datasets, add class and extra columns
# --------------------------------------------
create_datasets <- function(long_data) {
  # Split by presence of ", XX" at end of area_name
  county_indices <- grep(pattern = ", \\w\\w", long_data$area_name)
  noncounty_tibble <- long_data[-county_indices, ]
  class(noncounty_tibble) <- c("state", class(noncounty_tibble))
  
  county_tibble <- long_data[county_indices, ]
  class(county_tibble) <- c("county", class(county_tibble))
  
  final_county_tibble <- state_function(county_tibble)
  final_noncounty_tibble <- division_function(noncounty_tibble)
  
  return(list(county = final_county_tibble, noncounty = final_noncounty_tibble))
}

# --------------------------------------------
# 6) Final wrapper function to run all steps
# --------------------------------------------
my_wrapper <- function(url, value = "Enrollment Value") {
  result <- read_csv(url, show_col_types = FALSE) %>%
    long_format_conversion(value = value) %>%
    survey_function() %>%
    create_datasets()
  return(result)
}

# --------------------------------------------
# 7) Combine function
# --------------------------------------------
combine_wrapper_results <- function(wrapper1, wrapper2) {
  combined_county <- bind_rows(wrapper1$county, wrapper2$county)
  combined_noncounty <- bind_rows(wrapper1$noncounty, wrapper2$noncounty)
  list(county = combined_county, noncounty = combined_noncounty)
}

# --------------------------------------------
# 8) plot method for state class
# --------------------------------------------
plot.state <- function(df, var_name = "Enrollment Value") {
  df %>%
    filter(Division != "ERROR") %>%
    group_by(Division, Year) %>%
    summarize(mean_value = mean(get(var_name), na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = Year, y = mean_value, color = Division)) +
    geom_line() +
    labs(
      title = "Mean Value per Division over Years",
      y = paste("Mean of", var_name),
      x = "Year"
    )
}

# --------------------------------------------
# 9) plot method for county class
# --------------------------------------------
plot.county <- function(df, var_name = "Enrollment Value",
                        state = "NC", top_or_bottom = "top", n = 5) {
  
  # Filter data for selected state
  df_state <- df %>% filter(State == state)
  
  # Calculate mean for each county
  mean_df <- df_state %>%
    group_by(area_name) %>%
    summarize(mean_value = mean(get(var_name), na.rm = TRUE), .groups = "drop")
  
  # Sort by mean and pick top or bottom n
  if (top_or_bottom == "top") {
    mean_df <- mean_df %>% arrange(desc(mean_value))
  } else {
    mean_df <- mean_df %>% arrange(mean_value)
  }
  
  selected_areas <- mean_df %>% slice_head(n = n) %>% pull(area_name)
  
  # Filter original data for selected counties
  df_selected <- df_state %>% filter(area_name %in% selected_areas)
  
  # Plot actual values (not means)
  ggplot(df_selected, aes(x = Year, y = get(var_name), color = area_name)) +
    geom_line() +
    labs(
      title = paste(top_or_bottom, n, "counties in", state),
      y = var_name,
      x = "Year"
    )
}


