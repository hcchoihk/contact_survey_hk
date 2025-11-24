# Data Preprocessing for Hong Kong Contact Survey
# This script loads and cleans the raw contact survey data

# Load required libraries
library(dplyr)
library(readr)
library(lubridate)

# Set working directory to project root
if (!exists("project_root")) {
  project_root <- here::here()
}

# Load raw data
cat("Loading raw contact survey data...\n")
raw_data <- read_csv(file.path(project_root, "data/raw/contact_survey_data.csv"),
                     show_col_types = FALSE)

# Display basic information about the data
cat("\nData dimensions:", nrow(raw_data), "rows,", ncol(raw_data), "columns\n")
cat("\nColumn names:\n")
print(names(raw_data))

# Data cleaning and preprocessing
cat("\nPreprocessing data...\n")

cleaned_data <- raw_data %>%
  # Convert date to Date type
  mutate(date = as.Date(date)) %>%
  
  # Ensure categorical variables are factors
  mutate(
    gender = factor(gender),
    occupation = factor(occupation),
    contact_type = factor(contact_type),
    contact_gender = factor(contact_gender),
    contact_location = factor(contact_location)
  ) %>%
  
  # Create age groups for analysis
  mutate(
    age_group = cut(age, 
                    breaks = c(0, 18, 30, 45, 60, 100),
                    labels = c("0-18", "19-30", "31-45", "46-60", "60+"),
                    include.lowest = TRUE),
    contact_age_group = cut(contact_age,
                           breaks = c(0, 18, 30, 45, 60, 100),
                           labels = c("0-18", "19-30", "31-45", "46-60", "60+"),
                           include.lowest = TRUE)
  ) %>%
  
  # Remove any rows with missing essential data
  filter(!is.na(participant_id), !is.na(date))

# Display summary of cleaned data
cat("\nCleaned data summary:\n")
cat("Total participants:", length(unique(cleaned_data$participant_id)), "\n")
cat("Total contacts recorded:", nrow(cleaned_data), "\n")
cat("Date range:", min(cleaned_data$date), "to", max(cleaned_data$date), "\n")

# Calculate summary statistics by participant
participant_summary <- cleaned_data %>%
  group_by(participant_id, age, gender, occupation) %>%
  summarise(
    total_contacts = n(),
    physical_contacts = sum(contact_type == "Physical"),
    non_physical_contacts = sum(contact_type == "Non-physical"),
    avg_contact_duration = mean(duration_minutes, na.rm = TRUE),
    .groups = "drop"
  )

# Save processed data
cat("\nSaving processed data...\n")
write_csv(cleaned_data, file.path(project_root, "data/processed/cleaned_contact_data.csv"))
write_csv(participant_summary, file.path(project_root, "data/processed/participant_summary.csv"))

cat("\nPreprocessing complete!\n")
cat("Processed files saved to data/processed/\n")
