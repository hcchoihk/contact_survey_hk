# Data Analysis for Hong Kong Contact Survey
# This script performs descriptive and statistical analysis on the contact survey data

# Load required libraries
library(dplyr)
library(readr)
library(tidyr)

# Set working directory to project root
if (!exists("project_root")) {
  project_root <- here::here()
}

# Load processed data
cat("Loading processed contact survey data...\n")
cleaned_data <- read_csv(file.path(project_root, "data/processed/cleaned_contact_data.csv"),
                        show_col_types = FALSE)
participant_summary <- read_csv(file.path(project_root, "data/processed/participant_summary.csv"),
                               show_col_types = FALSE)

# ===== DESCRIPTIVE STATISTICS =====
cat("\n===== DESCRIPTIVE STATISTICS =====\n")

# Overall contact patterns
cat("\n1. Overall Contact Patterns:\n")
cat("   Mean contacts per participant:", 
    round(mean(participant_summary$total_contacts), 2), "\n")
cat("   Median contacts per participant:", 
    median(participant_summary$total_contacts), "\n")
cat("   Mean physical contacts per participant:", 
    round(mean(participant_summary$physical_contacts), 2), "\n")
cat("   Mean non-physical contacts per participant:", 
    round(mean(participant_summary$non_physical_contacts), 2), "\n")

# Contact type distribution
cat("\n2. Contact Type Distribution:\n")
contact_type_summary <- cleaned_data %>%
  group_by(contact_type) %>%
  summarise(
    count = n(),
    percentage = round(n() / nrow(cleaned_data) * 100, 1)
  )
print(contact_type_summary)

# Contacts by location
cat("\n3. Contacts by Location:\n")
location_summary <- cleaned_data %>%
  group_by(contact_location) %>%
  summarise(
    count = n(),
    percentage = round(n() / nrow(cleaned_data) * 100, 1),
    avg_duration = round(mean(duration_minutes), 1)
  ) %>%
  arrange(desc(count))
print(location_summary)

# Contacts by participant age group
cat("\n4. Contacts by Participant Age Group:\n")
age_group_summary <- cleaned_data %>%
  group_by(age_group) %>%
  summarise(
    n_participants = n_distinct(participant_id),
    total_contacts = n(),
    avg_contacts = round(n() / n_distinct(participant_id), 2),
    avg_duration = round(mean(duration_minutes), 1)
  )
print(age_group_summary)

# Contacts by occupation
cat("\n5. Contacts by Participant Occupation:\n")
occupation_summary <- participant_summary %>%
  group_by(occupation) %>%
  summarise(
    n_participants = n(),
    avg_total_contacts = round(mean(total_contacts), 2),
    avg_physical = round(mean(physical_contacts), 2),
    avg_non_physical = round(mean(non_physical_contacts), 2)
  ) %>%
  arrange(desc(avg_total_contacts))
print(occupation_summary)

# Contact mixing patterns (age assortativity)
cat("\n6. Age Mixing Patterns:\n")
mixing_matrix <- cleaned_data %>%
  filter(contact_type == "Physical") %>%
  group_by(age_group, contact_age_group) %>%
  summarise(contacts = n(), .groups = "drop") %>%
  pivot_wider(names_from = contact_age_group, values_from = contacts, values_fill = 0)
print(mixing_matrix)

# ===== STATISTICAL TESTS =====
cat("\n===== STATISTICAL TESTS =====\n")

# Test difference in contact numbers by gender
cat("\n7. Testing difference in contacts by gender:\n")
gender_test <- t.test(total_contacts ~ gender, data = participant_summary)
cat("   t-statistic:", round(gender_test$statistic, 3), "\n")
cat("   p-value:", round(gender_test$p.value, 4), "\n")

# Test difference in contact duration by contact type
cat("\n8. Testing difference in duration by contact type:\n")
duration_test <- t.test(duration_minutes ~ contact_type, data = cleaned_data)
cat("   t-statistic:", round(duration_test$statistic, 3), "\n")
cat("   p-value:", round(duration_test$p.value, 4), "\n")

# Correlation between age and number of contacts
cat("\n9. Correlation between age and number of contacts:\n")
cor_test <- cor.test(participant_summary$age, participant_summary$total_contacts)
cat("   Correlation coefficient:", round(cor_test$estimate, 3), "\n")
cat("   p-value:", round(cor_test$p.value, 4), "\n")

# Save analysis results
cat("\nSaving analysis results...\n")
write_csv(contact_type_summary, 
         file.path(project_root, "output/tables/contact_type_summary.csv"))
write_csv(location_summary, 
         file.path(project_root, "output/tables/location_summary.csv"))
write_csv(age_group_summary, 
         file.path(project_root, "output/tables/age_group_summary.csv"))
write_csv(occupation_summary, 
         file.path(project_root, "output/tables/occupation_summary.csv"))

cat("\nAnalysis complete!\n")
cat("Results saved to output/tables/\n")
