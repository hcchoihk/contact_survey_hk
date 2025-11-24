# Data Visualization for Hong Kong Contact Survey
# This script creates visualizations of the contact survey data

# Load required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# Set working directory to project root
if (!exists("project_root")) {
  project_root <- here::here()
}

# Create output directory if it doesn't exist
dir.create(file.path(project_root, "output/figures"), 
          showWarnings = FALSE, recursive = TRUE)

# Load processed data
cat("Loading processed contact survey data...\n")
cleaned_data <- read_csv(file.path(project_root, "data/processed/cleaned_contact_data.csv"),
                        show_col_types = FALSE)
participant_summary <- read_csv(file.path(project_root, "data/processed/participant_summary.csv"),
                               show_col_types = FALSE)

# Set theme for all plots
theme_set(theme_minimal() + 
         theme(plot.title = element_text(hjust = 0.5, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5)))

# ===== PLOT 1: Distribution of contacts per participant =====
cat("\nCreating Plot 1: Distribution of contacts per participant...\n")
p1 <- ggplot(participant_summary, aes(x = total_contacts)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(title = "Distribution of Total Contacts per Participant",
       subtitle = "Hong Kong Contact Survey",
       x = "Number of Contacts",
       y = "Number of Participants") +
  theme_minimal()

ggsave(file.path(project_root, "output/figures/01_contact_distribution.png"),
       p1, width = 8, height = 6, dpi = 300)

# ===== PLOT 2: Contact type comparison =====
cat("Creating Plot 2: Contact type comparison...\n")
p2 <- ggplot(cleaned_data, aes(x = contact_type, fill = contact_type)) +
  geom_bar(alpha = 0.8) +
  labs(title = "Physical vs Non-Physical Contacts",
       subtitle = "Hong Kong Contact Survey",
       x = "Contact Type",
       y = "Number of Contacts") +
  scale_fill_manual(values = c("Physical" = "#E74C3C", "Non-physical" = "#3498DB")) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(project_root, "output/figures/02_contact_type.png"),
       p2, width = 8, height = 6, dpi = 300)

# ===== PLOT 3: Contacts by location =====
cat("Creating Plot 3: Contacts by location...\n")
location_data <- cleaned_data %>%
  group_by(contact_location) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

p3 <- ggplot(location_data, aes(x = reorder(contact_location, count), y = count)) +
  geom_col(fill = "darkgreen", alpha = 0.8) +
  coord_flip() +
  labs(title = "Contacts by Location",
       subtitle = "Hong Kong Contact Survey",
       x = "Location",
       y = "Number of Contacts") +
  theme_minimal()

ggsave(file.path(project_root, "output/figures/03_contacts_by_location.png"),
       p3, width = 8, height = 6, dpi = 300)

# ===== PLOT 4: Average contacts by age group =====
cat("Creating Plot 4: Average contacts by age group...\n")
age_data <- cleaned_data %>%
  group_by(participant_id, age_group) %>%
  summarise(contacts = n(), .groups = "drop") %>%
  group_by(age_group) %>%
  summarise(avg_contacts = mean(contacts), 
           se = sd(contacts) / sqrt(n()))

p4 <- ggplot(age_data, aes(x = age_group, y = avg_contacts)) +
  geom_col(fill = "purple", alpha = 0.7) +
  geom_errorbar(aes(ymin = avg_contacts - se, ymax = avg_contacts + se), 
               width = 0.2) +
  labs(title = "Average Contacts by Age Group",
       subtitle = "Hong Kong Contact Survey (with standard error)",
       x = "Age Group",
       y = "Average Number of Contacts") +
  theme_minimal()

ggsave(file.path(project_root, "output/figures/04_contacts_by_age.png"),
       p4, width = 8, height = 6, dpi = 300)

# ===== PLOT 5: Contact duration by type =====
cat("Creating Plot 5: Contact duration by type...\n")
p5 <- ggplot(cleaned_data, aes(x = contact_type, y = duration_minutes, fill = contact_type)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Contact Duration by Type",
       subtitle = "Hong Kong Contact Survey",
       x = "Contact Type",
       y = "Duration (minutes)") +
  scale_fill_manual(values = c("Physical" = "#E74C3C", "Non-physical" = "#3498DB")) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(project_root, "output/figures/05_duration_by_type.png"),
       p5, width = 8, height = 6, dpi = 300)

# ===== PLOT 6: Contacts by occupation =====
cat("Creating Plot 6: Average contacts by occupation...\n")
occupation_data <- participant_summary %>%
  group_by(occupation) %>%
  summarise(avg_contacts = mean(total_contacts),
           avg_physical = mean(physical_contacts),
           avg_non_physical = mean(non_physical_contacts)) %>%
  pivot_longer(cols = starts_with("avg_"), 
              names_to = "contact_type", 
              values_to = "avg_contacts") %>%
  mutate(contact_type = recode(contact_type,
                              "avg_contacts" = "Total",
                              "avg_physical" = "Physical",
                              "avg_non_physical" = "Non-physical"))

p6 <- ggplot(occupation_data, aes(x = reorder(occupation, avg_contacts), 
                                  y = avg_contacts, fill = contact_type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  coord_flip() +
  labs(title = "Average Contacts by Occupation",
       subtitle = "Hong Kong Contact Survey",
       x = "Occupation",
       y = "Average Number of Contacts",
       fill = "Contact Type") +
  scale_fill_manual(values = c("Total" = "#34495E", 
                               "Physical" = "#E74C3C", 
                               "Non-physical" = "#3498DB")) +
  theme_minimal()

ggsave(file.path(project_root, "output/figures/06_contacts_by_occupation.png"),
       p6, width = 10, height = 6, dpi = 300)

# ===== PLOT 7: Age mixing heatmap =====
cat("Creating Plot 7: Age mixing heatmap...\n")
mixing_data <- cleaned_data %>%
  filter(contact_type == "Physical") %>%
  group_by(age_group, contact_age_group) %>%
  summarise(contacts = n(), .groups = "drop")

p7 <- ggplot(mixing_data, aes(x = age_group, y = contact_age_group, fill = contacts)) +
  geom_tile(color = "white") +
  geom_text(aes(label = contacts), color = "white", fontface = "bold") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Age Mixing Patterns (Physical Contacts)",
       subtitle = "Hong Kong Contact Survey",
       x = "Participant Age Group",
       y = "Contact Age Group",
       fill = "Number of\nContacts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(project_root, "output/figures/07_age_mixing_heatmap.png"),
       p7, width = 8, height = 7, dpi = 300)

cat("\nVisualization complete!\n")
cat("All plots saved to output/figures/\n")
