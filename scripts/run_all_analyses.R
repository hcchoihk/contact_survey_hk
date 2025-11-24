# Master Script for Hong Kong Contact Survey Analysis
# This script runs all analysis scripts in sequence

cat("=========================================\n")
cat("Hong Kong Contact Survey Analysis Pipeline\n")
cat("=========================================\n\n")

# Install and load required packages
required_packages <- c("dplyr", "readr", "ggplot2", "tidyr", "lubridate", "here")

cat("Checking required packages...\n")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

# Set project root
project_root <- here::here()
cat("\nProject root:", project_root, "\n\n")

# Run analysis scripts in sequence
scripts <- c(
  "01_data_preprocessing.R",
  "02_data_analysis.R",
  "03_visualization.R"
)

for (script in scripts) {
  script_path <- file.path(project_root, "scripts", script)
  cat("\n=========================================\n")
  cat("Running:", script, "\n")
  cat("=========================================\n\n")
  
  tryCatch({
    source(script_path)
  }, error = function(e) {
    cat("ERROR in", script, ":", conditionMessage(e), "\n")
    stop("Analysis pipeline stopped due to error.")
  })
  
  cat("\n")
}

cat("=========================================\n")
cat("Analysis Pipeline Complete!\n")
cat("=========================================\n")
cat("\nOutput files:\n")
cat("  - Processed data: data/processed/\n")
cat("  - Analysis tables: output/tables/\n")
cat("  - Visualizations: output/figures/\n")
