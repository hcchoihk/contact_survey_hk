# Hong Kong Contact Survey - Data Analysis

This repository contains the data and R scripts used for analyzing the social contact survey conducted in Hong Kong.

## Project Structure

```
.
├── data/
│   ├── raw/                          # Raw survey data
│   │   └── contact_survey_data.csv   # Original contact survey data
│   └── processed/                    # Cleaned and processed data (generated)
│       ├── cleaned_contact_data.csv
│       └── participant_summary.csv
├── scripts/
│   ├── 01_data_preprocessing.R       # Data cleaning and preprocessing
│   ├── 02_data_analysis.R            # Statistical analysis
│   ├── 03_visualization.R            # Data visualization
│   └── run_all_analyses.R            # Master script to run all analyses
└── output/
    ├── figures/                      # Generated plots (PNG)
    └── tables/                       # Analysis results (CSV)
```

## Data Description

The contact survey data includes the following variables:

- **participant_id**: Unique identifier for each survey participant
- **age**: Age of the participant
- **gender**: Gender of the participant (Male/Female)
- **occupation**: Occupation category of the participant
- **date**: Date of the contact
- **contact_type**: Type of contact (Physical/Non-physical)
- **contact_age**: Age of the contact person
- **contact_gender**: Gender of the contact person
- **contact_location**: Location where contact occurred (Home/Work/School/Community/Transport)
- **duration_minutes**: Duration of contact in minutes

## Requirements

### R Version
- R version 4.0.0 or higher

### Required R Packages
- `dplyr` - Data manipulation
- `readr` - Reading CSV files
- `ggplot2` - Data visualization
- `tidyr` - Data tidying
- `lubridate` - Date handling
- `here` - Project-relative paths

## Installation

Install required packages by running:

```r
install.packages(c("dplyr", "readr", "ggplot2", "tidyr", "lubridate", "here"))
```

## Usage

### Option 1: Run Complete Analysis Pipeline

To run all analysis scripts in sequence:

```r
source("scripts/run_all_analyses.R")
```

This will:
1. Install any missing required packages
2. Preprocess the raw data
3. Perform statistical analyses
4. Generate visualizations

### Option 2: Run Individual Scripts

You can also run scripts individually:

```r
# Set project root
library(here)
project_root <- here::here()

# 1. Data preprocessing
source("scripts/01_data_preprocessing.R")

# 2. Statistical analysis
source("scripts/02_data_analysis.R")

# 3. Generate visualizations
source("scripts/03_visualization.R")
```

## Analysis Scripts

### 1. Data Preprocessing (`01_data_preprocessing.R`)

This script:
- Loads raw survey data
- Cleans and validates the data
- Creates age group categories
- Converts data types appropriately
- Generates participant-level summary statistics
- Saves processed data to `data/processed/`

**Outputs:**
- `data/processed/cleaned_contact_data.csv`
- `data/processed/participant_summary.csv`

### 2. Data Analysis (`02_data_analysis.R`)

This script performs:
- Descriptive statistics on contact patterns
- Contact type distribution analysis
- Location-based contact analysis
- Age group analysis
- Occupation-based analysis
- Age mixing pattern analysis
- Statistical tests (t-tests, correlations)

**Outputs:**
- `output/tables/contact_type_summary.csv`
- `output/tables/location_summary.csv`
- `output/tables/age_group_summary.csv`
- `output/tables/occupation_summary.csv`

### 3. Visualization (`03_visualization.R`)

This script generates:
1. Distribution of contacts per participant
2. Physical vs non-physical contacts comparison
3. Contacts by location
4. Average contacts by age group
5. Contact duration by type
6. Average contacts by occupation
7. Age mixing patterns heatmap

**Outputs:**
- `output/figures/01_contact_distribution.png`
- `output/figures/02_contact_type.png`
- `output/figures/03_contacts_by_location.png`
- `output/figures/04_contacts_by_age.png`
- `output/figures/05_duration_by_type.png`
- `output/figures/06_contacts_by_occupation.png`
- `output/figures/07_age_mixing_heatmap.png`

## Key Findings

The analysis provides insights into:

1. **Contact Patterns**: Average number of contacts per participant, distribution across different demographics
2. **Contact Types**: Comparison between physical and non-physical contacts
3. **Location Analysis**: Where contacts typically occur (home, work, school, etc.)
4. **Age Patterns**: How contact patterns vary by age group and age mixing behavior
5. **Occupational Differences**: Variation in contact patterns across different occupations
6. **Duration Analysis**: How long contacts typically last by type

## Notes

- All output files (processed data, tables, figures) are generated automatically
- The `.gitignore` file is configured to exclude generated output files
- Raw data is preserved in `data/raw/` and should not be modified
- The sample data provided is for demonstration purposes

## Contact

For questions or issues related to this analysis, please open an issue in the repository.
