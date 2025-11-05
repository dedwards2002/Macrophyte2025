# === Macrophyte Mesocosm 2025: Setup & Exploration ===

# 1. Load required packages ----
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("janitor")) install.packages("janitor")
library(tidyverse)
library(janitor)

# 2. Set working directory ----
# Use your usual working folder
#setwd("/Users/dominiqueedwards/surge_spatial/mesocosm_data")

# 3. Read the uploaded CSV ----
# Replace the path below if you moved it locally
meso <- read_csv("/Users/dominiqueedwards/Downloads/Macrophyte mesocosm master sheet 2025 - Sheet1 (1).csv")

# 4. Clean names & remove empty rows/cols ----
meso <- meso %>%
  clean_names() %>%
  remove_empty("rows") %>%
  remove_empty("cols")

# 5. Quick exploration ----
glimpse(meso)     # column types
summary(meso)     # numeric summaries
head(meso, 10)    # first 10 rows
nrow(meso); ncol(meso)

# 6. Check treatment / replicate structure ----
# Once we know the column names, we’ll adjust this:
# e.g., meso %>% count(treatment, replicate)
