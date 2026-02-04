# === Macrophyte Mesocosm 2025: Setup & Exploration ===

# 1. Load required packages ----
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("janitor")) install.packages("janitor")
library(tidyverse)
library(janitor)

# 2. Set working directory ----
# Use your usual working folder
#setwd("/Users/dominiqueedwards/surge_spatial/Macrophyte mesocosm master sheet 2025 - Sheet1 (2)")

# 3. Read the uploaded CSV ----
# Replace the path below if you moved it locally
meso <- read_csv("/Users/dominiqueedwards/Macrophyte2025/Macrophyte mesocosm master sheet 2025 - Sheet1 (2).csv")

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


# DO sat over time ####
# ============================================================
# Percent DO Saturation (Surface, 0.1 m) Over Time
# Fix depth typo on 9/29 where 1 -> 0.1
# ============================================================

library(tidyverse)
library(lubridate)
library(janitor)

# ---- Load cleaned dataset ----
dat <- read_csv("mesocosm_2025_cleaned.csv") %>% 
  clean_names()

# ---- FIX THE TYPO: depth = 1 on 2025-09-29 should be 0.1 ----
dat <- dat %>%
  mutate(date = as.Date(date)) %>%
  mutate(
    depth = if_else(date == as.Date("2025-09-29") & depth == 1,
                    0.1, depth)
  )

# ---- Filter to SURFACE DEPTH: 0.1 m ----
surf <- dat %>%
  filter(!is.na(depth),
         abs(depth - 0.10) < 1e-6)  # captures floating point issues

# ---- Create treatment groups ----
surf <- surf %>%
  mutate(treat_group = case_when(
    str_detect(treatment, "^A") ~ "A",
    str_detect(treatment, "^B") ~ "B",
    str_detect(treatment, "^C") ~ "C",
    str_detect(treatment, "^D") ~ "D",
    treatment %in% c("ref 1", "ref 2") ~ "Ref",
    TRUE ~ treatment
  ))

# ---- Ensure treatment order ----
surf$treat_group <- factor(surf$treat_group,
                           levels = c("Ref", "A", "B", "C", "D"))

# ---- Boxplot of DO saturation over time ----
p <- ggplot(surf, aes(x = treat_group,
                      y = do_sat,
                      fill = treat_group)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.85) +
  facet_wrap(~ date, nrow = 1, scales = "free_x") +
  labs(
    title = "Surface DO Saturation (0.1 m) by Treatment Over Time",
    x = "Treatment Group",
    y = "DO Saturation (%)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "gray90"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

print(p)

