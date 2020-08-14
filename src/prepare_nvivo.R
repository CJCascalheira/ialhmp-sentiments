# Dependencies
library(tidyverse)
library(flextable)

# Import data
all <- read_csv("data/translated_all_minority.csv")
zero <- read_csv("data/translated_zero_minority.csv")

# All minorities 
all_1 <- all %>%
  # Drop the quantitative data
  select(-c("qual_life", "resources", "language")) %>%
  # Rename the columns for team members
  rename(
    "Person" = "id",
    "Country" = "country_code",
    "Age" = "age",
    "Gender" = "gender_code",
    "Sexual Orientation" = "sex_orient_code",
    "Race" = "race_code",
    "Minority Statuses" = "minority_code",
    "Qualitative Data" = "qual"
  ) %>%
  mutate(Person = paste0("all_", Person))

# Zero minorities
zero_1 <- zero %>%
  # Drop the quantitative data
  select(-c("qual_life", "resources", "language")) %>%
  # Rename the columns for team members
  rename(
    "Person" = "id",
    "Country" = "country_code",
    "Age" = "age",
    "Gender" = "gender_code",
    "Sexual Orientation" = "sex_orient_code",
    "Race" = "race_code",
    "Minority Statuses" = "minority_code",
    "Qualitative Data" = "qual"
  ) %>%
  mutate(Person = paste0("zero_", Person))

# EXPORT FILES AS WORD DOCUMENTS ------------------------------------------

# All minorities
for (i in 1:nrow(all_1)) {
  # Create the flextable
  ft <- all_1[i, ] %>%
    gather(key = "Variable", value = "Value") %>%
    flextable()
  
  # Fit the table to the width of the Word document
  ft <- autofit(ft)
  
  # Save as Word document
  save_as_docx(ft, path = paste0("doc/", all_1[i, ]$Person, ".docx"))
}

# Zero minorities
for (i in 1:nrow(zero_1)) {
  # Create the flextable
  ft <- zero_1[i, ] %>%
    gather(key = "Variable", value = "Value") %>%
    flextable()
  
  # Fit the table to the width of the Word document
  ft <- autofit(ft)
  
  # Save as Word document
  save_as_docx(ft, path = paste0("doc/", zero_1[i, ]$Person, ".docx"))
}

# Set dataframes up for combination
all_2 <- all_1 %>%
  mutate(Type = rep("All", nrow(all_1)))

# Combine the dataframes
combined <- zero_1 %>%
  mutate(Type = rep("Zero", nrow(.))) %>%
  rbind(all_2)

# Save to Excel file
write_excel_csv(combined, path = "data/combined_dataframes.xlsx")
