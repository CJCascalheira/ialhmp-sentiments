# Dependencies
library(tidyverse)
library(officer)

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

# EXPORT DOCUMENT FOR CASES -----------------------------------------------

# Set dataframes up for combination
all_2 <- all_1 %>%
  mutate(Type = rep("All", nrow(all_1)))

# Combine the dataframes
combined <- zero_1 %>%
  mutate(Type = rep("Zero", nrow(.))) %>%
  rbind(all_2)

# Save to csv file
write_csv(combined, path = "data/combined_dataframes.csv")

# EXPORT FILES AS WORD DOCUMENTS ------------------------------------------

# Set up a docx object
my_doc <- read_docx()

# Select the two columns
person_said <- combined %>%
  select(Person, `Qualitative Data`)

# Replicate each row
person_said_1 <- person_said[rep(1:nrow(person_said), each = 2), ]

# Set even rows to NA
person_said_1[1:nrow(person_said_1) %% 2 == 0, ] <- "\n\n"

# Update the docx object
my_doc <- my_doc %>%
  body_add_table(person_said_1)

# Save to Word file
print(my_doc, target = "doc/qual_responses_by_person.docx") %>% 
  invisible()
