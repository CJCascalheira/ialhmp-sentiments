# Dependencies
library(tidyverse)

# Import data
all <- read_csv("data/GMH_data_all-minority.csv")
zero <- read_csv("data/GMH_data_zero-minority.csv")

# Drop first row
all_qual <- all[-1,] %>%
  select(Q24)

# Remove missing values
all_qual_1 <- all_qual %>%
  filter(!is.na(Q24))

# Drop first row
zero_qual <- zero[-1,] %>%
  select(Q24)

# Remove missing values
zero_qual_1 <- zero_qual %>%
  filter(!is.na(Q24))

# Total responses - gross
nrow(all_qual) + nrow(zero_qual)

# Total non-missing responses
nrow(all_qual_1) + nrow(zero_qual_1)

# FEATURES - ALL MINORITY -------------------------------------------------

# Average character length - all
nchar(all_qual_1) / nrow(all_qual_1)

# Min, Max character length
min(nchar(all_qual_1$Q24))
max(nchar(all_qual_1$Q24))

# Word count per response
sapply(str_split(all_qual_1$Q24, " "), length) %>%
  as_tibble() %>%
  summarize(
    min = min(value),
    max = max(value),
    mean = mean(value)
  )

# FEATURES - ZERO MINORITY ------------------------------------------------

# Average character length - all
nchar(zero_qual_1) / nrow(zero_qual_1)

# Min, Max character length
min(nchar(zero_qual_1$Q24))
max(nchar(zero_qual_1$Q24))

# Word count per response
sapply(str_split(zero_qual_1$Q24, " "), length) %>%
  as_tibble() %>%
  summarize(
    min = min(value),
    max = max(value),
    mean = mean(value)
  )
