# Dependencies
library(tidyverse)

# Import data
safety <- read_csv("data/themes_csv/safety.csv")
connect <- read_csv("data/themes_csv/connect.csv")
environ <- read_csv("data/themes_csv/environment.csv")
identity <- read_csv("data/themes_csv/identity.csv")
disconnect <- read_csv("data/themes_csv/disconnect.csv")

# PARTICIPANTS PER THEME --------------------------------------------------

# Safety - number of participants endorsing this theme
safety %>%
  select(content) %>%
  filter(!is.na(content)) %>%
  nrow()

# Connectedness - number of participants endorsing this theme
connect %>%
  select(content) %>%
  filter(!is.na(content)) %>%
  nrow()

# Environmental Privilege - number of participants endorsing this theme
environ %>%
  select(content) %>%
  filter(!is.na(content)) %>%
  nrow()

# Discrimination and Identity - number of participants endorsing this theme
identity %>%
  select(content) %>%
  filter(!is.na(content)) %>%
  nrow()

# Disconnection - number of participants endorsing this theme
disconnect %>%
  select(content) %>%
  filter(!is.na(content)) %>%
  nrow()