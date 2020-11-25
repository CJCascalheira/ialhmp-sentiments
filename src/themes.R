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
saf_n <- safety %>%
  select(content) %>%
  filter(!is.na(content)) %>%
  nrow()
saf_n

# Connectedness - number of participants endorsing this theme
con_n <- connect %>%
  select(content) %>%
  filter(!is.na(content)) %>%
  nrow()
con_n

# Environmental Privilege - number of participants endorsing this theme
env_n <- environ %>%
  select(content) %>%
  filter(!is.na(content)) %>%
  nrow()
env_n

# Discrimination and Identity - number of participants endorsing this theme
ide_n <- identity %>%
  select(content) %>%
  filter(!is.na(content)) %>%
  nrow()
ide_n

# Disconnection - number of participants endorsing this theme
dis_n <- disconnect %>%
  select(content) %>%
  filter(!is.na(content)) %>%
  nrow()
dis_n

# Percentages
(saf_n / 1272) * 100
(con_n / 1272) * 100
(env_n / 1272) * 100
(ide_n / 1272) * 100
(dis_n / 1272) * 100
