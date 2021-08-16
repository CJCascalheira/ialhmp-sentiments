# Dependencies
library(tidyverse)
library(tidytext)

# Import data
safety <- read_csv("data/themes_csv/safety.csv")
connect <- read_csv("data/themes_csv/connect.csv")
environ <- read_csv("data/themes_csv/environment.csv")
identity <- read_csv("data/themes_csv/identity.csv")
disconnect <- read_csv("data/themes_csv/disconnect.csv")
all <- read_csv("data/clean_all_minority.csv")
zero <- read_csv("data/clean_zero_minority.csv")
not_safe <- read_csv("data/themes_csv/subthemes/not_safe.csv")
privileged <- read_csv("data/themes_csv/subthemes/privileged.csv")
underserved <- read_csv("data/themes_csv/subthemes/underserved.csv")

# Prepare demographic data
all_zero <- bind_rows(all, zero)

# Average words
all_zero %>%
  mutate(temp_id = 1:nrow(all_zero)) %>%
  unnest_tokens(word, qual) %>%
  count(id) %>%
  summarize(
    m = mean(n),
    sd = sd(n)
  )

# FILTER FOR PARTICIPANTS -------------------------------------------------

# Safety theme
(a <- all_zero %>%
  filter(id == "R_1jAJPKIevf6L3Yt"))
a[c(5,8)]

# Safety - unsafe subtheme
(a <- all_zero %>%
    filter(id == "R_25SDnnbl0Kzdkun"))
a[8]

# Connection - proximity theme
(a <- all_zero %>%
    filter(id == "R_25WQfxfT7RnXImZ"))
a[8]

# Connection - subtheme contrast with safety
(a <- all_zero %>%
    filter(id == "R_WlpGfqbzotW5I9X"))
a[8]

# Environmental Privilege
(a <- all_zero %>%
    filter(id == "R_1ikgqFnGistaSQl"))
a[8]

# Environmental Privilege - Underserved subtheme
(a <- all_zero %>%
    filter(id == "R_wOa1YKmNZsi5uIp"))
a[8]

# Discrimination
(a <- all_zero %>%
    filter(id == "R_2eXBZZiMB0KLL6F"))
a[8]

# Identity
(a <- all_zero %>%
    filter(id == "R_3EbA0M6XRbxbuTM"))
a[8]

# Disconnection
(a <- all_zero %>%
    filter(id == "R_3JDwRXeG52QHI66"))
a[8]

# Disconnection - distance
(a <- all_zero %>%
    filter(id == "R_2PALNgs7ZhX8EjP"))
a[8]

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

# DEMOGRAPHICS OF THEMES --------------------------------------------------

#######
# SAFETY
#######

# Safety filter
safety_filter <- safety %>%
  mutate(id = str_extract(content, "R_([0-9]|[A-z])+")) %>%
  filter(!is.na(id)) %>%
  select(id)

# Prepare to explore demographics
safety_clean <- semi_join(all_zero, safety_filter) %>%
  select(gender_code, sex_orient_code, race_code, minority_code)

# Demographics of safety theme - gender
safety_clean %>%
  count(gender_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / saf_n)

# Demographics of safety theme - sexual orientation
safety_clean %>%
  count(sex_orient_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / saf_n)

# Demographics of safety theme - race
safety_clean %>%
  count(race_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / saf_n)

# Demographics of safety theme - minority status
safety_clean %>%
  count(minority_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / saf_n) %>%
  filter(!is.na(minority_code), minority_code != "No Minority Identities/None/None of Above/Blank/Priveleged Identity Listed")

# Unsafe subtheme
not_safe_filter <- gather(not_safe, key = "column", value = "content") %>%
  mutate(id = str_extract(content, "R_([0-9]|[A-z])+")) %>%
  filter(!is.na(id)) %>%
  select(id)

# Prepare to explore demographics
not_safe_clean <- semi_join(all_zero, not_safe_filter) %>%
  select(gender_code, sex_orient_code, race_code, minority_code)

# Count not_safe subtheme
nrow(not_safe_clean)
nrow(not_safe_clean) / saf_n

# Demographics of not safe theme - gender
not_safe_clean %>%
  count(gender_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / nrow(not_safe_clean))

# Demographics of not_safe theme - sexual orientation
not_safe_clean %>%
  count(sex_orient_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / nrow(not_safe_clean))

# Demographics of not_safe theme - race
not_safe_clean %>%
  count(race_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / nrow(not_safe_clean))

# Demographics of not_safe theme - minority status
not_safe_clean %>%
  count(minority_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / nrow(not_safe_clean)) %>%
  filter(!is.na(minority_code), minority_code != "No Minority Identities/None/None of Above/Blank/Priveleged Identity Listed")

#######
# CONNECTEDNESS
#######

# Connectedness filter
connect_filter <- connect %>%
  mutate(id = str_extract(content, "R_([0-9]|[A-z])+")) %>%
  filter(!is.na(id)) %>%
  select(id)

# Prepare to explore demographics
connect_clean <- semi_join(all_zero, connect_filter) %>%
  select(gender_code, sex_orient_code, race_code, minority_code)

# Demographics of connect theme - gender
connect_clean %>%
  count(gender_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / con_n)

# Demographics of connect theme - sexual orientation
connect_clean %>%
  count(sex_orient_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / con_n)

# Demographics of connect theme - race
connect_clean %>%
  count(race_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / con_n)

# Demographics of connect theme - minority status
connect_clean %>%
  count(minority_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / con_n) %>%
  filter(!is.na(minority_code), minority_code != "No Minority Identities/None/None of Above/Blank/Priveleged Identity Listed")

#######
# ENVIRONMENTAL PRIVILEGE
#######

# Environmental privilege filter
environ_filter <- environ %>%
  mutate(id = str_extract(content, "R_([0-9]|[A-z])+")) %>%
  filter(!is.na(id)) %>%
  select(id)

# Prepare to explore demographics
environ_clean <- semi_join(all_zero, environ_filter) %>%
  select(gender_code, sex_orient_code, race_code, minority_code)

# Demographics of environ theme - gender
environ_clean %>%
  count(gender_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / env_n)

# Demographics of environ theme - sexual orientation
environ_clean %>%
  count(sex_orient_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / env_n)

# Demographics of environ theme - race
environ_clean %>%
  count(race_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / env_n)

# Demographics of environ theme - minority status
environ_clean %>%
  count(minority_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / env_n) %>%
  filter(!is.na(minority_code), minority_code != "No Minority Identities/None/None of Above/Blank/Priveleged Identity Listed")

# Privileged subtheme
gather(privileged, key = "column", value = "content") %>%
  mutate(id = str_extract(content, "R_([0-9]|[A-z])+")) %>%
  filter(!is.na(id)) %>%
  select(id)

# Underserved subtheme
gather(underserved, key = "column", value = "content") %>%
  mutate(id = str_extract(content, "R_([0-9]|[A-z])+")) %>%
  filter(!is.na(id)) %>%
  select(id)

#######
# DISCRIMINATION AND IDENTITY
#######

# Discrimination and identity filter
identity_filter <- identity %>%
  mutate(id = str_extract(content, "R_([0-9]|[A-z])+")) %>%
  filter(!is.na(id)) %>%
  select(id)

# Prepare to explore demographics
identity_clean <- semi_join(all_zero, identity_filter) %>%
  select(gender_code, sex_orient_code, race_code, minority_code)

# Demographics of identity theme - gender
identity_clean %>%
  count(gender_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / ide_n)

# Demographics of identity theme - sexual orientation
identity_clean %>%
  count(sex_orient_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / ide_n)

# Demographics of identity theme - race
identity_clean %>%
  count(race_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / ide_n)

# Demographics of identity theme - minority status
identity_clean %>%
  count(minority_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / ide_n) %>%
  filter(!is.na(minority_code), minority_code != "No Minority Identities/None/None of Above/Blank/Priveleged Identity Listed")

#######
# DISCONNECTION
#######

# Disconnection filter
disconnect_filter <- disconnect %>%
  mutate(id = str_extract(content, "R_([0-9]|[A-z])+")) %>%
  filter(!is.na(id)) %>%
  select(id)

# Prepare to explore demographics
disconnect_clean <- semi_join(all_zero, disconnect_filter) %>%
  select(gender_code, sex_orient_code, race_code, minority_code)

# Demographics of disconnect theme - gender
disconnect_clean %>%
  count(gender_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / dis_n)

# Demographics of disconnect theme - sexual orientation
disconnect_clean %>%
  count(sex_orient_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / dis_n)

# Demographics of disconnect theme - race
disconnect_clean %>%
  count(race_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / dis_n)

# Demographics of disconnect theme - minority status
disconnect_clean %>%
  count(minority_code) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / dis_n) %>%
  filter(!is.na(minority_code), minority_code != "No Minority Identities/None/None of Above/Blank/Priveleged Identity Listed")
