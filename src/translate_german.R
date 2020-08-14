# Dependencies
library(tidyverse)
library(translateR)

# Import data
all <- read_csv("data/clean_all_minority.csv")
zero <- read_csv("data/clean_zero_minority.csv")

# TRANSLATION -------------------------------------------------------------

# https://cloud.google.com/translate/docs/quickstarts
# https://cran.r-project.org/web/packages/translateR/translateR.pdf

# Language codes
getGoogleLanguages()

# German for all minority
all_german <- all %>% 
  filter(language == "DE")

# German for zero minority
zero_german <- zero %>%
  filter(language == "DE")

# Translate all minority
all_translated <- translate(dataset = all_german, content.field = "qual", 
                            google.api.key = Sys.getenv("TRANSLATE_GOOGLE"),
                            source.lang = "de", target.lang = "en")

# Recreate qual column
all_translated_1 <- all_translated %>%
  select(-qual) %>%
  rename("qual" = "translatedContent")

# Translate zero minority
zero_translated <- translate(dataset = zero_german, content.field = "qual", 
                            google.api.key = Sys.getenv("TRANSLATE_GOOGLE"),
                            source.lang = "de", target.lang = "en")

# Recreate qual column
zero_translated_1 <- zero_translated %>%
  select(-qual) %>%
  rename("qual" = "translatedContent")

# Combine the translated content with the original data frame
all_trans <- all %>%
  filter(language != "DE") %>%
  rbind(all_translated_1)

# Repeat for zero minority
zero_trans <- zero %>%
  filter(language != "DE") %>%
  rbind(zero_translated_1)

# EXPORT TRANSLATED DATAFRAMES --------------------------------------------

# Translated all minority set
write_csv(all_trans, path = "data/translated_all_minority.csv")

# Translated zero minority set
write_csv(zero_trans, path = "data/translated_zero_minority.csv")
