# Dependencies
library(tidyverse)
library(translateR)

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

# CLEAN & RECODE  ---------------------------------------------------------

# Names of variables - all minority
all_names <- c("id", "language", "country", "country_other", "country_code", 
               "age", "gender", "gender_other", "gender_code", "sex_orient", 
               "sex_orient_other", "sex_orient_code", "race", "Q8_9_TEXT", 
               "Q8GER", "Q54", "Q54_11_TEXT", "race_code", "loc", "loc_code", 
               "ethnicity", "ethnicity_other", "ethnicity_code", "minority", 
               "minority_other", "minority_code", "qual_life", "resources", "qual")

# Names of variables - zero minority
zero_names <- c("id", "language", "country", "country_other", "country_code", "age", "gender", 
  "gender_other", "gender_code", "sex_orient", "sex_orient_other", "sex_orient_code",
  "race", "Q8_9_TEXT", "Q8GER", "Q54", "Q54_11_TEXT", "race_code", "ethnicity", 
  "ethnicity_other", "ethnicity_code", "minority", "minority_other", "minority_code", 
  "qual_life", "resources", "qual")

# Set names of minority responses
all_1 <- all[-1, -c(1, 3)] %>%
  select(-c("MIDTOT", "X32", "X33"))
names(all_1) <- all_names

# Set names of zero minority responses
zero_1 <- zero[-1, -2]
names(zero_1) <- zero_names

# Clean set of all minority responses
all_minority <- all_1 %>%
  # Remove missing qualitative data
  filter(!is.na(qual)) %>%
  # Remove useless variables
  select(-starts_with(c("Q8", "Q54")), 
         -c("country", "country_other", "gender", "gender_other", "sex_orient", 
            "sex_orient_other", "race", "ethnicity", "ethnicity_other", "ethnicity_code",
            "minority", "minority_other", "loc", "loc_code")) %>%
  # Recode values
  within(., {
    # Country of residence
    country_code <- recode(country_code, `1` = "United States", `2` =	"Germany", `3` = "Other", `4` = "Canada", `5` = "United Kingdom", `6` = "United Arab Emerits", `7` = "Bulgaria", `8` = "Sweeden", `9` = "Greece", `10` = "Bermuda", `11` = "Switzerland", `12` = "Netherlands", `13` = "South Africa", `14` = "France", `15` = "Russia", `16` = "Malta", `17` = "Croatia", `18` = "China", `19` = "Romania", `20` = "Italy", `21` = "Spain", `22` = "Nigeria", `23` = "Iran", `24` = "Finland", `25` = "Austrailia", `26` = "Poland", `27` = "Laos", `28` = "Norway", `29` = "Brazil", `30` = "Israel", `31` = "Japan", `32` = "Turkey", `33` = "South Korea", `34` = "Jordan", `35` = "Taiwan", `36` = "Philippines", `37` = "Denmark", `38` = "Czech Republic", `39` = "Hungary", `40` = "Malaysia", `41` = "New Zealand", `42` = "Austria", `43` = "Ukraine", `44` = "Portugal", `45` = "Luxemburg")
    # Gender
    gender_code <- recode(gender_code, `1` = "Cisgender Man", `2` = "Cisgender Woman", `3` = "Transgender Man", `4` = "Transgender Woman", `5` = "Nonbinary", `6` = "Genderqueer", `7` = "Self-ID with No Write In", `8` = "Female/Woman", `9` = "Male/Man", `10` = "He/Him, They,Them", `11` = "No Response (Left Blank)", `12` = "Androgyne", `13` = "Bigender", `14` = "Agender", `15` = "Genderfluid", `16` = "Questioning")
    # Sexual orientation
    sex_orient_code <- recode(sex_orient_code, `1` = "Bisexual", `2` = "Lesbian", `3` = "Gay", `4` = "Heterosexual", `5` = "Panromantic/Pansexual", `6` = "Asexual", `7` = "Queer", `8` = "Self-ID with No Write In", `9` = "Straight", `10` = "Questioning", `11` = "Demisexual", `12` = "Fluid", `13` = "Autochrissexual", `14` = "No Response (Left Blank)", `15` = "Homoromantic")
    # Racial category
    race_code <- recode(race_code, `1` = "Asian",`2` = "Black/African", `3` = "Multi-Racial", `4` = "Latina/o/x", `5` = "White/European", `6` = "Native Hawaiian/Pacific Islander", `7` = "Middle Eastern", `8` = "American Indian/Alaskan Native", `9` = "Self-ID with No Write In", `10` = "Deutsch", `11` = "Deutsch mit Asian Background", `12` = "Deutsch mit Middle Eastern Background", `13` = "Deutsch mit Turkish Background", `14` = "Deutsch mit Eastern Asian Background", `15` = "Deutsch mit South East European Background", `16` = "Deutsch mit Italian Background", `17` = "Deutsch mit Greek Background", `18` = "Deutsch mit North African Background", `19` = "Deutsch mit Latin American Background", `20` = "No Response (Left Blank)", `21` = "Deutsch mit Multiple Backgrounds")
    # Other minority status
    minority_code <- recode(minority_code, `1` = "Larger Bodied", `2` = "Physically Disabled", `3` = "Unemployed", `4` = "Intellectually Disabled", `5` = "Low Resourced", `6` = "Emotionally Disabled", `7` = "Homeless", `8` = "Religious Minority", `9` = "Mentally Unwell", `10` = "Language Minority", `11` = "Non-resident/Undocumented", `12` = "Immigrant", `13` = "Self-ID with No Write In", `14` = "No Minority Identities/None/None of Above/Blank/Priveleged Identity Listed", `15` = "Short", `16` = "Not Actively Religious/Athiest", `17` = "Stressed/PTSD/Anxiety/Depression/Self-Critical", `18` = "Diabetes", `19` = "Addiction/In Recovery", `20` = "Physically Unwell", `21` = "International Student", `22` = "2nd Generation Immigrant", `23` = "Older Adult/Older", `24` = "Disordered Eating Patterns", `25` = "Chronic Pain/Illness", `26` = "Body Image Issues", `27` = "First Generation College Student", `28` = "Invisible Illness", `29` = "TBI Survivor", `30` = "Single Parent", `31` = "Furloughed", `32` = "Alone")
  })

# Clean set of no minority responses
zero_minority <- zero_1 %>%
  # Remove missing qualitative data
  filter(!is.na(qual)) %>%
  # Remove useless variables
  select(-starts_with(c("Q8", "Q54")), 
         -c("country", "country_other", "gender", "gender_other", "sex_orient", 
            "sex_orient_other", "race", "ethnicity", "ethnicity_other", "ethnicity_code",
            "minority", "minority_other")) %>%
  # Recode values
  within(., {
    # Country of residence
    country_code <- recode(country_code, `1` = "United States", `2` =	"Germany", `3` = "Other", `4` = "Canada", `5` = "United Kingdom", `6` = "United Arab Emerits", `7` = "Bulgaria", `8` = "Sweeden", `9` = "Greece", `10` = "Bermuda", `11` = "Switzerland", `12` = "Netherlands", `13` = "South Africa", `14` = "France", `15` = "Russia", `16` = "Malta", `17` = "Croatia", `18` = "China", `19` = "Romania", `20` = "Italy", `21` = "Spain", `22` = "Nigeria", `23` = "Iran", `24` = "Finland", `25` = "Austrailia", `26` = "Poland", `27` = "Laos", `28` = "Norway", `29` = "Brazil", `30` = "Israel", `31` = "Japan", `32` = "Turkey", `33` = "South Korea", `34` = "Jordan", `35` = "Taiwan", `36` = "Philippines", `37` = "Denmark", `38` = "Czech Republic", `39` = "Hungary", `40` = "Malaysia", `41` = "New Zealand", `42` = "Austria", `43` = "Ukraine", `44` = "Portugal", `45` = "Luxemburg")
    # Gender
    gender_code <- recode(gender_code, `1` = "Cisgender Man", `2` = "Cisgender Woman", `3` = "Transgender Man", `4` = "Transgender Woman", `5` = "Nonbinary", `6` = "Genderqueer", `7` = "Self-ID with No Write In", `8` = "Female/Woman", `9` = "Male/Man", `10` = "He/Him, They,Them", `11` = "No Response (Left Blank)", `12` = "Androgyne", `13` = "Bigender", `14` = "Agender", `15` = "Genderfluid", `16` = "Questioning")
    # Sexual orientation
    sex_orient_code <- recode(sex_orient_code, `1` = "Bisexual", `2` = "Lesbian", `3` = "Gay", `4` = "Heterosexual", `5` = "Panromantic/Pansexual", `6` = "Asexual", `7` = "Queer", `8` = "Self-ID with No Write In", `9` = "Straight", `10` = "Questioning", `11` = "Demisexual", `12` = "Fluid", `13` = "Autochrissexual", `14` = "No Response (Left Blank)", `15` = "Homoromantic")
    # Racial category
    race_code <- recode(race_code, `1` = "Asian",`2` = "Black/African", `3` = "Multi-Racial", `4` = "Latina/o/x", `5` = "White/European", `6` = "Native Hawaiian/Pacific Islander", `7` = "Middle Eastern", `8` = "American Indian/Alaskan Native", `9` = "Self-ID with No Write In", `10` = "Deutsch", `11` = "Deutsch mit Asian Background", `12` = "Deutsch mit Middle Eastern Background", `13` = "Deutsch mit Turkish Background", `14` = "Deutsch mit Eastern Asian Background", `15` = "Deutsch mit South East European Background", `16` = "Deutsch mit Italian Background", `17` = "Deutsch mit Greek Background", `18` = "Deutsch mit North African Background", `19` = "Deutsch mit Latin American Background", `20` = "No Response (Left Blank)", `21` = "Deutsch mit Multiple Backgrounds")
    # Other minority status
    minority_code <- recode(minority_code, `1` = "Larger Bodied", `2` = "Physically Disabled", `3` = "Unemployed", `4` = "Intellectually Disabled", `5` = "Low Resourced", `6` = "Emotionally Disabled", `7` = "Homeless", `8` = "Religious Minority", `9` = "Mentally Unwell", `10` = "Language Minority", `11` = "Non-resident/Undocumented", `12` = "Immigrant", `13` = "Self-ID with No Write In", `14` = "No Minority Identities/None/None of Above/Blank/Priveleged Identity Listed", `15` = "Short", `16` = "Not Actively Religious/Athiest", `17` = "Stressed/PTSD/Anxiety/Depression/Self-Critical", `18` = "Diabetes", `19` = "Addiction/In Recovery", `20` = "Physically Unwell", `21` = "International Student", `22` = "2nd Generation Immigrant", `23` = "Older Adult/Older", `24` = "Disordered Eating Patterns", `25` = "Chronic Pain/Illness", `26` = "Body Image Issues", `27` = "First Generation College Student", `28` = "Invisible Illness", `29` = "TBI Survivor", `30` = "Single Parent", `31` = "Furloughed", `32` = "Alone")
  })

# EXPORT DATA -------------------------------------------------------------

# Cleaned minority set (i.e., no missing values)
write_csv(all_minority, path = "data/clean_all_minority.csv")

# Cleaned zero minority set (i.e., no missing values)
write_csv(zero_minority, path = "data/clean_zero_minority.csv")
