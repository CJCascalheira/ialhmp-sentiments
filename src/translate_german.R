# Dependencies
library(tidyverse)
library(translateR)

# Import data
all <- read_csv("data/clean_all_minority.csv")
zero <- read_csv("data/clean_zero_minority.csv")

# TRANSLATION -------------------------------------------------------------

# https://cloud.google.com/translate/docs/quickstarts
# https://cran.r-project.org/web/packages/translateR/translateR.pdf

