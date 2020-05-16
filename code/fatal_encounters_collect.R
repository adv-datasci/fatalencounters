### Fatal Encounters collect
### kbmorales
### kbmorales@protonmail.com


# Setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(googlesheets)

# Data fetch --------------------------------------------------------------

## Connect to Fatal Encounters dataset
fatal_encounters_url <- gs_url("https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/edit?usp=sharing")

# Download data
fatal <- gs_read(fatal_encounters_url) %>%
  clean_names()

# Load fatal encounters database

# fatal <- read.csv("~/adsterm2/Term2Proj/data/fatal.csv",
#                   stringsAsFactors = FALSE)