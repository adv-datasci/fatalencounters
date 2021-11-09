### Fatal Encounters collect
### kbmorales
### kbmorales@protonmail.com


# Setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(gsheet)

# Data fetch --------------------------------------------------------------

fatal <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/edit?usp=sharing')

fatal <- fatal %>% clean_names()
