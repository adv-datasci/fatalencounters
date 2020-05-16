### Prep work for Fatal Encounters data
### kbmorales
### kbmorales@protonmail.com

# fatal_encounters_collect.R should be run first

# Setup -------------------------------------------------------------------

library(tidyverse)

# Data clean --------------------------------------------------------------

## Remove final row alerting to unverified data
stop_row <- which(fatal$subjects_name=="Items below this row have not been fact-checked.")

fatal <- fatal %>% filter(row_number() < stop_row)

fatal <- fatal %>%
  select(name = subjects_name,
         url_name = url_of_image_of_deceased,
         date = date_of_injury_resulting_in_death_month_day_year,
         age = subjects_age,
         sex = subjects_gender,
         race = subjects_race_with_imputations,
         description = a_brief_description_of_the_circumstances_surrounding_the_death,
         official_description = dispositions_exclusions_internal_use_not_for_analysis,
         news_link = link_to_news_article_or_photo_of_official_document,
         address = location_of_injury_address,
         city = location_of_death_city,
         county = location_of_death_county,
         state = location_of_death_state,
         zipcode = location_of_death_zip_code,
         lat = latitude,
         lon = longitude,
         agency = agency_responsible_for_death,
         cause = cause_of_death,
         mental_ill = symptoms_of_mental_illness_internal_use_not_for_analysis,
         year = date_year
  )

# Age
unique_ages <- fatal %>% 
  mutate(age_num = as.numeric(age)) %>% 
  filter(is.na(age_num), 
         !is.na(age)) %>% 
  pull(age)

fatal <- fatal %>%
  mutate(agerng = case_when(
    str_detect(age, "mon|days") ~ "< 1 year",
    as.numeric(age) <= 4 ~ "1 - 4 years",
    between(as.numeric(age), 5, 9)  ~ "5 - 9 years",
    between(as.numeric(age), 10, 14)  ~ "10 - 14 years",
    between(as.numeric(age), 15, 19)  ~ "15 - 19 years",
    between(as.numeric(age), 20, 24)  ~ "20 - 24 years",
    between(as.numeric(age), 25, 34) |
      str_detect(age, "25`")~ "25 - 34 years",
    between(as.numeric(age), 35, 44)  ~ "35 - 44 years",
    between(as.numeric(age), 45, 54) | 
      str_detect(age, "46/53|45 or 49") ~ "45 - 54 years",
    between(as.numeric(age), 55, 64)  ~ "55 - 64 years",
    between(as.numeric(age), 65, 74)  ~ "65 - 74 years",
    between(as.numeric(age), 75, 84)  ~ "75 - 84 years",
    as.numeric(age) > 84 ~ "85+ years",
    TRUE ~ "Unknown"
  ))

fatal$age[fatal$age == 0] <- "Unknown"

# Race
unique(fatal$race)
fatal$race[is.na(fatal$race)] <- "Unknown"
fatal$race[fatal$race == "Race unspecified" | fatal$race == "Other Race"] <- "Unknown"

fatal$race[fatal$race == "Native American/Alaskan"] <- "Native"
fatal$race[fatal$race == "Middle Eastern"] <- "White"
fatal$race[fatal$race == "Hispanic/Latino" | fatal$race == "HIspanic/Latino"] <- "Hispanic"
fatal$race[fatal$race == "European-American/White" |
             fatal$race == "European American/White"] <- "White"
fatal$race[fatal$race == "Asian/Pacific Islander"] <- "Asian"
fatal$race[fatal$race == "African-American/Black"] <- "Black"

# Gender
unique(fatal$sex)
fatal$sex[fatal$sex == "Femalr"] <- "Female"
fatal$sex[fatal$sex == "Transexual"] <- "Transgender"
fatal$sex[is.na(fatal$sex)] <- "Unknown"
fatal$sex[fatal$sex == "White"] <- "Unknown"

# Mental illness
fatal$mental_ill[is.na(fatal$mental_ill)] <- "Unknown"

# Clean cause
fatal$cause[fatal$cause == "Undetermined"] <- "Other"
fatal$cause[fatal$cause == "Stabbed"] <- "Other"
fatal$cause[fatal$cause == "Fell from a height"] <- "Other"
fatal$cause[fatal$cause == "Drug overdose"] <- "Other"
fatal$cause[fatal$cause == "Drowned"] <- "Other"
fatal$cause[fatal$cause == "Chemical agent/Pepper spray"] <- "Other"
fatal$cause[fatal$cause == "Burned/Smoke inhalation"] <- "Other"
fatal$cause[fatal$cause == "Beaten/Bludgeoned with instrument"] <- "Other"
fatal$cause[fatal$cause == ""] <- "Other"
fatal$cause[fatal$cause == "Asphyxiated/Restrained"] <- "Suffocated"
fatal$cause[fatal$cause == "Suffocated"] <- "Other"
fatal$cause[fatal$cause == "Medical emergency"] <- "Other"

###
### Generate new variable for EASY - if info easily meshes with death index stats
###

fatal$easy <- T
fatal$easy[fatal$sex == "Unknown"] <- F
fatal$easy[fatal$race == "Unknown"] <- F
fatal$easy[fatal$agerng == "Unknown"] <- F

# Create new datasets for easy and not easy 
fataleasy <- subset(fatal, easy == T)
fatalnoteasy <- subset(fatal, easy == F)
