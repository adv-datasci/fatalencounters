

# Death index?? -----------------------------------------------------------

# rename variables in death index file to match fatal encounters
deathindex <- deathindex %>%
  rename("agerng" = "Age.Group",
         "sex" = "Gender",
         "race" = "Race"
  )

# Make all Hispanic ethnicity categories in deathindex Hispanic/Latino race
i <- 1
for (i in 1:nrow(deathindex)) {
  if (str_detect(deathindex$Hispanic.Origin[i], START %R% "Hispanic or Latino") == TRUE)
    deathindex$race[i] <- "Hispanic/Latino"
}
deathindex <- subset(deathindex, select=(-Hispanic.Origin))

# Clean table -------------------------------------------------------------

# To save file
# saveRDS(cleantable, file = "data/processed_data/clean_fatal_dataset.RDS")

# Clean race
cleantable$race[cleantable$race == "Native American/Alaskan"] <- "Native"
cleantable$race[cleantable$race == "Middle Eastern"] <- "White"
cleantable$race[cleantable$race == "Hispanic/Latino"] <- "Hispanic"
cleantable$race[cleantable$race == "European-American/White"] <- "White"
cleantable$race[cleantable$race == "Asian/Pacific Islander"] <- "Asian"
cleantable$race[cleantable$race == "African-American/Black"] <- "Black"

# Clean cause
cleantable$cause[cleantable$cause == "Undetermined"] <- "Other"
cleantable$cause[cleantable$cause == "Stabbed"] <- "Other"
cleantable$cause[cleantable$cause == "Fell from a height"] <- "Other"
cleantable$cause[cleantable$cause == "Drug overdose"] <- "Other"
cleantable$cause[cleantable$cause == "Drowned"] <- "Other"
cleantable$cause[cleantable$cause == "Chemical agent/Pepper spray"] <- "Other"
cleantable$cause[cleantable$cause == "Burned/Smoke inhalation"] <- "Other"
cleantable$cause[cleantable$cause == "Beaten/Bludgeoned with instrument"] <- "Other"
cleantable$cause[cleantable$cause == ""] <- "Other"
cleantable$cause[cleantable$cause == "Asphyxiated/Restrained"] <- "Suffocated"
cleantable$cause[cleantable$cause == "Suffocated"] <- "Other"
cleantable$cause[cleantable$cause == "Medical emergency"] <- "Other"

# clean age
cleantable$agerng <- str_replace(cleantable$agerng, " years", "")
cleantable$agerng <- str_replace(cleantable$agerng, " year", "")
cleantable$agerng[cleantable$agerng == "1 - 4"] <- "01 - 04"
cleantable$agerng[cleantable$agerng == "5 - 9"] <- "05 - 09"

###
### Repeat for not ez dataset
###

notezcleantable$race[notezcleantable$race == "Native American/Alaskan"] <- "Native"
notezcleantable$race[notezcleantable$race == "Middle Eastern"] <- "White"
notezcleantable$race[notezcleantable$race == "Hispanic/Latino"] <- "Hispanic"
notezcleantable$race[notezcleantable$race == "European-American/White"] <- "White"
notezcleantable$race[notezcleantable$race == "Asian/Pacific Islander"] <- "Asian"
notezcleantable$race[notezcleantable$race == "African-American/Black"] <- "Black"

# Clean cause
notezcleantable$cause[notezcleantable$cause == "Undetermined"] <- "Other"
notezcleantable$cause[notezcleantable$cause == "Stabbed"] <- "Other"
notezcleantable$cause[notezcleantable$cause == "Fell from a height"] <- "Other"
notezcleantable$cause[notezcleantable$cause == "Drug overdose"] <- "Other"
notezcleantable$cause[notezcleantable$cause == "Drowned"] <- "Other"
notezcleantable$cause[notezcleantable$cause == "Chemical agent/Pepper spray"] <- "Other"
notezcleantable$cause[notezcleantable$cause == "Burned/Smoke inhalation"] <- "Other"
notezcleantable$cause[notezcleantable$cause == "Beaten/Bludgeoned with instrument"] <- "Other"
notezcleantable$cause[notezcleantable$cause == ""] <- "Other"
notezcleantable$cause[notezcleantable$cause == "Asphyxiated/Restrained"] <- "Suffocated"
notezcleantable$cause[notezcleantable$cause == "Suffocated"] <- "Other"
notezcleantable$cause[notezcleantable$cause == "Medical emergency"] <- "Other"

# clean age
notezcleantable$agerng <- str_replace(notezcleantable$agerng, " years", "")
notezcleantable$agerng <- str_replace(notezcleantable$agerng, " year", "")
notezcleantable$agerng[notezcleantable$agerng == "1 - 4"] <- "01 - 04"
notezcleantable$agerng[notezcleantable$agerng == "5 - 9"] <- "05 - 09"

# try setting unknown = NA
notezcleantable$sex[notezcleantable$sex == "Unknown"] <- NA
notezcleantable$race[notezcleantable$race == "Unknown"] <- NA
notezcleantable$agerng[notezcleantable$agerng == "Unknown"] <- NA

### Join EZ and not EZ tables
cleantable2 <- rbind(cleantable, notezcleantable, stringsAsFactors = FALSE)

# SAVE as separate dataset 

saveRDS(cleantable2, file = "data/processed_data/clean_fatal_dataset2.RDS")



# more cleaning
racepops$race[racepops$race == "Black or African-American"] <- "Black"