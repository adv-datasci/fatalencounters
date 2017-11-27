library(shiny)
library(rmarkdown)
library(caret)
library(rsconnect)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(rebus)

# Load fatal encounters database

fatal <- read.csv("~/adsterm2/Term2Proj/data/fatal.csv", stringsAsFactors = FALSE)
fatal <- fatal[-nrow(fatal)]

# Rename variables fatal encounters dataset to match Linda

fatal <- fatal %>%
  rename("name"="Subject.s.name",
         "age" = "Subject.s.age",
         "sex" = "Subject.s.gender",
         "race" = "Subject.s.race",
         "date" = "Date.of.injury.resulting.in.death..month.day.year.",
         "address" = "Location.of.injury..address.",
         "city" = "Location.of.death..city.",
         "state" = "Location.of.death..state.",
         "zipcode" = "Location.of.death..zip.code.",
         "county" = "Location.of.death..county.",
         "agency" = "Agency.responsible.for.death",
         "cause" = "Cause.of.death",
         "mental_ill" = "Symptoms.of.mental.illness.",
         "year" = "Date..Year."
  )

# rename variables in death index file to match fatal encounters
deathindex <- deathindex %>%
  rename("agerng" = "Age.Group",
         "sex" = "Gender",
         "race" = "Race"
  )

##
## EDA
##

# Age
uniqueages <- unique(fatal$age)
fatal$agerng <- as.character(fatal$age)
i <- 1
for (i in 1:nrow(fatal)) {
  if (str_detect(fatal$agerng[i], "month|days") == TRUE)
    fatal$agerng[i] <- "< 1 year"
  if (str_detect(fatal$agerng[i], START %R% number_range(1, 4) %R% END) == TRUE)
    fatal$agerng[i] <- "1 - 4 years"
  if (str_detect(fatal$agerng[i],  START %R% number_range(5, 9) %R% END) == TRUE)
    fatal$agerng[i] <- "5 - 9 years"
  if (str_detect(fatal$agerng[i], "10|11|12|13|14") == TRUE)
    fatal$agerng[i] <- "10 - 14 years"
  if (str_detect(fatal$agerng[i], "15|16|17|18|19") == TRUE)
    fatal$agerng[i] <- "15 - 19 years"
  if (str_detect(fatal$agerng[i], "20|21|22|23|24") == TRUE)
    fatal$agerng[i] <- "20 - 24 years"
  if (str_detect(fatal$agerng[i], "25|26|27|28|29|30|31|32|33|34") == TRUE)
    fatal$agerng[i] <- "25 - 34 years"
  if (str_detect(fatal$agerng[i], "35|36|37|38|39|40|41|42|43|44") == TRUE)
    fatal$agerng[i] <- "35 - 44 years"
  if (str_detect(fatal$agerng[i], "45|46|47|48|49|50|51|52|53|54|46/53|45 or 49") == TRUE)
    fatal$agerng[i] <- "45 - 54 years"
  if (str_detect(fatal$agerng[i], "55|56|57|58|59|60|61|62|63|64") == TRUE)
    fatal$agerng[i] <- "55 - 64 years"
  if (str_detect(fatal$agerng[i], "65|66|67|68|69|70|71|72|73|74") == TRUE)
    fatal$agerng[i] <- "65 - 74 years"
  if (str_detect(fatal$agerng[i], "75|76|77|78|79|80|81|82|83|84") == TRUE)
    fatal$agerng[i] <- "75 - 84 years"
  if (str_detect(fatal$agerng[i], "85|86|87|88|89|90|91|92|93|95|97|101|107") == TRUE)
    fatal$agerng[i] <- "85+ years"
}

fatal$age[fatal$age == ""] <- "Unknown"
fatal$agerng[fatal$agerng == ""] <- "Unknown"
ggplot(data = fatal, aes(x = agerng)) + geom_bar() + coord_flip()

#
# Race
#

fatal$race[fatal$race == ""] <- "Unknown"
fatal$race[fatal$race == "Race unspecified"] <- "Unknown"
ggplot(data = fatal, aes(x = race)) + geom_bar() + coord_flip()

# Make all Hispanic ethnicity categories in deathindex Hispanic/Latino race
i <- 1
for (i in 1:nrow(deathindex)) {
  if (str_detect(deathindex$Hispanic.Origin[i], START %R% "Hispanic or Latino") == TRUE)
    deathindex$race[i] <- "Hispanic/Latino"
}
deathindex <- subset(deathindex, select=(-Hispanic.Origin))

# Going to have to impute for unknown race based off demographics we have?

#
# Gender
#

fatal$sex[fatal$sex == "Femalr"] <- "Female"
fatal$sex[fatal$sex == ""] <- "Unknown"
ggplot(data = fatal, aes(x = sex)) + geom_bar()

# So called mental illness

fatal$mental_ill[fatal$mental_ill == ""] <- "Unknown"
ggplot(fatal, aes(x = mental_ill)) + geom_bar()

###
### Generate new variable for EASY - if info easily meshes with death index stats
###

fatal$easy <- 1
fatal$easy[fatal$sex == "Unknown"] <- 0
fatal$easy[fatal$race == "Unknown"] <- 0
fatal$easy[fatal$agerng == "Unknown"] <- 0
ggplot(data = fatal, aes(x = easy)) + geom_bar()

# Create new datasets for easy and not easy 

fataleasy <- subset(fatal, easy == 1)
fatalnoteasy <- subset(fatal, easy == 0)
