### Ken Morales
### Data prep for visualization
### kbmorales@protonmail.com

# Setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)

library(rgdal)
library(Hmisc)
library(DT)

## Read in fatal dataset

# Read in the data
# source(file.path("code",
#                  "fatal_encounters_collect.R")
# )
# 
# 
# # Clean data
# source(file.path("code",
#                  "fatal_encounters_clean.R")
# )


# State helper ------------------------------------------------------------

state_helper <- tibble(state = state.name,
                       state_ab = state.abb)

# FIPS data ---------------------------------------------------------------

## US Census FIPS codes for 2018

## Download 
# url = "https://www2.census.gov/programs-surveys/popest/geographies/2018/all-geocodes-v2018.xlsx"
# download.file(url,
#               paste0("data/",
#                      "all-geocodes-v2018.xlsx"))

## Read
geoids <- read_excel(paste0("data/",
                            "all-geocodes-v2018.xlsx"),
                     skip = 4) %>% 
  clean_names()

state_geoids <- geoids %>% 
  filter(summary_level == "040") %>% 
  select(state_code_fips,
         state = area_name_including_legal_statistical_area_description)

county_geoids <- geoids %>% 
  filter(summary_level == "050") %>% 
  mutate(GEOID = str_c(state_code_fips,
                       county_code_fips)) %>% 
  select(GEOID,
         state_code_fips,
         county = area_name_including_legal_statistical_area_description) %>% 
  left_join(state_geoids) %>% 
  select(-state_code_fips) %>% 
  mutate(county = str_remove(county,
                             " County$| Parish$| Municipality$")) %>% 
  mutate(county = str_replace(county,
                              " city$",
                              " City")) %>% 
  left_join(state_helper) %>% 
  select(GEOID, county, state = state_ab) %>% 
  mutate(state = ifelse(county == "District of Columbia",
                        "DC",
                        state))

# rm(state_geoids,
#    geoids)  

# Census Population Data --------------------------------------------------

## Source: https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html
census_pops <- read_csv(paste0("data/",
                               "cc-est2018-alldata.csv")) %>% 
  clean_names()

## Filter
census_pops <- census_pops %>% 
  select(-sumlev) %>% 
  filter(year == 1, # Only 2010 Census data
         agegrp == 0) %>%  # All ages 
  # Make GEOID col
  mutate(GEOID = str_c(state, county),
         asian_pop = nhaac_male + nhaac_female + nhnac_male + nhnac_female,
         black_pop = nhbac_male + nhbac_female,
         hispanic_pop = h_male + h_female,
         native_pop = nhiac_male + nhiac_female,
         white_pop = nhwa_male + nhwa_female # White alone
  ) %>% 
  select(GEOID,
         tot_pop,
         # state = stname,
         # county = ctyname,
         asian_pop,
         black_pop,
         hispanic_pop,
         native_pop,
         white_pop)

###
### Join
###

county_data <- county_geoids %>%
  left_join(census_pops)

# County deaths -----------------------------------------------------------

###
### County-level summary 
###

county_deaths <- fatal %>% 
  mutate(county = case_when(
    county == "Dona Ana" ~ "Doña Ana",
    county == "DC" ~ "District of Columbia",
    TRUE ~ county
  )) %>% 
  group_by(county,state,race) %>% 
  summarise(deaths = n()) %>% 
  ungroup() %>% 
  arrange(state,county) %>% 
  group_by(state, county) %>% 
  mutate(tot_deaths = sum(deaths)) %>% 
  ungroup() %>% 
  spread(race, deaths,
         sep = "_deaths_") %>% 
  clean_names()

###
### Join
###

county_deaths <- left_join(county_deaths,
                           county_data) %>% 
  ## Remove bad GEOIDs for now
  # TODO: Fix on case by case basis
  filter(!is.na(GEOID)) %>% 
  arrange(state, county) 


# Compute -----------------------------------------------------------------

death_rates <- county_deaths %>% 
  ## Compute death rates per 1000 pop
  mutate(death_rate = tot_deaths / (tot_pop/10000), ## Total for county
         # By race
         white_dr = race_deaths_white / (white_pop/10000),
         black_dr = race_deaths_black / (black_pop/10000),
         native_dr = race_deaths_native / (native_pop/10000),
         asian_dr = race_deaths_asian / (asian_pop/10000),
         hispanic_dr = race_deaths_hispanic / (hispanic_pop/10000)) %>% 
  mutate(blackrr = black_dr / white_dr,
         hisprr = hispanic_dr / white_dr,
         nativerr = native_dr / white_dr,
         asianrr = asian_dr / white_dr) %>% 
  select(GEOID, county, state, death_rate, blackrr, hisprr, nativerr, asianrr) %>% 
  arrange(desc(death_rate))

# County Shapefile --------------------------------------------------------

#If needed:
# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html

dsn = file.path("data",
                "cb_2016_us_county_20m")
us.map <- readOGR(dsn = dsn, stringsAsFactors = FALSE)

# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]

# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]

heatmap_data <- merge(us.map, 
                      death_rates, 
                      by=c("GEOID"))

# Save
saveRDS(heatmap_data,
        file.path("data",
                  "heatmap_data.RDS"))


# Review --------------------------------------------------------------------

# temp0<- fatal %>%
#   select(name, county, state)
# 
# ### Begin data prep
# #Grab air/water quality data from the EPA
# url = "https://data.cdc.gov/api/views/cjae-szjv/rows.csv?accessType=DOWNLOAD"
# dat <- read.csv(url, stringsAsFactors = FALSE)
# 
# # Make colnames lowercase
# names(dat) <- tolower(names(dat))
# dat %>% glimpse()
# 
# # Wide data set, subset only what we need.
# county_dat <- subset(dat, measureid == "296",
#                      select = c("reportyear","countyfips","statename", "countyname", "value", "unitname")) %>%
#   subset(reportyear==2011, select = c("countyfips", "value","statename", "countyname"))
# 
# # Rename columns to make for a clean df merge later.
# colnames(county_dat) <- c("GEOID", "airqlty","state","county")
# 
# 
# #Replace state names with abbreviations
# states<-state.abb[match(county_dat$state,state.name)]
# county_dat$state<-states
# 
# # Have to add leading zeros to any FIPS code that's less than 5 digits long to get a good match.
# # I'm cheating by using C code. sprintf will work as well.
# county_dat$GEOID <- formatC(county_dat$GEOID, width = 5, format = "d", flag = "0")
# 
# county_dat
# 
# ########## Include "risk" values ############
# 
# temp1<-inner_join(deaths_by_county,temp0,by =c("name","county"))
# 
# #Get the ID for the county by
# temp2<-inner_join(county_dat,temp1,by =c("state","county"))
# 
# #(name, county, state,GEOID)
# temp3<- temp2 %>%
#   select(GEOID,state,county,"pminusdratio")
# 
# 
# temp4<-left_join(county_dat,temp3,by=c("state","county")) %>%
#   select("GEOID.x", "state","county","pminusdratio")
# colnames(temp4)<-c("GEOID", "state","county","pminusdratio")
# temp5<-unique(temp4)
# 
# #Set NA to 0
# temp5$pminusdratio[is.na(temp5$pminusdratio)]<-0
# 
# county_dat<-temp5
