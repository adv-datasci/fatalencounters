library(dplyr)
library(rgdal)
library(Hmisc)
library(DT)

#12/4:
#TO DO:
#Edit this later to make it cleaner and easier to understand

#cleantable<-readRDS(file.path("data","clean_fatal_dataset.RDS"))

#This just cleans up the data table

allzips <- readRDS(file.path("data","raw_data","superzip.rds"))
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

#Does server or ui use allzips?
#No. But you need it to make cleantable here

####### Final data sets ################

filepath<-file.path("data","raw_data","deaths_by_county.RDS")
deaths_by_county<-readRDS(filepath)

filepath2<-file.path("data","raw_data","fataleasy.RDS")
fataleasy<-readRDS(filepath2)

fataleasy$zipcode <-as.character(fataleasy$zipcode)

cleantable <- allzips %>%
  select(
    #city = city.x,
    #state = state.x,
    zipcode = zipcode,
    lat = latitude,
    long = longitude
  )

#cleantable is used to create:
#   1) interactive map in tab 1
#   2) data explorer in tab 3
cleantable <- inner_join(cleantable,fataleasy,by = c("zipcode" = "zipcode"))

saveRDS(cleantable, file.path("data","processed_data","clean_fatal_dataset.RDS"))

temp0<-cleantable %>%
  select(name, county, state)

#We want to join based on zipcode (easiest).
#We only want the zipcodes from cleantable if they appear in fatal.
#Return all rows from zipcodes (x) where there are matching values in fatal (y)

#Can join by c("city", "state") if desired, but it might be more difficult
#This doesn't work as well...there will be multiple zipcodes for each city and state,
# and a lot of shootings will overlap (this is a problem even with zipcode)
#Just use zipcode for now.
#Can get the zipcode into dataset later

######## Add in code for county-level data

### Begin data prep
#Grab air/water quality data from the EPA
url = "https://data.cdc.gov/api/views/cjae-szjv/rows.csv?accessType=DOWNLOAD"
dat <- read.csv(url, stringsAsFactors = FALSE)

# Make colnames lowercase
names(dat) <- tolower(names(dat))

# Wide data set, subset only what we need.
county_dat <- subset(dat, measureid == "296",
                     select = c("reportyear","countyfips","statename", "countyname", "value", "unitname")) %>%
  subset(reportyear==2011, select = c("countyfips", "value","statename", "countyname"))

# Rename columns to make for a clean df merge later.
colnames(county_dat) <- c("GEOID", "airqlty","state","county")


#Replace state names with abbreviations
states<-state.abb[match(county_dat$state,state.name)]
county_dat$state<-states

# Have to add leading zeros to any FIPS code that's less than 5 digits long to get a good match.
# I'm cheating by using C code. sprintf will work as well.
county_dat$GEOID <- formatC(county_dat$GEOID, width = 5, format = "d", flag = "0")

########## Include "risk" values ############

temp1<-inner_join(deaths_by_county,temp0,by =c("name","county"))

#Get the ID for the county by
temp2<-inner_join(county_dat,temp1,by =c("state","county"))

#(name, county, state,GEOID)
temp3<- temp2 %>%
  select(GEOID,state,county,"pminusdratio")

#Set NA to 0
temp4<-left_join(county_dat,temp3,by=c("state","county")) %>%
  select("GEOID.x", "state","county","pminusdratio")

colnames(temp4)<-c("GEOID", "state","county","pminusdratio")
temp5<-unique(temp4)

temp5$pminusdratio[is.na(temp5$pminusdratio)]<-0

#saveRDS(temp5,file.path("data","county_death_data.RDS"))

county_dat<-temp5

######################

#If needed:
# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html

dsn = file.path("data","raw_data","cb_2016_us_county_20m")
us.map <- readOGR(dsn = dsn, stringsAsFactors = FALSE)

# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]

# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]

heatmap_data <- merge(us.map, county_dat, by=c("GEOID"))

saveRDS(heatmap_data,file.path("data","processed_data","heatmap_data.RDS"))
