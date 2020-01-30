### Global.R
###
###

# Read in the data
source(file.path("code",
                 "final_code",
                 "fatal_encounters_clean.R"))

cleantable <-readRDS(file.path("data",
                               "processed_data",
                               "clean_fatal_dataset2.RDS")
)
