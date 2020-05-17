### Expensive tasks to be performed prior to app launch
### kbmorales
### kbmorales@protonmail.com

# Read in the data
source(file.path("code",
                 "fatal_encounters_collect.R")
)


# Clean data
source(file.path("code",
                 "fatal_encounters_clean.R")
       )

## Read heatmap data
leafmap <-readRDS(file.path("data",
                            "processed_data",
                            "heatmap_data.RDS"))
