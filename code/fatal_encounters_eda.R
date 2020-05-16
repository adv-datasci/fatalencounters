# kbmorales
# kbmorales@protonmail.com

# Run collect and clean scripts first

library(tidyverse)
library(ggplot2)

# EDA ---------------------------------------------------------------------

# Age
ggplot(data = fatal, aes(x = agerng)) + geom_bar() + coord_flip()

# Race
ggplot(data = fatal, aes(x = race)) + geom_bar() + coord_flip()

# Gender
ggplot(data = fatal, aes(x = sex)) + geom_bar()

# So called mental illness
ggplot(fatal, aes(x = mental_ill)) + geom_bar()

# Easy?
ggplot(data = fatal, aes(x = easy)) + geom_bar()
