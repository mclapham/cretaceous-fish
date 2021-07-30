#read single_stage.R
#this function will clean up the PBDB data
#selects collections that are assigned to a single geological time interval
#load dplyr package
library(dplyr)
source("https://raw.githubusercontent.com/mclapham/PBDB-R-scripts/master/single_stage.R")

#read fish data from PBDB
#Cretaceous bony fishes from marine environments
#excludes records where genus name is uncertain
#bony fishes
fish_data_raw <- read.csv("https://paleobiodb.org/data1.2/occs/list.txt?base_name=Actinopterygii&envtype=marine&interval=Cretaceous&idreso=genus&idqual=genus_certain&show=class")
#all invertebrates (this will be a big file, so will take a while)
#invert_data_raw <- read.csv("https://paleobiodb.org/data1.2/occs/list.txt?base_name=Mollusca,Brachiopoda,Cnidaria,Porifera,Echinodermata,Bryozoa,Ostracoda,Malacostraca&envtype=marine&interval=Cretaceous&idreso=genus&idqual=genus_certain&show=class")

#cleans raw data file
fish_data_cleaned <- single.stage(fish_data_raw)
#invert_data_cleaned <- single.stage(invert_data_raw)

#load in time csv
interval_order <- read.csv("https://paleobiodb.org/data1.2/intervals/list.txt?scale=1&max_ma=145&min_ma=66&scale_level=5")

#sort by interval
fish_data_age <- left_join(fish_data_cleaned, interval_order, by = c("max_stage" = "interval_name"))
fish_data_sorted <- arrange(fish_data_age, desc(interval_no))
