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

#iterate through dataframe
fish_data_age <- left_join(fish_data_cleaned, interval_order, by = c("max_stage" = "interval_name"))

fish_data_sorted <- arrange(fish_data_age, desc(interval_no))

#preprocess data to account for lazarus taxa
column_names <- unique(fish_data_sorted$genus)
occurrence_intervals <- array(c(1), dim = c(2, length(column_names)), dimnames = list(c(1, 2),column_names))
for (row in 1:nrow(fish_data_sorted)) {
  current_genus <- fish_data_sorted[row, 'genus']
  if (occurrence_intervals[1, current_genus] == 1) {
    occurrence_intervals[1, current_genus] <- fish_data_sorted[row, 'interval_no']
    occurrence_intervals[2, current_genus] <- fish_data_sorted[row, 'interval_no'] #to eliminate empty end interval values in the case of single occurrences
  } else occurrence_intervals[2, current_genus] <- fish_data_sorted[row, 'interval_no']
}

#calculating nBt, nBl, nB for each set of intervals using interval_orders
for(x in c(122, 121, 120, 119, 118, 117, 116, 115, 114, 113)) {
  occurrences_before <- unique(fish_data_sorted[fish_data_sorted$interval_no == x + 1,]$genus)
  occurrences_current <- unique(fish_data_sorted[fish_data_sorted$interval_no == x,]$genus)
  occurrences_after <- unique(fish_data_sorted[fish_data_sorted$interval_no == x - 1,]$genus)
  for(col in 1:ncol(occurrence_intervals)) {
    if (x + 1 >= occurrence_intervals[1, col] & x + 1 <= occurrence_intervals[2, col]) append(occurrences_before, col, after = 1)
    if (x >= occurrence_intervals[1, col] & x <= occurrence_intervals[2, col]) append(occurrences_current, col, after = 1)
    if (x - 1 >= occurrence_intervals[1, col] & x - 1 <= occurrence_intervals[2, col]) append(occurrences_after, col, after = 1)
  }
  occurrences_before <- unique(occurrences_before)
  occurrences_current <- unique(occurrences_current)
  occurrences_after <- unique(occurrences_after)
  
  nBt <- length(unique(intersect(occurrences_before, occurrences_after)))
  nBl <- length(unique(intersect(occurrences_before, occurrences_current)))
  nB <- nBt + nBl
}
