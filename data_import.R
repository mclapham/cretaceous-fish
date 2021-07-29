#read single_stage.R
#this function will clean up the PBDB data
#selects collections that are assigned to a single geological time interval
source("https://raw.githubusercontent.com/mclapham/PBDB-R-scripts/master/single_stage.R")

#read fish data from PBDB
#Cretaceous bony fishes from marine environments
#excludes records where genus name is uncertain
#bony fishes
fish_data_raw <- read.csv("https://paleobiodb.org/data1.2/occs/list.txt?base_name=Actinopterygii&envtype=marine&interval=Cretaceous&idreso=genus&idqual=genus_certain&show=class")
#all invertebrates (this will be a big file, so will take a while)
invert_data_raw <- read.csv("https://paleobiodb.org/data1.2/occs/list.txt?base_name=Mollusca,Brachiopoda,Cnidaria,Porifera,Echinodermata,Bryozoa,Ostracoda,Malacostraca&envtype=marine&interval=Cretaceous&idreso=genus&idqual=genus_certain&show=class")

#cleans raw data file
fish_data_cleaned <- single.stage(fish_data_raw)
invert_data_cleaned <- single.stage(invert_data_raw)
