#read single_stage.R
#this function will clean up the PBDB data
#selects collections that are assigned to a single geological time interval
#load dplyr package
library(dplyr)
library(ggplot2)

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

#load in time csv
interval_order <- read.csv("https://paleobiodb.org/data1.2/intervals/list.txt?scale=1&max_ma=145&min_ma=66&scale_level=5")

#iterate through dataframe
fish_data_age <- left_join(fish_data_cleaned, interval_order, by = c("max_stage" = "interval_name"))
invert_data_age <- left_join(invert_data_cleaned, interval_order, by = c("max_stage" = "interval_name"))

fish_data_sorted <- arrange(fish_data_age, desc(interval_no))
invert_data_sorted <- arrange(invert_data_age, desc(interval_no))

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
counter <- 1
fish_nB <- numeric(0)
fish_nBt <- numeric(0)
fish_nBl <- numeric(0)
fish_likelihoods <- numeric(0)
for(x in c(122, 121, 120, 119, 118, 117, 116, 115, 114, 113)) {
  occurrences_before <- unique(fish_data_sorted[fish_data_sorted$interval_no == x + 1,]$genus)
  occurrences_current <- unique(fish_data_sorted[fish_data_sorted$interval_no == x,]$genus)
  occurrences_after <- unique(fish_data_sorted[fish_data_sorted$interval_no == x - 1,]$genus)
  for(col in 1:ncol(occurrence_intervals)) {
    if (x + 1 <= occurrence_intervals[1, col] & x + 1 >= occurrence_intervals[2, col]) occurrences_before <- append(occurrences_before, colnames(occurrence_intervals)[col], after = length(occurrences_before))
    if (x <= occurrence_intervals[1, col] & x >= occurrence_intervals[2, col]) occurrences_current <- append(occurrences_current, colnames(occurrence_intervals)[col], after = length(occurrences_current))
    if (x - 1 <= occurrence_intervals[1, col] & x - 1 >= occurrence_intervals[2, col]) occurrences_after <- append(occurrences_after, colnames(occurrence_intervals)[col], after = length(occurrences_after))
  }
  occurrences_before <- unique(occurrences_before)
  occurrences_current <- unique(occurrences_current)
  occurrences_after <- unique(occurrences_after)
  
  nBt <- length(unique(intersect(occurrences_before, occurrences_after)))
  nB <- length(unique(intersect(occurrences_before, occurrences_current)))
  nBl <- nB - nBt
  
  fish_nB[counter] <- nB
  fish_nBt[counter] <- nBt
  fish_nBl[counter] <- nBl
  
  fish_likelihoods[counter] <- nBt * log(nBt / nB) + nBl * log(nBl / nB)
  counter <- counter + 1
}

column_names <- unique(invert_data_sorted$genus)
occurrence_intervals <- array(c(1), dim = c(2, length(column_names)), dimnames = list(c(1, 2),column_names))
for (row in 1:nrow(invert_data_sorted)) {
  current_genus <- invert_data_sorted[row, 'genus']
  if (occurrence_intervals[1, current_genus] == 1) {
    occurrence_intervals[1, current_genus] <- invert_data_sorted[row, 'interval_no']
    occurrence_intervals[2, current_genus] <- invert_data_sorted[row, 'interval_no'] #to eliminate empty end interval values in the case of single occurrences
  } else occurrence_intervals[2, current_genus] <- invert_data_sorted[row, 'interval_no']
}

#calculating nBt, nBl, nB for each set of intervals using interval_orders
counter <- 1
invert_nB <- numeric(0)
invert_nBt <- numeric(0)
invert_likelihoods <- numeric(0)
both_likelihood <- numeric(0)
for(x in c(122, 121, 120, 119, 118, 117, 116, 115, 114, 113)) {
  occurrences_before <- unique(invert_data_sorted[invert_data_sorted$interval_no == x + 1,]$genus)
  occurrences_current <- unique(invert_data_sorted[invert_data_sorted$interval_no == x,]$genus)
  occurrences_after <- unique(invert_data_sorted[invert_data_sorted$interval_no == x - 1,]$genus)
  for(col in 1:ncol(occurrence_intervals)) {
    if (x + 1 <= occurrence_intervals[1, col] & x + 1 >= occurrence_intervals[2, col]) occurrences_before <- append(occurrences_before, colnames(occurrence_intervals)[col], after = length(occurrences_before))
    if (x <= occurrence_intervals[1, col] & x >= occurrence_intervals[2, col]) occurrences_current <- append(occurrences_current, colnames(occurrence_intervals)[col], after = length(occurrences_current))
    if (x - 1 <= occurrence_intervals[1, col] & x - 1 >= occurrence_intervals[2, col]) occurrences_after <- append(occurrences_after, colnames(occurrence_intervals)[col], after = length(occurrences_after))
  }
  occurrences_before <- unique(occurrences_before)
  occurrences_current <- unique(occurrences_current)
  occurrences_after <- unique(occurrences_after)
  
  nBt <- length(unique(intersect(occurrences_before, occurrences_after)))
  nB <- length(unique(intersect(occurrences_before, occurrences_current)))
  nBl <- nB - nBt
  
  invert_nB[counter] <- nB
  invert_nBt[counter] <- nBt

  
  invert_likelihoods[counter] <- nBt * log(nBt / nB) + nBl * log(nBl / nB)
  both_likelihood[counter] <- (nBt + fish_nBt[counter]) * log((nBt + fish_nBt[counter]) / (nB + fish_nB[counter])) + (nBl + fish_nBl[counter]) * log((nBl + fish_nBl[counter]) / ((nB + fish_nB[counter]) + fish_nB[counter]))

  counter <- counter + 1
}
#CODE FOR AIC CALCULATIONS

#Small sample-corrected Akaike Information Criterion calculation - value adjusted for complexity of particular model
aicc <- function(ml,K,n) {	-2 * ml + 2 * K + (2 * K * (K + 1)) / (n - K - 1) }

#Akaike weights calculation - converts value to 0 - 1 scale
aicw <- function(x) { exp(-0.5 * x) / sum(exp(-0.5 * x)) }

#need the vector of likelihoods calculated from the summed counts
#and the vectors of nB for fish and inverts
onerate_aic <- aicc(both_likelihood, 1, fish_nB + invert_nB)

#need one vector of fish likelihoods and one vector of invert likelihoods
#and the vectors of nB for fish and inverts
tworate_aic <- aicc(fish_likelihoods + invert_likelihoods, 2, fish_nB + invert_nB)

#Akaike weights
akaike_weight <- apply(data.frame(onerate_aic, tworate_aic), 1, aicw)['tworate_aic', ]

fish_extinction_rate <- -log(fish_nBt / fish_nB)
invert_extinction_rate <- -log(invert_nBt / invert_nB)

extinction <- data.frame(rate = c(fish_extinction_rate, invert_extinction_rate), 
                         time = rev(rep(interval_order$min_ma[2 : (nrow(interval_order) - 1)], 2)), 
                         type = rep(c('fish', 'invert'), each = 10),
                         weight = akaike_weight)
interval_order$midpoint <- rowMeans(subset(interval_order, select = c(min_ma, max_ma)))

ggplot(extinction) +
  geom_vline(xintercept = 93.9) +
  geom_rect(data = interval_order, aes(xmin = max_ma, xmax = min_ma, ymin = -0.2, ymax = -0.1),
            color="black", fill = interval_order$color) +
  geom_text(data = interval_order, aes(x = midpoint, y = -0.15, label = strtrim(interval_name, 2))) +
  geom_line(data = extinction, aes(x = time, y = rate, group = type)) +
  geom_point(data = filter(extinction, type == "fish"), aes(x = time, y = rate, fill = weight), shape=21, size=3) +
  scale_x_reverse() +
  scale_fill_gradient2(low = "#5e3c99", mid = "white", high = "#e66101") +
  xlab("Age (Ma)") + ylab("Extinction rate") +
  theme_classic() +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size=17))
