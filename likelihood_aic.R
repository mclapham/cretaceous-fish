#function to calculate likelihood
likelihood.calc <- function(occurrences, x) {

  occurrences_before <- unique(occurrences$genus[occurrences$interval_no >= x + 1])
  occurrences_current <- unique(occurrences$genus[occurrences$interval_no == x])
  occurrences_after <- unique(occurrences$genus[occurrences$interval_no <= x - 1])

  nBt <- length(unique(occurrences_before[occurrences_before %in% occurrences_after]))
  nBl <- length(unique(occurrences_before[occurrences_before %in% occurrences_current & !occurrences_before %in% occurrences_after]))
  nB <- nBt + nBl
  
  likelihood <- nBt * log(nBt / nB) + nBl * log(nBl / nB)
  
  as.data.frame(likelihood)
}

#group by time interval and run function on each
fish_data_sorted %>% 
  group_by(interval_no) %>% 
  do(likelihood.calc(fish_data_sorted, .$interval_no))

#can then run the function again on the invertebrate dataset, for example
invert_data_sorted %>% 
  group_by(interval_no) %>% 
  do(likelihood.calc(invert_data_sorted, .$interval_no))


#CODE FOR AIC CALCULATIONS

#Small sample-corrected Akaike Information Criterion calculation
aicc <- function(ml,K,n) {	-2 * ml + 2 * K + (2 * K * (K + 1)) / (n - K - 1) }

#Akaike weights calculation
aicw <- function(x) { exp(-0.5 * x) / sum(exp(-0.5 * x)) }

#need the vector of likelihoods calculated from the summed counts
#and the vectors of nB for fish and inverts
onerate_aic <- aicc(both_likelihood, 1, fish_nB + invert_nB)

#need one vector of fish likelihoods and one vector of invert likelihoods
#and the vectors of nB for fish and inverts
tworate_aic <- aicc(fish_likelihoods + invert_likelihoods, 2, fish_nB + invert_nB)

#Akaike weights
apply(data.frame(BC_onerate_aic, BC_tworate_aic), 1, aicw)
