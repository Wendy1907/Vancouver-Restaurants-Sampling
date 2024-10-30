# 1. Calculate the sample size 
# a. SRS:
n <- (1.96)^2*0.25/(0.05)^2
n <- round(n, 0)

data_size <- nrow(cleaned_full_data)

n_fpc <- round(n/(1+n/data_size), 0)
# n_fpc is 344

# b. Stratified Sampling
n_strata = n_fpc/8
