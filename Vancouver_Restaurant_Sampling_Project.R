library(tidyverse)

# Data cleaning and data preparation
full_data <- read.csv("cleaned_full_data.csv", header = T)
data <- full_data[,c("X", "restaurant", "star", "num_reviews", "city")] %>%
  rename(id = X)%>% 
  drop_na()
data

dim(data)
summary(data)

# Simple Random Sampling
# Population mean of star
p_mean_star <- mean(data$star)
p_mean_star

IDS <- data$id
N <- length(data$id)
n <- 335

srs_sample <- sample.int(N, n, replace = F)
srs_sample.IDS <- IDS[srs_sample]
data_srs_sample <- subset(data, id %in% srs_sample.IDS)

dim(data_srs_sample)

# Sample Mean
srs_sample.mean <- mean(data_srs_sample$star)
srs_sample.mean

# Sample Variance
srs_sample.variance <- sum((data_srs_sample$star-srs_sample.mean)^2)/(length(data_srs_sample$star)-1)
srs_sample.variance

# Sample SD
srs_sample.sd <- sqrt(srs_sample.variance)
srs_sample.sd

# Calculate 95% CI for population mean
srs_sample.se <- sqrt((1-n/N)/n)*srs_sample.sd
srs_CI <- c(srs_sample.mean - 1.96*srs_sample.se, srs_sample.mean + 1.96*srs_sample.se)
srs_CI