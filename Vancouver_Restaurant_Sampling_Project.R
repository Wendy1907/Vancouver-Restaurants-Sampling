library(tidyverse)

# Data cleaning and data preparation
full_data <- read.csv("cleaned_full_data.csv", header = T)
data <- full_data[,c("X", "restaurant", "star", "num_reviews", "city")] %>%
  rename(id = X)%>% 
  drop_na()
data

dim(data)
summary(data)

# Population mean of star
p_mean_star <- mean(data$star)
p_mean_star

IDS <- data$id
N <- length(data$id)
n <- 335

# Simple Random Sampling (SRS)
srs_sample <- sample.int(N, n, replace = F)
srs_sample.IDS <- IDS[srs_sample]
data_srs_sample <- subset(data, id %in% srs_sample.IDS)

dim(data_srs_sample)

# SRS Sample Mean
srs_sample.mean <- mean(data_srs_sample$star)
srs_sample.mean

# SRS Sample Variance
srs_sample.variance <- sum((data_srs_sample$star-srs_sample.mean)^2)/(length(data_srs_sample$star)-1)
srs_sample.variance

# SRS Sample SD
srs_sample.sd <- sqrt(srs_sample.variance)
srs_sample.sd

# SRS Calculate 95% CI for population mean
srs_sample.se <- sqrt((1-n/N)/n)*srs_sample.sd
srs_sample.se
srs_CI <- c(srs_sample.mean - 1.96*srs_sample.se, srs_sample.mean + 1.96*srs_sample.se)
srs_CI


# Stratified Sampling Proportional Allocation
# Calculate n.h
N.h <- tapply(data$star, data$city, length)
N.h
city_allocation <- round((N.h/N)*n)
city_allocation

# Generated Stratified Sample
n.h <- c(13, 41, 27, 55, 22, 80, 76, 22)

cities <- names(N.h)

set.seed(0)
data_str_sample <- NULL
for (i in 1: length(cities))
{
  row.indices <- which(data$city == cities[i])
  sample.indices <- sample(row.indices, n.h[i], replace = F)
  data_str_sample <- rbind(data_str_sample, data[sample.indices, ])
}

# Stratified Estimation
str_mean.h <- tapply(data_str_sample$star, data_str_sample$city, mean)
str_mean.h

str_variance.h <- tapply(data_str_sample$star, data_str_sample$city, var)
str_variance.h

str_se.h <- sqrt((1-n.h/N.h)*str_variance.h/n.h)

str_prop.mean <- sum(N.h/N * str_mean.h)
str_prop.mean

str_prop.se <- sqrt(sum((N.h/N)^2 * str_se.h^2))
str_prop.se

# Compare SRS and stratified
srs <- c(srs_sample.mean, srs_sample.se)
str_prop <- c(str_prop.mean, str_prop.se)
rbind(srs, str_prop)


# Proportion:
# Adding a binary indicator for ratings > 4.5 stars
data <- data %>%
  mutate(rating_above_4_5 = ifelse(star > 4.5, 1, 0))

# Population proportion of restaurants with rating > 4.5
p_prop_star_above_4_5 <- mean(data$rating_above_4_5)
p_prop_star_above_4_5

IDS <- data$id
N <- length(data$id)
n <- 335

# Simple Random Sampling (SRS)
srs_sample <- sample.int(N, n, replace = FALSE)
srs_sample.IDS <- IDS[srs_sample]
data_srs_sample <- subset(data, id %in% srs_sample.IDS)

# SRS Sample Proportion
srs_sample.prop <- mean(data_srs_sample$rating_above_4_5)
srs_sample.prop

# SRS Sample Variance for Proportion
srs_sample.variance <- sum((data_srs_sample$rating_above_4_5 - srs_sample.prop)^2) / (length(data_srs_sample$rating_above_4_5) - 1)
srs_sample.variance

# SRS Sample SD for Proportion
srs_sample.sd <- sqrt(srs_sample.variance)
srs_sample.sd

# SRS Calculate 95% CI for population proportion
srs_sample.se <- sqrt((1 - n/N) / n) * srs_sample.sd
srs_CI <- c(srs_sample.prop - 1.96 * srs_sample.se, srs_sample.prop + 1.96 * srs_sample.se)
srs_CI

# Stratified Sampling Proportional Allocation
N.h <- tapply(data$rating_above_4_5, data$city, length)
city_allocation <- round((N.h / N) * n)

n.h <- c(13, 41, 27, 55, 22, 80, 76, 22) # Adjust based on city_allocation
cities <- names(N.h)

set.seed(0)
data_str_sample <- NULL
for (i in 1:length(cities)) {
  row.indices <- which(data$city == cities[i])
  sample.indices <- sample(row.indices, n.h[i], replace = FALSE)
  data_str_sample <- rbind(data_str_sample, data[sample.indices, ])
}

# Stratified Estimation for Proportion
str_prop.h <- tapply(data_str_sample$rating_above_4_5, data_str_sample$city, mean)
str_variance.h <- tapply(data_str_sample$rating_above_4_5, data_str_sample$city, var)
str_se.h <- sqrt((1 - n.h / N.h) * str_variance.h / n.h)

str_prop.mean <- sum(N.h / N * str_prop.h)
str_prop.mean

str_prop.se <- sqrt(sum((N.h / N)^2 * str_se.h^2))
str_prop.se

# Compare SRS and stratified estimates for the proportion
srs <- c(srs_sample.prop, srs_sample.se)
str_prop <- c(str_prop.mean, str_prop.se)
rbind(srs, str_prop)