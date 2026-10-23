# Learning Statistics With R - Danielle Navarro
# https://learningstatisticswithr.com/
# Chapter 5 - Descriptive Statistics
# https://learningstatisticswithr.com/05-descriptives.html

# Packages ----

library(lsr)

# Data ----

load("data/aflsmall.Rdata")

# afl_margins:  winning margin (number of points) for all 176 home and away 
# games played during the 2010 season

# afl_finalists: names of all 400 teams that played in all 200 finals matches
# played during the period 1987 to 2010

print(afl_margins)

hist(afl_margins)

# Measures of central tendency ----

## Mean ----

mean(x = afl_margins)

## Median ----

median(x = afl_margins)

## Trimmed mean ----

dataset <- c(-15, 2, 3, 4, 5, 6, 7, 8, 9, 12)

mean(x = dataset)
median(x = dataset)

mean(x = dataset, trim = .1)
mean(x = dataset[2:9])

mean(x = afl_margins)
mean(x = afl_margins, trim = .05)

## Mode ----

table(afl_finalists)

lsr::modeOf(x = afl_finalists)
lsr::maxFreq(x = afl_finalists)

# Measures of variability ----

## Range ----

max(afl_margins)
min(afl_margins)
range(afl_margins)

## Interquartile range (IQR) ----

quantile(x = afl_margins, probs = .5)
median(x = afl_margins)

quantile(x = afl_margins, probs = c(.25, .75))

quantile(x = afl_margins, probs = .75) - quantile(x = afl_margins, probs = .25)
IQR(x = afl_margins)

## Mean Absolute Deviation ----

X <- c(56, 31, 56, 8, 32)  # enter the data
x_bar <- mean(X)           # step 1. the mean of the data
AD <- abs(X - x_bar)       # step 2. the absolute deviations from the mean
AAD <- mean(AD)            # step 3. the average absolute deviations
print(AAD)                 # print the results

lsr::aad(X)

## Variance ----

var(afl_margins)                           # N-1
mean((afl_margins - mean(afl_margins))^2)  # N

(mean((afl_margins - mean(afl_margins))^2)) * length(afl_margins) / (length(afl_margins) - 1)

## Standard deviation ----

sd(afl_margins)
sqrt(var(afl_margins))

sqrt(mean((afl_margins - mean(afl_margins))^2))
sqrt((mean((afl_margins - mean(afl_margins))^2)) * length(afl_margins) / (length(afl_margins) - 1))

## Median Absolute Deviation ----

# mean absolute deviation from the mean:
mean(abs(afl_margins - mean(afl_margins)))

# *median* absolute deviation from the *median*:
median(abs(afl_margins - median(afl_margins)))

stats::mad(x = afl_margins, constant = 1)

# Under certain circumstance, you can multiply the raw MAD value by 1.4826 and 
# obtain a number that is directly comparable to the standard deviation

sd(x = afl_margins)
stats::mad(x = afl_margins)

# Skewness and kurtosis ----

# Skewness
1 / (length(afl_margins) * sd(afl_margins)^3) * sum((afl_margins - mean(afl_margins))^3)
psych::skew(afl_margins)

# Kurtosis
1 / (length(afl_margins) * sd(afl_margins)^4) * sum((afl_margins - mean(afl_margins))^4) - 3
psych::kurtosi(x = afl_margins)

x <- rnorm(1000)
1 / (length(x) * sd(x)^4) * sum((x - mean(x))^4)
1 / (length(x) * sd(x)^4) * sum((x - mean(x))^4) - 3

# Getting an overall summary of a variable ----

