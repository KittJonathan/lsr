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

## “Summarising” a variable ----

summary(object = afl_margins)

# blowout: game in which the winning margin exceeds 50 points
blowouts <- afl_margins > 50
blowouts

summary(object = blowouts)

# summarise a factor
summary(object = afl_finalists)

# summarise a character vector
summary(object = as.character(afl_finalists))

## “Summarising” a data frame ----

load("data/clinicaltrial.Rdata")

summary(clin_trial)

## “Describing” a data frame ----

psych::describe(x = clin_trial)

## Descriptive statistics separately for each group ----

psych::describeBy(x = clin_trial, group = clin_trial$therapy)

by(data = clin_trial, INDICES = clin_trial$therapy, FUN = psych::describe)
by(data = clin_trial, INDICES = clin_trial$therapy, FUN = summary)

aggregate(
  mood_gain ~ drug + therapy, # mood_gain by drug/therapy combination
  data = clin_trial,          # data is in the clin_trial data frame
  FUN = mean                  # print out group means
)

aggregate(mood_gain ~ drug + therapy, clin_trial, sd)

# Standard scores ----

(X - mean(X)) / sd(X)
scale(X)

# Correlations ----

## Data ----

load("data/parenthood.Rdata")
head(parenthood)

psych::describe(parenthood)

hist(parenthood$danielle_sleep)
hist(parenthood$baby_sleep)
hist(parenthood$danielle_grump)

## The strength and direction of a relationship ----

plot(parenthood$danielle_sleep, parenthood$danielle_grump)
plot(parenthood$baby_sleep, parenthood$danielle_grump)
plot(parenthood$baby_sleep, parenthood$danielle_sleep)

## The correlation coefficient ----

# Pearson's correlation coefficient:
# r = -1 ..... perfect negative relationship
# r = +1 ..... perfect positive relationship
# r =  0 ..... no relationship

# covariance:
# cov_xy = (1 / N - 1) * sum((x - mean(x)) * (y - mean(y)))

# correlation coefficient:
# r = cov_xy / (sd(x) * sd(y))

## Calculating correlations in R ----

var1 <- parenthood$danielle_sleep
var2 <- parenthood$danielle_grump

(sum((var1 - mean(var1)) * (var2 - mean(var2)))) / (sd(var1) * sd(var2)) / 99

cor(x = parenthood$danielle_sleep, y = parenthood$danielle_grump)

# correlate all pairs of variables in "parenthood":
cor(x = parenthood)

## Interpreting a correlation ----

# Correlation      | Strength    | Direction
# -1.0 to -0.9     | Very Strong | Negative
# -0.9 to -0.7     | Strong      | Negative
# -0.7 to -0.4     | Moderate    | Negative
# -0.4 to -0.2     | Weak        | Negative
# -0.2 to 0        | Negligible  | Negative
# 0 to 0.2         | Negligible  | Positive
# 0.2 to 0.4       | Weak        | Positive
# 0.4 to 0.7       | Moderate    | Positive
# 0.7 to 0.9       | Strong      | Positive
# 0.9 to 1.0       | Very Strong | Positive

# Anscombe's quartet
load("data/anscombesquartet.Rdata")

cor(x1, y1)
cor(x2, y2)
cor(x3, y3)
cor(x4, y4)

plot(x1, y1)
plot(x2, y2)
plot(x3, y3)
plot(x4, y4)

## Spearman’s rank correlations ----