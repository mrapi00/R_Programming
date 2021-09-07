# Mahmudul Rapi
# ORF245, Problem Set # 7,Q#3
data <- read.csv('2010_Census_Populations_LosAngeles.csv')

median_age = data$Median.Age
xbarAge = mean(median_age) # obtain average
sdAge = sd(median_age) # obtain sd
n = nrow(data) # obtain number of observations

## 2-sided C.I. at 95% confidence level
# Lower bound for a 2-sided C.I.
xbarAge - sdAge/sqrt(n) * qnorm(0.975) #35.57364

# Upper bound for a 2-sided C.I.
xbarAge + sdAge/sqrt(n) * qnorm(0.975) #37.48153
t.test(median_age) #35.57000 to 37.48517

household_size = data$Average.Household.Size
xbarSize = mean(household_size) # obtain average
sdSize = sd(household_size) # obtain sd

## Upper C.I.  at 90% confidence level
xbarSize + sdSize/sqrt(n) * qnorm(0.90) #2.88808
