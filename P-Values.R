# Mahmudul Rapi
# ORF245, Problem Set # 8,Q#2
data <- read.csv('2010_Census_Populations_LosAngeles.csv')

household_size = data$Average.Household.Size
xbarSize = mean(household_size) # obtain average
sdSize = sd(household_size) # obtain sd
n = nrow(data) # number of samples
miu = 3 # assume null hypothesis true
z = (xbarSize - miu)/(sdSize / sqrt(n)) # calculate z

## compute
p = 2 * (1- pnorm(abs(z))) # calculates p-value for miu != 3
p # returns p-value = 0.0002391386

t.test(household_size, mu = miu) # returns p-value = 0.0002805

t.test(household_size, alternative = "less", mu = 4)
# p-value < 2.2e-16

t.test(household_size, alternative = "greater", conf.level = 0.9)
# 90 percent confidence interval: [2.768033, Inf)
