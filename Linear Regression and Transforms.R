# Mahmudul Rapi
# ORF245 PSET#9
# 4/17/2021
x = c(99.0, 101.1, 102.7, 103, 105.4, 107, 108.7, 110.8, 112.1, 
      112.4, 113.6, 113.8, 115.1, 115.4, 120)
mean(x) # 109.34

y = c(28.8, 27.9, 27, 25.2, 22.8, 21.5, 20.9, 19.6, 17.1, 18.9, 16,
      16.7, 13, 13.6, 10.8)
mean(y) #19.98667

qxx = function(x){
  avg = mean(x)
  sum = 0
  for (val in x){
    sum = sum + (val - avg) * (val - avg)
  }
  return(sum)
}
qxx(x) #521.196

qxy = function(x, y){
  avgx = mean(x)
  avgy = mean(y)
  len = length(x) # length of x and y assumed to be equal
  sum = 0
  for (i in seq(1, len, 1)){
    sum = sum + (y[i] - avgy) * (x[i] - avgx)
  }
  return(sum)
}
qxy(x, y) #-471.542

plot(x, y, title("X vs Y")) #plots data
abline(a = 118.91, b = -0.9047) #plot my estimated line

error_sum_squared = function(x, y, B0, B1){
  len = length(x) # length of x and y assumed to be equal
  sum = 0
  for (i in seq(1, len, 1)){
    summand = y[i] - B0 - x[i] * B1
    sum = sum + summand * summand
  }
  return(sum)
}
SSE = error_sum_squared(x, y, 118.91, -0.9047) # 11.43901

sqrt(SSE / (length(x) - 2)) # 0.93804

SST = qxx(y) #call function on y, i.e. Qyy
SST # 438.0573
coeff  = 1 - SSE / SST
coeff #0.973887

data <- read.csv('2010_Census_Populations_LosAngeles.csv')
males = data$Total.Males
females = data$Total.Females

t.test(males, conf.level = 0.98) # [14984.61, 17798.52]
t.test(males, females, alternative = "less", conf.level = 0.99) #[-Inf 1543.887]
t.test(males, females, mu = 1000, alternative = "less")
# p-value of 0.0449 < alpha = 0.05, so we are in favor of the
# alternative hypothesis that the mean is less than 1000

lm(y ~ x) #Estimated intercept of 118.909, and slope -0.9047

x2 = c(1.7, 2.2, 2.3, 2.6, 2.7, 3, 3.2, 3.3, 4.1, 4.3, 4.6, 5.7, 6.1)
y2 = c(1.3, 1.8, 1.6, 2, 2.1, 3.2, 3, 2.6, 4.1, 3.7, 5, 5.8, 5.3)
x_transformed <- rep(NA, length(x2))
y_transformed <- rep(NA, length(y2))

for (i in seq(1, length(x), 1)){
  x_transformed[i] = log(x2[i])
  y_transformed[i] = log(y2[i])
}

model = lm(y_transformed ~ x_transformed) # intercept of -0.4148, slope 1.2334

alpha = exp(-0.4148)
beta = 1.2334
print(alpha) #0.66047

eq = function(x){alpha * x^beta}
curve(eq, from=0, to=7)
plot(x2, y2)
