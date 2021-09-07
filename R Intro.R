# Mahmudul Rapi
# ORF245, Problem Set # 1,Q#1

install.packages("listing")
y = c(7.267, 6.689, 6.226, 9.265, 5.559, 4.428, 3.504, 0.467, 
      1.359, 0.892, 3.363, 1.158, 1.826, 2.082, 3.931, 1.235, 
      4.873, 8.934, 0.101, 6.590)

hist(y, breaks = c(0, 2.5, 5, 7.5, 10))
mean(y) #computes mean, outputs 3.98745
median(y) #computers median, outputs 3.7175
sd(y) # computers standard deviation, outputs 2.847963
ran = range(y)
ran[2] - ran[1] #computes the range, 9.164
variation = sd(y)^2
variation #computes variation, 8.110894
