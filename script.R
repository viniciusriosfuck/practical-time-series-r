# Functions ----

average_of_differences = function (x1, x2) {
  mean(x1 - x2)
}


# check if denominator is n or n-1
# boost the estimate a little bit (for those who like unbiased estimators)
homemade_std = function(x) {
  x_mean = mean(x)
  n = length(x)
  sqrt( sum( (x-x_mean)**2 ) / (n-1) )
}

standard_deviation_of_differences = function (x1, x2) {
  sd(x1 - x2)
}

t_student = function(x_mean, x_ref, s_d, n) {
  (x_mean - x_ref) / (s_d/sqrt(n))
}

p_value_two_tailed_test = function(tStudent, n) {
  dof = n-1  # degrees of freedom
  # pt: left area under the probability curve
  2 * pt(tStudent, dof)
}

hypothesis_test = function(p_value, alpha) {
  if (p_value < alpha) {
    "reject null"
  } else { # p >= alpha  
    "do not reject null"
  }
}

confidence_interval = function(x_mean, s_d, n, alpha) {
  # q: x-value equivalent to this area under the curve
  x_mean + c(-1, 1) * qt(1-alpha/2, n-1) * s_d / sqrt(n)
}

covariance = function(x,y) {
  sum( ( x-mean(x) ) * ( y-mean(y) ) ) / ( length(x)-1 )
}


autocovariance = function(x, k) {
  x_bar = mean(x)
  N = length(x)
  
  cum = 0
  
  for ( t in 1:(N-k) ) {
    cum = cum + ( x[t] - x_bar ) * ( x[t+k] - x_bar ) / N
  }
  cum
}


# Built-in ----
data()

demo()

demo(graphics)

ls()


# Coagulation ----

install.packages("faraway")

data(package='faraway')

data(coagulation, package='faraway')

coagulation
plot(coag ~ diet, data=coagulation)
summary(coagulation)
coagulation$diet
coagulation$coag

ourModel = lm(coag~diet-1, coagulation)
summary(ourModel)


# Rats ----
data(rats, package='faraway')

summary(rats)

plot(time~treat,data=rats)
plot(time~poison,data=rats)



# World Cup 2010 ----

data(worldcup, package='faraway')
mean(worldcup$Time)

summary(worldcup)

# ----
data.1 = c(35,8,10,23,42)
summary(data.1)
mean(data.1)
sum(data.1)/length(data.1)

small.size.dataset=c(
  91,49,76,112,97,42,70,100, 8, 112,
  95, 90, 78, 62, 56, 94, 65, 58, 109, 
  70, 109, 91, 71, 76, 68, 62, 134, 57, 83, 66)

hist(small.size.dataset)

hist(small.size.dataset, xlab='My data points')

hist(small.size.dataset, xlab='My data points', main='Histogram of my data')

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F)

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green')

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green')
lines(density(small.size.dataset))

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green')
lines(density(small.size.dataset), col='red', lwd=5)

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green', breaks=10)

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green', breaks=10)
lines(density(small.size.dataset), col='red', lwd=5)
# ----


set.seed(2016)  # There is a typo in the video (set.seed=2016)

Test_1_scores=round(rnorm(50, 78, 10))

Test_2_scores=round(rnorm(50, 78, 14))

Test_1_scores # Data won't be the same with the data generated in the video lecture since there was a typo in set.seed. 

Test_2_scores # Data won't be the same with the data generated in the video lecture since there was a typo in set.seed. 

plot(Test_2_scores~Test_1_scores)

plot(Test_2_scores~Test_1_scores, 
     main='Test scores for two exams (50 students)', 
     xlab='Test_1_scores', 
     ylab='Test 2 scores')

plot(Test_2_scores~Test_1_scores, 
     main='Test scores for two exams (50 students)', 
     xlab='Test_1_scores', ylab='Test 2 scores', col='blue')


x=c(1,2,3,4);
y=c(5, 7, 12, 13);
( m=lm(y~x) )


# CO2 ----

help(co2)
class(co2)

plot(co2, main='Atmospheric CO2 Concentration')

# Linear regression
co2.values = as.numeric(co2)
co2.times = as.numeric( time(co2) )
SSxx = sum( (co2.times - mean(co2.times) ) * (co2.times - mean(co2.times) ) )
SSxy = sum( (co2.values - mean(co2.values) ) * (co2.times - mean(co2.times) ) )
( slope = SSxy / SSxx )
( intercept = mean(co2.values) - slope*mean(co2.times) )

co2.fitted.values = slope*co2.times + intercept
co2.residuals = co2.values - co2.fitted.values


co2.linear.model = lm(co2 ~ time(co2))
plot(co2, main='Atmospheric CO2 Concentration with Fitted Line')
abline(co2.linear.model)

( co2.residuals = resid( co2.linear.model ) )

par(mfrow=c(1,3))
( c02.residuals = resid( co2.linear.model ) )
hist(co2.residuals, main= "Histogram of CO2 Residuals")
qqnorm(c02.residuals, main= "Normal Probability Plot")
qqline(c02.residuals)
plot(c02.residuals ~ time(co2), main="Residuals on Time")

par(mfrow = c(1,1))

plot(c02.residuals ~ time(co2), 
     xlim=c(1960, 1963), 
     main="Zoomed in Residuals on Time")


# Football ----

library(faraway)
plot(punting$Distance ~ punting$Hang); 
abline( lm(punting$Distance ~ punting$Hang) )



m = lm(punting$Distance ~ punting$Hang) 
qqnorm(resid(m)) 
qqline(resid(m))
# There seem to be no systematic departures from normality. 
# It's not perfect, but the residuals seem to follow a straight line 
# in the normal probability plot


# Sleep ----
help(sleep)


plot(extra~group, data=sleep, 
     main="Extra Sleep in Gossett Data by Group")

help(attach)
attach(sleep)
extra.1=extra[group==1]
extra.2=extra[group==2]


# Hypothesis testing
t.test(extra.1, extra.2, paired=TRUE, alternative="two.sided")
# z, t, chi-squared, F

diffs = extra.1-extra.2
qqnorm(diffs, main= "Normal Probability Plot")
qqline(diffs)


plot(extra.2~extra.1, 
     xlab='extra sleep with drug 1', 
     ylab='extra sleep with drug 2' ,
     main='Extra Sleep Drug 2 against Extra Sleep Drug 1')
sleep.linear.model = lm(extra.2 ~ extra.1 )
abline(sleep.linear.model)

summary(sleep.linear.model)

predict(sleep.linear.model, data.frame( extra.1 = c(2) ) )

residuals = resid(sleep.linear.model)
residuals[3]

# alpha = 0.05  | 0.01
# Type I Error: reject a true Null Hypothesis (FP)

# check if p-value < 0.05
# check if CI contains 0

alpha = 0.05  # significance level


# H0: Null (no difference)
x_ref = 0
# H1: Alternative (two.sided: it is not the same)

# d_bar: average of the differences = difference of the averages
# mean(extra.1 - extra.2)
mean(extra.1) - mean(extra.2)

( d_bar = average_of_differences(extra.1, extra.2) )

sd(extra.1)
homemade_std(extra.1)


# s_d: std of the the differences != difference of the stds
# sd(extra.1 - extra.2)

sd(extra.1) - sd(extra.2)

( s_d = standard_deviation_of_differences(extra.1, extra.2) )
n = length(extra.1)

( tStudent = t_student(d_bar, 0, s_d, n) )

( p_value = p_value_two_tailed_test(tStudent, n) )

hypothesis_test(p_value, alpha)

confidence_interval(d_bar, s_d, n, alpha)

# statistic from data
# parameter: descriptors from distribution, errors

# BPM ----

bpm.before = c(64, 45, 51, 56, 76, 
               77, 48, 65, 88, 37, 
               49, 71, 63, 57, 43);
bpm.after = c(82, 63, 64, 71, 91, 
              99, 66, 85, 100, 52, 
              59, 84, 73 ,75, 58 );
boxplot(bpm.before, bpm.after, names=c("Before", "After") )
qqnorm( bpm.after - bpm.before);

qqline(bpm.after - bpm.before)

t.test(bpm.before, bpm.after, paired=TRUE, alternative = "less")


# Trees ----

help(trees)

pairs(trees, pch = 21, bg = c("red"))

# Covariance
cov(trees)

covariance(trees$Girth, trees$Height)


# Correlation
cor(trees)




# Rock ----

help(rock)

pairs(rock)

cor(rock)


# ----

quiz_data=c(
  37, 86, 79, 95, 61, 93, 
  19, 98, 121, 26, 39, 11,
  26, 75, 29,130, 42, 8)

hist(quiz_data, breaks=10, main='Histogram', xlab='Quiz data')


hist(quiz_data, freq=F, breaks=10, main='Histogram', xlab='Quiz data', col='blue')
lines(density(quiz_data), col='red', lwd=5)

summary(data) # Edit this line



# cheddar ----
library(faraway)
help(cheddar)
dim(cheddar)

m=lm(taste~H2S, data=cheddar)
summary(m)

m$coefficients

sum(m$residuals)

sum(m$fitted.values)


# ----
data = c(7,  5,  1,  7,  2,  5,  2,  4, 10,  6);
t.test(data, alternative = "two.sided", paired=FALSE)
# The p-value is much less that 0.05 
# so this data set is rather improbable under the null hypothesis. 








# astsa ----
# Applied Statistica Time Series Analysis

install.packages("astsa")
require(astsa)
help(astsa)
help(jj)

help(flu)

help(globtemp)

help(globtempl)


# Autocovariance ----
help(star)


# Correlogram
autocorf = acf(star, type='covariance')

(ck <- rep(NA, 28))

for (k in 0:27 ) {
  ck[k+1] = autocovariance(star, k)
  
}

df = data.frame(autocorf$acf)
df$autocorf.acf
ck

mean(abs(df$autocorf.acf - ck))


plot(ck~autocorf$acf)


rk = ck / ck[1]
rk


# Random ----
purely_random_process = ts(rnorm(100))
purely_random_process


(acf(purely_random_process, type='covariance'))


(rk_acf = acf(purely_random_process, main='Correlogram of a purely random process'))


ck <- rep(NA, 21)
for (k in 0:20 ) {
  ck[k+1] = autocovariance(purely_random_process, k)
}
rk = ck / ck[1]
rk

mean(abs(rk_acf$acf - rk))



data = rnorm(100,0,1)
acf(2+3*x + data, main="ACF: Noise or Signal?")


# Random Walk ----

x=NULL
x[1]=0
for (i in 2:1000) {
  x[i] = x[i-1] + rnorm(1)  # white noise: Z
}
random_walk=ts(x)

plot(random_walk, main='A random walk', color='blue', lwd=2 )
acf(random_walk)

# remove the trend (X): obtain a stationary stochastic process
# purely random process (Z)
plot(diff(random_walk))

acf(diff(random_walk))



# Moving Average ----

N = 10000
order = 2


noise=rnorm(N)

ma_2=NULL
for (i in (order+1):N) {
  ma_2[i] = noise[i] + 0.7*noise[i-1] + 0.2*noise[i-2]
}

moving_average_process = ma_2[(order+1):N]

moving_average_process = ts(moving_average_process)

par(mfrow=c(2,1))



# Autocorrelation function of the process 
# cuts off and becomes zero at the order of the process.
plot(moving_average_process, main="Moving average process order 2")
acf(moving_average_process, main="Correlogram")

par(mfrow=c(1,1))




# Simulating MA(4) process.
# X_t= Z_t+0.2 Z_(t-1)+0.3 Z_(t-2)+ 0.4 Z_(t-3)


2^10 == 2**10

set.seed(2^10)
z=NULL
z=rnorm(1000)
data=NULL
for(i in 4:1000){
  data[i-3]=z[i]+0.2*z[i-1]+0.3*z[i-2]+0.4*z[i-3]
}
data=ts(data)

# find acf below

(acf(data, type='covariance'))

(acf(data))


# ----

# Simulating a non-stationary time series

# Set seed so thet we generate the same dataset
set.seed(2017)
# time variable 
t=seq(0,1,1/100)
# generate a time series
some.time.series=2+3*t+ rnorm(length(t))

(acf(some.time.series, type='covariance'))

(acf(some.time.series))

# obtain acv for this time series below