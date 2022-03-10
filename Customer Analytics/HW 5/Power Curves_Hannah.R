##----------------------------------------------------------------
# IAA Homework 5: Power Curve Base Script
# Aaron Baker (ambaker31991@gmail.com)
##----------------------------------------------------------------

rm(list = ls())

require(dplyr)
require(ggplot2)
require(scales)

#--------------------------------------
# Calculate minimum sample size
#--------------------------------------

# Estimates of the control parameters:
p0 <- 0.501 # This typically comes from the historical data
daily_sample_amount <- 1400 # This typically the average historical daily amount in the time period expecting to test

#Set of minimum sample size parameters
alpha <- 0.05 #the probability of a false positive
beta <- 0.20 #the probability of a false negative
power <- 1 - beta #the probability of a true positive
mdl <- 0.02 # the minimum detectable lift to design test for. defined as the % difference
smp_ratio <- 1 #equal sample size in each variant 
direction <- 'two.sided' #type of test

#Calculate the minimum sample size (continuous) using power.t.test (base R)
min_n_base <- power.prop.test(n=NULL, #leaving this Null will return the minimum sample required
                           p1=p0,
                           p2=(p0*(1+mdl)),
                           sig.level=alpha, 
                           power=power, 
                           alternative=c(direction))

min_n_base$n #note this is per variant
plot(min_n_base$n, min_n_base$power)

## POWER CURVE ONE ## Hannah PPT
mdl_list = c(0.01, 0.03, 0.05, 0.07, 0.1)
n_list = c()
power_list = c()

for (i in mdl_list) {

  min_n_base <- power.prop.test(n=NULL, #leaving this Null will return the minimum sample required
                                p1=p0,
                                p2=(p0*(1+i)),
                                sig.level=alpha, 
                                power=power, 
                                alternative=c(direction))
  n_list <- c(n_list, min_n_base$n)
  power_list <- c(power_list, min_n_base$power)
}

plot(n_list, mdl_list)

#Total Test Sample:
min_n <- ceiling(n_list+1) #typically round up and add one to deal with rounding errors
total_n <- min_n * 2

#Total Duration of Test
duration = ceiling(total_n/daily_sample_amount)

plot(duration, mdl_list) #16 day difference in duration between mdl = 0.03 and mdl = 0.05

## POWER CURVE TWO ## Taylor PPT
mdl_list = c(0.03, 0.05, 0.1)
alpha_list = c(0.01, 0.05, 0.10)
power_list = c()
df = data.frame( mdl = c(0.03,0.03,0.03,0.05,0.05,0.05,0.1,0.1,0.1),
                 alpha = c(alpha_list,alpha_list,alpha_list))

for (i in mdl_list) {
  for (a in alpha_list){
  
    min_n_base <- power.prop.test(n=14*daily_sample_amount, 
                                  p1=p0,
                                  p2=(p0*(1+i)),
                                  sig.level=a, 
                                  power=NULL, 
                                  alternative=c(direction))
    
    power_list <- c(power_list, min_n_base$power)
    }
}
df$power = power_list

df$alpha = as.factor(df$alpha)
ggplot(data = df, aes(x = mdl, y = power, color = alpha)) + geom_point()
# mdl = 0.03 and alpha = 0.01 does not reach power of 80% in 14 days

# reco mdl = 0.05 and alpha = 0.01 fpr power of 0.991 in a 14 day duration
# increasing mdl doesn't net much additional power, decreasing mdl gives up sunstantial pwer
# increasing alpha doesn't net additional power but does increase false positive rate 

