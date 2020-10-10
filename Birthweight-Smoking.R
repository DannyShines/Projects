setwd("C:\\Users\\barbe\\Desktop\\ECO_321\\R")

data_weight<-read.csv("birthweight_smoking(2).csv",header=TRUE)

dim(data_weight)

n <- dim(data_weight)[1]
m <- dim(data_weight)[2]

n
m

names(data_weight)

data_weight[5:6]<-NULL

# removed tripe3 and tripe0

attach(data_weight) ### detach(data_weight) to remove
#add new log(birthweight) column and calculations
data_weight$"log(birthweight)"<-log(data_weight$birthweight)
str(data_weight)

### Separate Sample between Smokers and Non-Smokers
Smoker<-ifelse(smoker > 0,1,0)

# summarize 
summary(birthweight[Smoker==0])
summary(birthweight[Smoker==1])
        
# Sample Mean and Sample Variance for Smokers[1]
mean(birthweight[Smoker==1])
var(birthweight[Smoker==1])

# Sample Mean and Sample Variance for Nonsmokers[0]
mean(birthweight[Smoker==0])
var(birthweight[Smoker==0])


n0 <- length(birthweight[Smoker==0])
n1 <- length(birthweight[Smoker==1])
 
diff_mean <- mean(birthweight[Smoker==1]) - mean(birthweight[Smoker==0])

var_smoker0 <- var(birthweight[Smoker==0])/n0
var_smoker1 <- var(birthweight[Smoker==1])/n1


# \t\ > 1.96 , so we reject the null hypothesis that the difference of 
# the means is equal to zero for alpha = 5%
ttest <- diff_mean/sqrt(var_smoker0 + var_smoker1)
ttest

qnorm(0.975) # equals 1.959964



2*pnorm(-abs(ttest))

#since p-value is less than the significance level of 0.05, we reject 
#the Null Hypothesis


#Confidence Interval for the Difference in Mean 95%
CI = diff_mean + sqrt((var_smoker0)+(var_smoker1)) * c(-qnorm(0.975), qnorm(0.975))
CI

# Since the Sample Means arent in the range of the Confidence Interval 
# I can reject the Null Hypothesis


















### Confidence Intervals
### Construct a 1-alpha = 90% CI for the population mean of sample (alpha = # between 0 and 1 thats usually low like 0.05)
### Reliability Factor comes from the normal distribution
### (n is large enough), so we need to find the value of the standard
### normal distribution at the level 1-(alpha/2) = 0.95
### In R we can do that using the command qnorm [ qnorm(0.95) = 1.645 = Z(1.645) ]
qnorm(0.95)
qnorm(0.05)   # this is -(qnorm(0.95))
### This should be approximately the number you find in the Z-table
### This is the 90% CI

#Lower Bound
mean(birthweight) - (sd(birthweight)/sqrt(n))*qnorm(0.95)  #Z(1.645)
#Upper Bound
mean(birthweight) + (sd(birthweight)/sqrt(n))*qnorm(0.95)  #Z(1.645)

#Standard Error of Sample Mean = sqrt(Variance of the Sampling Distribution of Point Estimator)


 ### HYPOTHESIS TESTING

### Test that the population mean of birth weight is equal to $$$$ for alpha = 5%
### Reliability factor comes from the normal distribution
### (n > 100)
### This is a two-tailed test, so we need to find the value of the standard
### normal distribution at the level 1-(alpha/2) = 0.0975
### Null Hypothesis: mu = 

### 1) Approach 1: Critical Values
# Lower critical value
mean(birthweight) - (sd(birthweight)/sqrt(n)*qnorm(1-(0.5/2)))
# Upper critical value
mean(birthweight) + ((sd(birthweight)/sqrt(n))*qnorm(1-(0.5/2)))

### Sample mean is higher than the upper critical value: 



### 2) Approach 2: P-Value (smallest prob such that I will not reject the null hypothesis)
### The p-value of this test is equal to 


2*(1 - pnorm(abs((mean(birthweight)-   ))))