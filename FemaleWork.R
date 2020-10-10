setwd("C:\\Users\\barbe\\Desktop\\ECO_321\\R")

datawork<- read.csv("FemaleWork.csv", header=TRUE)

attach(datawork)

install.packages(c('sandwich','lmtest'))
library(sandwich)
library(lmtest)

###2a.
ols.fit<- lm(workhour ~ child, data = datawork)
summary(ols.fit) #This is under the assumption of HOMOSKEDACITY

#                             (SD of Sampling)
#                             (Distribu. )
#                   Estimate  Std. Error t value Pr(>|t|)          #t-value = test statistic = estimate/std. error
# B0^ = (Intercept) 36.25097    0.02829 1281.50   <2e-16 ***
# B1^ = child       -1.00966    0.04193  -24.08   <2e-16 ***
# Multiple R-squared:  0.001763
# (Measure of Goodness of Fit) Residual standard error: 11.96 on 328397 degrees of freedom (effective number of observations that we use when computing)

sum.fit.het <- coeftest(ols.fit, vcov = vcovHC(ols.fit, "HC1")) # Std Error will be under HETEROSKEDACITY 
#             Estimate Std. Error  t value  Pr(>|t|)    
#(Intercept) 36.250973   0.028318 1280.143 < 2.2e-16 ***
#  child     -1.009660   0.041918  -24.087 < 2.2e-16 ***




plot(datawork$child,
     datawork$workhour,
     main='',
     xlab="Child",
     ylab="Work Hours",
     type = "p",
     cex=1,
     col='black')

abline(ols.fit,col='red')

#> ols.fit$coefficients
#(Intercept)       child 
#36.25097    -1.00966 

predict.workhour <- betahat0 + (betahat1*datawork$workhour)

#compute the predicted error terms

Uhat <- datawork$workhour - predict.workhour

hist(Uhat)
mean(Uhat)
cov(Uhat, datawork$child)

betahat0 <- ols.fit$coefficients[1]
betahat1 <- ols.fit$coefficients[2]

###2b.
betahat0
#(Intercept) 
#36.25097

betahat1
#  child 
#-1.00966 


###2c. betahat0 captures an estimate of the work hours for women without children.


###2d. Yes, women with children work less. We can see that because betahat1 has a negative slope. 
###    As the child increases to 1, workhours decrease by -1.00966.



###2e. 
summary(ols.fit)

qnorm(1-(.01/2))
#[1] 2.575829
#(Intercept) 36.25097    0.02829 1281.50

We will be rejecting the null hypothesis that the actual Beta0 is equal to zero
# P value of child: (<2e-16) < 1% 


tbeta12 <- (ols.fit$coefficients[2] + 2)/sum.fit.het[[4]] 

abs(tbeta12)<qnorm(1-(.01/2))
# FALSE

## Compute the p-value (compare with table)
p.value2 <- 2*(1 - pnorm(abs(tbeta12)))

# child       -1.009660   0.041918  -24.087 < 2.2e-16 ***

p value is less than alpha so we reject the null hypothesis


###2f.  95% CI 

CI_beta1 <- ols.fit$coefficients[2] + sum.fit.het[[4]]*c(-qnorm(1-(0.05/2)),qnorm(1-(0.05/2))) 
#     child       -1.009660   0.041918  -24.087 < 2.2e-16 ***