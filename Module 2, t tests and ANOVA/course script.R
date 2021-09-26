################################################################
##        T tests and ANOVA
################################################################
## 0. File import and the working directory

## The working directory is R's 'home base.' If you don't specify where R should look for a file, it will default to looking in the working directory. To get a sense of where you are:

getwd()
list.files()

directory <- getwd()
## You can change the working directory by using the command:

setwd("directory")

## An easier way is to navigate to Session -> Set Working Directory -> Choose Directory


## 1. Review on plots
rm(list = ls())
#### 1.1. 'plot()'

##Some objects exist in R without having the be named or loaded in. One such object is the data set mtcars. 

attach(mtcars)

# Now we can directly use component names. This makes a 'phantom copy.' Don't use attach if you plan to modify the data set. You can undo this action using the detach() function.

summary(mtcars)
help(mtcars)
plot(wt,mpg) # if inputs are 2 vectors, scatterplot of 1st on X, and 2nd on Y
plot(mpg~wt) # you can also use foumula expression
pairs(cbind(mpg,wt,hp,cyl)) # pairs()
detach(mtcars)

## We see that mpg and weight have a pretty linear looking relationship, making it a good candidate for a linear regression. We'll explore that in the next section.

#### 1.2. other high-level plotting functions

directory


mydata =read.csv(file= 'lowbwt.csv', head=TRUE, sep=",")

                
#"C:/Users/07062962468/Desktop/Statistical Analysis in R/lowbwt.csv", head=TRUE, sep=",")
head(mydata)
summary(mydata)
# ID:   Identification Code 
# LOW: Low Birth Weight (0 = Birth Weight >= 2500g, 1 = Birth Weight < 2500g)
# AGE: mother's age in years
# LWT: mother's weight in lbs
# RACE: mother's race (1 = white, 2 = black, 3 = other)
# SMOKE: smoking status during pregnancy
# PTL: no. of previous premature labors
# HT:  history of hypertension
# UI:	presence of uterine irritability
# FTV:no. of physician visits during first trimester
# BWT: Birth Weight in Grams
#BWT <-mydata[,11]
attach(mydata)

hist(BWT,freq=T) # histogram

boxplot(BWT) # boxplot
# plots of BWT by SMOKE#
SMOKE<-mydata[,6]
boxplot(BWT~SMOKE,col=rainbow(3)) # you can also use formula

#### 1.3. Example on linear fit
# Let's do a linear regression using weight to predict mpg.
y = mtcars$mpg
x = mtcars$wt

# Now use high-level ftn 'plot()' to create a basic plot

plot(x,y)
fit = lm(y~x) # SLR fit 

# Unlike some other stats packages, R does not provide the output when the regression is run. Instead, it saves the output in an object of class 'lm.' This is a little less user friendly but much more flexible because it allows you to manipulate each element of the regression with very fine control.

fit
summary(fit)
?lm

abline(fit$coefficients,col='red',lwd=2,lty=2)

# Now use low-level ftn to add extra info

title(main='Simple linear regression')
legend('topright',legend=c('SLR fit'),col=c('red'),lty=2,lwd=2)

# It's important to make sure the SLR assumptions aren't violated. We can do this by looking at a scatterplot and histogram of the residuals

plot(fit$res)
hist(fit$res)

# Plot is a powerful and generic function. You can put a lot of different inputs into plot() and get interesting results. Let's try plotting the LM object

plot(fit)

## 2. T-test

# Developed by William Gosset, an employee of Guinness Brewery

# Still use the 'low birth weight' data set. 
#compare BWT for the mothers who smoke or not.
# 
#################################################################
# 2.1 One sample t-test
# Research Question (one sample test): Is the mean of a population different from the null hypothesis?

# Consider testing whether the baby's average birth weight is different from 2500 which is the cutoff value
# ALWAYS plot data first
boxplot(BWT,ylab='Birth Weight')
stripchart(BWT,method='jitter',vertical=T,add=T)
abline(h=2500,col="Red",lwd=2)
abline(h=mean(BWT),lwd=2)
title("All BWTs",sub='Red Line = Cutoff Line \n Black Line=Average BWT')
# The average appears to be larger than 2500g.

# Statistical procedure to test whether the mean of Birth Weight is different from  2500
#t.test
t.test(BWT,mu=2500)
# Results suggest rejecting the null hypothesis that the mean is equal to 2500 - 
# p-value of  1.175e-14

#################################################################
# 2.2 Two sample t-test
# Research Question (two sample test): Are the means of two populations different?

# Next consider whether the birth weight for the babies whose mothers smoke and those whose mother do not
# are different or not.
boxplot(BWT~SMOKE,xlab='Smoke or not',ylab='Birth Wieght',names=c('Do not Smoke','Smoke'))
stripchart(BWT~SMOKE,method="jitter",vertical=T,add=T)
title("Average Birth Weight by Smoke")
# Birth Weight appears to be higher for those whose mothers do not smoke.

# Statistical procedure to test whether the mean BWT is different for these two groups of students
t.test(BWT~SMOKE) # Default is Welch t-test with unequal variance assumption
t.test(BWT~SMOKE,var.equal=T) # T-test with equal variance assumption

# We can test the equality of variances assumption
var.test(BWT~SMOKE) 

# Welch (unequal variance assumption) is more conservative, with respect to rejecting the null hypothesis
#Both tests provide evidence to reject the null hypothesis - equal BWT for two groups
# Welch p-value  0.00743

# Suppose data in two vectors rather than data frame
BWT.N = BWT[SMOKE==0];BWT.Y = BWT[SMOKE==1]
t.test(BWT.N,BWT.Y)
# identical results

############################################################################
# 2.3 Sample Size Calculation for Power or Margin of Error
# Research Question: How many observations are needed for a given power or sample size
# Power = probability rejecting null when null is false

# Compute power for a given sample size, difference between means - delta, & standard deviation
power.t.test(n=20, delta=2, sd=2) # power

# Compute minimum sample size for given power, difference between means, and standard deviation
?power.t.test
power.t.test(delta=2,sd=2,power=.8) # min sample size to give power of 0.8

# load pwr library for additional power calculations, including Anova

################################################################################
# 2.4 Paired T-test
# T-test for data that is inherently paired or linked
# Research Question: Given the paired structure of the data are the means of two 
# sets of observations significantly different?

# Consider the following example: In a warehouse, the employees have asked management 
# to play music to relieve the boredom of the job. The manager wants to know whether
# efficiency is affected by the change. The data below gives efficiency ratings of 15
# employees recorded before and after the music system was installed.
# link of this dataset:http://www-ist.massey.ac.nz/dstirlin/CAST/CAST/HtestPaired/testPaired_c1.html

efficiency_before=c(21,35,40,38,23,27,28,39,22,35,28,20,39,28,34)
efficiency_after= c(32,35,38,57,37,30,39,28,40,48,33,33,39,41,40)

# Plot the differences of the two populations
par(mfcol=c(1,1))
diff=efficiency_after-efficiency_before
boxplot(diff,ylab='Difference in efficiency')
stripchart(diff,method='jitter',vertical=T,add=T)
abline(h=0,col="Red",lwd=3)
title("Difference in Efficiency \n efficiency_after-efficiency_before")

# Statistical test for difference between two observations
t.test(efficiency_after,efficiency_before,paired=T)
t.test(diff) #Alternatively test whether the difference is equal to zero - Same Results

#################################################################
# 2.5 Normality Assumption
# Assumption of normal distribution underpins t-test

# Assess normality visually
par(mfcol=c(1,2))
# For large sample size histograms are useful, beware of how the data is "binned"
# paricularly in smaller sample sizes 
hist(BWT.N)
hist(BWT.Y)

# Normal quantile-quantile Plot
# compares expected quantiles from normal distribution with observed quantiles
qqnorm(BWT.N)    # normal quantile-quantile plot
qqline(BWT.N)
qqnorm(BWT.Y)
qqline(BWT.Y)


# Statistical Test for Normality
# Null distribution: data came from a normal distribution
shapiro.test(BWT.N)  # Shapiro-Wilk Normality Test
shapiro.test(BWT.Y)

#####################################################################
# 2.6 Wilcoxon RACE Sum Statistic:
# Non-parametric Alternative when Normality Assumption Not Satisified

par(mfcol=c(1,1))
# Plot average yields for the two years
boxplot(BWT.N,BWT.Y,xlab='Smoke',ylab='Birth Weight',names=c('No','Yes'))
stripchart(BWT.N,method="jitter",add=T,vertical=T,at=1,col="darkorange3")
stripchart(BWT.Y,method="jitter",add=T,vertical=T,at=2,col="darkred")

title("Birth Weight by Smoke")


# Need to use a non-parametric alternative to test whether the two distributions are the same
wilcox.test(BWT.N,BWT.Y)
# p= 0.007109, reject null that the two distributions are the same
# paired version also available --Wilcoxon Signed RANK test
#wilcox.test(x, y, paired = TRUE)

##  END of 2. T-test
################

################
##  3. ANOVA - (ANalysis Of VAriance)
# ANOVA is used to compare means of more than two groups.

# 3.1 One-Way ANOVA
# One-way ANOVA: consider comparing BWT for 3 raceS
# First plot data
par(mfcol=c(1,1))
boxplot(BWT~RACE,xlab='Race',ylab='Birth Weight')
stripchart(BWT~RACE,method="stack",vertical=T,add=T)
title("Comparison of BWT by RACEs of the undergraduate institution")
# One-way ANOVA:
# Tests null hypothesis that at least one factor is different
a.1= aov(BWT~factor(RACE))
summary(a.1)
# Use contrasts to test differences between treatment pairs
# pairwise comparisons: Tukey Procedure controls multiple comparison problem
TukeyHSD(a.1)
plot(TukeyHSD(a.1))

# one-way anova without equal var assumption
oneway.test(BWT~factor(RACE))

#There is also a non-parametric version of ANOVA known as Kruskal-Wallis Test
kruskal.test(BWT~factor(RACE))

# 3.2 Two-way ANOVA: consider comparing BWT~RACE*SMOKE
# First plot data
par(mfrow=c(1,2))
#Boxplots and Stripcharts visualize Main Effects of Factors
boxplot(BWT~RACE,subset=SMOKE==0,xlab='RACE',main='Not Smoke')
stripchart(BWT~RACE,subset=SMOKE==0,method="jitter", vertical=T,add=T)
boxplot(BWT~RACE,subset=SMOKE==1,xlab='RACE',main='Smoke')
stripchart(BWT~RACE,subset=SMOKE==1,method="jitter", vertical=T,add=T)

#Interaction Plot 
par(mfrow=c(1,1))
interaction.plot(RACE, SMOKE, BWT, type="b", col=c(1:3),leg.bty="o", 
                 leg.bg="beige", lwd=2, pch=c(18,24,22), xlab="Race", 
                 ylab="Birth Weight", main="Interaction Plot")

##try interaction plot with "admit" on x-axis

# Two-way ANOVA:
a.2 = aov(BWT~factor(SMOKE)*factor(RACE)) # main effects and intaction
summary(a.2) 
summary(aov(BWT~factor(SMOKE)+factor(RACE))) # main effects only


# pairwise comparisons
TukeyHSD(a.2) ##15 pairs in total
plot(TukeyHSD(a.2,'factor(SMOKE):factor(RACE)'))

##  END of 3. ANOVA
################


##############################################################################
##  4. logistic Regression
##############################################################################
##  4. Regression
# still use the lowbwt dataset
# Predictors
# age: mother's age in years
# lwt: mother's weight in lbs
# race: mother's race (1 = white, 2 = black, 3 = other)
# smoke: smoking status during pregnancy
# ptl: no. of previous premature labors
# ht:	history of hypertension
# ui:	presence of uterine irritability
# ftv:no. of physician visits during first trimester

# The response variable, admit/don't admit, is a binary variable.

# view the first few rows of the data
head(mydata)
#We can get basic descriptives for the entire data set by using summary. 
#To get the standard deviations, we use sapply to apply the sd function 
#to each variable in the dataset.
summary(mydata)
sapply(mydata,sd)


par(mfrow=c(1,1))
plot(LOW~AGE,xlab="Age", ylab="LOW baby weight Indicator")
abline(v=30,col='navy',lwd=3)


RACE_f=factor(RACE)
SMOKE_f=factor(SMOKE) 
logit = glm(LOW ~ AGE+LWT+RACE_f+SMOKE_f+PTL+HT+UI+FTV, data = mydata, family = "binomial")

summary(logit)

# get all the coefficients
logit$coef
# get back all of these odds ratios
exp(logit$coef)

# Interpretation of coefficients

# exp(-0.02955) = 0.97088.  That is, each year increases in age decreases your odds of giving 
#to birth to an infant of low birth weight by 3% (1-0.97 = 0.03).

#The odds associated with smoking are exp(0.93885) = 2.557.  
#If you smoke, you increase your odds of giving birth to an infant of low birth weight by 155%.


##  END of 4. Regression
################
#####################################################################


