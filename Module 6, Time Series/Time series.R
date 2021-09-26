percapitas = read.csv("per capita metrics.csv")

library(fUnitRoots)
library(urca)
library(vars)
library(zoo)
install.packages("fGarch")
library(tseries)
library(MSBVAR)
library(fGarch)

y = percapitas[,2]
garchFit(formula = ~ garch(1, 1), data = y, skew = 1, shape = 4)
         

garch(y, order = c(1, 1))        
garchFit(y~garch(1,1))
norm.samples = rnorm(100)
cof=matrix(norm.samples,nrow=10,ncol=10)
Arabica=cof[,1]
Robusta=cof[,2]
Arabica
cof
colnames(cof)=c("a","b","c","d","e","f","g","h","i","j")
cof
plot(Arabica)
#Test for unit roots

adf.test(Robusta)
kpss.test(Arabica)
kpss.test(cof$Arabica)

adf.test(diff(Arabica,1))
adf.test(diff(Robusta,1))
kpss.test(diff(Arabica,1))
kpss.test(diff(Robusta,1))

# Since first order differencing eliminates the unit root, the maximum order of integration
# is concluded to be I(1).

#Granger Test
data=read.csv("projectdata.csv")
data=data[,-1]
pval=numeric(10);pval
for(i in 1:10){ 
  pval[i]= Box.test(data[,i])$p.value
}
pval[i]
granger.test(data,2)
####################################
#VAR Model, lag=2
V.2<-VAR(cof1[,2:3],p=2,type="both")
serial.test(V.2)
###################################
w = rnorm(500,0,1) # 500 N(0,1) variates 2 
v = filter(w, sides=2, rep(1/3,3)) # moving average
par(mfrow=c(2,1))
plot.ts(w, main="white noise")
plot.ts(v, main="moving average")
######################

set.seed(154) # so you can reproduce the results
w = rnorm(200,0,1); x = cumsum(w) # two commands in one line
wd = w +.2; xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main="random walk")
lines(x); lines(.2*(1:200), lty="dashed")
#########################
cs = 2*cos(2*pi*1:500/50 + .6*pi)
w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))

#Excercise 1
#create a matrix parVec that has two columns. Each column contains values for μ and φ respectively:
 N <- 1000
 parVec <- matrix(0,N,2)
parVec[,1] <- runif(N,70,85)
parVec[,2] <- rnorm(N,.0001,.1)
parVec
a =parVec[,1]
b=parVec[,2]
plot(a~b,type="b")


# Plots for per capita data
par(mfrow = c(3, 1))
par(mar = c(2, 4, 2, 2))
plot(ts(percapitas$nig.co2, 1960), type = 'l', ylab = "CO2", main = "Nigerian Economic Data, Per Capita")
plot(ts(percapitas$nig.gdp, 1960), type = 'l', ylab = "GDP")
plot(ts(percapitas$nig.eng, 1960), type = 'l', ylab = "Energy")

plot(ts(percapitas$alg.co2, 1960), type = 'l', ylab = "CO2", main = "Algerian Economic Data, Per Capita")
plot(ts(percapitas$alg.gdp, 1960), type = 'l', ylab = "GDP")
plot(ts(percapitas$alg.eng, 1960), type = 'l', ylab = "Energy")

plot(ts(percapitas$sa.co2, 1960), type = 'l', ylab = "CO2", main = "South African Economic Data, Per Capita")
plot(ts(percapitas$sa.gdp, 1960), type = 'l', ylab = "GDP")
plot(ts(percapitas$sa.eng, 1960), type = 'l', ylab = "Energy")

#Perform granger test
# y = var.lag.specification(tmp[,1:3])
# p = y$results[which(y$results[,3] == min(y$results[,3])),1]
# lags[index] = p
# g.tests[[index]] = granger.test(tmp[,1:3], p)
# 

x=up
up$Date<-mdy(up$Date)
y=nse_asi
for (y$Date <- x$Date) y$Last_Price-x$Last_Price else return(x$Last_Price)
for()
  
  