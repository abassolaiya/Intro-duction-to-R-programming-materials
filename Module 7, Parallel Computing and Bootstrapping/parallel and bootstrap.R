########################################
# Parallel Computing and the Bootstrap
########################################

library(snowfall)

# Part One: applications of the bootstrap
# Normal data vs bootstrap. Since the data are in fact normal, this interval is perfectly good.

n = 20
sim.data = rnorm(n, mean = 5, sd = 2)
mean.interval = mean(sim.data) + c(-1, 1)*1.96*sd(sim.data)/sqrt(n-1)
theoretical.interval = 5 + c(-1, 1)*1.96*2/sqrt(n-1)

# The bootstrapping procedure. Note that the number of bootstrap replications need not be the same at n, but it's crucial to resample n data points at each bootstrap iteration.

runs = 2000
bs.means = numeric(runs)

for(i in 1:runs){
  tmp = sample(sim.data, n, replace = T)
  bs.means[i] = mean(tmp)
}

bs.interval = quantile(bs.means, probs = c(.025, .975))
mean.interval
bs.interval

# We see the intervals are quite close. Now for an example using the chi-squared distribution with 3 degrees of freedom. First, we plot the density to give an idea of the shape of the distribution

k = 3    # degrees of freedom
seq = seq(0, 7, by = .01)
plot(seq, dchisq(seq, k), type = 'l', ylab = "Density", main = "Density function for a chi-squared(3) random variable", xlab = "x")

n = 50
sim.data = rchisq(seq, k)
normal.interval = mean(sim.data) + c(-1, 1)*1.96*sd(sim.data)/sqrt(n-1)

runs = 2000
bs.means = numeric(runs)

for(i in 1:runs){
  tmp = sample(sim.data, n, replace = T)
  bs.means[i] = mean(tmp)
}

bs.interval = quantile(bs.means, probs = c(.025, .975))
normal.interval
bs.interval

# Next we do the bootstrap for the median and standard deviation

runs = 2000
bs.medians = numeric(runs)
bs.sds = numeric(runs)

for(i in 1:runs){
  tmp = sample(sim.data, n, replace = T)
  bs.medians[i] = median(tmp)
  bs.sds[i] = sd(tmp)
}

bs.median = mean(bs.medians)
bs.sd = mean(bs.sds)
true.median = qchisq(.5, k)
true.sd = sqrt(2 * k)
bs.median
true.median
bs.sd
true.sd

# I haven't been able to find closed form distributions for the median and sd of a chi squared random variable. We could find confidence intervals using questionable normal assumptions, or we could take quantiles from our bootstrapped samples

bs.median.interval = quantile(bs.medians, probs = c(.025, .975))
bs.sd.interval = quantile(bs.sds, probs = c(.025, .975))
bs.median.interval
bs.sd.interval

# Bootstrapping a linear model
data = mtcars[,c(1, 3, 6)]
# We will use mpg as the response, disp and wt as the predictors
runs = 2000
p = 3 # number of coefficients
n = 32 # this is the number of observations
bs.coefs = matrix(NA, nrow = runs, ncol = p)

for(i in 1:runs){
  row.indices = sample(1:n, n, replace = T)
  bs.data = data[row.indices,]
  bs.coefs[i,] = lm(mpg ~ disp + wt, data = bs.data)$coef
}

bs.estimates = apply(bs.coefs, 2, mean)
bs.ci = apply(bs.coefs, 2, quantile, c(0.025, .975))
bs.sd = apply(bs.coefs, 2, sd)
lm1 = lm(mpg ~ disp + wt, data = bs.data)
summary(lm1)
bs.estimates
bs.sd
bs.ci

# Part 2: snowfall

#Basic example to demonstrate setup.

sfInit(parallel = T, cpus = 4)
sfSapply(1:100, exp)
sfStop()

#Bootstrap example demonstrating function definition and computing time.

runs = 10000
cores = 12
elapsed.times = numeric(length(cores))

bs.means = function(index, data){
  n = length(data)
  tmp = sample(data, n, replace = T)
  return(mean(tmp))
}

sim.data = rchisq(10000, 3)

for(i in 1:cores){
  type = (i != 1)
  sfInit(parallel = type, cpus = i)
  sfExport("sim.data")
  t0 = Sys.time() 
  means = sfSapply(1:runs, bs.means, data = sim.data)
  elapsed.time = Sys.time() - t0
  sfStop()
  elapsed.times[i] = as.numeric(elapsed.time, units="secs")
}
plot(elapsed.times, type = 'l', xlab = "Threads", ylab = "Runtimes (sec)", main = "Runtimes Across Multiple Threads")


