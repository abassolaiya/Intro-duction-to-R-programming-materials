########################################
# The Metropolis Algorithm
########################################

#########################################
#This code produced the figure on slide 6
#########################################

grid = seq(-5, 3, by = .01)
plot(grid, dnorm(grid), type = 'l', ylim = c(0, .5), xlab = 'x', ylab = 'Density', main = "Example Path for the Metropolis Algorithm")
at = cbind(c(-4, -3, -2, -1, 0, .5, -.4, -.6, 1), dnorm(c(-4, -3, -2, -1, 0, .5, -.4, -.6, 1)))
points(at)
text(at, labels = paste("X", as.character(1:9), sep = ''), pos = 3)

########################################
#This is the actual Metropolis algorithm
########################################

# We will run the chain for 5000 iterations. Remember to produce more samples than you need since you'll have to discard burn in. We will begin the chain at x = 20

target = function(x){
  if(x > 0){
    return(x^2 * exp(-2*x))
  }else{
    return(0)
  }
}

mcmc = 5000
samples = numeric(mcmc)
old.sample = 20


# The following loop will execute the Metropolis Algorithm

for(i in 1:mcmc){
  x.star = rnorm(1, old.sample, 1)
  alpha = target(x.star)/target(old.sample)
  if (runif(1) < alpha){
    old.sample = x.star
  }
  samples[i] = old.sample
}

# In order to determine what level of burn in we need and to assess convergence, 

plot(samples, type = 'l', main = "Trace Plot")
s
# We remove the first 100 samples, treating it as burn in.

samples = samples[-(1:100)]
plot(samples, type = 'l', main = "Trace Plot")

hist(samples, main = "Samples from Target Distribution", freq = F, ylim = c(0, .7))

# Note that the historgam closely follows the density of a gamma(3, 2) density.

grid = seq(0, 6, by = .01)
lines(grid, dgamma(grid, 3, 2), col = 'red')
