########################################
# Rejection Sampling
########################################


# Target function from which we want samples, in this case a beta(2, 2) density

target = function(x){
  6 * x * (1 - x)
}

grid = seq(0, 1, by = .01)
plot(grid, target(grid), type = 'l', main = "Target Density", ylab = "f(x)", col = 'red')

# Now the rejection sampler
#These two are the limits for the x axis
support.min = 0
support.max = 1

# This is the height of the function. It needs to be greater than the maximum of the target density.
ceiling = 1.6
proposal = function(x, support.min, support.max, ceiling){
  dunif(x, support.min, support.max) * ceiling * (support.max - support.min)
}
plot(grid, proposal(grid, support.min, support.max, ceiling), type = 'l', ylim = c(0, 1.7), ylab = "Density", xlab = "x", main = "Plot of Target Density and Proposal Density")
lines(grid, target(grid), col = 'red')
legend(.4, .5, legend = c("Target: g(x)", "Proposal: Mf(x)"), fill = c("red", "black"), bty = 'n')

# This loop performs the actual calculations

target.draws = numeric(0)
draws.count = 0
tries = 0
number.of.samples = 5000
while(draws.count < number.of.samples){
  x.star = runif(1, support.min, support.max)
  prob = target(x.star)/proposal(x.star, support.min, support.max, ceiling)
  if(runif(1) < prob){
    target.draws = c(target.draws, x.star)
    draws.count = draws.count + 1
  }
  tries = tries + 1
}

efficiency = draws.count / tries

# Examine the histogram of the draws with the true density superimposed. Notice that it adheres to the density very well.
hist(target.draws, breaks = 60, freq = F, main = "Samples from Target Using Rejection Sampling", xlab = "x", sub = "Red line is true density")
lines(grid, target(grid), col = 'red')

#########################################
# Here's a more complicated example, using a type IV generalized logistic distribution with alpha = beta = 2
#########################################

target = function(x){
  6 * exp(-2 * x) / ((1 + exp(-x))^4)
}

grid = seq(-5,  5, by = .01)
plot(grid, target(grid), type = 'l', main = "Target Density", ylab = "f(x)", col = 'red')

# Notice that this density is defined on the whole real line, so it is impossible to choose a uniform proposal which will cover it. To get around this, we can either use a different proposal or, the easier way, truncate the density to a range outside of which there is very little probability. We see that outside of +/- 5 there is almost 0 probability, so we will do that.

support.min = -5
support.max = 5

ceiling = .5
plot(grid, proposal(grid, support.min, support.max, ceiling), type = 'l', ylim = c(0, .6), ylab = "Density")
lines(grid, target(grid), col = 'red')

# This loop performs the actual calculations

target.draws = numeric(0)
draws.count = 0
tries = 0
number.of.samples = 50000
while(draws.count < number.of.samples){
  x.star = runif(1, support.min, support.max)
  prob = target(x.star)/proposal(x.star, support.min, support.max, ceiling)
  if(runif(1) < prob){
    target.draws = c(target.draws, x.star)
    draws.count = draws.count + 1
  }
  tries = tries + 1
}

efficiency = draws.count / tries

hist(target.draws, breaks = 60, freq = F, main = "Samples from Target Using Rejection Sampling", xlab = "x", sub = "Red line is true density")
lines(grid, target(grid), col = 'red')


