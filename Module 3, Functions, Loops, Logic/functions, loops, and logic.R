##################################################################
#     Functions, Loops, and Logic
##################################################################

## Basics of Logic
## The == operator tests for equality. Be sure not to confuse '==' with '='. For this reason, some prefer to use '<-' instead of '='.

a = 1
b <- 1
c = 2
a == b
a == c

if(a == b){
  print("This one is true.")
}else{
  print("This one is false.")
}

# A common mistake is to write "if(a = c)" instead of "if(a == c)"

if(a == c){
  print("This one is true.")
}else{
  print("This one is false.")
}


## Addition Function

add = function(x, y){
  z = x + y
  return(z)
}

add(3, 4)

#Notice that even though the function add() defines a variable called z, that variable does not appear in R's memory. This is because z is a local variable, which only exists inside of the function.

z
# Nothing.
z = 5
z
# Now we have z in memory in the global environment. This will not interfere with the z that appears inside of the add function
add(10, 12)

## Our function also has a method for matrices.
A = matrix(1:4, nrow = 2)
B = matrix(2:5, nrow = 2)
add(A, B)

## But not a method for characters
add("a", "b")

## The ls() function takes no arguments. It lists all the objects in R's current memory
ls()

## You can also define functions with no arguments

say.hi = function(){
  print("E kaasan!")
}
say.hi()

## If we make another function with the same name, it will over write the old function. Let's give say.hi() a name argument

say.hi = function(name){
  text = paste("E kaasan, ", name, "!", sep = '')
  print(text)
}
say.hi(name = "Ian")

## Fucntions can be given a default argument. This lets you run a function without specifying all the arguments. For example, what if you don't know the name of the person you're greeting?

say.hi = function(name = NULL){
  text = paste("E kaasan", name)
  print(text)
}
say.hi()
say.hi(name = "Ian")

## Functions and if() statements work very well together. 

are.they.equal = function(a, b){
  if(a == b){
    print("They are equal.")
  }else{
    print("They are not equal.")
  }
}
are.they.equal(1, 1)
are.they.equal(1, 2)


##Let's modify our say.hi() function to make the proper greeting depending on the time.

say.hi = function(name = NULL, morning= TRUE){
  if(morning == TRUE){
    text = paste("E kaaro", name)
    print(text)
  }else{
    text = paste("E kassan", name)
    print(text)
  }
}
say.hi(name = "Ian")
say.hi(name = "Ian", morning = FALSE)

## See slides.
## Multiple outputs

describe = function(vector){
  x1 = length(vector)
  x2 = mean(vector)
  x3 = median(vector)
  out = list(length = x1, mean= x2, median = x3)
  return(out)
}
describe(c(1, 6, 8))

## We can save the results from this function and extract components using the $ operator
output = describe(c(1, 6, 8))
output$length
output$mean
output$median

## Here's an example we'll use in our next section, nested functions. This function will examine input to determine whether or not the input is a matrix and, if it is, will tell us its dimensions.


examine.matrix = function(mat){
  # The next line will set test to TRUE if the input is a matrix and FALSE if it's not.
  test = (class(mat) == "matrix")
  # The next line is equivalent to if(test == TRUE)
  if(test){
    rows = nrow(mat)
    cols = ncol(mat)
    out = list(is.matrix = test, rows = rows, cols = cols)
    return(out)
  }else{
    out = list(is.matrix = test, rows = 1, cols = 1)
    return(out)
  }
}
A = matrix(1:4, nrow = 2, ncol = 2)
examine.matrix(A)
examine.matrix(4)

examine.matrix(A)$is.matrix
examine.matrix(A)$rows
examine.matrix(A)$cols

## Nested functions. This function will perform either standard or matrix multiplication depending on the kind of input it's given.

multiply = function(x1, x2){
  x1.is.mat = examine.matrix(x1)$is.matrix
  x2.is.mat = examine.matrix(x2)$is.matrix
  are.conformable = (examine.matrix(x1)$cols == examine.matrix(x2)$rows)
  if(are.conformable == FALSE){
    return("Matrices cannot be multiplied.")
  }
  if(x1.is.mat & x2.is.mat & are.conformable){
    return(x1 %*% x2)
  }else{
    return(x * y)
  }
}
A = matrix(1:6, nrow = 2, ncol = 3)
B = matrix(2:7, nrow = 3, ncol = 2)
A
B
x = 4
y = 2
multiply(A, B)
multiply(B, A)
multiply(x, y)

## For loops

for(i in 1:5){
  print("Hello, world!")
}

for(i in 1:5){
  print(paste("I am on iteration", i))
}

for(i in c(1, 3, 5, 7)){
  print(paste("Right now I'm equal to", i))
}

# Functions can be called multiple times by using loops

names = c("Ian", "Olawale", "Sarah", "Kunle")
for(i in names){
  say.hi(name = i)
}

## Functions are useful for performing the same statistical analysis on multiple columns of a data set. For example, consider Fisher's iris data
head(iris)

## We can perform analyses on each numeric column like this
means = numeric(4)
medians = numeric(4)
sds = numeric(4)

for(i in 1:4){
  means[i] = mean(iris[,i])
  medians[i] = median(iris[,i])
  sds[i] = sd(iris[,i])
}
means
medians
sds

## Loops are also helpful for ploting

for(i in 1:4){
  hist(iris[,i])
}

# These labels are awful. But, we can fix them.
xlabs = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width")
mains = paste("Histogram of", xlabs)

for(i in 1:4){
  hist(iris[,i], main = mains[i], xlab = xlabs[i])
}

# While loops
# While loops can emulate for loops
i = 1
while(i <= 5){
  print(paste("I am on iteration", i))
  i = i + 1
}

## While loops are more useful for stochastic processes. For example, how many times do I rolled to roll a die before I get a 6?
rolled.a.six = F
# Here, rolled.a.six is a boolean object, with F short for FALSE. Below we use !rolled.a.six. In R, ! is logical negation, so when rolled.a.six is F, !rolled.a.six is T.
rolls = 0
while(!rolled.a.six){
  rolls = rolls + 1
  result = sample(1:6, 1)
  if(result == 6){
    rolled.a.six = T
  }
}
rolls

## If we run this code a lot, we can see that the answer varies from time to time. In the language of probability, the number of rolls is a random variable. If we could do this, say, 1000 times we could get an estimate of the probability mass function for this random variable. We can do this by nesting loops.

runs = 1000
rolls.until.six = numeric(runs)
for(i in 1:runs){
  rolled.a.six = F
  rolls = 0
  while(!rolled.a.six){
    rolls = rolls + 1
    result = sample(1:6, 1)
    if(result == 6){
      rolled.a.six = T
      rolls.until.six[i] = rolls
    }
  }
}
hist(rolls.until.six)

# This distribution has a name, the geometric distribution. rolls.until.six is a set of 1000 samples from this distribution.

## Apply functions
## We can demonstrate apply() on the numeric parts of the iris data set

iris.numbers = iris[,1:4]

# row means, not too meaningful
apply(iris.numbers, 1, mean)
# Column means
apply(iris.numbers, 2, mean)
# Here's an example of another argument which is passed to the function inside of apply(). The quantile() function needs a vector and a quantile to extract. Here, we apply quantile() to all columns of iris.data and have it extract the .4 quantile.
apply(iris.numbers, 2, quantile, .4)

#tapply()
# Suppose we want to compute the average flower characteristics partitioned by the flower species

tapply(iris[,1], iris$Species, mean)
tapply(iris[,2], iris$Species, mean)
tapply(iris[,3], iris$Species, mean)
tapply(iris[,4], iris$Species, mean)


## sapply()
# This will apply a given function to every element of a given vector or list.

sapply.example = list(scalar1 = x, scalar2 = y, matrix1 = A, matrix2 = B)
sapply(sapply.example, examine.matrix)

## We can use sapply() to get samples from the geometric distribution, as above, without using a for loop. This will come in handy when we move on to parallel computing.

#notice that this function's argument, index, does not appear in the function. This is done so as to work with the sapply() function. In reality, what we want to do is just run this function a bunch of times, so we trick R by running it on the numbers 1 to 1000. This has no practical effect on the function, but lets us use sapply()

roll.trial = function(index = 0){
  rolled.a.six = F
  rolls = 0
  while(!rolled.a.six){
    rolls = rolls + 1
    result = sample(1:6, 1)
    if(result == 6){
      rolled.a.six = T
      return(rolls)
    }
  }
}

samples = sapply(1:1000, roll.trial)
hist(samples)
hist(rolls.until.six)

#######################
# Exercises
#######################
# 1. Write a function that will add 3 scalars by modifying the code below

add3 = function(){
  sum = 
  return(sum)
}

#2. Write a function that will print your name and your home town by modifying the code below.

name.and.home = function(){
  
  
  print(paste(name, home))
}

#3. Write a function that will take a vector, return the sum of all of its elements, and plot it by modifying the code below.

add.and.print = function(){
  plot()
  sum = sum()
  return()
}

#4. There is a data set in r called 'women' which lists the heights and weights of 15 women. Use the apply() function to compute the average height and weight of the women. Then, use apply() to compute the standard deviations of the height and weight. Use the built in functions mean() and sd().

women

#5. Write a function which will compute the body mass index (BMI) of a women. Give the function one argument, a vector with 2 elements. The first element should be the height of the woman and the second the weight of the woman.

bmi = function(){
  height = 
  weight = 
  out = weight/(height^2) * 703
  return(out)
}

#6. Use the apply() function to find the bmi of each women (each row) of the women data set

#7. Using the built in data set mtcars, use tapply() to find the average mpg for the cars with varying numbers of cylinders. You'll have to use the $ to refer to individual rows. For example, for mpg use mtcars$mpg

tapply()

#8. use the roll.trial() function along with sapply() to produce 500 samples from the geometric distribution. What is the average number of rolls needed until a 6 is rolled?










