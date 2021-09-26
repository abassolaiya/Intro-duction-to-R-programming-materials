########################################
#      R Graphics
########################################

cereals = read.table("cereals.txt", header = T)
attach(cereals) # be very careful with attaching

# Section 1
# 1.1: plot()
# plot() makes a scatterplot of two numeric variables.
head(cereals)
plot(calories, carbo)
?plot

# Lets make axis lables and titles using main, xlab, ylab and sub

plot(calories, carbo, main = "Plot of Calories vs Carbohydrates", xlab = "Calories", ylab = "Carbohydrates", sub = "cereal data")

# The default type is "p" for point. Using "l" for line is useful for displaying functional relationships

x = seq(-10, 10)
y = x^2
plot(x, y, type = 'l')
# alternatively:
plot(x, x^2, type = 'l')

# Here's a stats classic to demonstrate plot() with one positional argument

x = seq(-3, 3, by = .01)
normal.curve = dnorm(x)
plot(normal.curve, type = 'l', main = "The Normal Density")

# Once the plot is produced in R it can be exported for use in other programs such as Word, LaTeX or what have you. This is most easily done through the export button in the plotting window. Plotting through code will be covered later.

# 1.2: hist()
# hist() creates a histogram for one numerical data vector by grouping it into 'bins' and giving the count or proportion of data in those bins. It's useful for gettng a sense of the distribution of your data.

hist(calories)

my.histogram = hist(calories)
plot(my.histogram)
my.histogram

# 1.3 boxplot()
# box-and-whisker plots show the interquartile range, median and outliers for groups of numeric data.

boxplot(calories)

# This is useful for visually comparing the effects of a grouping variable.

boxplot(calories ~ mfr)

# By default, boxplot is rather sparse. We can add lables using the same options as in plot. We can also make the group names a little more explicit.

box.main = "Boxplots of Calories by Manufacturer"
xlable = "Manufacturer"
ylable = "Calories"
box.names = c("American", "General Mills", "Kelloggs", "Nabisco", "Post", "Quaker", "Ralston")

boxplot(calories ~ mfr, main = box.main, xlab = xlable, ylab = ylable, names = box.names)

# 1.4 barplot()
# Barplots show the counts of observations in specific groups. The function expects the data to be entered in a slightly
# different way, namely it wants a a vector of bar heights. This function is most easily used with the table() function.

# Lets make a barplot showing the number of cereals from each manufacturer
# The intuitive and wrong way to go:

barplot(mfr)

# Instead, use:

mfr.counts = table(mfr)
mfr.counts
barplot(mfr.counts)

# Or just:

barplot(table(mfr))

# Now, labels!

barplot(mfr.counts, main = "Number of cereals by each manufacturer", xlab = "Manufacturer", ylab = "Count of cereals")

# 1.5 pie()
# In general, statisticians dislike pie charts. In fact, R discourages their use in their own help documentation!
# pie() accepts data the same way as barplot(), that is, it's best to use table() first.

pie(mfr.counts, main = "Number of cereals by each manufacturer")

# Pie charts aren't good for statistical displays of information but are sometimes requested by clients.

# Section 2
# 2.1: points & legend
# Sometimes you'll need to plot additional points or lines on top of a graph. The lines() function can accomplish
# both of these things by selecting type = 'p' for points and type = 'l' for lines. We'll focus on points for now.

# Lets compare protein and carbohydrates to total calories. Since both of these variables are measured in units of grams, we can graph them on the same y axis. We'll need to change the y axis since there'll be more than one variable. The natural choice is to give the units of measure.

plot(calories, protein, ylab = "grams")
lines(calories, carbo, type = 'p') 

# One danger of this approach is that the window might not be the right size. We can use the range() fuction to verify.
range(protein)
range(carbo)

# We can fix the range using the ylim argument.

plot(calories, protein, ylim = c(-2, 25), ylab = "grams")  #We don't worry about xlim.
lines(calories, carbo, type = 'p')

# Now we know all our points fit, but we can't tell them apart! We need to distinguish which points correspond to what data and provide some sort of legend. We can use the option pch (plotting character) to plot different shapes.

plot(calories, protein, ylim = c(-2, 25), pch = 3, ylab = "grams")
lines(calories, carbo, type = 'p', pch = 4)

# Now we add a legend using the legend() command. Legends can be placed 3 ways: (x, y) coords (don't), keywords or locator(1).

?legend
legend = c("Protein", "Carbohydrates")
pch = c(3, 4)
legend("topleft", legend = legend, pch = pch)

# We can also use the locator(1) function. This function lets you click on the graph and it will return the coordinates of the point you clicked.

locator(1)

# Using this lets you click on where you want the legend to be. I'm also including the option bty = 'n' (box type) because I prefer legends without boxes. Note we have to do plot() and lines() again since you can't remove graphics on a plot

plot(calories, protein, ylim = c(-2, 25), pch = 3, ylab = "grams")
lines(calories, carbo, type = 'p', pch = 4)
legend(locator(1), legend = legend, pch = pch, bty = 'n')

# Perhaps we'd rather use different colors to distinguish the two units. Use col instead of pch. How did I know to use col? There are a lot more graphical parameters to be found by looking at par.

?par

# Colors can be specified by number (don't) or by keyword. See Rcolors.pdf.

plot(calories, protein, ylim = c(-2, 25), col = "green", ylab = "graphs")
lines(calories, carbo, type = 'p', col = "red")

# To get colors in a legend we need to set the fill option

legend("topleft", legend = legend, fill = c("green", "red"), bty = 'n')

# 2.2: lines
# There are two ways to add lines to plots. You can use lines() and supply a list of (x, y) coordinates or you can use abline and give it a y-intercept and a slope (for lines of the form y = a + b*x).

# A common usage of lines in stats is for drawing regression or best-fit lines over data.

plot(calories, rating, main = "Plot of Rating vs Calories", xlab = "Rating", ylab = "Calories")

# It looks like there may be a linear relationship between the variables. Using the lm() command to fit a line, we find that the best fit line is rating = 95.788 - 0.497*calories. We can plot this two ways:

plot(calories, rating, main = "Plot of Rating vs Calories", xlab = "Rating", ylab = "Calories")
abline(95.788, -0.497)

#or:

lm1 = lm(rating~calories)
plot(calories, rating, main = "Plot of Rating vs Calories", xlab = "Rating", ylab = "Calories")
abline(lm1$coefficients)


# It's helpful to include the slope and intercept of a line when making these kinds of plot. Text can be added to a plot by using the text() command and giving it coordinates and character strings. We can use locator again. Note that the point you select is the midpoint of the string, so plot accordingly.

text(locator(1), labels = "rating = 95.788 - 0.497*Calories")

# Section 3

#3.1: Histogram color overlay example

irisdata<-iris
setosa = irisdata[1:50,1:4]
versicolor = irisdata[51:100,1:4]
virginica = irisdata[101:150,1:4]
par(las=1)
sepl1 = hist(setosa[,1],breaks=seq(4,9,.5),col=rgb(0,0,1,1/4),ylim=c(0,25),
             main='Histogram color overlay',sub="Adapted from J. Anderson's HW2", xlab='Sepal Length')                     
sepl2 = hist(versicolor[,1],add=T,col=rgb(1,0,0,1/4),) 
sepl3 = hist(virginica[,1],add=T, col=rgb(0,1,0,1/4))


legend(x=7,y=22,legend=c('Setosa','Versicolor','Virginica'),col=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4),rgb(0,1,0,1/4)),pch=15)

