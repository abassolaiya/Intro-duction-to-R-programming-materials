########################################
# Analysis of Survey Data
########################################

surveydata = read.csv("survey data.csv")
head(surveydata)
summary(surveydata)

# First, it will be easier to interpret our results if we replace the coded 1's and 0's in the factors with words. This way we don't have to constantly think about what a 0 or a 1 means in context.

surveydata$sex[surveydata$sex == 1] = "M"
surveydata$sex[surveydata$sex == 0] = "F"
surveydata$secondary[surveydata$secondary == 1] = "Yes"
surveydata$secondary[surveydata$secondary == 0] = "No"
surveydata$class[surveydata$class == 1] = "Yes"
surveydata$class[surveydata$class == 0] = "No"

# Now, R thinks the values for sex, secondary, and class are character variables. We can make R think of these as factors using the transform() function.

surveydata = transform(surveydata, sex = as.factor(sex), secondary = as.factor(secondary), class = as.factor(class))

# Finally, we can use the same function to reorient the problematic question "camc". 

surveydata = transform(surveydata, camc = 6 - camc)
summary(surveydata)

# Let's group all the 'cam' questions into a new variable camera. We see that these questions are in columns 4 through 1. We can do the scoring by applying the mean() function to each row in those columns

camera = apply(surveydata[,4:10], 1, mean)

# Rather than count the column numbers, we can use the grep() function to extract those numbers for us. grep() is a pattern matching function. We'll see more of grep later, with a different example.

data.names = colnames(surveydata)
grep("cam", data.names)
grep("cam", data.names, value = T)
grep("cell", data.names)
grep("cell", data.names, value = T)
grep("tab", data.names)
grep("tab", data.names, value = T)

# So, we let grep() find the column indices for us. In's also possible to count by hand, but that method is more tedious and error prone.

camera.index = grep("cam", colnames(surveydata))
phone.index = grep("cell", colnames(surveydata))
tablet.index = grep("tab", colnames(surveydata))
camera = apply(surveydata[,camera.index], 1, mean)
phone = apply(surveydata[,phone.index], 1, mean)
tablet = apply(surveydata[,tablet.index], 1, mean)

compiled.data = data.frame(surveydata[,1:3], camera, phone, tablet)

# Now we're in a position to address the research questions. Before we do that, though, we should make some graphical displays and look at the data. For these kinds of data, barcharts are most appropriate.

# Question 1: Does the sex of the customer have an effect on their opinion of our products?
# We can address this with a barchart of the average scores each sex gave each of the products.

cam.by.sex = tapply(compiled.data$camera, compiled.data$sex, mean)
phone.by.sex = tapply(compiled.data$phone, compiled.data$sex, mean)
tablet.by.sex = tapply(compiled.data$tablet, compiled.data$sex, mean)
barplot(cam.by.sex)
barplot(phone.by.sex)
barplot(tablet.by.sex)

# These barplots aren't very good by default. They need , at the very least labels. Let's do one and then use a loop to do all three simultaneously.We'll also use the text() function to add the actual numbers to the plot.

barplot(cam.by.sex, main = "Barplots of Average Camera Scores by Sex", ylab = "Averaged Camera Score", col = terrain.colors(2), ylim = c(0,5))
text(locator(2), as.character(round(cam.by.sex, 2)))

# The locator(n) function allows us to specify n coordinate pairs by clicking on the graph. We can also specify the coordinates in other ways. My preferred way is to use the data to determine the y axis and locator(2) to determine the x axis.

locator(2)
# We see that the x axis coordinates are at .6 and 1.8.
# Now in text, we can give the x coordinates as c(.6, 1.8), and use the cam.by.sex information to decide on the y axis. The argument pos = 3 will place the text above the point we specify.

barplot(cam.by.sex, main = "Barplots of Average Camera Scores by Sex", ylab = "Averaged Camera Score", col = terrain.colors(2), ylim = c(0,5))
text(c(.7, 1.9), cam.by.sex, as.character(round(cam.by.sex, 2)), pos = 3)

## Now that we have the plots produced, we need to export them. We could use the export command from the graphics window, but again, this will be tedious for large numbers of plots. Just as we made the graphs in a loop we can also export using the jpeg() command. Remember to use dev.off() when you're done.

jpeg("camera by sex barplot.jpg")
barplot(cam.by.sex, main = "Barplots of Average Camera Scores by Sex", ylab = "Averaged Camera Score", col = terrain.colors(2), ylim = c(0,5))
text(c(.7, 1.9), cam.by.sex, as.character(round(cam.by.sex, 2)), pos = 3)
dev.off()

# Since we only have 3 barplots to make, we could easily repeat this code 3 times. However, this will become tedious for many plots. Let's use a loop to ease the process. 

# Put all the data into one matrix for easy referencing.
barplot.data = rbind(cam.by.sex, phone.by.sex, tablet.by.sex)

# We need to make objects which contain the parameters for the plots. The paste() function is especially helpful for this kind of thing.

barplot.mains = paste("Barplots of Average",  c("Camera", "Cell Phone", "Tablet"), "Scores by Sex")
y.labels = paste("Averaged",  c("Camera", "Cell Phone", "Tablet"), "Score")

for(i in 1:3){
  barplot(barplot.data[i,], main = barplot.mains[i], ylab = y.labels[i], col = terrain.colors(2), ylim = c(0,5))
  text(c(.7, 1.9), barplot.data[i,], as.character(round(barplot.data[i,], 2)), pos = 3)
}

# Now we will perform the loop, but save the pictures as we go.
filenames = paste(c("Camera", "Cell Phone", "Tablet"), "by sex.jpg")

for(i in 1:3){
  jpeg(filenames[i])
  barplot(barplot.data[i,], main = barplot.mains[i], ylab = y.labels[i], col = terrain.colors(2), ylim = c(0,5))
  text(c(.7, 1.9), barplot.data[i,], as.character(round(barplot.data[i,], 2)), pos = 3)
  dev.off()
}

## It's also possible to put all three bar plots on the same plot, the method I personally prefer. We can do this by giving barplot the whole matrix and setting beside = T. Notice the difference between these next two lines, one with barplot.data and one with it's transpost, t(barplot.data). 

barplot.data
barplot(barplot.data, beside = T)
t(barplot.data)
barplot(t(barplot.data), beside = T)

# There are a few options for the side by side barplot. One is with a legend.
barplot(t(barplot.data), beside = T, main = "Product Scores by Sex", ylab = "Score", col = terrain.colors(2), ylim = c(0, 5), xaxt = 'n')
# xaxt = 'n' suppressed x axis labeling. We do this so we can specify our own labels using the mtext() command
mtext(c("Camera", "Phone", "Tablet"), side = 1, line = 1, at = c(2, 5, 8))
legend("topleft", fill = terrain.colors(2), legend = c("F", "M"), bty = 'n')

#Another option places both factor labels on the x axis.

barplot(t(barplot.data), beside = T, main = "Product Scores by Sex", ylab = "Score", col = terrain.colors(2), ylim = c(0, 5), xaxt = 'n')
# xaxt = 'n' suppressed x axis labeling. We do this so we can specify our own labels using the mtext() command
mtext(c("Camera", "Phone", "Tablet"), side = 1, line = 2, at = c(2, 5, 8))
mtext(rep(c("F", "M"), 3), side = 1, line = 0, at = c(1.5, 2.5, 4.5, 5.5, 7.5, 8.5))
text(c(1.5, 2.5, 4.5, 5.5, 7.5, 8.5), t(barplot.data), as.character(round(t(barplot.data), 2)), pos = 3)
box()

## Similar plots for the class and secondary school breakdown will be performed as exercises. 

##Now I will show a plotting example where looping is necessary: barplots for the answers from all the questions individually. First, lets make a file to keep these pictures in.

dir.create("Pictures")

# Now we can use the working command getwd() and paste() to make a character string for out folder address.

folder.address = paste(getwd(), "/Pictures", sep = '')

for(i in 4:25){
  picture.name = paste(folder.address, "/", colnames(surveydata)[i], ".jpeg", sep = '')
  barplot.data = table(surveydata[,i])
  jpeg(picture.name)
  barplot(barplot.data, main = paste("Barplot for Question", colnames(surveydata)[i]), ylab = "Frequency", col = terrain.colors(5))
  box()
  dev.off()
}

## After producing plots, we can address the research question with a Wilcoxon non parametric test.

wilcox.test(compiled.data$camera ~ compiled.data$sex)
wilcox.test(compiled.data$phone ~ compiled.data$sex)
wilcox.test(compiled.data$tablet ~ compiled.data$sex)

# This output can either be copied from the R terminal or you can write it to a file using sink()

sink("wilcox test output for sex.txt")
wilcox.test(compiled.data$camera ~ compiled.data$sex)
wilcox.test(compiled.data$phone ~ compiled.data$sex)
wilcox.test(compiled.data$tablet ~ compiled.data$sex)
sink()

# Calling sink() with no arguments tells R to resume sending text to the console.

#####################
#Part II: Word Search
#####################

input = read.csv("words.csv")
words = as.character(input[,1])

politics.count = grep("politics", words, ignore.case = T, value = T)
finance.count = grep("finan", words, ignore.case = T, value = T)
leader.count = grep("leader", words, ignore.case = T, value = T)

#####################
# Exercises
#####################

##During the course, we produced barplots, t tests, and pictures for the research question "Does the sex of the customer have an effect on their opinion of our products?" Now we will do similar analyses for whether or not they attended secondary school and whether or not they attended the sensitization class.

#1. Find the mean scores for camera, phone, and tablet broken apart by secondary, as we did for sex earlier

cam.by.secondary = tapply(compiled.data$camera, ,)
phone.by.secondary = tapply()
tablet.by.secondary = tapply()

#2. Now, produce and export barplots for the breakdowns by secondary education just as we did before for sex. I've copied the relevant code from above, you just need to go through and modify as appropriate.

barplot.data = rbind(cam.by.secondary, phone.by.secondary, tablet.by.secondary)

# Update the names here to reflect secondaty education instead of sex

barplot.mains = paste("Barplots of Average",  c("Camera", "Cell Phone", "Tablet"), "Scores by Sex")
y.labels = paste("Averaged",  c("Camera", "Cell Phone", "Tablet"), "Score")

# If you have the labels correct, this code will work just fine as it is.

for(i in 1:3){
  barplot(barplot.data[i,], main = barplot.mains[i], ylab = y.labels[i], col = terrain.colors(2), ylim = c(0,5))
  text(c(.7, 1.9), barplot.data[i,], as.character(round(barplot.data[i,], 2)), pos = 3)
}

# Make sure to change the file names for these new plots or else the old plots will be overwritten.

filenames = paste(c("Camera", "Cell Phone", "Tablet"), "by sex.jpg")

for(i in 1:3){
  jpeg(filenames[i])
  barplot(barplot.data[i,], main = barplot.mains[i], ylab = y.labels[i], col = terrain.colors(2), ylim = c(0,5))
  text(c(.7, 1.9), barplot.data[i,], as.character(round(barplot.data[i,], 2)), pos = 3)
  dev.off()
}

#3. Perform Wilcoxon tests on the scores to see if they significantly differ by secondary school education. Write the output to a file with a good name.


sink(".txt")
wilcox.test(compiled.data$camera ~ )
wilcox.test()
wilcox.test()
sink()

#4. Do questions 1-3 for the class variable.

#5. Hypothesize some other reasons that the health center from part II might fail, for example government interference. Search the words vector for these reasons.
