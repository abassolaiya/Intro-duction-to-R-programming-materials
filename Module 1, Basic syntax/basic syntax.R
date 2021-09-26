#######################################################################
#                 Basic Syntax
#######################################################################
#Note: 
# is used for comments.
# R is case sensitive.Most often,use lower case letters.
# There is a large repository of documentation in R to help you.
# Type ??topic or help(topic) to get help in r or use google.
# Try to run these codes along with me as I teach.
# Use control +enter on PC or command +enter on Mac,or simply click on run at the top of this pane to run the codes.
# Ask Ian for any assistance with these codes.
# 1. Object creation:
#a. Expression
3+7
exp(3)
2*5
10/4
2*5+10/4
log(5)
#b. Assignment
#You can use either "=" or an arrow "<-" to assign a variable.
a=2*5+10/4
a=a+4
b<-exp(5)
#We can also use . and _ 
sum.1=3+5
exponent_2=exp(2)
c=2*3
d=10/4
exponent_2
d
ls() # Lists all the elemnts of the workspace
#rm() removes element from the workspace
rm(sum.1)
ls()
# 2. Vectors
# Sequences.
vec1=1:10
vec2=1:-10
vec3=seq(0,5,by=.5)
length(vec1) # Gives the length of the vector.
vec4=seq(0,5,length=15)
v1=seq(1,3,by=0.5)
v2=seq(1,3,length=5)
# Vectors with no pattern
#Expression
c(1,2,3,4,5)
c(2,-1,7)
#Assignment
x=c(1,2,3,4,5)
y=c(2,-1,7,0,1)
#Vectors of characters
char=c("aa","bb", "cc", "dd")
position=c(VT = "first", UVA = "second", RU = "third")
#repeating(replicating) values
rep1=rep(1,5)
rep2=rep("a",4)
rep3=rep(c(2,6,3),each=5)
rep4=rep(c("a","b"),c(5,2))
#Arithmetic with vectors
x+1
2*x
x^2
2^x
x+y
x*y #Coordinate by coordinate product
x%*%y #Dot product
v1+vec1
sum(x)
sort(y)
round(vec4)
round(vec4,2)
#Logical operations
x<3 #Tells whether each value is less than 3
x==3 #Tells whether each value is exacty 3
x>=3
x<=3
#Random Sampling
#We can sample from a vector of numbers by using the sample command
sample(vec4,5) #Gives us a random sample of five numbers from vec4
sample(letters,10)
sample(vec4,5,replace=T)
#Generating random samples from probability distributions
runif(10,2,5)
rnorm(10,5,2)
rbinom(10,3,0.2)
rexp(10,0.1)
rpois(10,2)
#etc
#Subsets
q=c(1:10)
q[2:5]
q[c(1,3)]
q[c(1:3)]
q[-4]
# 3. Matrices
#Creation
mat1=matrix(c(1,2,3,4,5,6),nrow=2,ncol=3)
mat2=matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=TRUE)
mat3=matrix(vec4,nrow=3,ncol=5)
mat4=matrix(c(2,4,5,7,3,1),2,3)
#Functions on matrices
dim(mat1)      #Useful for knowing the number of observations and variables in your data set.

dim(mat1)[1] #tells us the number of rows
dim(mat1)[2]# tells the number of columns
dim(mat2)
dim(mat3)
#cbind and rbind command
q1 = c(1, 1, 2)
q2 = c(2, 2, 2)
q3 = c(0, 1, 0)
mat5=cbind(q1,q2,q3) #binds all the columns together as a matrix
mat6=rbind(q1,q2,q3) #binds them as rows
#Diag Function
           I4=diag(4) #creates an identity matrix of diagonal 4
           mat7=2*I4
           
           # Operations
           #addition
           mat1+mat2
           mat1+5
           #Subtraction
           mat1-mat2
           mat2-5   
           # Inverse.Solve function is used for inverse.
           solve(I4) 
           solve(mat6)
           # Transpose
           t(mat1)
           t(mat2)
           #Element-wise multiplication
           mat1*mat2
           # Matrix multiplication
           t(mat1)%*%mat1
           t(mat2)%*%mat2
           
           # Subsets
           #Referencing a cell:useful when you want to reference a particular observation in your data 
           mat1[1,1]
           mat1[1,2]          
           mat1[2,1]
           mat1[2,2]
           #referencing a row:
           mat1[1,]
           mat1[2,]
           #referencing a column:
           mat1[,1]
           mat1[,2]
           mat1[,3]           
           mat1[,c(1,3)] #To view the first and third column
           #######################################################################
           #                         Data Import
           ####################################################################### 
getwd()
           setwd("/Users/olawale/Desktop/Short Course Folder")
           mydatacsv<- read.table('prices.csv', sep=',', header=T)
           mydatatxt<- read.table('prices.txt', sep='\t', header=T)       
           mydatacsv
           mydatatxt
           
           #######################################################################
           #                      Exploratory Data Analysis
           #######################################################################
            #First,lets do practice excercise 1 on the slides.
           #Exploratory Data Analysis: 
           mean(SQFT)
           var(SQFT)
           min(SQFT)
           max(SQFT)
           median(SQFT)
           
           summary(SQFT) 
           
           #Exploratory Data Analysis: Graphs

           hist(SQFT, main="Histogram of square feet of living space", col="dodgerblue", breaks=10)
           boxplot(SQFT, main="Boxplot of square feet of living space", col="khaki1", ylab="SQFT")

           boxplot(SQFT~mydatacsv[,4])
           boxplot(SQFT~mydatacsv[,4],col="pink")
           boxplot(SQFT~mydatacsv[,4],col=c("green","yellow"))          
           qqnorm(SQFT, main="Normal QQ Plot SQFT")
           
           #######################################################################
           #                             Loops
           #######################################################################           
           
           #For Loops
           for(i in 1:10){
             print(i)
           }
           
           for(i in 1:3){
             print(median(mydatacsv[,i]))
           }      
           
           #While loops
           x <- 1
           while(x < 15) {
             x <- x+1; 
             print(x);
           }
           
           #Printing example: Rows 1-5
           i=1           
           while(i<=5){
             print(mydatacsv[i,])
             i=i+1
           }          
           
           #######################################################################
           #                             If/Else Statements
           #######################################################################           
           
           # if statement
           #Print if NE=1           
           for(i in 1:dim(mydatacsv)[1]){
             if(mydatacsv[i,4]=="1"){
               print(mydatacsv[i,])
             }
           }    
           
           # if/else statement:           
                     
          x=1:10
          for(i in 1:10){
          if(x[i]>=5)
          print(T)
          else
          print(F)
          } 
           #NE=1 example
           for(i in 1:dim(mydatacsv)[1]){
             if(mydatacsv[i,4]=="1")
               print("NE")
             else
               print("other")
           }   
           
           # if/else if/else statement:   
           
           
           #######################################################################
           #                             Exporting data
           #######################################################################                      
           write.csv(mydatacsv,file="mydatacsv.csv")  
           write.csv(vec2,file="vec2.csv") 
           write.table(mydatacsv,file="mydatatxt.txt", sep=" ")
           write.table(mydatacsv,file="mydatatxt2.txt", sep="\t")  
           
           
           
           
           #########################################################################
           #Practice Excercise 2a
           #########################################################################
           date()
           getwd()
           
           setwd("/users/olawale/desktop/Short Course Folder")
           public<- read.table('pubfile.csv', sep=',', header=T)
           date()
           dim(public)           
           povpct=public[,35]
           ms= public[,5]          
           m=mean(povpct)
           var=var(povpct)
           min=min(povpct)
           max=max(povpct)
           med=median(povpct)           
           sumvec=c(m,var,min,max,med)         
           summary(povpct)
           
          
           hist(povpct, main="Histogram of povpct", col="tomato", breaks=20)
           box()
           
           #######################################################################
           #                             Practice 2b . 
           #######################################################################
           boxplot(povpct)
           boxplot(povpct, main="Boxplot of povpct", col="limegreen", ylab="povpct") #same color for all boxes                      
           boxplot(povpct~ms,col="blue", main="Boxplot of povpct by Marital Status")           
           boxplot(povpct~ms,col=c("blue", "blue", "green", "yellow", "red", "yellow"), main="Boxplot of povpct by Marital Status")     #Different color in some boxes      
           qqnorm(povpct, main="Normal QQ Plot povpct") 
           
           count=0           
           for(i in 1:dim(public)[1]){
             if (public[i,20]==1){ #Each cell in column 20 equals 1
               if(public[i,2]<15)count=count+1  #Each cell in column 2 is lower than 15
             }
           }
           count
           
           write.csv(povpct,file="povpct.csv") 
           write.table(public,file="public.txt", sep="\t")
           