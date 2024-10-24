#importing file and creating a variable
sales <- read.csv("C:/Users/veron/OneDrive/Documents/cis389/yearly_sales.csv")

#displays first 6 records of dataset
head(sales)
#gives descriptive stats about dataset
summary(sales)

#plotting data
plot(sales$num_of_orders,sales$sales_total,main="Number of Orders vs. Sales")

#statistical analysis 
results <- lm(sales$sales_total ~ sales$num_of_orders)
results
summary(results)

#plots histogram of residuals with a set break amount
hist(results$residuals, breaks = 800)

#adding column to find the average of orders
sales$per_order <- sales$sales_total/sales$num_of_orders
write.table(sales, "sales_modified.txt", sep="\t", row.names=FALSE)

#installing a package and connecting to the database
install.packages("RODBC")
library(RODBC)
conn <- odbcConnect("training2", uid="user", pwd="password")

#querying database where housing income is greater than $1mil
housing_data <- sqlQuery(conn, "select serialno, state, persons, rooms from housing where hinc > 1000000")
head(housing_data)

#creating new jpeg file
jpeg(file="C:\Users\veron\OneDrive\Documents\cis389\yearly_sales.csv")
#exporting histogram to jpeg file
hist(sales$num_of_orders)
#shutting off graphic device
dev.off()

#numeric, character and logical data types
#creating numeric, character and logical data type variables
#numeric
i <-- 1
#character
sport <- "cycling"
#logical
flag <-- TRUE

#showing characteristics of variable
class(i)
typeof(i)
class(sport)
typeof(sport)
class(flag)
typeof(flag)

#testing for integers
is.integer(i)
j < as.integer(i)
is.integer(j)

#returning element values
length(i)
length(flag)
length(sport)

#vectors
#checking if vectors exist
is.vector(i)
is.vector(flag)
is.vector(sport)

#creating vectors with c 
u <- c("red", "yellow", "blue")
#returns results
u
#returns 1st element
u[1]
#creates vector from 1-5
v <- 1:5
#returns results
v
#sums all vector numbers of v
sum(v)
#creates a vector of even nums
w <- v * 2
#returns results of w
w
#returns 3 element of w
w[3]
#sums 2 vector elements by elements
z <- v + w
#returns results of z
z
#returns true of false if elements are greater than 8
z > 8
#returns only elements higher than 8
z[z > 8]
#returns value greater than 8 or less than 5
z[z > 8 | z < 5]

#more vectors
#creating vector with length of 3
a <- vector(length=3)
#returns false
a
#creating numeric vector with length of 3
b <- vector(mode="numeric", 3)
#returns double
typeof(b)
#adds 3.1 to the 2nd element
b[2] <- 3.1
#returns 3.1 on 2nd element, the rest is 0
b
#length set to 0 for integer vector
c <- vector(mode="integer", 0)
#calling c shows 0
c
#length of c shows 0
length(c)
#length of b shows 3
length(b)
#returns no value
dim(b)

#Arrays and Matrices
#dimensions: 3 regions, 4 quarters and 2 years
#shows the number gained over 
quarterly_sales <- array(0, dim=c(3,4,2))
quarterly_sales[2,1,1] <- 158000
quarterly_sales

#2D matrix of quarterly sales of 3 regions
sales_matrix <- matrix(0, nrow = 3, ncol = 4)
sales_matrix

#imports matrixcalc library
install.packages("matrixcalc")
library(matrixcalc)
#makes a 3x3 matrix
M <- matrix(c(1,3,3,5,0,4,3,3,3),nrow = 3,ncol = 3)
#Multiplies the inverse
M %*% matrix.inverse(M)

#reading data from file and checking if data is there
sales <- read.csv("C:/Users/veron/OneDrive/Documents/cis389/yearly_sales.csv")
is.data.frame(sales)

#returns order numbers
length(sales$num_of_orders)

#checks if objects are vector
is.vector(sales$cust_id)
is.vector(sales$sales_total)
is.vector(sales$num_of_orders)
is.vector(sales$gender)
#checks if object is a factor
is.factor(sales$gender)

#shows a structure of the data frame
str(sales)
#extracts 4th column of data frame
sales[,4]
#extracts gender column of data frame
sales$gender
#shows first 2 rows of data frame
sales[1:2,]
#gets the first, third and fourth columns
sales[,c(1,3,4)]
#gets the cust_id and sales_total columns
sales[,c("cust_id","sales_total")]
#gets all data that shows female
sales[sales$gender=="F",]

#type of classes
class(sales)
typeof(sales)

#lists
#creating a list with a string, number, another list and a vector
housing <- list("own","rent")
assortment <- list("football", 7.5, housing, v, M)
assortment

#examine 5th object in list
class(assortment[5])
#length of variable
length(assortment[5])
#type of variable
class(assortment[[5]])
#amount of characters in variable
length(assortment[[5]])

#factors
#checks class
class(sales$gender)
#check if factor is ordered
is.ordered(sales$gender)

#displays first 6 values
head(sales$gender)

install.packages("ggplot2")
library(ggplot2)
#loading data frame
data(diamonds) 
#gives the whole structure of the diamond package
str(diamonds)
#displays first 6 values + levels
head(diamonds$cut)

#making empty character vector to same length as sales
sales_group <- vector(mode="character", length=length(sales$sales_total))
#group customers to sales amount
sales_group[sales$sales_total<100] <- "small"
sales_group[sales$sales_total>=100 & sales$sales_total< 500] <- "medium"
sales_group[sales$sales_total>=500] <- "big"
#creating and adding ordered factor to sales data frame
spender <- factor(sales_group, levels=c("small","medium","big"),  ordered = TRUE)
#combining values
sales <- cbind(sales,spender)
str(sales$spender)
head(sales$spender)

#building a table
sales_table <- table(sales$gender,sales$spender)
sales_table

#checking results
class(sales_table)
typeof(sales_table)
dim(sales_table)

#doing a chi-squared test
summary(sales_table)

#descriptive statistics
summary(sales)

#simplifying function calls
x <- sales$sales_total
y <- sales$num_of_orders

#using math functions
cor(x, y)
cov(x, y)
IQR(x)
mean(x)
median(x)
range(x)
sd(x)
var(x)

#calcs the s.d for first 3 variables in sales
apply(sales[,c(1:3)], MARGIN=2, FUN=sd)

#function providing difference between max and min values
my_range <- function(v) {range(v)[2] - range(v)[1]}
my_range(x)

summary(data)

x <- rnorm(50)
y <- x + rnorm(50, mean=0, sd=0.5)

data <- as.data.frame(cbind(x, y))
summary(data)

library(ggplot2)
ggplot(data, aes(x=x, y=y)) + 
  geom_point(size=2) +
  ggtitle("Scatterplot of X and Y") +
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=14),
        plot.title = element_text(size=20, face="bold"))

#load in new dataset
data(anscombe)
anscombe

#show num of rows
nrow(anscombe)

#checks which group each data point belongs to
levels <- gl(4, nrow(anscombe))
levels

#grouping into a data frame
mydata <- with(anscombe, data.frame(x=c(x1,x2,x3,x4), y=c(y1,y2,y3,y4),mygroup=levels))
mydata

#creating scatterplot with ggplot2 package
library(ggplot2)
#setting color theme
theme_set(theme_bw())

#creating the 4 plots
ggplot(mydata, aes(x,y)) +
  geom_point(size=4) +
  geom_smooth(method="lm", fill=NA, fullrange=TRUE) +
  facet_wrap(~mygroup)

#creating vector for age
age <- c(25, 30, 35, 40)
#making a graph to display age
hist(age, breaks=100, main="Age Distribution of Account Holders", 
     xlab="Age", ylab="Frequency", col="gray")

#creating a vector and using is.na to look for missing values
x <- c(1, 2, 3, NA, 4)
is.na(x)

#shows the mean
mean(x)
#removes the missing values when checking for mean
mean(x, na.rm=TRUE)

#using the na.exclude function to return object with incomplete cases removed
DF <- data.frame(x = c(1, 2, 3), y = c(10, 20, NA))
DF
DF1 <- na.exclude(DF)
DF1

#create vector mortgage then put it into histogram
mortgage <- c(0, 2, 4, 6, 8, 10)
hist(mortgage, breaks=10, xlab="Mortgage Age", col="gray",
     main="Portfolio Distribution, Years Since Origination")

#bring in dataset
data(mtcars)
#create a dotchart of dataset
dotchart(mtcars$mpg, labels=row.names(mtcars), cex=.7,
         main="Miles Per Gallon(MPG) of Car Models",
         xlab="MPG")
#create barplot of dataset
barplot(table(mtcars$cyl), main="Distribution of Car Cylinder Counts", 
        xlab="Number of Cylinders")

#randomly generate 4000 observations from the log normal distribution
income <- rlnorm(4000, meanlog = 4, sdlog = 0.7)
summary(income)

#income * 1000
income <- 1000 * income
summary(income)

#creating histogram from data
hist(income, breaks=500, xlab="Income", main="Histogram of Income")
#creating density plot
plot(density(log10(income), adjust=0.5), 
     main="Distribution of Income (log10 scale)")
#adding rug to density plot
rug(log10(income))

#loading the diamonds dataset
data(diamonds)

#keep premium + ideal cuts of diamonds
niceDiamonds <- diamonds[diamonds$cut=="Premium" |
                         diamonds$cut == "Ideal",]
summary(niceDiamonds$cut)

#density plot for diamond prices
ggplot(niceDiamonds, aes(x=price, fil=cut)) +
  geom_density(alpha = .3, color=NA)

#density plot for diamond prices of log10
ggplot(niceDiamonds, aes(x=log10(price), fill=cut)) +
  geom_density(alpha = .3, color=NA)

#generate 75 numbers between 0-10
x <- runif(75, 0, 10)
x <- sort(x)
y <- 200 + x^3 - 10 * x^2 + x + rnorm(75, 0, 20)
#using linear regression
lr <- lm(y ~ x)
#loess
poly <- loess(y ~ x)
#fitting nonlinear line
fit <- predict(poly)
#plot points
plot (x,y)
#draw fitted line for linear regression
points(x, lr$coefficients[1] + lr$coefficients[2] * x,
       type = "l", col=2)
#draw the fitted line with Loess
points(x, fit, type = "l", col="4")

#dotchart and barplot
#dotchart of mtcars
cars <- mtcars[order(mtcars$mpg),]
#group variable
cars$cyl <- factor(cars$cyl)
#adding colors
cars$color[cars$cyl == 4] <- "red"
cars$color[cars$cyl == 6] <- "blue"
cars$color[cars$cyl == 8] <- "darkgreen"
#plot dotchart
dotchart(cars$mpg, labels=row.names(cars), cex=.7, groups = cars$cyl,
         main="Miles Per Gallon(MPG) of Car Models\nGrouped by Cylinder",
         xlab="Miles Per Gallon", color=cars$color, gcolor="black")

#barplot of mtcars
counts <- table(mtcars$gear, mtcars$cyl)
barplot(counts, main="Distribution of Car Cylinder, Counts and Gears",
        xlab="Number of Cylinders", ylab="Counts",
        col=c("#0000FFFF", "#0080FFFF", "#00FFFFFF"),
        legend = rownames(counts), beside=TRUE,
        args.legend = list(x="top", title = "Number of Gears"))
        
#box and whisker plot
#add ggplot library
library(ggplot2)
# Createing zcta dataframe
zcta <- data.frame(
  MeanHouseholdIncome = c(55000, 75000, 68000, 62000, 80000),
  MeanEducation = c(12, 16, 14, 13, 17),
  Zip1 = c(98146, 44928, 82304, 94720, 20472)
)

#error fix here p95,96
ggplot(data=zcta, aes(x=as.factor(Zip1), y=log10(MeanHouseholdIncome))) +
  geom_point(aes(color=factor(Zip1)), alpha=0.2, position="jitter") +
  geom_boxplot(outlier.size=0, alpha=0.1) +
  guides(colour="FALSE") +
  ggtitle("Mean Household Income by Zip Code")

#hexbin plot
#plotting data points
plot(log10(MeanHouseholdIncome) ~ MeanEducation, data=zcta)
#add straight fitted line of linear regression
abline(lm(log10(MeanHouseholdIncome) ~ MeanEducation, data=zcta), col="red")

#installing hexbin package
install.packages("hexbin")
library(hexbin)
#
hexbinplot(log10(MeanHouseholdIncome) ~ MeanEducation,
          data=zcta, trans = sqrt, inv = function(x) x^2, type=c("g","r"))

#scatterplot matrix
#defining colors
colors <- c("red", "green", "blue")

#draw matrix plot
pairs(iris[1:4], main="Fisher's Iris Dataset",
      pch = 21, bg = colors[unclass(iris$Species)] )

#set clip plotting to figure region
par(xpd = TRUE)

#add legend
legend(0.2, 0.02, horiz = TRUE, as.vector(unique(iris$Species)),
       fill = colors, bty="n")

#density plot
#generate random log normal income data
income = rlnorm(5000, meanlog=log(40000), sdlog=log(5))
#part 1 - creating density plot
plot(density(log10(income), adjust=0.5),
     main="Distribution of Account Values (log10 scale)")
#adding rug to density plot
rug(log10(income))
#part 2 - make the histogram
#create log like bins
breaks = c(0, 1000, 5000, 10000, 50000, 100000, 5e5, 1e6, 2e7)
#creating bins and labeling the data
bins = cut(income, breaks, include.lowest=T,
           labels = c("< 1K", "1-5K", "5-10K", "10-50K", 
                      "50-100K", "100-500K", "500K-1M", "> 1M"))
#plotting bins
plot(bins, main = "Distribution of Account Values",
     xlab = "Account value ($ USD)",
     ylab = "Number of Accounts", col="blue")

#generate random observations from the two populations
#normal distribution centered at a 100
x <- rnorm(10, mean=100, sd=5)
#normal distribution centered at 105
y <- rnorm(20, mean=105, sd=5)

#run t test
t.test(x, y, var.equal=TRUE)

#obtain t value for a two-sided test at a 0.05 significance level
qt(p=0.05/2, df=28, lower.tail=FALSE)

#run Welch's t-test
t.test(x ,y, var.equal=FALSE)

#running wilcox
wilcox.test(x, y, conf.int = TRUE)

#customers visiting a website, getting promotional offers
offers <- sample(c("offer1", "offer2", "nopromo"), size=500, replace=T)

#simulated 500 observations of purchase sizes on the 3 offer options
purchasesize <- ifelse(offers=="offer1", rnorm(500, mean=80, sd=30),
                       ifelse(offers=="offer2", rnorm(500, mean=85, sd=30),
                              rnorm(500, mean=40, sd=30)))

#creating data frame of offer option and purchase size
offertest <- data.frame(offer=as.factor(offers),
                        purchasesize_amt=purchasesize)

#display a summary of offertest where offer="offer1"
summary(offertest[offertest$offer=="offer1",])

#displaying a summary of offertest where offer="offer2"
summary(offertest[offertest$offer=="offer2",])

#display summary of offertest where offer="nopromo"
summary(offertest[offertest$offer=="nopromo",])

#fitting ANOVA test
model <- aov(purchase_amt ~ offers, data=offertest)

#model summary
summary(model)

#add Tukey model
TukeyHSD(model)






































