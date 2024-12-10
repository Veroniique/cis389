#extra credit work
#source: https://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles#google_vignette
#making use of ggplot2
#installing packages + library
install.packages("ggplot2")
library("ggplot2")

#creating a main title, x and y labels + labs
ggtitle("This is my main title")
xlab("CIS 389 Class for Big Data Analytics")
ylab("Grades throughout the quarter")
labs("CIS 389")

#Using Toothgrowth data frame
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
head(ToothGrowth)

#example of plotting on box and whiskers plot
library(ggplot2)
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot()
p

#changing the main title and axis labels
p + ggtitle("PLot of length \n by dose") +
  xlab("Dose (mg)") + ylab("Teeth length")

#changing plot titles using the function labs()
p + labs(title="Plot of length \n by dose",
         x = "Dose (mg)", y = "Teeth length")

#changing legend titles with labs()
#default plot with color
p <- ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) +
  geom_boxplot()
p
#adding new legends
p + labs(fill = "Dose (mg)")

#changing the appearance of the main title and axis labels
#main title
p + theme(plot.title = element_text(family="TT Arial", face="bold", colour="blue", size=14))
#x axis title
p + theme(axis.title.x = element_text(family="TT Arial", face="bold", colour="red", size=14))
#y axis title
p + theme(axis.title.y = element_text(family="TT Arial", face="bold", colour="green", size=14))

#default plot
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot() +
  ggtitle("Plot of length \n by dose") +
  xlab("Dose (mg)") + ylab("Teeth length)")
p
#changing color, the size and the face of
#main title, x and y axis labels
p + theme(
  plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="green", size=14, face="bold")
)

#removing x and y axis labels
#hiding main title and axis titles
p + theme(
  plot.title = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank())








