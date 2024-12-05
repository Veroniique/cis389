#importing libraries
library(ggplot2)
library(dplyr)

#import lego data set
lego <- read.csv("C:/Users/veron/OneDrive/Desktop/CIS389/final/lego_sets_and_themes.csv")

#show detailed summary of lego dataset
summary(lego)

#first 6 data entries
head(lego)

#last 6 entries of dataset
tail(lego)

#filtering my lego to specific theme categories
lego_filtered <- lego %>%
  filter(theme_name %in% c("Creator", "Technic", "Racers", "City", "Town", "Brickheadz"))

#bar chart of my favorite themed sets, shows total number
ggplot(lego_filtered, aes(x = factor(theme_name))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Top 6 favorite Lego Themes", x = "Theme Name", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#line chart of themes released over the years
ggplot(lego_filtered, aes(x = year_released, group = theme_name, color = theme_name)) +
  geom_line(stat = "count") +
  theme_minimal() +
  labs(title = "Lego Themes Over Time", x = "Year", y = "Number of Sets")

#Favorite themes in pie chart showing by %
#calculate percentage of each theme
theme_percent <- round((theme_counts / sum(theme_counts)) * 100, 1)
#add labels
label_percent <- paste(names(theme_counts), "(", theme_percent, "%)", sep="")
#add pie chart
pie(theme_counts, labels = label_percent,
    main = "My Top Lego Themes",
    col = rainbow(length(theme_counts)))
