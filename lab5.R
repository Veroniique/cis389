#Association Rules
#installing packages and adding libraries
install.packages('arules')
install.packages('arulesViz')
library('arules')
library('arulesViz')

#adding Groceries dataset
data(Groceries)
Groceries

#shows first 6 lines
summary(Groceries)
class(Groceries)

#display first 20 rows of the grocery dataset
Groceries@itemInfo[1:20,]

#display the 10th-20th transactions 
apply(Groceries@data[,10:20], 2,
      function(r) paste(Groceries@itemInfo[r,"labels"], collapse=","))

#create frequent sets with Apriori algorithm
itemsets <- apriori(Groceries, parameter=list(minlen=1, maxlen=1,
                                              support=0.02, target="frequent itemsets"))

#summarize the new itemsets
summary(itemsets)

#use inspect to display top 10 frequent 1-itemsets
#item should show 198 times to be considered frequent
inspect(head(sort(itemsets, by="support"), 10))

#making 1 items into 2 items
itemsets <- apriori(Groceries, parameter=list(minlen=2, maxlen=2,
                                              support=0.02, target="frequent itemsets"))
#showing summary of 2 items
summary(itemsets)

#show top 10 most frequent 2 itemsets
inspect(head(sort(itemsets, by="support"),10))

#making 2 items into 3 items
itemsets <- apriori(Groceries, parameter=list(minlen=3, maxlen=3,
                                              support=0.02, target="frequent itemsets"))

#showing 3 item summary
inspect(sort(itemsets, by="support"))

#making 3 items into 4 items
itemsets <- apriori(Groceries, parameter=list(minlen=4, maxlen=4,
                                              support=0.02, target="frequent itemsets"))

#create a frequent set without a maxlength
itemsets <- apriori(Groceries, parameter=list(minlen=1, support=0.02,
                                              target="frequent itemsets"))

#set new rules for the dataset
rules <- apriori(Groceries, parameter=list(support=0.001,
                                           confidence=0.6, target="rules"))

#show summary of rules
summary(rules)

#compute the 1/Support(Y)
slope <- sort(round(rules@quality$lift / rules@quality$confidence, 2))
#display num of times each slope appears in the database
unlist(lapply(split(slope,f=slope), length))

#top 10 rules sorted by lift
inspect(head(sort(rules, by="lift"), 10))

#show confidence levels above 0.9
confidentRules <- rules[quality(rules)$confidence > 0.9]
confidentRules

#create matrix based visualization
plot(confidentRules, method="matrix", measure=c("lift","confidence"))

#create plot on top 5 rules with highest lift
highLiftRules <- head(sort(rules, by="lift"), 5)
plot(highLiftRules, method="graph", control=list(type="items"))


















