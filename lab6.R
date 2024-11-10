#lab 6: 7 November 2024

#setting working directory
setwd("C:/Users/veron/OneDrive/Desktop/CIS389/week6/Rstatistics")
#show directory location
getwd()
#show the files in the dataSets folder
list.files("dataSets")

#load in the datasets
#read the states datasets
states.data <- readRDS("dataSets/states.rds")
#show label
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#show the last labels
tail(states.info, 8)

#linear regression
#examining the data
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
#show a summary of the data
summary(sts.ex.sat)
#finding correlation between expense and csat
cor(sts.ex.sat)

#plotting data
#creating a scatterplot of data
plot(sts.ex.sat)

#filtering linear regression model
#the formula with the dataset
sat.mod <- lm(csat ~ expense,
              data=states.data)
#summarize the results
#shows regression coefficients table
summary(sat.mod)

#create a summary of expense and percent
#helps see the association between each other
summary(lm(csat ~ expense + percent, data = states.data))

#the lm class and methods
#examining the model objects
class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

#getting more info about the fit
confint(sat.mod)
#created a histogram of residuals
hist(residuals(sat.mod))

#linear regression assumptions
#setting up the plot layout
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
#creating a plot
plot(sat.mod, which = c(1, 2))

#comparing 2 models
#comparing congressional votes for SAT scores as a prediction
#add model, adding house and senate as predictors
sat.voting.mod <- lm(csat ~ expense + house + senate,
                     data = na.omit(states.data))
#update it
sat.mod <- update(sat.mod, data=na.omit(states.data))
#compare the models with anova function
anova(sat.mod, sat.voting.mod)
#show the coefficient of the voting model
coef(summary(sat.voting.mod))

#exercise 0: least squares regression
#interactions and factors
#modeling interactions
#adding an interaction model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data)
#showing the results
coef(summary(sat.expense.by.percent))

#regression with categorical predictors
#predicting SAT scores from region
#making sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#adding region to the model
sat.region <- lm(csat ~ region,
                 data=states.data)
#showing results
coef(summary(sat.region))
anova(sat.region)

#setting factor reference groups and contrasts
#print default contrasts
contrasts(states.data$region)
#change the reference group
coef(summary(lm(csat ~ C(region, base= 4),
                data=states.data)))
#change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

#exercise 1: interactions and factors
#regression with binary outcomes
#logistic regression
#load in more datasets from the Statistics folder
NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

#logistic regression example
#prediciting the probability of being diagnosed with hypertension
#based on age, sex, sleep and bmi
#checking structure of hypev
str(NH11$hypev)
#checking levels of hypev
levels(NH11$hypev)
#collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
#run the regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
               data=NH11, family="binomial")
#summarize the data
coef(summary(hyp.out))

#logistic regression coefficients
#transform coefficients to make them easy to interpret
hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

#generating predicted values
#question to predict:
#how much more likely is a 63 year old female to have hypertension
#compared to a 33 year old female
#creating dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33,63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
#predict hypertionsion at those levels
cbind(predDat, predict(hyp.out, type="response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

#adding new library
#this library helps compute quantities of interest
library(effects)
plot(allEffects(hyp.out))

#exercise 2: logistic regression
#multivel modeling
#multivel modeling overview

#adding new library
library(lme4)
#examing the data
Exam <- readRDS("dataSets/Exam.rds")

#The null model and ICC
#null model, grouping by school but not fixed effects
Norm1 <- lmer(normexam ~ 1 + (1|school),
              data=Exam, REML = FALSE)
summary(Norm1)

#Adding fixed-effects predictors
Norm2 <- lmer(normexam~standLRT + (1|school),
              data=Exam,
              REML = FALSE)
#summarizing the data
summary(Norm2)

#multiple degree of freedom comparisons
#compare the 2 models
anova(Norm1, Norm2)

#random slopes
#estimate distribution of the slope of exam on standarized test
Norm3 <- lmer(normexam~standLRT + (standLRT|school), data=Exam,
              REML = FALSE)
#summarize norm3
summary(Norm3)

#testing significance of the random slope
#comparing models with and without the random slope term
anova(Norm2, Norm3)

#exercise 3: multilevel modeling
#installing packages and adding datasets
install.packages("multilevel")
install.packages("lme4")
library(multilevel)
library(lme4)
#loading the data
data(bh1996, package="multilevel")

#building the model
null_model <- lmer(WBEING ~ 1 + (1 | GRP), data=bh1996)
summary(null_model)

#calculating the ICC
var_between <- as.numeric(VarCorr(null_model)$GRP[1])
var_within <- attr(VarCorr(null_model), "sc")
ICC <- var_between / (var_between + var_within)
ICC
     
#adding predictors
model_1 <- lmer(WBEING ~ HRS + LEAD + (1 | GRP), data=bh1996)
#summarizing the model
summary(model_1)

#adding the random effect
model_2 <- lmer(WBEING ~ HRS + LEAD + (HRS | GRP), data=bh1996)
summary(model_2)

#testing significance of the random slope
#comparing models with and without the random slope term
anova(model_1, model_2)







