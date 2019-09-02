# Author: Mannat Sandhu
# February 9th, 2019
# Gender Wage Gap Analysis

library(foreign)
library(dplyr)

# Set to correct working directory/change to actual hyperlink.
org_example <- read.dta("~/Desktop/org_example.dta")

# Restricting the data to only those respondents who are in the labor force
# and for the years 1993 and 2013 separately.
org_example_1993 <- subset(org_example, nilf == 0 & year == 1993)
org_example_2013 <- subset(org_example, nilf == 0 & year == 2013)

org_example_1993 <- select(org_example_1993, rw, educ, female, age, wbho, hourslw)
org_example_2013 <- select(org_example_2013, rw, educ, female, age, wbho, hourslw)

# org_example_1993 = org_example_1993[complete.cases(org_example_1993),]
# org_example_2013 = org_example_2013[complete.cases(org_example_2013),]


# Set missing values to 0 as asked in the question.
org_example_1993$hourslw[is.na(org_example_1993$hourslw)] <- 0
org_example_2013$hourslw[is.na(org_example_2013$hourslw)] <- 0


# Part (a)
# Run the models with poisson distribution.
poissonreg_1993 <- glm(rw ~ (as.factor(age)*female + educ  + wbho - 1), org_example_1993, family = poisson(link="log"))
summary(poissonreg_1993)

poissonreg_2013 <- glm(rw ~ (as.factor(age)*female + educ  + wbho - 1), org_example_2013, family = poisson(link="log"))
summary(poissonreg_2013)


# Part (b)
age <- seq(70, from=20)

# Get predictions for the 1993 female and male data.
prediction_data_1993_female <- as.data.frame(age)
prediction_data_1993_female$female = 1
prediction_data_1993_female$wbho = "White"
prediction_data_1993_female$educ = "College"
prediction_data_1993_female$logrw = predict(poissonreg_1993, prediction_data_1993_female)

prediction_data_1993_male <- prediction_data_1993_female
prediction_data_1993_male$female = 0
prediction_data_1993_male$logrw = predict(poissonreg_1993, prediction_data_1993_male)

# Plot the data
plot(x = prediction_data_1993_male$age, y = prediction_data_1993_male$logrw,xlab = "Age", ylab = "Log Wage", main = "Graph 1b 1993 Female/Male Data")
lines(x = prediction_data_1993_male$age, y = prediction_data_1993_male$logrw, lty = 1)
lines(x = prediction_data_1993_female$age, y = prediction_data_1993_female$logrw, lty = 2)
legend("bottomright",c("Male", "Female"), lty = c(1,2))

# Get predictions for the 2013 female and male data.
prediction_data_2013_female <- as.data.frame(age)
prediction_data_2013_female$female = 1
prediction_data_2013_female$wbho = "White"
prediction_data_2013_female$educ = "College"
prediction_data_2013_female$logrw = predict(poissonreg_2013, prediction_data_2013_female)


prediction_data_2013_male <- prediction_data_2013_female
prediction_data_2013_male$female = 0
prediction_data_2013_male$logrw = predict(poissonreg_2013, prediction_data_2013_male)

plot(x = prediction_data_2013_male$age, y = prediction_data_2013_male$logrw, xlab = "Age", ylab = "Log Wage", main = "Graph 1b 2013 Female/Male Data")
lines(x = prediction_data_2013_male$age, y = prediction_data_2013_male$logrw, lty = 1)
lines(x = prediction_data_2013_female$age, y = prediction_data_2013_female$logrw, lty = 2)
legend("bottomright",c("Male", "Female"), lty = c(1,2))


# Part (c)
poissonreg_1993_hourslw <- glm(hourslw ~ (as.factor(age)*female + educ  + wbho - 1), org_example_1993, family = poisson(link="log"))
summary(poissonreg_1993)

poissonreg_2013_hourslw <- glm(hourslw ~ (as.factor(age)*female + educ  + wbho - 1), org_example_2013, family = poisson(link="log"))
summary(poissonreg_2013)

# 1993
prediction_data_1993_female$log_hoursrw = predict(poissonreg_1993_hourslw, prediction_data_1993_female)
prediction_data_1993_male$log_hoursrw = predict(poissonreg_1993_hourslw, prediction_data_1993_male)
# Plot the data
plot(x = prediction_data_1993_male$age, y = prediction_data_1993_male$log_hoursrw,xlab = "Age", ylab = "Log Hours worked", main="Graph 1c 1993 Female/Male Data")
lines(x = prediction_data_1993_male$age, y = prediction_data_1993_male$log_hoursrw, lty = 1)
lines(x = prediction_data_1993_female$age, y = prediction_data_1993_female$log_hoursrw, lty = 2)
legend("topright",c("Male", "Female"), lty = c(1,2))


# 2013
prediction_data_2013_female$log_hoursrw = predict(poissonreg_2013_hourslw, prediction_data_2013_female)
prediction_data_2013_male$log_hoursrw = predict(poissonreg_2013_hourslw, prediction_data_2013_male)
# Plot the data
plot(x = prediction_data_2013_male$age, y = prediction_data_2013_male$log_hoursrw, xlab = "Age", ylab = "Log Hours worked", main="Graph 1c 2013 Female/Male Data")
lines(x = prediction_data_2013_male$age, y = prediction_data_2013_male$log_hoursrw, lty = 1)
lines(x = prediction_data_2013_female$age, y = prediction_data_2013_female$log_hoursrw, lty = 2)
legend("topright",pch=0,c("Male", "Female"), lty = c(1,2))