############## TASK 1 : Prediction using Supervised ML ############33

student_data <- read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
View(student_data)


# structure of dataset
str(student_data)

install.packages("ggplot2")
library("ggplot2")

ggplot(student_data,aes(x=Hours,y=Scores))+geom_point(color="darkred")

#checking if NA values are present

sum(is.na(student_data))

# checking for blanks
sapply(student_data, function(x) length(which(x == ""))) 


#set.seed generates random numbers and sample function after set.seed gives same result everytime when we run 
set.seed(100)

# SPLITTING THE DATA INTO 70:30
# sample function for getting the indices of your 70% of observations. 

train_indices= sample(1:nrow(student_data), 0.7*nrow(student_data))

# created an object "train_data" and stored the 70% of the data of student_data 
# by just passing the indices inside the student_data  dataset


train_data=student_data[train_indices,]

test_data=student_data[-train_indices,]

# MODEL
#lm = linear model 
# as we have only one independent var and one dependent variable, linear regression is used

model1=lm(Scores~Hours,data=student_data)

summary(model1)
#Adjusted R-squared:  0.9509 


# plot 

ggplot(student_data,aes(x=Hours,y=Scores))+geom_point(color="red")+geom_abline(intercept = 2.4837,slope = 9.7758,col="blue")

## once the model is created , we can predict the values of test data using the above model 

predict1=predict(model1,test_data[-2])

# Add a new column "pred_scores" into the test dataset

test_data$pred_scores = predict1

test_score=subset(test_data,select=c(2,3))

test_score



# correlation between the actual scores and pred scores
r = cor(test_data$pred_scores,test_data$Scores)

# R-squared measure how close the datapoints are to the fitted regression line
rsquare = r^2

rsquare
#predicted r^2 value is 0.9662323



# What will be predicted score if a student studies for 9.25 hrs/ day?

student_Score <- predict(model1,data.frame(Hours = c(9.25)))
student_Score 
print(paste("the predicted score for a student who studied for 9.25hrs/day = ",round(student_Score ,4)))
#"the predicted score for a student who studied for 9.25hrs/day =  92.9099"
                 
      
