
setwd("A:/Documents and Data/R/Machine Learning A-Z Template Folder/Part 2 - Regression/Section 4 - Simple Linear Regression/Simple_Linear_Regression")
#Importing the data
dataset = read.csv('Salary_Data.csv')

#Variables for the model
sum_yoe = sum(dataset$YearsExperience)
sum_yoe_sq = sum(dataset$YearsExperience*dataset$YearsExperience)
sum_yoe_sal = sum(dataset$YearsExperience*dataset$Salary)
sum_sal = sum(dataset$Salary)

change = 1
theta0 = 0
theta1 = 0
m = nrow(dataset)
alpha = 0.008


while (change == 1) {
  temp0 = theta0 - (alpha/m)*(m*theta0 + theta1*sum_yoe - sum_sal)
  temp1 = theta1 - (alpha/m)*(theta0*sum_yoe + theta1*sum_yoe_sq - sum_yoe_sal)
  if(temp0 == theta0 && temp1 == theta1){
    change = 0}
  theta0 = temp0
  theta1 = temp1
  theta0
  theta1
  }

#Function definition
fun <- function(x) {
  return(theta0 + theta1*x)
}

regressor = lm(formula = Salary ~ . , data = dataset)
y_pred = predict(object = regressor, newdata = dataset)

library(ggplot2)
ggplot() +
geom_line(mapping = aes(x = dataset$YearsExperience, y = dataset$Salary), color = "blue") +
geom_line(mapping = aes(x = dataset$YearsExperience, y = fun(dataset$YearsExperience)), color = "red") 

#Relation between my_function and lm
cor(y_pred, fun(dataset$YearsExperience))
