
setwd("A:/Documents and Data/R/Machine Learning A-Z Template Folder/Part 2 - Regression/Section 5 - Multiple Linear Regression")

dataset = read.csv('50_Startups.csv')
dataset = dataset[-4]

head(dataset)

#Initializing vectors
parameter_vector = matrix(data = 0, nrow = ncol(dataset), ncol = 1)
design_matrix = cbind(matrix(data = 1, nrow = 50, ncol = 1 ), as.matrix(dataset[1:3]))
colnames(design_matrix) = NULL
y = as.matrix(dataset[4])
colnames(y) = NULL
m = nrow(dataset)
temp = parameter_vector
alpha = 0.00000000001
change = 1

while (change == 1){
    hypothesis_matrix = design_matrix%*%parameter_vector
    diff = (hypothesis_matrix - y)
    temp = parameter_vector - t(alpha/m*t(diff)%*%design_matrix)
    print(temp)
    if(all(temp == parameter_vector)){
        change = 0
    }
    parameter_vector = temp
}
