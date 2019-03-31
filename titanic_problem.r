#Importing the dataset
training_set = read.csv('train.csv', stringsAsFactors = FALSE)
testing_set = read.csv('test.csv', stringsAsFactors = FALSE)


#Converting to factors
#Training data
training_set$Survived = factor(training_set$Survived, levels = c(0, 1))
training_set$Pclass = factor(training_set$Pclass, levels = c(1, 2, 3))
training_set$Sex = factor(training_set$Sex)
training_set$Embarked = factor(training_set$Embarked)

#Testing data
testing_set$Pclass = factor(testing_set$Pclass, levels = c(1, 2, 3))
testing_set$Sex = factor(testing_set$Sex)
testing_set$Embarked = factor(testing_set$Embarked, levels = levels(training_set$Embarked))


#Removing unwanted columns
#Training data
training_set = training_set[-11]
training_set = training_set[-9]
training_set = training_set[-4]
training_set = training_set[-1]

#Testing data
testing_set_id = testing_set[1]
testing_set = testing_set[-10]
testing_set = testing_set[-8]
testing_set = testing_set[-3]
testing_set = testing_set[-1]


#Removing NA's
training_set$Age = ifelse(is.na(training_set$Age),
                     ave(training_set$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     training_set$Age)


testing_set$Age = ifelse(is.na(testing_set$Age),
                          ave(testing_set$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                          testing_set$Age)

testing_set$Fare = ifelse(is.na(testing_set$Fare),
                         ave(testing_set$Fare, FUN = function(x) mean(x, na.rm = TRUE)),
                         testing_set$Fare)


#Feature Scaling
training_set[4:7] = scale(training_set[4:7])
testing_set[3:6] = scale(testing_set[3:6])


#Fitting the model to training dataset
library(h2o)
h2o.init(nthreads = -1)
classifier = h2o.deeplearning(y = 'Survived',
                         training_frame = as.h2o(training_set),
                         activation = 'Rectifier',
                         hidden = c(4,4),
                         epochs = 100,
                         train_samples_per_iteration = -2)
#Predicting test set results
y_pred = h2o.predict(classifier, newdata = as.h2o(testing_set))
y_pred = ifelse(y_pred > 0.5, 1, 0)
y_pred = as.vector(y_pred)
y_pred
results = y_pred[419:836]
results2 = y_pred[837:1254]


#Exporting to the file
write.csv(z, file = "titanin_results2.csv", sep = ",")
