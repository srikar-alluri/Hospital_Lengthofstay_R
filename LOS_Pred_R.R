library(readxl)
library(ggplot2)
library(dplyr)
library(readxl)
library(fastDummies)
library(class)
library(e1071)
library(caret)
library(pROC)
library(stringr)

# read full file
df_complete<- read_excel("/Users/srikaralluri/Downloads/Dataset.xlsx")

# make a copy to avoid multiple times load
df_full<-df_complete

## Data cleaning and preparation

# remove unnecessary columns related to prescription and diagnosis and treatment costs
df_full<- df_full[,-c(3,14:15,17:34)]

# omit rows with na
df_full<-na.omit(df_full)

# convert ID columns to characters
df_full$`Facility Id`<-as.character(df_full$`Facility Id`)

# Duplicate column CCS Diagnosis Description as Disease Long Description
df_full['Disease Long Description'] <- df_full$`CCS Diagnosis Description`


# Split CCS Diagnosis Description column into Health Condition Type and Health Long
df_full[c('Health Condition Type', 'Health Long')] <- str_split_fixed(df_full$`CCS Diagnosis Description`, ' ', 2)

# replace unnecessary ";" in health condition column due to column split
df_full$`Health Condition Type` <- str_replace(df_full$`Health Condition Type`, ";", "")

# get row counts for each Health Condition Type
hc_rows<-df_full %>% 
  count(`Health Condition Type`)

# convert the hc_rows tibble to dataframe
hc_df<-as.data.frame(hc_rows)
#print(hc_df)

# order the data based on row count
hc_df_sorted<-hc_df[order(hc_df$n, decreasing = TRUE),]
print(hc_df_sorted)

#
hc_desired_top<-c ('Liveborn',
                   'Septicemia',
                   'Osteoarthritis',
                   'Pneumonia',
                   'Cancer',
                   'Cardiac',
                   'Fracture',
                   'Diabetes',
                   'Schizophrenia',
                   'Spondylosis',
                   'Asthma',
                   'Hypertension',
                   'Diverticulosis',
                   'Coronary',
                   'Epilepsy')



df_full<-df_full %>% filter(df_full$`Health Condition Type` %in% hc_desired_top[2:11])

# create a Long stay Predictor column (if stay is more than 4 days call it 1 else 0)
df_full['Long Stay'] <- ifelse(df_full$`Length of Stay`>3,1,0)

# remove redundant columns from the previous health condition calculations
df_full[c('CCS Diagnosis Description', 'Health Long')] <- NULL

# Final Data frame structure
str(df_full)

# % of Long stays = 1 in the dataset
sum(ifelse(df_full$`Long Stay`==1,1,0))/nrow(df_full)

# list of unique Health Condition Type
unique(df_full$`Health Condition Type`);


## Linear Model
  
## Linear Model

# Assign the dataframe 'df_full' to the variable 'Linear'
Linear <- df_full

# Generate a random sample of row indices for training set
inTrain <- sample(nrow(Linear), 0.7 * nrow(Linear))

# Create a training dataframe using the sampled rows
train <- data.frame(Linear[inTrain,])

# Create a test dataframe by excluding the sampled rows from the original dataframe
test <- data.frame(Linear[-inTrain,])

# Build a linear regression model using the training data
model <- lm(train$Long.Stay ~  Health.Service.Area  + Age.Group + Gender + Race + Ethnicity + Type.of.Admission + Health.Condition.Type, data = train)

# Display a summary of the linear regression model
summary(model)

# Extract the response variable from the training data
train_linear <- train$Long.Stay

# Set the cutoff threshold for classification

for (cutoff in seq(0.1, 1.0, 0.1)) {
  # Predict the response variable for the training data using the model
  Pred.probability.train <- predict(model, newdata = train, type = "response")
  
  # Convert predicted probabilities to binary predictions based on the cutoff
  PredictedTrain <- ifelse(Pred.probability.train > cutoff, 1, 0)
  
  # Convert binary predictions to a factor variable with levels "0" and "1"
  PredictedTrain <- factor(PredictedTrain, levels = c("0", "1"))
  
  # Create a confusion matrix for the training predictions
  confusionTrain <- table(train_linear, PredictedTrain)
  
  # Calculate the accuracy of the training predictions
  AccuracyTrain <- (confusionTrain[1, 1] + confusionTrain[2, 2]) / sum(confusionTrain)
  
  # Calculate the error rate of the training predictions
  ErrorTrain <- 1 - AccuracyTrain
  # Calculate recall, precision, and false positive rate for the test predictions
  recall <- confusionTrain[2, 2] / (confusionTrain[2, 2] + confusionTrain[2, 1])
  precision <- confusionTrain[2, 2] / (confusionTrain[2, 2] + confusionTrain[1, 2])
  fpr <- confusionTrain[1, 2] / (confusionTrain[1, 2] + confusionTrain[1, 1])
  
  # Print evaluation metrics for each cutoff value
  print(paste("Cutoff:", cutoff, "Accuracy:", AccuracyTrain, "Error:", ErrorTrain, "recall:", recall, "precision:", precision, "fpr:", fpr ))
}

# Set the cutoff threshold for classification
cutoff <- 0.5


# Extract the response variable from the test data
test_linear <- test$Long.Stay

# Predict the response variable for the test data using the model
Pred.probability.test <- predict(model, newdata = test, type = "response")

# Convert predicted probabilities to binary predictions based on the cutoff
PredictedTest <- ifelse(Pred.probability.test > cutoff, 1, 0)

# Convert binary predictions to a factor variable with levels "0" and "1"
PredictedTest <- factor(PredictedTest, levels = c("0", "1"))

# Create a confusion matrix for the test predictions
confusionTest <- table(test_linear, PredictedTest)

# Calculate the accuracy of the test predictions
AccuracyTest <- (confusionTest[1, 1] + confusionTest[2, 2]) / sum(confusionTest)

# Calculate the error rate of the test predictions
ErrorTest <- 1 - AccuracyTest

# Calculate recall, precision, and false positive rate for the test predictions
recall <- confusionTest[2, 2] / (confusionTest[2, 2] + confusionTest[2, 1])
precision <- confusionTest[2, 2] / (confusionTest[2, 2] + confusionTest[1, 2])
fpr <- confusionTest[1, 2] / (confusionTest[1, 2] + confusionTest[1, 1])

# Print evaluation metrics for each cutoff value
print(paste("Cutoff:", cutoff, "Accuracy:", AccuracyTest, "Error:", ErrorTest, "recall:", recall, "precision:", precision, "fpr:", fpr ))


# ROC curve
library(pROC)
roc_train <- plot(roc(train_linear, Pred.probability.train), print.auc = TRUE, col = "blue")
roc_test <- plot(roc(test_linear,  Pred.probability.test), print.auc = TRUE, col = "green", print.auc.y = .4, add = TRUE)

## Logistic Model

## Logistic Model

# Splitting the data into training and test sets
train_indices <- sample(nrow(df_full), 0.7 * nrow(df_full))
train_data <- df_full[train_indices, ]
test_data <- df_full[-train_indices, ]

# Fitting a generalized linear model (GLM) to the training data
m2 <- glm(`Long Stay` ~ `Age Group` + `Hospital County` + `Gender` + `Race` + Ethnicity + `Type of Admission` + `Health Condition Type`, data = train_data, family = "binomial")
summary(m2)

# Iterating over different cutoff values
for (cutoff in seq(0.1, 1.0, 0.1)) {
  # Obtaining the actual and predicted values for the training data
  train_log <- train_data$`Long Stay`
  predicted.probability.train <- predict(m2, type = "response") 
  Predicted <- ifelse(predicted.probability.train > cutoff, 1, 0)
  Predicted <- factor(Predicted, levels = c(0, 1))
  
  # Calculating the confusion matrix and performance metrics for the training data
  confusionTrain <- table(train_log, Predicted)
  AccuracyTrain <- (confusionTrain[1, 1] + confusionTrain[2, 2]) / sum(confusionTrain)
  ErrorTrain <- 1 - AccuracyTrain
  recall <- confusionTest[2, 2] / (confusionTest[2, 2] + confusionTest[2, 1])
  precision <- confusionTest[2, 2] / (confusionTest[2, 2] + confusionTest[1, 2])
}

# Setting a specific cutoff value for the test data
cutoff <- 0.5
test_log <- test_data$`Long Stay`
predicted.probability.test <- predict(m2, type = "response", newdata = test_data) 

Predicted_test <- ifelse(predicted.probability.test > cutoff, 1, 0)
Predicted_test <- factor(Predicted_test, levels = c(0, 1))

# Calculating the confusion matrix and performance metrics for the test data
confusionTest <- table(test_log, Predicted_test)
AccuracyTest <- (confusionTest[1, 1] + confusionTest[2, 2]) / sum(confusionTest)
ErrorTest <- 1 - AccuracyTest
Num_error <- confusionTest[1, 2] + confusionTest[2, 1]
recall <- confusionTest[2, 2] / (confusionTest[2, 2] + confusionTest[2, 1])
precision <- confusionTest[2, 2] / (confusionTest[2, 2] + confusionTest[1, 2])
fpr <- confusionTest[1, 2] / (confusionTest[1, 2] + confusionTest[1, 1])

# Printing the performance metrics for the test data
print(paste("Cutoff:", cutoff, "Accuracy:", AccuracyTest, "Error:", ErrorTest, "recall:", recall, "precision:", precision, "fpr:", fpr))
Actual_test <- test_data$`Long Stay`
# ROC curve
library(pROC)
roc_rose <- plot(roc(Actual_test, predicted.probability.test), print.auc = TRUE, col = "blue")
## Next, the additional argument "add = TRUE" adds the test ROC to the previous plot
roc_rose <- plot(roc(Actual_test,  predicted.probability.test), print.auc = TRUE, 
                 col = "green", print.auc.y = .4, add = TRUE)


## Naive Bayes

df_full <- df_full[-c(2, 3, 6, 10, 13) ]
table(df_full$`Long Stay`)
cols <- c(1:10)
df_full[,cols] <- lapply(df_full[,cols] , factor)
df_full

set.seed(123)

# Split the data into train, validation, and test sets
inTrain <- createDataPartition(df_full$`Long Stay`, p = 0.6, list = FALSE)

# Create train, validation, and test dataframes
train_df <- df_full[inTrain,]
df_temp <- df_full[-inTrain,]
inVal <- sample(nrow(df_temp), 0.5*(nrow(df_temp)))
validation_df <- df_temp[inVal,]
test_df <- df_temp[-inVal,]
test_output<-as.vector(test_df$`Long Stay`)

# Extract validation output
validation_output <- as.vector(validation_df$`Long Stay`)

# Train the model
model <- naiveBayes(`Long Stay` ~ ., data = train_df)

# Print model apriori probabilities
print(model$apriori)

# Iterate over different cutoffs
for (cutoff in seq(0.1, 0.9, by = 0.1)) {
  # Obtain predicted probabilities
  predicted.probability <- predict(model, newdata = validation_df[, -10], type = "raw")
  
  
  # Calculate parameters of validation data based on the cutoff
  prediction <- predicted.probability[, 2] > cutoff
  
  print(confusion_table <- table(validation_df$`Long Stay`, prediction, dnn = list('actual', 'predicted')))
  print(cutoff)
  print((acc = (confusion_table[1,1]+confusion_table[2,2])/sum(confusion_table)))
  print((err = (confusion_table[1,2]+confusion_table[2,1])/sum(confusion_table)))
  print((recall = (confusion_table[2,2]/(confusion_table[2,2]+confusion_table[2,1])))) #TPR
  print((fpr = (confusion_table[1,2]/(confusion_table[1,2]+confusion_table[1,1]))))
  print((precision = (confusion_table[2,2]/(confusion_table[2,2]+confusion_table[1,2]))))
  
}

# parameters for test data at selected cutoff = 0.4
predicted.probability_test <- predict(model, newdata = test_df[, -10], type = "raw")
test_prediction <- predicted.probability_test[, 2] >= 0.4
print(confusion_table_test <- table(test_df$`Long Stay`, test_prediction, dnn = list('actual', 'predicted')))
print((acc = (confusion_table_test[1,1]+confusion_table_test[2,2])/sum(confusion_table_test)))
print((err = (confusion_table_test[1,2]+confusion_table_test[2,1])/sum(confusion_table_test)))
print((recall = (confusion_table_test[2,2]/(confusion_table_test[2,2]+confusion_table_test[2,1])))) #TPR
print((fpr = (confusion_table_test[1,2]/(confusion_table_test[1,2]+confusion_table_test[1,1]))))
print((precision = (confusion_table_test[2,2]/(confusion_table_test[2,2]+confusion_table_test[1,2]))))

# ROC curve
PL <- as.numeric(test_df$`Long Stay`)-1
prob <- predicted.probability_test[,2]
df1 <- data.frame(test_prediction, PL, prob)
plot(roc(test_output, prob), print.auc = TRUE, col = "blue",legacy.axes=TRUE)


library(readxl)
# read the data
#df<- read_excel("/Users/vikashgujju/Downloads/Health_Dataset.xlsx")

df<-df_full
#df[c("Facility Id","Length of Stay","Disease Long Description","Hospital County","Facility Name","Zip Code - 3 digits")]<-NULL

df[c("Facility Id","Length of Stay","Disease Long Description",
     "Hospital County","Facility Name","Zip Code - 3 digits","Patient Disposition")]<-NULL
#df[c("Facility Id","Length of Stay","Disease Long Description")]<-NULL
#head(df)

#colnames(df)[colnames(df) == "Zip Code - 3 digits"] <-"Zip_code"
colnames(df) <- gsub(" ", "_", colnames(df))
df$Long_Stay <-as.character(df$Long_Stay)

# Other than predictor convert all variables into factors
names <- c(1:8)
df[,names] <- lapply(df[,names] , factor)
str(df)

set.seed(123457)

#Partitioning Data
train<-sample(nrow(df),0.7*nrow(df))
df_train<-df[train,]
df_test<-df[-train,]

library(tree)
# 2)	Build a Classification Tree model using the training data
tree.long_stay=tree(Long_Stay~., data=df_train)
# To predict on the train set
tree.pred=predict(tree.long_stay,df_test,type="class")
#tree.pred=predict(tree.long_stay,df_test,type="vector")
#tree.pred <- ifelse(tree.pred[,2]>cutoff,1,0)
# The confusion matrix (Prediction is rows, actual is columns)
(c = table(df_test$Long_Stay,tree.pred))
(acc = (c[1,1]+c[2,2])/sum(c))
(err = (c[1,2]+c[2,1])/sum(c))
(recall = (c[2,2]/(c[2,2]+c[2,1]))) #TPR
(fpr = (c[1,2]/(c[1,2]+c[1,1])))
(precision = (c[2,2]/(c[2,2]+c[1,2])))

# We will use cross-validation using the cv.tree function which will give us the 
# error rate for different tree sizes. The prune.misclass argument gives the number 
# of misclassification errors
#
cv.long_stay=cv.tree(tree.long_stay,FUN=prune.misclass)
names(cv.long_stay)

cv.long_stay
#
# In the output, $size is the tree size and $dev is the number of errors. Pick the 
# The following plots help identify best size
plot(cv.long_stay$size,cv.long_stay$dev,type="b")
z<-cv.long_stay$size[which.min(cv.long_stay$dev)]
z
# Here we get 4 for best tree, but note that this number will depend on the seed set

# Now prune the tree to a best size of 4
prune.long_stay=prune.misclass(tree.long_stay,best=z)
plot(prune.long_stay)
text(prune.long_stay,pretty=0)

# 6)	Construct the classification confusion matrix and compute the accuracy for the test data
# Predict using the pruned tree
tree.long_stay=predict(prune.long_stay,df_test,type="class")
(c = table(df_test$Long_Stay,tree.long_stay))
(acc = (c[1,1]+c[2,2])/sum(c))
(err = (c[1,2]+c[2,1])/sum(c))
(recall = (c[2,2]/(c[2,2]+c[2,1]))) #TPR
(fpr = (c[1,2]/(c[1,2]+c[1,1])))
(precision = (c[2,2]/(c[2,2]+c[1,2])))
#

#------------------
library(randomForest)

#
# We first do bagging (which is just RF with m = p)
set.seed(1)
bag.long_stay=randomForest(Long_Stay~.,data=df_train,mtry=7,importance=TRUE)
bag.long_stay
yhat.bag = predict(bag.long_stay,newdata=df_test)
long_stay.test=df_test$Long_Stay
(c = table(long_stay.test,yhat.bag))
(acc = (c[1,1]+c[2,2])/sum(c))
(err = (c[1,2]+c[2,1])/sum(c))
(recall = (c[2,2]/(c[2,2]+c[2,1]))) #TPR
(fpr = (c[1,2]/(c[1,2]+c[1,1])))
(precision = (c[2,2]/(c[2,2]+c[1,2])))
importance(bag.long_stay)
varImpPlot(bag.long_stay)


# Now RF with m = 4
set.seed(1)
bag.long_stay=randomForest(Long_Stay~.,data=df_train,mtry=4,importance=TRUE)
bag.long_stay
yhat.bag = predict(bag.long_stay,newdata=df_test)
long_stay.test=df_test$Long_Stay
(c = table(long_stay.test,yhat.bag))
(acc = (c[1,1]+c[2,2])/sum(c))
(err = (c[1,2]+c[2,1])/sum(c))
(recall = (c[2,2]/(c[2,2]+c[2,1]))) #TPR
(fpr = (c[1,2]/(c[1,2]+c[1,1])))
(precision = (c[2,2]/(c[2,2]+c[1,2])))
importance(bag.long_stay)
varImpPlot(bag.long_stay)


# Boosting
#---------- to edit from here
library(gbm)
set.seed(1)
df_train$Long_Stay <- ifelse(df_train$Long_Stay==1,1,0)
df_test$Long_Stay <- ifelse(df_test$Long_Stay==1,1,0)
boost.long_stay=gbm(Long_Stay~.,df_train,distribution="bernoulli",n.trees=5000,interaction.depth=5)
summary(boost.long_stay)
par(mfrow=c(1,2))
plot(boost.long_stay,i="Health_Condition_Type")
plot(boost.long_stay,i="Health_Service_Area")
yhat.boost=predict(boost.long_stay,newdata=df_test,n.trees=5000,type="response")
yhat.test=df_test$Long_Stay

for (i in 1:10)
{
  predicted <- ifelse(yhat.boost>(i/10),1,0)
  print(i/10)
  c = table(yhat.test,predicted)
  print('acc')
  print((c[1,1]+c[2,2])/sum(c))
  print('err')
  print((c[1,2]+c[2,1])/sum(c))
  print('recall')
  print((c[2,2]/(c[2,2]+c[2,1]))) #TPR
  print('fpr')
  print((c[1,2]/(c[1,2]+c[1,1])))
  print('precision')
  print((c[2,2]/(c[2,2]+c[1,2])))
};

# pick a cutoff value with good balance between recall and precision
predicted <- ifelse(yhat.boost>0.4,1,0)
yhat.test=df_test$Long_Stay
(c = table(yhat.test,predicted))

(acc = (c[1,1]+c[2,2])/sum(c))
(err = (c[1,2]+c[2,1])/sum(c))
(recall = (c[2,2]/(c[2,2]+c[2,1]))) #TPR
(fpr = (c[1,2]/(c[1,2]+c[1,1])))
(precision = (c[2,2]/(c[2,2]+c[1,2])))

(F1score = 2 * ((precision * recall)/ (precision+recall)))

# plot ROC curve
library(pROC)
roc_rose <- plot(roc(df_test$Long_Stay, yhat.boost), print.auc = TRUE, col = "blue",legacy.axes = TRUE)
legend("bottom",
       legend=c("Boosting", "Linear", "Logistic","Naive Bayes"),
       col=c("blue","green", "red","purple"),
       lwd=10, cex =.5, xpd = TRUE, horiz = TRUE);

