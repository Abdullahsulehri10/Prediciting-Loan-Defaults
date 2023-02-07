setwd("G:/IBA/Courses resources/Analytical Approach to Marketing Decisions/Project3")
data <- read.csv(file = "Data.csv", header=TRUE)
colnames(data)
data<- data[,-c(1,2)]
colnames(data)

set.seed(123)
data <- data[sample(1:nrow(data)),]
rownames(data) <- 1:nrow(data)

data$grade <- as.factor(data$grade)
data$short_emp <- as.factor(data$short_emp)
data$home_ownership <- as.factor(data$home_ownership)
data$purpose <- as.factor(data$purpose)
data$term.Months. <- as.factor(data$term.Months.)
data$last_delinq_none <- as.factor(data$last_delinq_none)
data$last_major_derog_none <- as.factor(data$last_major_derog_none)
data$Above.Avg.Income <- as.factor(data$Above.Avg.Income)
data$Below.Avg.DTI <- as.factor(data$Below.Avg.DTI)
data$bad_loan <- as.factor(data$bad_loan)

train <- data[1:12726, ]
test <- data[12727:18994, ]

weights <- ifelse(train$bad_loan == "0", 1, 4)

library(rpart)
library(rpart.plot)
library(PRROC)

# Model 1
RPART <- rpart(bad_loan ~ ., data = train, weights = weights)
rpart.plot(RPART)

TestPred <- predict(RPART, test, type = 'prob')
TestPred <- TestPred[,2]
TrainPred <- predict(RPART, train, type = 'prob')
TrainPred <- TrainPred[,2]

# Train
ROC_train <- roc.curve(scores.class0 = TrainPred,weights.class0 = as.numeric(as.character(train$bad_loan)),curve = T)
print(ROC_train)
plot(ROC_train)

PRCURVE_train <-pr.curve(scores.class0 = TrainPred,weights.class0 = as.numeric(as.character(train$bad_loan)),curve = T)
print(PRCURVE_train)
plot(PRCURVE_train)

# Test
ROC <- roc.curve(scores.class0 = TestPred,weights.class0 = as.numeric(as.character(test$bad_loan)),curve = T)
print(ROC)
plot(ROC)
PRCURVE <- pr.curve(scores.class0 = TestPred,weights.class0 = as.numeric(as.character(test$bad_loan)),curve = T)
print(PRCURVE)
plot(PRCURVE)

# Model 2
RPART2 <- rpart(bad_loan ~ ., data = train, weights = weights, control = rpart.control(cp = 0.002, maxdepth = 6,minbucket = 30))
rpart.plot(RPART2)

TestPred2 <- predict(RPART, test, type = 'prob')
TestPred2 <- TestPred2[,2]
TrainPred2 <- predict(RPART, train, type = 'prob')
TrainPred2 <- TrainPred2[,2]

# Train
ROC_train <- roc.curve(scores.class0 = TrainPred2,weights.class0 = as.numeric(as.character(train$bad_loan)),curve = T)
print(ROC_train)
plot(ROC_train)

PRCURVE_train <-pr.curve(scores.class0 = TrainPred2,weights.class0 = as.numeric(as.character(train$bad_loan)),curve = T)
print(PRCURVE_train)
plot(PRCURVE_train)

# Test
ROC <- roc.curve(scores.class0 = TestPred2,weights.class0 = as.numeric(as.character(test$bad_loan)),curve = T)
print(ROC)
plot(ROC)
PRCURVE <- pr.curve(scores.class0 = TestPred2,weights.class0 = as.numeric(as.character(test$bad_loan)),curve = T)
print(PRCURVE)
plot(PRCURVE)
RPART2

# Confusion Matrix for Model 1
library(caret)
TrainPredC <- predict(RPART, train, type = 'class')
TestPredC <- predict(RPART, test, type = 'class')

confusionMatrix(TrainPredC,
                train$bad_loan,
                positive = "1",
                mode = "prec_recall")
confusionMatrix(TestPredC,
                test$bad_loan,
                positive = '1',
                mode = 'prec_recall')

# Confusion Matrix for Model 2
TrainPredC <- predict(RPART2, train, type = 'class')
TestPredC <- predict(RPART2, test, type = 'class')

confusionMatrix(TrainPredC,
                train$bad_loan,
                positive = "1",
                mode = "prec_recall")
confusionMatrix(TestPredC,
                test$bad_loan,
                positive = '1',
                mode = 'prec_recall')

# Cross Validation

i = 10
rand <- runif(n = 18994)
for (i in 1:10) 
test_indices <- rand <= i / 10 & rand > (i - 1) / 10
train_cv <- data[!test_indices,]
test_cv <- data[test_indices,]
weights_cv <- ifelse(train_cv$bad_loan == "0", 1, 4)
model_cv <- rpart(bad_loan ~ ., data = train_cv, weights = weights_cv)
pred_cv <-predict(model_cv, newdata = test_cv, type = "prob")
roc_cv <- roc.curve(scores.class0 = pred_cv,weights.class0 = as.numeric(as.character(test_cv$bad_loan)),curve = T)
print(roc_cv)
