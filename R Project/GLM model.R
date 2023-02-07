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

GLM <- glm(bad_loan~.,data = train,family = 'binomial',weights = weights)
Stepwise <- step(GLM)
TrainPred <- Stepwise$fitted.values
TestPred <- predict(Stepwise,test,type = "response")
Pred <- c(TrainPred,TestPred)
write.csv(Pred,file="Pred2.csv")

library(PRROC)
# Train
ROC <- roc.curve(scores.class0 = TrainPred,weights.class0 = as.numeric(as.character(train$bad_loan)),curve = T)
print(ROC)
plot(ROC)
PRCURVE <- pr.curve(scores.class0 = TrainPred,weights.class0 = as.numeric(as.character(train$bad_loan)),curve = T)
print(PRCURVE)
plot(PRCURVE)
#Test
ROC <- roc.curve(scores.class0 = TestPred,weights.class0 = as.numeric(as.character(test$bad_loan)),curve = T)
print(ROC)
plot(ROC)
PRCURVE <- pr.curve(scores.class0 = TestPred,weights.class0 = as.numeric(as.character(test$bad_loan)),curve = T)
print(PRCURVE)
plot(PRCURVE)

# Cross Validation

i = 10
rand <- runif(n = 18994)
for (i in 1:10) 
test_indices <- rand <= i / 10 & rand > (i - 1) / 10
train_cv <- data[!test_indices,]
test_cv <- data[test_indices,]
weights_cv <- ifelse(train_cv$bad_loan == "0", 1, 4)
model_cv <-
glm(bad_loan ~ .,data = train_cv,family = "binomial",weights = weights_cv)
STEP_cv <- step(model_cv)
pred_cv <-predict(STEP_cv, newdata = test_cv, type = "response")
roc_cv <- roc.curve(scores.class0 = pred_cv,weights.class0 = as.numeric(as.character(test_cv$bad_loan)),curve = T)
print(roc_cv)

summary(Stepwise)
