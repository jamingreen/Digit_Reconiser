library(caret)
library(neuralnet)
library(nnet)
library(rpart)
library(randomForest)
library(h2o)

train <- read.csv("dataset/train.csv")

ind <- nearZeroVar(train)
train <- train[-ind]

train$label <- as.factor(train$label)
inTrain <- sample(c(1,2,3), size = nrow(train), prob = c(0.08, 0.02, 0.9), replace= T)

training <- train[inTrain == 1,]
cv <- train[inTrain == 2,]

mod_dt <- rpart(formula = label~., data = training, method = "class")
mod_rf <- train(label~., data = training, trControl = trainControl(method = "cv", number = 5, verboseIter = T), tuneLenght = 5, method = "rf")
probs <- predict(mod)

mod_nn <- nnet(label~., data = training, niteration = 2000, size = 30)

confusionMatrix(predict(mod_rf, cv), cv$label)

