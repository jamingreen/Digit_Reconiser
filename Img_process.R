displayDigit <- function(X){
  m <- matrix(unlist(X),nrow = 28,byrow = T)
  m <- t(apply(m, 2, rev))
  image(m,col=grey.colors(255))
}

train <- read.csv("dataset/train.csv", header = TRUE, stringsAsFactors = F)
displayDigit(train[9,-1])


nzv <- nearZeroVar(train)
nzv.nolabel <- nzv-1

inTrain <- createDataPartition(y=train$label, p=0.7, list=F)

training <- train[inTrain, ]
CV <- train[-inTrain, ]

X <- as.matrix(training[, -1]) # data matrix (each row = single example)
N <- nrow(X) # number of examples
y <- training[, 1] # class labels

K <- length(unique(y)) # number of classes
X.proc <- X[, -nzv.nolabel]/max(X) # scale
D <- ncol(X.proc) # dimensionality

Xcv <- as.matrix(CV[, -1]) # data matrix (each row = single example)
ycv <- CV[, 1] # class labels
Xcv.proc <- Xcv[, -nzv.nolabel]/max(X) # scale CV data

Y <- matrix(0, N, K)

for (i in 1:N){
  Y[i, y[i]+1] <- 1
}