#http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har
library(data.table)
library(caret)
#install.packages("corrplot")
library(corrplot)

####____Explore Data____####
#training <- fread("C:/Users/soomen/OneDrive - Capgemini/Desktop/Ontwikkeling/Coursera R/MachineLearning/Assignment/pml-training.csv", header = TRUE, sep = ",", dec = ".")
testing  <- read.csv("C:/Users/soomen/OneDrive - Capgemini/Desktop/Ontwikkeling/Coursera R/MachineLearning/Assignment/pml-testing.csv")
training <- read.csv("C:/Users/soomen/OneDrive - Capgemini/Desktop/Ontwikkeling/Coursera R/MachineLearning/Assignment/pml-training.csv")



inTrain <- createDataPartition(training$classe, p=0.7, list=FALSE)
traindata <- training[inTrain,]
testdata <- training[-inTrain,]
dim(testdata)

####____Data Cleaning____####

#If more than 75% of the rows are NA, remove the row
traindata <- traindata[, colSums(is.na(traindata)) < nrow(traindata) * 0.75]
testdata <- testdata[, colSums(is.na(testdata)) < nrow(testdata) * 0.75]

#Check for NearZeroVariance columns and delete them
NearZeroVariance <- nearZeroVar(traindata)
traindata <- traindata[ ,-NearZeroVariance]
testdata <- testdata[ ,-NearZeroVariance]

#Delete first columns since they contain unusable formats
traindata <- traindata[,-c(1:5)]
testdata <- testdata[,-c(1:5)]


#Classe is column number 54 so it is left out of the correlation matrix
corrMat <- cor(traindata[,-54])
corrplot(corrMat, method = "color", type = "lower", tl.cex = 0.8, tl.col = rgb(0,0,0))


####____Random Forest____####

modelrf <- train(classe~., method= "rf", data=traindata, trControl = trainControl(method="cv",number = 2, verboseIter=FALSE))
modelrf$finalModel
predictrf <- predict(modelrf, newdata=testdata)
confusionrf <- confusionMatrix(predictrf, testdata$classe)
confusionrf

####____Descision Tree____####
library(rattle)
modeldt <- train(classe~., method= "rpart", data = traindata)
modeldt$finalModel
predictdt <- predict(modeldt, newdata=testdata)
confusiondt <- confusionMatrix(predictdt, testdata$classe)
confusiondt
fancyRpartPlot(modeldt$finalModel)


####____Boosted model___####

controls <- trainControl(method = "repeatedcv", number = 5, repeats = 1, verboseIter = FALSE)
modelbm <- train(classe ~ ., data = traindata, trControl = controls, method = "gbm", verbose = FALSE)
modelbm$finalModel

predictbm <- predict(modelbm, newdata=testdata)
confbm <- confusionMatrix(predictbm, testdata$classe)
confbm

####____Final Result with RandomForest____####
predictresult <- predict(modelrf, newdata=testing)
predictresult
