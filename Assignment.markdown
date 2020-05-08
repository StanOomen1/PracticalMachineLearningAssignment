---
title: "Assignment"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##Loading in Packages and Data

```{r}
library(data.table)
library(caret)
library(corrplot)
library(rattle)

testing  <- read.csv("C:/Users/soomen/OneDrive - Capgemini/Desktop/Ontwikkeling/Coursera R/MachineLearning/Assignment/pml-testing.csv")
training <- read.csv("C:/Users/soomen/OneDrive - Capgemini/Desktop/Ontwikkeling/Coursera R/MachineLearning/Assignment/pml-training.csv")

```

##Creating training and testing datasets

```{r}
inTrain <- createDataPartition(training$classe, p=0.7, list=FALSE)
traindata <- training[inTrain,]
testdata <- training[-inTrain,]
```

##Data Cleaning

If more than 75% of the rows are NA, remove the row

```{r}
traindata <- traindata[, colSums(is.na(traindata)) < nrow(traindata) * 0.75]
testdata <- testdata[, colSums(is.na(testdata)) < nrow(testdata) * 0.75]
```

Check for NearZeroVariance columns and delete them

```{r}
NearZeroVariance <- nearZeroVar(traindata)
traindata <- traindata[ ,-NearZeroVariance]
testdata <- testdata[ ,-NearZeroVariance]
```

Delete first columns since they contain unusable formats


```{r}
traindata <- traindata[,-c(1:5)]
testdata <- testdata[,-c(1:5)]
```

Classe is column number 54 so it is left out of the correlation matrix
```{r}

corrMat <- cor(traindata[,-54])
corrplot(corrMat, method = "color", type = "lower", tl.cex = 0.8, tl.col = rgb(0,0,0))

```

##Random Forest

```{r}


modelrf <- train(classe~., method= "rf", data=traindata, trControl = trainControl(method="cv",number = 2, verboseIter=FALSE))
modelrf$finalModel
predictrf <- predict(modelrf, newdata=testdata)
confusionrf <- confusionMatrix(predictrf, testdata$classe)
confusionrf
```


##Descision Tree

```{r}
modeldt <- train(classe~., method= "rpart", data = traindata)
modeldt$finalModel
predictdt <- predict(modeldt, newdata=testdata)
confusiondt <- confusionMatrix(predictdt, testdata$classe)
confusiondt
fancyRpartPlot(modeldt$finalModel)
```



##Boosted model
```{r}
controls <- trainControl(method = "repeatedcv", number = 5, repeats = 1, verboseIter = FALSE)
modelbm <- train(classe ~ ., data = traindata, trControl = controls, method = "gbm", verbose = FALSE)
modelbm$finalModel

predictbm <- predict(modelbm, newdata=testdata)
confbm <- confusionMatrix(predictbm, testdata$classe)
confbm
```


##Final Result with RandomForest
Random forest has the highest accuracy as can be seen above, hence it is used for the final prediction.
```{r}
predictresult <- predict(modelrf, newdata=testing)
predictresult
```



