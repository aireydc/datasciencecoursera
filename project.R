<code>

rm(list=ls())
setwd("~/Desktop/Coursera/Machine_Learning/project")
library("caret")
library("randomForest")

pml <- read.csv("pml-training.csv")
classe <- as.data.frame(pml[,160])
names(classe) = c("classe")
summary(classe)
all(!is.na(classe$classe)) # no missing in "classe"
# keep vars in test data as there is no point training on other vars
varstokeep <- read.delim("variables.txt", header=FALSE, as.is=TRUE)
names(varstokeep) <- c("varstokeep")
pml <- pml[,as.character(varstokeep$varstokeep)]
nearZeroVar(pml) # vars ok in terms of enough variance

set.seed(1967)
inTrain <- createDataPartition(y=classe$classe, p=0.7, list=FALSE)

trainx <- pml[inTrain,]
trainy <- classe[inTrain,]

testx <- pml[-inTrain,]
testy <- classe[-inTrain,]

result = rfcv(trainx, trainy)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))
# so 10 or fewer variables is useful

rf_ctrl <- trainControl(method="repeatedcv", number=5, repeats=5)
rf_grid <- expand.grid(mtry=3:10)
rf_model <- train(trainx, trainy, method="rf", trControl=rf_ctrl, tuneGrid=rf_grid, metric="Accuracy")

plot(rf_model)

rf_testing <- predict(rf_model, newdata=testx)
rf_cf <- confusionMatrix(rf_testing, testy)


# Confusion Matrix and Statistics
# 
# Reference
# Prediction    A    B    C    D    E
# A 1674    6    0    0    0
# B    0 1131    7    0    0
# C    0    2 1019   11    0
# D    0    0    0  952    4
# E    0    0    0    1 1078
# 
# Overall Statistics
# 
# Accuracy : 0.9947          
# 95% CI : (0.9925, 0.9964)
# No Information Rate : 0.2845          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9933          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            1.0000   0.9930   0.9932   0.9876   0.9963
# Specificity            0.9986   0.9985   0.9973   0.9992   0.9998
# Pos Pred Value         0.9964   0.9938   0.9874   0.9958   0.9991
# Neg Pred Value         1.0000   0.9983   0.9986   0.9976   0.9992
# Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
# Detection Rate         0.2845   0.1922   0.1732   0.1618   0.1832
# Detection Prevalence   0.2855   0.1934   0.1754   0.1624   0.1833
# Balanced Accuracy      0.9993   0.9958   0.9953   0.9934   0.9980

