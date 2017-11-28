starttime<-Sys.time()
blr<-read.csv(file="C:/Users/manis/Desktop/Binary Logistic Regression Data 18.05.2017_28KObs_ProfisorApproved.csv",header=TRUE)
colnames(blr)
for (i in c(1:5,7:18,21:24,28,29)){
  blr[,i] <- as.factor(blr[,i])
}
for (i in c(6,19,20,25,26,27)){
  blr[,i]<-as.numeric(blr[,i])
}
str(blr)
set.seed(25)
ranuni <- sample(x=c("Training","Testing"),size=nrow(blr),replace=T,prob=c(0.7,0.3))
TrainingData <- blr[ranuni=="Training",]
TestingData <- blr[ranuni=="Testing",]
indVariables <- colnames(blr[,1:28])
rhsOfModel <- paste(indVariables,collapse="+")
rhsOfModel
model <- paste("Profisor_IsApproved ~ ",rhsOfModel)
model
frml <- as.formula(model)
frml
library(MASS)
TrainModel <- glm(formula=frml,family="binomial",data=TrainingData)

finalModel <- stepAIC(object=TrainModel)# stepwise regression
 summary(object=finalModel)
finalModel$anova
TrainPred <- finalModel$fitted.values
TrainRspns <- ifelse(TrainingData$Profisor_IsApproved ==1,yes=1,no=0)
library(MKmisc)
HLgof.test(fit=TrainPred,obs=TrainRspns)
library(pROC)
trainingROC <- roc(response=TrainingData$Profisor_IsApproved,predictor=TrainPred,plot=T,auc=T)
trainingROC$auc
TrainPredRspns <- ifelse(test= TrainPred < 0.5, yes= 0, no= 1)
tab<-table(TrainingData$Profisor_IsApproved,TrainPredRspns)
TestPred <- predict(object=finalModel,newdata=TestingData, type="response")
TestPredRspns <- ifelse(test= TestPred < 0.5, yes= 0, no= 1)
table(TrainingData$Profisor_IsApproved,TrainPredRspns)
sum(diag(tab))/sum(tab)
endtime<-Sys.time()
starttime-endtime