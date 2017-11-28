#Reading CSV
dt<-read.csv(file="C:/Users/manis/Desktop/Binary Logistic Regression Data 18.05.2017_28KObs_ProfisorApproved_ACV.csv",header=TRUE)

#removing insignificant variables which are recieved from BLR
dt<-dt[,-c(3,6,11,18,19,22)]



#Converting all variables into factor
for (i in c(1:23)){
  dt[,i] <- as.factor(dt[,i])
}
str(dt)

#Partitioning data into training and Validation datasets
set.seed(1234)
pd<-sample(2,nrow(dt),replace = TRUE,prob = c(0.8,0.2))
train<-dt[pd==1,]
validate<-dt[pd==2,]

#Running Ctree model
library(party)
tree<-ctree(Profisor_IsApproved~.,data = train,controls=ctree_control(mincriterion = 0.99,minsplit = 500))
tree
plot(tree)

#running rpart model
library(rpart)
tree1<-rpart(Profisor_IsApproved~.,data=train)
library(rpart.plot)
text(tree1,use.n=TRUE)
rpart.plot(tree1,tweak=1.2)
rpartpredict<-predict(tree1,validate)

#misclassification error for train data
trainpred<-predict(object=tree,newdata=train)
tab<-table(train$Profisor_IsApproved,trainpred)
print(tab)
1-sum(diag(tab))/sum(tab)

#misclassification error for validate data
testpred<-predict(tree,newdata=validate)
tab<-table(testpred,validate$Profisor_IsApproved)
print(tab)
1-sum(diag(tab))/sum(tab)

#ROC and AUC
library(pROC)
prediction<-ifelse(trainpred==0,yes=0,no=1)
trainingROC<-roc(response=train$Profisor_IsApproved,predictor = prediction,plot=T,auc=T)
trainingROC
