#reading data
olr<-read.csv(file="C:/Users/manis/Desktop/RegressionAnalysis/OrdinalRegression_MockedUpData_LoanStatus.csv",header=TRUE)
colnames(olr)


#converting columns into factors
for (i in c(1:8)){
  olr[,i] <- as.factor(olr[,i])
}


str(olr)
summary(olr)


#write.csv(olr,file="C:/Users/manis/Desktop/CheckingOLR.csv",row.names = TRUE)

#partition data set
ind<-sample(2,nrow(olr),replace=TRUE,prob = c(0.8,0.2))
train<-olr[ind==1,]
test<-olr[ind==2,]

#ordinal logistic regression or proportional odds logistic regression
library(nnet)
mlr<-multinom(Loan.Status~.,olr)
summary(mlr)

predict(mlr,data=olr,type = "prob")

#misclassification error
cm<- table(predict(mlr),olr$Loan.Status)
print(cm)
