#reading data
olr<-read.csv(file="C:/Users/manis/Desktop/Ordinal Logistic Regression Data 18.05.2017_MBB_15616Obs_Ratings.csv",header=TRUE)
colnames(olr)

#removing insignificant variables
olr<-olr[,-c(3,6,11,18,19,22)]

#converting columns into factors
for (i in c(1:14,16:18,22,23)){
  olr[,i] <- as.factor(olr[,i])
}
for (i in c(15,19:21)){
  olr[,i]<-as.numeric(olr[,i])
}

str(olr)
summary(olr)

#relevel of risk category
olr$riskcategory<-relevel(olr$RiskCategory,ref="1")


#multinominal logistic regression
library(nnet)
mlr<-multinom(riskcategory~.,olr)
summary(mlr)

#predict
predict(mlr,data=olr,type = "prob")

#misclassification error
cm<- table(predict(mlr),olr$RiskCategory)
print(cm)
x