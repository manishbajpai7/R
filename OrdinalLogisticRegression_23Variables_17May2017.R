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

for (i in c(23)){
olr[,i]<-as.ordered(olr[,i])
}

str(olr)
summary(olr)

#Removing Rank Deficient Variable

olr<-olr[,-c(1,2,3,7,14)]


#write.csv(olr,file="C:/Users/manis/Desktop/CheckingOLR.csv",row.names = TRUE)

#partition data set
ind<-sample(2,nrow(olr),replace=TRUE,prob = c(0.8,0.2))
train<-olr[ind==1,]
test<-olr[ind==2,]

#ordinal logistic regression or proportional odds logistic regression
library(MASS)
model<-polr(RiskCategory~.,data=train,Hess = TRUE)
summary(model)

#p value calculation
(ctable<-coef(summary(model)))
p<-pnorm(abs(ctable[,"t value"]),lower.tail = FALSE) * 2
(ctable<-cbind(ctable,"p value" = p))

#prediction
pred<- predict(model, train, type='prob')
print(pred, digits = 3)

#confusion Matrix & Misclassification error for training data
(tab<-table(pred,train$Final.Decision))
1-sum(diag(tab))/sum(tab)

#Confusion Matrix & Misclassification Error for test data
pred1<-predict(model,test)
(tab1<-table(pred1,test$Final.Decision))
1-sum(diag(tab1))/sum(tab1)