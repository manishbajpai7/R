x<-c("CIBIL 300-599","CIBIL 600-749","CIBIL 750-900", "HIMARK 300-619", "HIMARK 620-719","HIMARK 720-850","Residence Address Verification-Positive","Residence Address Verification-Negative","Residence Address Verification-Reference","Fraud Check Postive", "Fraud Check Negative","Fraud Check Reference", "Ever Loan Defaults Multiple","Ever Loan Defaults Yes","Ever Loan Defaults No", "Source of Income Daily Wage Earner","Source of Income Self Employed","Source of Income Employed", "Household Income Less than 20000", "Household Income Between 20000 and 40000","Household Income More than 40000", "Household Savings Less than 10000","Household Savings Between 10000 and 20000","Household Savings Greater than 20000","Income Stability Less than 6 months","Income Stability Less than 1 year","Income Stability More than 1 year","Property Technical Appraisal Positive","Property Technical Appraisal Negative","Property Technical Appraisal Reference","Borrower Contribution Less than 20%","Borrower Contribution between 20 to 30 %","Borrower Contribution between 30 to 70 %" )
CIBIL<-c("CIBIL 300-599","CIBIL 600-749","CIBIL 750-900")
HIMARK<-c("HIMARK 300-619", "HIMARK 620-850")
RAV<-c("Residence Address Verification-Positive","Residence Address Verification-Negative","Residence Address Verification-Reference")
FCU<-c("Fraud Check Postive", "Fraud Check Negative","Fraud Check Reference")
LD<-c("Ever Loan Defaults Yes","Ever Loan Defaults No")
SI<-c("Source of Income Daily Wage Earner","Source of Income Self Employed","Source of Income Employed")
Income<-c("Household Income Less than 20000", "Household Income Between 20000 and 40000","Household Income More than 40000")
Savings<-c("Household Savings Less than 10000","Household Savings Between 10000 and 20000","Household Savings Greater than 20000")
Stability<-c("Income Stability Less than 6 months","Income Stability Less than 1 year","Income Stability More than 1 year")
TPV<-c("Property Technical Appraisal Positive","Property Technical Appraisal Negative","Property Technical Appraisal Reference")
BC<-c("Borrower Contribution Less than 20%","Borrower Contribution between 20 to 30 %","Borrower Contribution between 30 to 70 %")
check<-expand.grid(CIBIL,HIMARK,RAV,FCU,LD,SI,Income,Savings,Stability,TPV,BC)
length(RAV)
