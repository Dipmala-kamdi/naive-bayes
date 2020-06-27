setwd("D:\\excelr_DS\\assignment\\naive bayes")
library(mlbench)

salary_test<- read.csv(file.choose())
View(salary_test)
any(is.na(salary_test))

summary(salary_test)
str(salary_test)
colnames(salary_test)

barplot(table(as.factor(salary_test[,14]),as.factor(salary_test[,2])),legend=c("<=50K",">50K"))
barplot(table(as.factor(salary_test[,14]),as.factor(salary_test[,3])),legend=c("<=50K",">50K"))
barplot(table(as.factor(salary_test[,14]),as.factor(salary_test[,5])),legend=c("<=50K",">50K"))
barplot(table(as.factor(salary_test[,14]),as.factor(salary_test[,5])),legend=c("<=50K",">50K"))
barplot(table(as.factor(salary_test[,14]),as.factor(salary_test[,6])),legend=c("<=50K",">50K"))
barplot(table(as.factor(salary_test[,14]),as.factor(salary_test[,6])),legend=c("<=50K",">50K"))

train_db<-salary_test[,c()]
set.seed(5)
train<-order(runif(505))
test<--train
training<-salary_test[train,]
testing<-salary_test[test,]

library(e1071)
model<-naiveBayes(training$Salary~.,data=training)
pred<-predict(model,newdata = testing[,-14])
mean(pred==testing[,14])

acc<-NULL

for (i in 1:30){train<-order(runif(550))
set.seed(550)
test<--train
training<-salary_test[train,]
testing<-salary_test[test,]
model<-naiveBayes(training$Salary~.,data=training)
pred<-predict(model,testing[,-14])
acc<-c(acc,mean(pred==testing[,14]))

}
acc

summary(salary_test$Salary)
