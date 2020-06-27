setwd("D:\\excelr_DS\\assignment\\naive bayes")
library(mlbench)

mydata <- read.csv(file.choose())
View(mydata)
summary(mydata)
str(mydata)

barplot(table(as.factor(mydata[,1]),as.factor(mydata[,2])),legend=c("ham","spam"))
plot(as.factor(mydata[mydata$type=="ham",2]))
plot(as.factor(mydata[mydata$type=="spam",2]))

train_db<-mydata[,c()]
set.seed(3)
train<-order(runif(290))
test<--train
training<-mydata[train,]
testing<-mydata[test,]

library(e1071)
model<-naiveBayes(training$type~.,data=training)
pred<-predict(model,newdata = testing[,-1])
mean(pred==testing[,1])

acc<-NULL


for (i in 1:20){train<-order(runif(350))
set.seed(350)
test<--train
training<-mydata[train,]
testing<-mydata[test,]
model<-naiveBayes(training$type~.,data=training)
pred<-predict(model,testing[,-1])
acc<-c(acc,mean(pred==testing[,1]))

}
acc

summary(mydata$type)



for (i in 1:50){train<-order(runif(500))
set.seed(500)
test<--train
training<-mydata[train,]
testing<-mydata[test,]
model<-naiveBayes(training$type~.,data=training)
pred<-predict(model,testing[,-1])
acc<-c(acc,mean(pred==testing[,1]))

}
acc
