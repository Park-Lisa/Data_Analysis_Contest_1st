library(rsq)
data(hcrabs)
attach(hcrabs)
num.S<- num.satellites
num.S[which(num.S>0)]<- 1



# Comparison using BIC 

model.6c=glm(num.S~color+width,family=binomial) # number of parameter is 5
num.param = length(summary(model.6c)$coef[,1])
num.param
summary(model.6c)$aic-2*5 +log(length(num.S))*5  # BIC value for model.6c
summary(model.6c)$aic-2*5#LR

model.5=glm(num.S~color+width+spine,family=binomial)
num.param = length(summary(model.5)$coef[,1])
num.param
summary(model.5)$aic -2*7 + log(length(num.S))*7 # BIC value for model.7b
summary(model.5)$aic -2*7 #LR




table6.3<-expand.grid(list(M=c("divorced","married"),E=c("yes","no"),
                           P=c("yes","no"), G=c("Female","Male")))
count<-c(17,4,54,25,36,4,214,322,28,11,60,42,17,4,68,130)
table6.3a<-table6.3[rep(1:(length(count)),count),]

stage31<-glm(M~E+P,family=binomial,data=table6.3a)
stage32<-glm(M~E*P,family=binomial,data=table6.3a)
stage33<-glm(M~E*P+G,family=binomial,data=table6.3a)

anova(stage31,stage32,stage33) 
1-pchisq(12.914+4.5477,df=2) #1,2 ????
1-pchisq(4.5477,df=1) #2,3 ????


summary(stage31)
anova(stage32)
summary(stage33)



#aic, bic
num.param = length(summary(stage31)$coef[,1])
num.param
summary(stage31)$aic-2*3 +log(length(table6.3$M))*3


num.param = length(summary(stage32)$coef[,1])
num.param
summary(stage32)$aic-2*4 +log(length(table6.3$M))*4



num.param = length(summary(stage33)$coef[,1])
num.param
summary(stage33)$aic-2*5 +log(length(table6.3$M))*5
#################################################################

library(ggplot2) 
library(dplyr)
library(Epi)
library (pROC)


r1<-ROC(form=table6.3a$M~table6.3a$E+table6.3a$P,data=table6.3a,plot="ROC")
r2<-ROC(form=table6.3a$M~table6.3a$E*table6.3a$P,data=table6.3a,plot="ROC")
r3<-ROC(form=table6.3a$M~table6.3a$E*table6.3a$P+table6.3a$G,data=table6.3a,plot="ROC")

plot(r1) 
plot(r2,add=T,lty=2)
plot(r3,add=T,lty=3)

roc.test(r1,r2,plot=T)







install.packages("melbench")
library(mlbench)
install.packages("caret")
library(caret)

BP<-factor(c("<117","117-126","127-136","137-146","147-156","157-166","167-
186",">186")) 
CHD<-c(3,17,12,16,12,8,16,8) 
n<-c(156,252,284,271,139,85,99,43) 

###############
train.sample<-createDataPartition(scores,p=0.8,list=F) 

scores1<-sample(scores,400,replace = TRUE)
CHD.n1<-sample(CHD/n,400,replace = TRUE)
train.data<-cbind(scores1,CHD.n1)
train.data<-data.frame(train.data)
resLL<-glm(CHD.n1~scores1,family=binomial, data=train.data,weights=n) 

scores2<-sample(scores,200,replace = TRUE)
CHD.n2<-sample(CHD/n,200,replace = TRUE)
test.data<-cbind(scores2,CHD.n2)
test.data<-data.frame(train.data2)




probability<-predict(resLL,test.data,type="response")
predicted.class<-ifelse(probability>0.5,"pos","neg")
mean(predicted.class==test.data$scores2)


resCHD<-glm(CHD/n~1,family=binomial, weights=n)
resLL<-glm(CHD/n~scores,family=binomial, data=train.data,weights=n) 
probability<-predict(resLL,test.data,type="response") 
predicted.class<-ifelse(probability>0.5,"pos","neg") 
mean(predicted.class==test.data$diabetes)

mean(predicted.class==sample(scores,8,replace = TRUE))

model.6<-glm(diabetes~pregnant+glucose+pressure+mass+pedigree,data=train.data, family=binomial)
probability<-predict(model.6,test.data,type="response") 

predicted.class<-ifelse(probability>0.5,"pos","neg") 

mean(predicted.class==test.data$diabetes)


################################

#Independence model: 
resCHD<-glm(CHD/n~1,family=binomial, weights=n) 
resCHD$deviance 

#Predicted responses, deviance residuals, Pearson residuals, and standardized Pearson residuals. 
pred.indep<-n*predict(resCHD, type="response") 
dev.indep<-resid(resCHD, type="deviance") 
pear.indep<-resid(resCHD, type="pearson") 

pear.std.indep<-resid(resCHD, type="pearson")/sqrt(1-lm.influence(resCHD)$hat) 
structure(cbind(pred.indep, dev.indep, pear.indep, pear.std.indep), dimnames = list(BP, c("fitted", "deviance resid", "pearson resid", "pearson std resid"))) 


#A plot of the standardized residuals shows an increasing trend. 
plot(pear.std.indep,xlab="",ylab="Standardized Pearson Residuals", pch=16, axes=F) 
axis(1,at=1:8, labels=as.character(BP), srt=90) 
axis(2);abline(h=0)



#Linear Logit Model: 
scores<-c(seq(from=111.5,to=161.5,by=10),176.5,191.5) 
resLL<-glm(CHD/n~scores,family=binomial,weights=n) 
resLL$deviance 

pred.indep<-n*predict(resLL, type="response") 
dev.indep <- resid(resLL, type = "deviance") 
pear.indep <- resid(resLL, type = "pearson") 
pear.std.indep <- resid(resLL, type = "pearson")/sqrt(1 - lm.influence(resLL)$hat) 
structure(cbind(pred.indep, dev.indep, pear.indep, pear.std.indep), dimnames = list(as.character(scores), c("fitted", "deviance resid", "pearson resid", "pearson std resid"))) 



win.graph(width=10, height=8) # R only
plot(scores,CHD/n,pch="X",yaxt="n",xaxt="n",ylim=c(0,.2), 
     xlab="Blood Pressure Level",ylab="Proportion",bty="L") 
axis(side=1, at=seq(from=110,to=200,by=10)) 
axis(side=2, at=seq(from=0,to=.2,by=.05)) 
lines(scores,predict(resLL, type="response"),type="l")



install.packages("ROCR")
install.packages("Metrics")
library(ROCR) 
library(Metrics)


#partition and create training, testing data
library(caret)
split <- createDataPartition(y = CHD/n, p = 0.6,list = FALSE)

new_train <- train[split] 
new_test <- train[-split]

> log_model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare + title, data = new_train[,-c("PassengerId","Name","Ticket")],family = binomial(link="logit"))
> log_predict <- predict(log_model,newdata = new_test,type = "response")




log_model1<-glm(CHD/n~1,family=binomial(link="logit"), weights=n)
log_predict1 <- predict(log_model1,newdata = new_test,type = "response")
log_predict1 <- ifelse(log_predict1 > 0.5,1,0)


pr <- prediction(log_predict1, new_test$CHD/n)
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
plot(perf) > auc(new_test$Survived,log_predict1) 


log_model2<-glm(CHD/n~scores,family=binomial(link="logit"),weights=n) 
log_predict2 <- predict(log_model2,newdata = new_test,type = "response")
log_predict2 <- ifelse(log_predict2 > 0.5,1,0)

pr <- prediction(log_predict2, new_test$CHD/n)
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
plot(perf) > auc(new_test$Survived,log_predict2)              
                
#7 roc
install.packages("roccv")
library(roccv)
#model1:
fit1 = rocCV(1, CHD/n, method = 'logistic')
fit1=roc(CHD/n, rep(1,8), ci=TRUE)
fit2=roc(CHD/n, scores, ci=TRUE)
roc.test(fit1,fit2)



library(pROC)
library(mlbench)
roccv(resCHD, resLL)

library(ggplot2)
rocs <- roc(table6.3$M ~table6.3$E + table6.3$P, data = table6.3)
ggroc(rocs)

r1<-ROC(form=CHD/n~1,plot="ROC")
r2<-ROC(form=CHD/n~scores,plot="ROC")


plot(r1) 
plot(r2,add=T,lty=2)

plot(r3,add=T,lty=3)

roc.test(r1,r2,plot=T)
                
                
              
