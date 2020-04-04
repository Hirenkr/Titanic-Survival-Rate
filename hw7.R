
#Assignment 7

library(ISLR)
library(MASS)
library(class)
library(caret)
Train <- read.csv("C:/Users/Hiren/OneDrive/OneDrive - Texas A&M University/TAMU/Spring'20/Data Analysis/HW/HW7/ProjectTrain.csv")
Test <- read.csv("C:/Users/Hiren/OneDrive/OneDrive - Texas A&M University/TAMU/Spring'20/Data Analysis/HW/HW7/ProjectTest.csv")

names(Train)

##1

#Logistic Regression

##Assigning Mean value of Age to Train age datas with NA 
mean.age.train = mean(na.omit(Train$Age))
###Since mean is 29.62543, we round it to 29.5 as mentioned in Assignment (Variable Notes) 
mean.age.train = 29.5
num.train = which(is.na(Train$Age))
Train$Age[num.train] = mean.age.train

##Assigning Mean value of Age to Test age datas with NA 
names(Test)
mean.age.test=mean(na.omit(Test$Age))
mean.age.test
###Since mean is 29.87014, we round it to 29.5 as mentioned in Assignment (Variable Notes) 
mean.age.test = 29.5
num.test=which(is.na(Test$Age))
Test$Age[num.test] = mean.age.test



##Filling up the blank spaces in Embarked using Mode value
blank.embarked.train = which(Train$Embarked == "")
blank.embarked.train
S.embarked.train = length(which(Train$Embarked == "S"))
C.embarked.train = length(which(Train$Embarked == "C"))
Q.embarked.train = length(which(Train$Embarked == "Q"))

###From the above lengths, we observe 'S' to be the highest in the port of embarked
###Thereby the mode stands to be S. Further we assign S as embarled to the data slots with missing values of Embarked
Train$Embarked[Train$Embarked == ""]="S"
levels(Train$Embarked)

##Assigning Factor to the discrete variables
Train$Pclass = factor(Train$Pclass)
Train$Survived = factor(Train$Survived)
Train$Embarked = factor(Train$Embarked)
Test$Pclass = factor(Test$Pclass)
Test$Survived = factor(Test$Survived)
Train$Age = unlist(Train$Age)
Test$Age = unlist(Test$Age)


##----------LogisticRegression------------
##Logistic Regression for Training Dataset
logistic.fit.train=glm(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked, data = Train, family = binomial)
summary(logistic.fit.train)

##Prediction for Testing data based on the Logistic fit
logistic.prob.test = predict(logistic.fit.train, Test, type = "response")
logistic.pred.test = rep(0, length(Test$Survived))
logistic.pred.test[logistic.prob.test>0.5] = 1

conf_mat.logistic = table(logistic.pred.test,Test$Survived)[2:1,2:1]
mean(logistic.pred.test==Test$Survived)

conf_mat.logistic
true.positive.logistic = sensitivity(conf_mat.logistic)
false.positive.logistic = 1 - specificity(conf_mat.logistic)
true.positive.logistic
false.positive.logistic

##----------LDA-------------
##LDA Training]

lda.train.fit=lda(Survived~Parch + Sex + Age + SibSp + Pclass + Embarked, data = Train)
summary(lda.train.fit)

##LDA Prediction
lda.pred = predict(lda.train.fit, Test)
lda.class <- lda.pred$class

conf_mat.lda = table(lda.class,Test$Survived)[2:1,2:1]
mean(lda.class==Test$Survived)

conf_mat.lda
true.positive.lda = sensitivity(conf_mat.lda)
false.positive.lda = 1 - specificity(conf_mat.lda)
true.positive.lda
false.positive.lda

##----------QDA-------------
##QDA Training
qda.train.fit= qda(Survived~ Pclass + Parch + Sex + Age + SibSp + Embarked, data = Train)
summary(qda.train.fit)

##QDA Prediction
qda.pred = predict(qda.train.fit, Test)
names(qda.pred)
qda.class <- qda.pred$class
length(qda.class)

conf_mat.qda = table(qda.class,Test$Survived)[2:1,2:1]
mean(qda.class==Test$Survived)

conf_mat.qda
true.positive.qda = sensitivity(conf_mat.qda)
false.positive.qda = 1 - specificity(conf_mat.qda)
true.positive.qda
false.positive.qda


set.seed(1)
##----------KNN------------
train.X=cbind(Train$Pclass,Train$Sex,Train$Age,Train$SibSp,Train$Parch,Train$Embarked)  #training data of predictors
test.X=cbind(Test$Pclass,Test$Sex,Test$Age,Test$SibSp,Test$Parch,Test$Embarked)#test data of predictors
t<-rnorm(4)
true.positive.k<-rnorm(4)
false.positive.k<-rnorm(4)
n<-c(5,10,20,25)
for (i in 1:4)
{
  knn.pred=knn(train.X, test.X, Train$Survived,k=n[i])
  conf_mat.knn = table(knn.pred,Test$Survived)[2:1,2:1]
  true.positive.k[i] = sensitivity(conf_mat.knn)
  false.positive.k[i] = 1 - specificity(conf_mat.knn)
  t[i]= mean(knn.pred==Test$Survived)
}
plot(n,t,'b',xlab = "Value of K", ylab = "Test Accuracy")
t
true.positive.k
false.positive.k

##2

##Assigning NA to Cabin data with no value

cabin.train = Train$Cabin
cabin.train <- factor(cabin.train)
cabin.training <- c('ABC','SDX')
for (i in 1:624)
{
  cabin.training[i] = substring(cabin.train[i],1,1) 
    
}
cabin.training[cabin.training == ""] = 'X'
Train$Cabin <- cabin.training


cabin.test = Test$Cabin
cabin.test <- factor(cabin.test)
cabin.testing <- c('ABC','SDX')
for (i in 1:267)
{
  cabin.testing[i] = substring(cabin.test[i],1,1) 
  
}
cabin.testing[cabin.testing == ""] = 'X'
Test$Cabin <- cabin.testing


##----------Logistic Regression without Cabin-----------
##Logistic Regression for Training Dataset
logistic.fit.train=glm(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked, data = Train, family = binomial)
summary(logistic.fit.train)

##Prediction for Testing data based on the Logistic fit
logistic.prob.test = predict(logistic.fit.train, Test, type = "response")
logistic.pred.test = rep(0, length(Test$Survived))
logistic.pred.test[logistic.prob.test>0.5] = 1

table(logistic.pred.test,Test$Survived)
mean(logistic.pred.test==Test$Survived)

##----------Logistic Regression with Cabin-----------
##Logistic Regression for Training Dataset
logistic.fit.train.cabin=glm(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+Cabin, data = Train, family = binomial)
summary(logistic.fit.train.cabin)

##Prediction for Testing data based on the Logistic fit
logistic.prob.test.cabin = predict(logistic.fit.train.cabin, Test, type = "response")
logistic.pred.test.cabin = rep(0, length(Test$Survived))
logistic.pred.test.cabin[logistic.prob.test.cabin>0.5] = 1

table(logistic.pred.test.cabin,Test$Survived)
mean(logistic.pred.test.cabin==Test$Survived)

  
##3

###Determining the adjusted odds ratio
logistic.fit.adjusted = glm(Survived~Pclass+Sex+Embarked, data = Train, family = binomial)
summary(logistic.fit.adjusted)

logistic.fit.adjusted$coefficients
or <- exp(logistic.fit.adjusted$coefficients)
or
cbind(or, exp(confint(logistic.fit.adjusted)))

##4

##----------Logistic Regression------------
##Logistic Regression for Training Dataset
logistic.fit.train=glm(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked, data = Train, family = binomial)
summary(logistic.fit.train)
logistic.prob.test = predict(logistic.fit.train, Test, type = "response")
logistic.pred.test = rep(0, length(Test$Survived))


##Prediction for Testing data based on the Logistic fit for threshold = 0.8
logistic.pred.test[logistic.prob.test>0.8] = 1
conf_mat = table(logistic.pred.test,Test$Survived)[2:1,2:1]
mean(logistic.pred.test==Test$Survived)
false.positive.t1 = 1 - specificity(conf_mat)
true.positive.t1 = sensitivity(conf_mat)
true.positive.t1
false.positive.t1

##Prediction for Testing data based on the Logistic fit for threshold = 0.5
logistic.pred.test[logistic.prob.test>0.5] = 1
conf_mat = table(logistic.pred.test,Test$Survived)[2:1,2:1]
mean(logistic.pred.test==Test$Survived)
false.positive.t2 = 1 - specificity(conf_mat)
true.positive.t2 = sensitivity(conf_mat)
true.positive.t2
false.positive.t2

##Prediction for Testing data based on the Logistic fit for threshold = 0.2
logistic.pred.test[logistic.prob.test>0.2] = 1
conf_mat =  table(logistic.pred.test,Test$Survived)[2:1,2:1]
mean(logistic.pred.test==Test$Survived)
false.positive.t3 = 1 - specificity(conf_mat)
true.positive.t3 = sensitivity(conf_mat)
true.positive.t3
false.positive.t3

##5

##ROC Model for LDA
library(pROC)
lda.pred$posterior
roc(Test$Survived,lda.pred$posterior[,2],percent = TRUE, plot = TRUE, ci =TRUE, main="ROC Curve")
Test$Survived
lda.pred$posterior[,2]

##6
set.seed(1)
###Determining the adjusted odds ratio
names(Train)
logistic.fit.adjusted = glm(Survived~Pclass+Sex+Embarked+Age+SibSp+Parch+Embarked, data = Train, family = binomial)
summary(logistic.fit.adjusted)

logistic.fit.adjusted$coefficients
or <- exp(logistic.fit.adjusted$coefficients)
or
cbind(or, exp(confint(logistic.fit.adjusted)))


train.X=cbind(Train$Pclass,Train$Sex,Train$Age,Train$SibSp)  #training data of predictors
test.X=cbind(Test$Pclass,Test$Sex,Test$Age,Test$SibSp) #test data of predictors
t<-rnorm(4) 
true.positive.k1<-rnorm(4)
false.positive.k1<-rnorm(4)
n<-c(5,10,20,25)
for (i in 1:4)
{
  knn.pred=knn(train.X, test.X, Train$Survived,k=n[i])
  conf_mat.knn = table(knn.pred,Test$Survived)[2:1,2:1]
  true.positive.k1[i] = sensitivity(conf_mat.knn)
  false.positive.k1[i] = 1 - specificity(conf_mat.knn)
  t[i]= mean(knn.pred==Test$Survived)
}
plot(n,t,'b',xlab = "Value of K", ylab = "Test Accuracy")
t
true.positive.k1
false.positive.k1

