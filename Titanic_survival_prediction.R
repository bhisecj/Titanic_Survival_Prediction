# Data analysing and understanding
test_data<-titanic_test
train_data<-titanic_train
str(train_data)
summary(train_data$Age)
dim(train_data)


#Clean the given data 
medAge<-median(train_data$Age,na.rm = T)
train_data$Age[is.na(train_data$Age)]<-medAge

unique(train_data$Sex)
train_data$Sex=ifelse(train_data$Sex=='male',1,0)
train_data$Sex

unique(train_data$Embarked)
table(train_data$Embarked)
train_data$Embarked[is.na(train_data$Embarked)]<-'S'
train_data$Embarked<-sub('S',0,train_data$Embarked)
train_data$Embarked<-sub('C',1,train_data$Embarked)
train_data$Embarked<-sub('Q',2,train_data$Embarked)
ind<-sample(2,nrow(train_data),replace =T,prob =c(0.8,0.2))
train_data=train_data[ind==1,]
validation_data=train_data[ind==2,]
Survived=train_data$Survived
Sex=train_data$Sex
Age=train_data$Age
train_data$Embarked=as.numeric(train_data$Embarked)
Embarked=train_data$Embarked
Siblings=train_data$SibSp
table(validation_data$Survived)

#train the logistic regression model
model=glm(Survived~Sex+Age+Siblings+Embarked, data = train_data)

#prediction on validation data
Survived=validation_data$Survived
Sex=validation_data$Sex
Age=validation_data$Age
validation_data$Embarked=as.numeric(validation_data$Embarked)
Embarked=validation_data$Embarked
Siblings=validation_data$SibSp

predicted=predict(model,validation_data)
predicted=ifelse(predicted>0.5,1,0)

#Classification accuracy on validation data
tab<-table(predicted,validation_data$Survived)
confusion_matrix<-(sum(diag(tab))/sum(tab))

#Prediction on Titanic test data
dim(test_data)

#Clean the test data
medAge<-median(test_data$Age,na.rm = T)
test_data$Age[is.na(test_data$Age)]<-medAge

unique(test_data$Sex)
test_data$Sex=ifelse(test_data$Sex=='male',1,0)
test_data$Sex

unique(test_data$Embarked)
table(test_data$Embarked)
test_data$Embarked[is.na(test_data$Embarked)]<-'S'
test_data$Embarked<-sub('S',0,test_data$Embarked)
test_data$Embarked<-sub('C',1,test_data$Embarked)
test_data$Embarked<-sub('Q',2,test_data$Embarked)
Sex=test_data$Sex
Age=test_data$Age
test_data$Embarked=as.numeric(test_data$Embarked)
Embarked=test_data$Embarked
Siblings=test_data$SibSp

#predict the values of target feature for test data
test_predict<-predict(model, test_data)
test_predict=ifelse(test_predict>0.5,1,0)
table(test_predict)
submit<-cbind.data.frame(test_data$PassengerId,test_predict)
write.csv(submit,file = 'titanic_submission.csv')