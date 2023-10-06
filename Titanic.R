library(ggplot2)


#Logistic Regression Model
df.train<-read.csv('titanic_train.csv')
print(head(df.train))
str(df.train)
head(df.train)

#Checking how many survived vs not survived
ggplot(df.train,aes(x=Survived)) + geom_bar()

#Checking different levels of Passenger class
ggplot(df.train,aes(Pclass))+geom_bar(aes(fill=factor(Pclass)))


#Checking count of males and females on board
ggplot(df.train,aes(Sex))+geom_bar(aes(fill=Sex))

#Checking count of different age groups on board                                  
ggplot(df.train,aes(x=Age))+geom_histogram(fill='blue',bins = 20,alpha=0.5)

#Checking count of Siblings and Spouse on Board
ggplot(df.train,aes(SibSp))+geom_bar()

#Checking different Fare Paid
ggplot(df.train,aes(Fare))+geom_histogram(color='black',fill='green',bins=20,alpha=0.5)

#Plotting boxplot by different Pclass to find average age for different class and later we will fill the age by different average age group of different class

pl<-ggplot(df.train,aes(x=Pclass,y=Age))
pl<-pl+geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4))
pl+scale_y_continuous(breaks = seq(min(0),max(80),by=2))


#Imputation of Age based on Class
impute_age<-function(age,class)
{
  out<-age
  for(i in 1:length(age))
  {
    if(is.na(age[i]))
    {
      if(class[i]==1)
      {
        age[i]<-37
      }else if(class[i]==2)
      {
        age[i]<-29
      }else
      {
        age[i]=24
      }
      
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

fixed.ages<-impute_age(df.train$Age,df.train$Pclass)
fixed.ages

df.train$Age<-fixed.ages


missmap(df.train,main='Imputation Check',color=c('yellow','black'),
        legend=F)


library(dplyr)
df.train<-select(df.train,-PassengerId,-Name,-Ticket,-Cabin)
head(df.train,1)


logistic.model<-glm(Survived ~ .,family=binomial(link='logit'),data=df.train)
summary(logistic.model)

library(caTools)
set.seed(101)
split<-sample.split(df.train$Survived,SplitRatio = 0.7)
final.train<-subset(df.train,split==T)
final.test<-subset(df.train,split==F)
final.log.model<-glm(Survived ~ .,family=binomial(link='logit'),data=final.train)
summary(final.log.model)

fitted.probabilities<-predict(final.log.model,final.test,type='response')
fitted.results<-ifelse(fitted.probabilities>0.5,1,0)
mis.error<-mean(fitted.results!=final.test$Survived,na.rm = T)
accuracy<-1-mis.error
accuracy

#Confusion Matrix
table(final.test$Survived,fitted.probabilities>0.5)

#Now lets test the accuracy of our model in titanic_test.csv
df.test<-read.csv('titanic_test.csv')
head(df.test,1)
missmap(df.test,main = 'Imputation Check',color=c('red','black'),legend=F)

test.Age<-impute_age(df.test$Age,df.test$Pclass)
df.test$Age<-test.Age

#Visualizing Age feature of df.test it with ggplot 
ggplot(df.test,aes(x=Age))+geom_histogram(fill='blue',alpha=0.5,bins=20)


#Now lets make the model

fitted_test_prob<-predict(final.log.model,df.test,type='response')
fitted.test.result<-ifelse(fitted_test_prob>0.5,1,0)
mis_test_error<-mean(df.test$Survived!=fitted.test.restult,na.rm = T)
