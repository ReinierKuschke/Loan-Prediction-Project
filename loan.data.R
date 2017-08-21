## Reinier Kuschke
## 12/08/2017
## Loan Prediction


# Load Data

loans <- read.csv('loan_data.csv')

# Load Libraries
library(ggplot2)
library(caTools)
library(e1071)

# View Data

str(loans)

summary(loans)

# Clean

## Check for NA Values 

any(is.na(loans))

## Check for Duplicates

anyDuplicated(loans)

## Convert to Factors

loans$credit.policy <- factor(loans$credit.policy)

loans$inq.last.6mths <- factor(loans$inq.last.6mths)

loans$delinq.2yrs <- factor(loans$delinq.2yrs)

loans$pub.rec <- factor(loans$pub.rec)

loans$not.fully.paid <- factor(loans$not.fully.paid)

# Exploritory Data Analysis

pl1 <- ggplot(loans,aes(x=fico)) + geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5)+ scale_fill_manual(values = c('green','red')) + theme_bw()


pl2 <- ggplot(loans,aes(x=factor(purpose))) + geom_bar(aes(fill=not.fully.paid),position = "dodge") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


pl3<-ggplot(loans,aes(int.rate,fico)) +geom_point(aes(color=not.fully.paid),alpha=0.3) + theme_bw()

# Building Model

## Train/Test split

set.seed(123)

spl = sample.split(loans$not.fully.paid, 0.7)

train = subset(loans, spl == TRUE)

test = subset(loans, spl == FALSE)

#train model


model <- svm(not.fully.paid ~ .,data=train)

summary(model)

#Test Model

predicted.values <- predict(model,test[1:13])

table(predicted.values,test$not.fully.paid)

## Tune Model

tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',
                     ranges=list(cost=c(1,10), gamma=c(0.1,1)))

model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)

# Model Performance

## Accurancy

(2338+38)/ (2338+422+75+38)

### Accuracy = 0.8270101


## Recall

2338/(2338+422)

### Recall = 0.8471014


## Precision

2338/(2338+38)

### Precision= 0.9840067







