##########################################################################
# Prepare to use the entire dataset as training
## First run R code

setwd('C:\\Users\\everyday use\\Dropbox\\Luann\\Challenge1Data\\for_submission')
source('functions.r')

train=read.csv('Challenge_1_Training.csv')
attach(train)
table(readmitted)
train$admission_source_id=as.factor(admission_source_id)
train$discharge_disposition_id = as.factor(discharge_disposition_id)
train$admission_type_id =as.factor(admission_type_id)
removeFactor=c(1,2, 30, 38, 40, 41,45, 46 , 47)
#colnames(train[,removeFactor])

# check to see which variables have no (or nearly no different values)
rem=NULL; numberOflevels=NULL
for (i in 1:ncol(train)){
if (length(summary(train[,i]) )==1 ) rem=c(rem, i)
if (is.factor(train[,i])==T) numberOflevels= c( numberOflevels, length(levels(train[,i])))
#print(i)
#print(summary(train[,i]) )
}
count=table(numberOflevels)
barplot(count, xlab='Number of Categories', ylab='Number of variables', col='purple' )
#text( x=1:length(count) , y=count+3, labels=count)

trainkeep=data.frame(train[,-removeFactor])

test=read.csv('Challenge_1_Validation.csv')
test$admission_source_id=as.factor(test$admission_source_id)
test$discharge_disposition_id = as.factor(test$discharge_disposition_id)
test$admission_type_id =as.factor(test$admission_type_id)
testkeep=data.frame(test[,-removeFactor])

### Check to see if factors in traing and test data have the same levels. If not assign common levels.
## # saw that variable 22, 28, 30, 37 etc have different categories in the test data from training data
checking=NULL; changed=NULL
for (i in 1:ncol(testkeep)){
if (!(is.factor(trainkeep[,i]) & is.factor(testkeep[,i]) )) checking=c(checking, i)
if (is.factor(testkeep[,i])&(length(unique(trainkeep[,i]))!= length(unique(testkeep[,i]))) ){ 
    print(i); changed=c(changed, i)
    cat('train', unique(trainkeep[,i]), 'test', unique(testkeep[,i]), '\n' )
    newlevels=unique(c(levels(trainkeep[,i]), levels(testkeep[,i]) ) )
    levels(testkeep[,i])=newlevels; levels(trainkeep[,i])=newlevels
}
}

trainX= model.matrix(readmitted ~ . , data=trainkeep )[,-1]
dim(trainX)
trainY=as.factor(train[, ncol(train)])
write.csv(trainX, file='trainXwhole.csv', row.names=F)
write.csv(trainY, file='trainYwhole.csv', row.names=F)

testX= model.matrix( ~ . , data=testkeep )[,-1]
dim(testX)
write.csv(testX, file='testXnew.csv', row.names=F)
write.csv(data.frame(encounter_id=test[,1]),  file="test_encounter_id.csv",row.names=F)

#trainX=as.matrix(read.csv('trainXwhole.csv'))
#trainY=read.csv('trainYwhole.csv')
#install.packages('glmnet')
#library(glmnet)
##lasso=glmnet(trainX, trainY, family="multinomial", alpha=1)
##plot(ridge, xvar='dev')

#testX=as.matrix(read.csv('testXnew.csv'))
#testY=read.csv('testY.csv')
#cv1=cv.glmnet(trainX, trainY, family="multinomial", alpha=1, type.measure="class")
#Ypred=predict ( cv1, newx=testX, s=cv1$lambda.min, type ="class") 

#res=table(Ypred, drop(as.matrix(testY)))
#sum(diag(res))/sum(res)
# 0.5764697
#(res[1,2]+res[2,1]+res[1,3]+res[3,1])/sum(res)



################################ Below is python 2.7 code
#
# From Dos version of Python window, run the following code

import os
import pandas as pd
import numpy as np
import statsmodels.api as statmod
import pylab   #for generating plots
#import pasty

from sklearn.cross_validation import train_test_split
from sklearn.linear_model import LogisticRegression as LR
from sklearn.metrics import roc_auc_score as AUC

from patsy.contrasts import Treatment
from patsy import (ModelDesc,dmatrices, dmatrix, Term, LookupFactor)

os.chdir('C:\\Users\\everyday use\\Dropbox\\Luann\\Challenge1Data\\for_submission')

train75DesignMatrix = pd.read_csv("trainXwhole.csv")   # from R
train75Y = pd.read_csv("trainYwhole.csv")


###
test75DesignMatrix = pd.read_csv("testXnew.csv")
test_x_id =  pd.read_csv("test_encounter_id.csv")
#test75Y = pd.read_csv("testY.csv")   won't be available


pathname='C:\\Users\\everyday use\\Dropbox\\Luann\\Challenge1Data\\for_submission\\'
from sklearn.metrics import accuracy_score
def train_and_predict( model, train_x, train_y, test_x,  filename):	
    model.fit( train_x, train_y )	
    p = model.predict( test_x )	
    output = pd.DataFrame( data={"Predicted_readmitted":p } )
    # Use pandas to write the comma-separated output file
    output.to_csv(os.path.join(os.path.dirname(pathname), filename),index=False, quoting=3)   
    print "Wrote results to"+ filename
    #my_accuracy = accuracy_score(actual_labels, predicted_labels, normalize=False) / float(actual_labels.size)
    #my_accuracy = accuracy_score(test_y, p, normalize=False) / float(test_y.size)
    #accuracy=model.score(test_x, test_y)
    #return my_accuracy
    #return accuracy



lr = LR(penalty='l1', multi_class='ovr')
accuLR=train_and_predict(lr, train75DesignMatrix, train75Y, test75DesignMatrix, 'LuannJungPredictChallenge1.csv')
#print "logistic regression accuracy:", accuLR