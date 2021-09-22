

    ##################################SVM#####################################
#installing the packages
install.packages("caret")
library(caret)
library(e1071)
    
#Reading the data set
setwd("C://Users//pooja//Downloads//Simplilearn Rprogramming//RProject_college_admission")
dafm = read.csv("College_admission.csv")
head(dafm)
str(dafm)
summary(dafm)

# Converting variables to factor
dafm$admit=as.factor(dafm$admit)
dafm$rank=as.factor(dafm$rank)
str(dafm)

#checking for missing values
sapply(dafm, function(x) sum(is.na(x)))
n=nrow(dafm)
n

ntrain=round(n*0.80)
ntrain
set.seed(314)
tindex = sample(n, ntrain)   # Create a random index
train_dafm = dafm[tindex,]   # Create training set
#train_dafm
test_dafm = dafm[-tindex,]   # Create test set
#test_dafm

svm1 <- svm(admit~ gre+gpa+rank, data=train_dafm,method="C-classification",kernal="radial",gamma=0.1,cost=10)
svm1$SV
summary(svm1)


plot(svm1,data=train_dafm,gre~gpa,slice = list(admit=3,rank=4))
prediction = predict(svm1, test_dafm)
xtab = table(test_dafm$admit, prediction)
xtab
(59+2)/nrow(test_dafm)

confusionMatrix(table(test_dafm$admit,prediction))





