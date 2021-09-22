
############################Decission Tree####################################
install.packages("rpart.plot")
library(rpart.plot)
library(rpart)
library(caret)


setwd("C://Users//pooja//Downloads//Simplilearn Rprogramming//RProject_college_admission")
df? = read.csv("College_admission.csv")
head(dfm)
str(dfm)
summary(dfm)

dfm$admit=as.factor(dfm$admit)
dfm$rank=as.factor(dfm$rank)

str(dfm)
summary(dfm)

v=dfm$admit
table(v)

set.seed(341)
dfm[, 'train'] = ifelse(runif(nrow(dfm)) < 0.80, 1, 0)
trainset=df?[dfm$train==1,]
testset=dfm[dfm$train==0,]

trainColNum = grep('train', names(trainset))

trainset = trainset[, -trainColNum]
testset = testset[, -trainColNum]


treeFit = rpart(admit~ gre+gpa+rank,data=trainset,method = 'class')
print(treeFit)

rpart.plot?treeFit, box.col=c("yellow", "blue"))
Prediction1 = predict(treeFit,newdata=testset[-4],type = 'class')


confusionMatrix(Prediction1,testset$admit)


## Pruning the decision tree

printcp(treeFit)
opt  =  which.min(treeFit$cptable[,'xerror'])


cp =  tree?it$cptable[opt, 'CP']
pruned_model =  prune(treeFit,cp)
rpart.plot(pruned_model, box.col=c("red", "green"))


rpart_pruned_predict = predict(pruned_model, newdata=testset[-5],type = 'class')
mn2 = mean(rpart_pruned_predict==testset$admit)
mn2


confusionMa?rix(rpart_pruned_predict,testset$admit)
