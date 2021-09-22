
                    ######################LOgistic Regression###########################
#installing the packages
install.packages("ggpubr")
install.packages("moments")
install.packages("ROCR")
library(dplyr)
library(ggpubr)
library(moments)
library(caTools)
library(car)
library(ROCR)

#Reading the data set
setwd("C://Users//pooja//Downloads//Simplilearn Rprogramming//RProject_college_admission")
dafr <- read.csv("College_admission.csv")
head(dafr)

# Distribution of gre variable
ggdensity(dafr, x = "gre", fill = "lightgray", title = "gre") +
  scale_x_continuous(limits = c(100, 1000)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

#Distribution of gpa variable
ggdensity(dafr, x = "gpa", fill = "lightgray", title = "gpa") +
  scale_x_continuous(limits = c(1, 4)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

#Computing the skewness
skewness(dafr$gre,na.rm = TRUE)
skewness(dafr$gpa,na.rm = TRUE)
#Checking the Structure of the data set
str(dafr)
#converting variables into factor
dafr$admit=as.factor(dafr$admit)
dafr$rank=as.factor(dafr$rank)


#Checking structure and summary of the dataset after converting to factor
str(dafr)
summary(dafr)

#DataCleaning
#checking for missing values
sapply(dafr, function(x) sum(is.na(x)))

#outlier detection
boxplot(dafr$gre)
quantile(dafr$gre,c(0,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))
dafr=dafr[dafr$gre>=350,]
boxplot(dafr$gre)

boxplot(dafr$gpa)
quantile(dafr$gpa,c(0,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))
dafr=dafr[dafr$gpa>=2.5,]
boxplot(dafr$gpa)



#splitting the data into training and test dataset to be able to assess performance of the model
split = sample.split(dafr, SplitRatio = 0.80)
split
set.seed(312)


#Logistic regression on full data
train_regre=subset(dafr,split==TRUE)
test_regre=subset(dafr,split==FALSE)


logistic_model = glm(admit ~ ., 
                      data = train_regre, 
                      family = "binomial")
summary(logistic_model)

model = glm(admit~gre+ gpa+ Race+  rank, data=dafr, family=binomial())
summary(model)

model = glm(admit~gre+  gpa+  I(rank==2)+  I(rank==3)+  I(rank==4),data=dafr,  family=binomial())
summary(model)

logistic_model = glm(admit~ gre+ gpa+  I(rank==2)+  I(rank==3)+  I(rank==4), train_regre, family=binomial())
summary(logistic_model)

logistic_model = glm(admit~ gpa+  I(rank==2)+  I(rank==3)+  I(rank==4), train_regre, family=binomial())
summary(logistic_model)
#From model summary we can see that all the predictor variables are significant as we expected
vif(model)

# Predicted Probabilities
# Making prediction on testing data
predict_reg = predict(logistic_model, 
                       test_regre, type = "response")
predict_reg

# Changing probabilities
predict_reg = ifelse(predict_reg >0.5, 1, 0)

# Evaluating model accuracy
# Creating a confusion matrix
table(test_regre$admit, predict_reg)
test_regre$admit
predict_reg


# ROC-AUC Curve
ROCPred = prediction(predict_reg, test_regre$admit)
ROCPer = performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")

auc = performance(ROCPred, measure = "auc")
auc = auc@y.values[[1]]
auc
