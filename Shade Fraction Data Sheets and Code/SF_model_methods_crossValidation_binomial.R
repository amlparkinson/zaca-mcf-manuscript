library(tidyverse)
sf <-  read_csv("Final Zaca MCF SF_MainData.csv")

# cross validation model ------------------------------------------------------------------------------------------------


library(caret)
library(plyr)
library(boot)

#divide the response variable into equal number of observations
tc_yes <- sf[which(sf$new_model_class == "1"),]
tc_no <- sf[which(sf$new_model_class == "0"),]

yes <- sample(1:nrow(tc_yes), 99 , replace = FALSE) # pull 99 random observations from the tc_yes class and separate
tc_yes_equal <- tc_yes[yes,]
no <- sample(1:nrow(tc_no), 99 , replace = FALSE) # pull 99 random observations from the tc_no class and separate
tc_no_equal <- tc_no[no,]

equal_tc_data <- do.call("rbind", list(tc_yes_equal, tc_no_equal)) # combine all random observations

# youtube code
control_parameters <- trainControl(method='cv',
                        number=5,
                        savePredictions = T,
                        classProbs=T)


parameter_grid <- expand.grid(mtry=c(2,3,4))

cv_model <- train(new_model_class~ shade_2018, 
                  data=equal_tc_data, 
                  method= "glm",
                  family=binomial(),
                  trControl=control_parameters,
                  tuneGrid=parameter_grid)

tc_model <- glm(new_model_class~shade_2018+min_2000_2006+prefire, data=train, family="binomial")

# try code with RF data -----------------------------------------------------------------------------------

train_data <- do.call("rbind", list(polygon_data_high_train, polygon_data_low_train, polygon_data_moderate_train, polygon_data_none_train)) 
train_data <- as.data.frame(train)

# youtube code
control_parameters <- trainControl(method='cv',
                                   number=5,
                                   savePredictions = T,
                                   classProbs=T)


parameter_grid <- expand.grid(mtry=c(2,3,4))

cv_model <- train(Mortality_Qualitative~precip+temp+tpi+elev,  
                  data=train_data, 
                  method= "rf",
                  #family=binomial(),
                  trControl=control_parameters,
                  tuneGrid=parameter_grid)

# code went through without errors. IDK what to do next. 

#end--------------------------------------------------------------------------------------------
for (i in 1:5) {
  
  cv.error.k <- rep(0,5)
  tc_model <-  glm(equal_tc_data$new_model_class~equal_tc_data$shade_2018+equal_tc_data$min_2000_2006, data=train, family="binomial")
  cv.err.k[i]=cv.glm(equal_tc_data, tc_model, K=5)$delta[1]

  cv.err.k
  }


# SF binomial model with train and test data -----------------------------------------------------------------------------------------------------------------------

tc_yes <- sf[which(sf$new_model_class == "1"),]
tc_no <- sf[which(sf$new_model_class == "0"),]

yes <- sample(1:nrow(tc_yes), 99 , replace = FALSE) # pull 99 random observations from the tc_yes class and separate
tc_yes_equal <- tc_yes[yes,]
no <- sample(1:nrow(tc_no), 99 , replace = FALSE) # pull 99 random observations from the tc_no class and separate
tc_no_equal <- tc_no[no,]

equal_tc_data <- do.call("rbind", list(tc_yes_equal, tc_no_equal)) # combine all random observations

#train and test data
x<- sample(198, 130)
train<-equal_tc_data[x,]
test<-equal_tc_data[-x,]

#model code
tc <- glm(new_model_class~shade_2018+min_2000_2006+prefire, data=train, family="binomial")
sum <-summary(tc)
sum


# predict data 
pred<-predict(tc, test, type="response") 

# error rate for data
pred1 <- ifelse (pred>0.5,1,0)
tab<- table(Predicted=pred1, Actual=test$new_model_class)
tab

acc <-1-sum(diag(tab))/sum(tab)   #error calculation. gives % observations predicted incorrectly 
acc <- 1- acc 
acc


# SF binomial model just run on train data ----------------------------------------------------------------------------------------------------------
acc_list <- list()
table_values <- list()
model_aic <-list()
auc_list <- list ()
#prediction_list <- list()

for (i in 1:500)
  {
  #divide the response variable into equal number of observations
  tc_yes <- sf2[which(sf2$new_model_class == "1"),]
  tc_no <- sf2[which(sf2$new_model_class == "0"),]
  
  # pull 175 random observations from the high class and separate
  yes <- sample(1:nrow(tc_yes), 99 , replace = FALSE)
  polygon_data_high_train <- tc_yes[yes,]
  # pull 175 random observations from the low class and separate
  no <- sample(1:nrow(tc_no), 99 , replace = FALSE)
  polygon_data_low_train <- tc_no[no,]
  
  # combine all random observations
  train <- do.call("rbind", list(polygon_data_high_train, polygon_data_low_train))
  
  tc <- glm(new_model_class~shade_2018+min_2000_2006+prefire+Shape_Area, data=train, family="binomial")
  sum <-summary(tc)
  sum
  
  
  # predict data 
  pred<-predict(tc, train, type="response") 
  
  # error rate for data
  pred1 <- ifelse (pred>0.5,1,0)
  tab<- table(Predicted=pred1, Actual=train$new_model_class)
  tab
  
  acc <-1-sum(diag(tab))/sum(tab)   #error calculation. gives % observations predicted incorrectly 
  acc <- 1- acc 
  acc
  
  pred <- predict(tc,train)
  pred1 <-prediction(pred, train$new_model_class)
  roc <- performance(pred1, "tpr", "fpr")
  auc <-performance(pred1, "auc")
  auc1 <- unlist(slot(auc, "y.values"))
  auc_list <- append(auc_list, auc1)
  
}

model_aic <- as.data.frame(unlist(model_aic))
acc_list <- as.data.frame(unlist(acc_list))
auc_list <-as.data.frame(unlist(auc_list))
combo <- cbind(model_aic, acc_list, auc_list)
write.table(combo, file="combo_data.csv", sep=",")

table_values <- as.data.frame(unlist(table_values))
write.table(table_values, file="table_values.csv", sep=",")



model_aic <- as.data.frame(unlist(model_aic))
write.table(model_aic, file="aic.csv", sep=",")

acc_list <- as.data.frame(unlist(acc_list))
write.table(acc_list, file="acc_list.csv", sep = ",")

auc_list <-as.data.frame(unlist(auc_list))
write.table(auc_list, file="auc.csv", sep=",")

new_data <- cbind(train, pred1, pred)
write.table(sf2, file="R SF data sheet to compare.csv", sep=",")
#download data--------------------------------------------------------------

new_data <- cbind(train, pred1)
write.table(new_data, file="predictions.csv", sep=",")

# significance test for additional variables--------------------------------------
install.packages ("arc")
library(arc)

arch.test(tc, output=T)

#goodness of fit----------------------------------------------------------------
with(tc, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail=F))
#p <0.05 "have high confidence that this model is stat significant"

#ROC ------------------------------------------------------------------------------------------
library(ROCR)

#predict training data
pred121 <- prediction(pred, train$new_model_class)

#ROC plot
pred <- predict(tc,train)
pred1 <-prediction(pred, train$new_model_class)
roc <- performance(pred1, "tpr", "fpr")
plot(roc)
abline(a=0, b=1)
abline(h=0.9, v=0.55)
#AUC = area under the curve. Higher AUC=better
auc <-performance(pred1, "auc")
auc1 <- unlist(slot(auc, "y.values"))
print(auc1)

legend (0.57, 0.15, "Average AUC=0.9007", title="Legend")

#best value -----------------------------------------------------------------------------------
pred123 <- predict (tc, train, type="response")
pred1234 <- prediction(pred123, train$new_model_class)
eval <- performance(pred1234, "acc")
plot(eval)

max <-which.max(slot(eval, "y.values") [[1]])
y.acc <- slot(eval, "y.values") [[1]][max]
x.acc <- slot(eval, "x.values")[[1]] [max]

y.acc
x.acc
abline(v=x.acc, h=y.acc)

#model for tc: yes TC= complete mortality and 95-99% mortality ---------------------------------------

#divide the response variable into equal number of observations
tc_yes <- sf1[which(sf1$new_model_class == "1"),]
tc_no <- sf1[which(sf1$new_model_class == "0"),]

# pull 175 random observations from the high class and separate
yes <- sample(1:nrow(tc_yes), 101 , replace = FALSE)
polygon_data_high_train <- tc_yes[yes,]
polygon_data_high_test <- tc_yes[-yes,]
# pull 175 random observations from the low class and separate
no <- sample(1:nrow(tc_no), 101 , replace = FALSE)
polygon_data_low_train <- tc_no[no,]
polygon_data_low_test <- tc_no[-no,]

# combine all random observations
train <- do.call("rbind", list(polygon_data_high_train, polygon_data_low_train))
# recombine classes that were not pulled
test <- do.call("rbind", list(polygon_data_high_test, polygon_data_low_test))

tc <- glm(new_model_class~shade_2018+prefire+avg_2000_2006, data=train, family="binomial")
summary(tc)

# p value

# predict data 
pred<-predict(tc, train, type="response") 

print(pred, digits=3)

# error rate for data
pred1 <- ifelse (pred>0.5,1,0)
tab<- table(Predicted=pred1, Actual=train$new_model_class)
tab
acc <-1-sum(diag(tab))/sum(tab)   #error calculation. gives % observations predicted incorrectly 
1-acc

pred121 <- prediction(pred, train$new_model_class)

#ROC plot-----------------------------------------------------------------------------------
pred <- predict(tc,train)
pred1 <-prediction(pred, train$new_model_class)
roc <- performance(pred1, "tpr", "fpr")
plot(roc)
abline(a=0, b=1)

#AUC = area under the curve. Higher AUC=better
auc <-performance(pred1, "auc")
auc1 <- unlist(slot(auc, "y.values"))
print(auc1)

legend (0.7, 0.2, "AUC=0.90", title= "Legend" )

# test sig between the model option accuracies --------------------------------------------
acc <- read.csv(file.choose(), header=T)

# test anova assumption shapiro test for normality
shapiro.test(acc$X2018_avgpref_pref) # p=0.22
shapiro.test(acc$X2018_avgpref) # p=0.02
shapiro.test(acc$X2018_pref) # p=0.23
shapiro.test(acc$X2018_minpref_pref) # p=0.43
shapiro.test(acc$X2018_minpref) # p=0.001
shapiro.test(acc$X2018) # p=0.26

# anova test for homogeneity of variances
bartlett.test(acc$x2018~acc$minpref) # p-value < 0.05 (fails assumption)

# anova
anova <-aov(acc$shade_2018~sf$mortality_level)
summary(anova)
tukey <- TukeyHSD(anova)
print(tukey)

# are the model accuracies significantly different----------------------------
modacc <- read.csv(file.choose(), header=T)

plot(model_acc_all~model_type_all, data=modacc, 
     xlab="Model Type", ylab="Average Model Accuracy")

kruskal.test(model_acc_all~model_type_all, data=modacc)
pairwise.wilcox.test(modacc$model_acc_all, modacc$model_type_all, p.adj="holm")



plot(model_acc_6main~model_type_6main, data=modacc, 
     xlab="Model Type", ylab="Average Model Accuracy")

kruskal.test(model_acc_6main~model_type_6main, data=modacc)
pairwise.wilcox.test(modacc$model_acc_6main, modacc$model_type_6main, p.adj="holm")

# test assumptions
shapiro.test(modacc$X2018) # p<2.2e-16 for all 
shapiro.test(modacc$X2018_pre) 
shapiro.test(modacc$X2018_avg_pre) 
shapiro.test(modacc$X2018_avg) 
shapiro.test(modacc$X2018_min) 
shapiro.test(modacc$X2018_min_pre) 
shapiro.test(modacc$X2018_max) 
shapiro.test(modacc$X2018_max_pre) 
shapiro.test(sf$shade_2018) 


qqnorm(modacc$X2018)
qqline(modacc$X2018)

bartlett.test(modacc$model_acc_all~modacc$model_type_all)# p-value < 0.05 (fails assumption)
