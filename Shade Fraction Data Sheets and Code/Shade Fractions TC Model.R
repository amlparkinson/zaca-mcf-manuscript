# load packages ----------------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(dplyr)
library(pROC)
library(ROCR)

# add data ---------------------------------------------------------------------------------------------------

sf2 <-read_csv(here::here("Shade Fraction Data Sheets and Code", "Final Zaca MCF SF_MainData.csv")) %>% 
  dplyr::mutate(mortality_qualitative= fct_relevel(mortality_qualitative, levels=c("None", "Low", "Moderate", "High"))) %>% 
  dplyr::mutate(other_mortality = fct_relevel(other_mortality, levels = c("None", "Low", "Moderate", "High", "Type Converted")))


#You can predict values with the model using the predict() function. This enables you to graph the fit line. You'll need to create a data frame with the values you want to input for each predictor variable. You may want to set all of the variables except one to a constant value.
# create lists ------------------------------------------------------------------------------------------------------------------------------------------
acc_list <- list()
table_values <- list()
model_aic <-list()
auc_list <- list ()
#prediction_list <- list()

#type conversion model ----------------------------------------------------------------------------------------------------------------------------
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
  
  tc <- glm(new_model_class~shade_2018+max_2000_2006, data=train, family="binomial")
  sum <-summary(tc)
  sum
  aic<- sum$aic
  aic
  model_aic <- append(model_aic, aic)
  
  # predict data 
  pred<-predict(tc, train, type="response") 
  
  # error rate for data
  pred1 <- ifelse (pred>0.5,1,0)
  tab<- table(Predicted=pred1, Actual=train$new_model_class)
  tab
  table_values <- append(table_values, tab)
  acc <-1-sum(diag(tab))/sum(tab)   #error calculation. gives % observations predicted incorrectly 
  acc <- 1- acc 
  acc
  acc_list <- append(acc_list, acc)
  
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

# download data ----------------------------------------------------------------------------------------------------------------------------
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


# significance test for additional variables; didnt work---------------------------------------------------------------------
install.packages ("arc")  
library(arc)

arch.test(tc, output=T)

#goodness of fit---------------------------------------------------------------- not useful for this analysis
with(tc, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail=F))
#p <0.05 "have high confidence that this model is stat significant"

#ROC -------------------------------------------------------------------------------------------------------------------------
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
