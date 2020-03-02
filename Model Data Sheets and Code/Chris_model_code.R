install.packages("raster")
install.packages("tidyverse")
install.packages("extrafont")
install.packages("gtable")
install.packages("grid")
install.packages("stargazer")

set.seed(1797)

# Load packages --------------------------------------------------

library(raster)
library(tidyverse)
library(car)
library(reshape2)
library(extrafont)
library(gtable)
library(grid)
library(stargazer)
library("foreign")
library("rms")
library("MASS")
library("Hmisc")
library("reshape2")
library("dplyr")
library("ggpubr")
library("tidyr")
library("car")
library("ggplot2")
library("readxl")
library("stats")
library("RandomForest")

# Load and prepare shapefile ------------------------------------------------------

pine <- shapefile("E:/zaca/chris/ground_truth/all_mixed_pine", stringsAsFactors = TRUE)

# Binomial Logistic Regression (Chris)--------------------------------------------

#Prepare data
all_env_variables <- brick(stack(list.files(path = "E:/zaca/environmental_data/all_variables/stack", full.names = TRUE)))
pine <- shapefile("E:/zaca/chris/ground_truth/all_mixed_pine", stringsAsFactors = TRUE)
all_env_variables <- crop(all_env_variables, extent(pine))
environment_extract <- raster::extract(all_env_variables, pine, method = "simple", fun = mean, na.rm = TRUE, sp = TRUE)

env_df <- as.data.frame(environment_extract) %>% 
  filter(prefire != 0) %>% 
  mutate(dem_scaled = dem_10m_zaca/1000, insolation_scaled = insolation_10m_zaca/100000, tmean_scaled = tmean_10m_zaca/10, southness = abs(180 - aspect_10m_zaca), precip_mm = rain_10m_zaca/100) %>% 
  mutate(postfire_success = as.numeric(as.character(car::recode(postfire, "'0' = 0; '1_25' = 125; '25_75' = 500; '75_100' = 875")))) %>% 
  mutate(postfire_failure = 1000 - postfire_success) %>% 
  droplevels(.)

env_df$prefire <- relevel(env_df$prefire, ref = "75_100")

blr <- glm(cbind(postfire_success, postfire_failure) ~ TPI_five_10m_zaca + insolation_scaled + prefire, family = binomial(link = "logit"), data = env_df)

blr

#ifelse(condition, yes, no)
# example ifelse code to get probabilities
class.out <- ifelse(prob < 0.25, "None", 
                    ifelse(prob < 0.5, "Low", 
                           ifelse(...)))

table(class.out, actual.classes)

# Random Forest -----------------------------------------------------------------------------------------------------------------------------

bc1<- read.csv(file.choose(), header=T)

for (i in 1:15)
{
  
  bc1_high <- bc1[which(bc1$Mortality_Qualitative == "High"),]
  bc1_low <- bc1[which(bc1$Mortality_Qualitative == "Low"),]
  bc1_moderate <- bc1[which(bc1$Mortality_Qualitative == "Moderate"),]
  bc1_none <- bc1[which(bc1$Mortality_Qualitative == "None"),]
  
  # pull 175 random observations from the high class and separate
  x_high <- sample(1:nrow(bc1_high), 41 , replace = FALSE)
  polygon_data_high_train <- bc1_high[x_high,]
  polygon_data_high_test <- bc1_high[-x_high,]
  # pull 175 random observations from the low class and separate
  x_low <- sample(1:nrow(bc1_low), 41 , replace = FALSE)
  polygon_data_low_train <- bc1_low[x_low,]
  polygon_data_low_test <- bc1_low[-x_low,]
  # pull 175 random observations from the moderate class and separate
  x_moderate <- sample(1:nrow(bc1_moderate), 41 , replace = FALSE)
  polygon_data_moderate_train <- bc1_moderate[x_moderate,]
  polygon_data_moderate_test <- bc1_moderate[-x_moderate,]
  # pull 175 random observations from the none class and separate
  x_none <- sample(1:nrow(bc1_none), 41 , replace = FALSE)
  polygon_data_none_train <- bc1_none[x_none,]
  polygon_data_none_test <- bc1_none[-x_none,]
  
  
  # combine all random observations
  train <- do.call("rbind", list(polygon_data_high_train, polygon_data_low_train, polygon_data_moderate_train, polygon_data_none_train))
  # recombine classes that were not pulled
  test <- do.call("rbind", list(polygon_data_high_test, polygon_data_low_test, polygon_data_moderate_test, polygon_data_none_test))
  
  rfm<-randomForest(Mortality_Qualitative~prefire+sr+rain+tmax+TPI_five,
                    train, ntree=400, importance=T, nscale=0)
  rfm
  
  #plot to visualize variable importance
  importance(rfm)                  
  varImpPlot(rfm)
  varUsed(rfm) #shows how often a varible is used in the model
  #predict train data
  p<- predict(rfm, train)
  
  #train data accuracy
  tab<- table(p, train$Mortality_Qualitative)
  tab
  1-sum(diag(tab))/sum(tab)
  
  #predcit test data
  pp<-predict(rfm, test)
  
  #test data accuracy
  tabb<- table(pp, test$Mortality_Qualitative)
  tabb
  acc<-1-sum(diag(tabb))/sum(tabb)
  acc #% missclassified
  (1-acc)*100 #model accuracy
  
  test_accuracy <-(1-acc)*100 #model accuracy
  a<-print(test_accuracy)
}


# Ordinal logistic regression ---------------------------------------------------------------------------------------------------

bc1<- read.csv(file.choose(), header=T)

#divide the response variable into equal number of observations
bc1_high <- bc1[which(bc1$Mortality_Qualitative == "High"),]
bc1_low <- bc1[which(bc1$Mortality_Qualitative == "Low"),]
bc1_moderate <- bc1[which(bc1$Mortality_Qualitative == "Moderate"),]
bc1_none <- bc1[which(bc1$Mortality_Qualitative == "None"),]

# pull 175 random observations from the high class and separate
x_high <- sample(1:nrow(bc1_high), 41 , replace = FALSE)
polygon_data_high_train <- bc1_high[x_high,]
polygon_data_high_test <- bc1_high[-x_high,]
# pull 175 random observations from the low class and separate
x_low <- sample(1:nrow(bc1_low), 41 , replace = FALSE)
polygon_data_low_train <- bc1_low[x_low,]
polygon_data_low_test <- bc1_low[-x_low,]
# pull 175 random observations from the moderate class and separate
x_moderate <- sample(1:nrow(bc1_moderate), 41 , replace = FALSE)
polygon_data_moderate_train <- bc1_moderate[x_moderate,]
polygon_data_moderate_test <- bc1_moderate[-x_moderate,]
# pull 175 random observations from the none class and separate
x_none <- sample(1:nrow(bc1_none), 41 , replace = FALSE)
polygon_data_none_train <- bc1_none[x_none,]
polygon_data_none_test <- bc1_none[-x_none,]

# combine all random observations
train <- do.call("rbind", list(polygon_data_high_train, polygon_data_low_train, polygon_data_moderate_train, polygon_data_none_train))
# recombine classes that were not pulled
test <- do.call("rbind", list(polygon_data_high_test, polygon_data_low_test, polygon_data_moderate_test, polygon_data_none_test))


ordinal<-polr(Mortality_Qualitative~slope+rain+tmax+TPI_five+southness, data=train, Hess=T)
summary(ordinal)

# p value
ptable<-coef(summary(ordinal))
p<-pnorm(abs(ptable[, "t value"]), lower.tail=F)*2
ptable<- cbind(ptable, "p value"=p)
ptable

# predict training data 
pred<-predict(ordinal, train) # can also try type="fitted.ind"
print(pred, digits=3)

#confusion matrix and error for training data
tab<- table(pred, train$Mortality_Qualitative)
1-sum(diag(tab))/sum(tab)   #error calculation. gives % observations predicted incorrectly 

# predict test data
pred1<-predict(ordinal, test)
print(pred1, digits=3)

#confusion matrix and error for test data
tab1<-table(pred1, test$Mortality_Qualitative)
acc1<-1-sum(diag(tab1))/sum(tab1)
acc1 #% missclassified
(1-acc1)*100 #model accuracy

confusionMatrix(pred, train$Mortality_Qualitative)
confusionMatrix(pred1, test$Mortality_Qualitative)


#Binomial logistic regression amp -------------------------------------------------------------------------------------

bc1<- read.csv(file.choose(), header=T)


y<- cbind(bc1$postfire_success, bc1$postfire_failure)

binary <- glm(y ~ TPI_five + insolation_scaled + prefire + tmax, data=bc1, family='quasibinomial'(link = "logit"))
summary(binary)

# p value
ptable<-coef(summary(binary))
p<-pnorm(abs(ptable[, "t value"]), lower.tail=F)*2
ptable<- cbind(ptable, "p value"=p)
ptable


# predict data 
pred<-predict(binary, bc1, type="response") 
print(pred, digits=3)

# error rate for data
tab<- table(pred, bc1$y)
1-sum(diag(tab))/sum(tab)   #error calculation. gives % observations predicted incorrectly 


