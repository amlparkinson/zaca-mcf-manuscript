# trial from youtube video: works!! https://www.youtube.com/watch?v=84JSk36og34

library(caret)
library(stringr)
library(randomForest)

# make sure y variable is recgonized as a factor
prefire_qual <- as.factor(canopy$prefire_qual)

#add explanatory variables to a single data frame
explantory_variables <- canopy %>% select(min_2000_2006, shade_2000)

# create folds
folds <-  createMultiFolds(prefire_qual, k=10, times=10)

# train Control
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = folds)

# cross validation model for random forest
rf.cv <- train(x= explantory_variables, y=prefire_qual, method="rf", tuneLength = 2, ntree=350, trControl=control)

# summary statistics
rf.cv
 

# trial: dont understand what is going on https://github.com/RobertSWalker/cross-validation-glm-vs-lmer-vs-brms/blob/master/cv_glm_vs_lmer_vs_brms.R

pkgs <- c("lme4", "brms", "rcompanion", "tidyverse", "magrittr")
#install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)

# Function to center x variables to make intercepts interpretable
center_colmeans <- function(x) { xcenter = colMeans(x, na.rm=TRUE)
x - rep(xcenter, rep.int(nrow(x), ncol(x)))}
varsToCenter = c("disp","hp","drat","wt","qsec")
mtcars[varsToCenter] = center_colmeans(mtcars[varsToCenter])

#convert certain columns to factors
cols <- c("cyl", "vs", "am", "gear", "carb")
mtcars %<>% mutate_at(cols, funs(factor(.))) # or ordered

# k fold cv function for GLM
kcv_functionGLM = function(form,df,y){
  for(i in 1:k){
    valIndices <- which(folds==i,arr.ind=TRUE)
    train <- df[-valIndices, ]
    model = glm(form, family=gaussian(link="identity"), data=train)
    validation <- df[valIndices, ]
    pred[[i]] = predict(model, validation, allow.new.levels = TRUE)
  }
  return(cor(unlist(pred),y)^2)
}

# k fold cv function for lmer
kcv_functionLMER = function(form,df,y){
  for(i in 1:k){
    valIndices <- which(folds==i,arr.ind=TRUE)
    train <- df[-valIndices, ]
    model = lmer(form, data=train)
    validation <- df[valIndices, ]
    pred[[i]] = predict(model, validation, allow.new.levels = TRUE)
  }
  return(cor(unlist(pred),y)^2)
}

# k fold cv function for brms
kcv_functionBR = function(form,df,y){
  for(i in 1:k){
    valIndices <- which(folds==i,arr.ind=TRUE)
    train <- df[-valIndices, ]
    model = brm(form, data=train, cores=4, iter = 1e3, 
                chains = 2, control = list(adapt_delta = 0.9))
    validation <- df[valIndices, ]
    pred[[i]] = predict(model, validation, allow.new.levels = TRUE)[,1]
  }
  return(cor(unlist(pred),y)^2)
}

# write functional form for GLM 
form1 = as.formula("mpg ~ 0 + wt + cyl")

# write functional form for lmer and brms
form2 = as.formula("mpg ~ 0 + wt + cyl + (wt | cyl)")

k = 10 # set number of cross validation folds

listLMER = list()
listGLM = list()

for(j in 1:5){ # number of repeated cross validations
  pred = list()
  mtcars <- mtcars[sample(nrow(mtcars)),]
  folds <- cut(seq(1,nrow(mtcars)),breaks=k,labels=FALSE)
  listGLM[[j]] = kcv_functionGLM(form1,mtcars,mtcars$mpg)
  listLMER[[j]] = kcv_functionLMER(form2,mtcars,mtcars$mpg)
}

cat("Median cross-validated R-sq for linear model =",median(unlist(listGLM)))
cat("Median cross-validated R-sq for lmer model =",median(unlist(listLMER)))

# Bayesian regression takes time and so not repeated, priors left at defaults
kcv_functionBR(form2,mtcars,mtcars$mpg) # cross-validated rsq for brms

#END


# trial: didnt work (https://github.com/jeffwen/Kth_Fold_Cross_Validation_R/blob/master/cross_validation_code.R)--------------------------------------------------------

library(plyr)
library(tidyverse)
library(randomForest)


# load data ---------------------------------------------------------------

canopy <- read_csv("Final Zaca MCF SF_MainData.csv") %>% 
  mutate(prefire_qual = case_when(
    prefire %in% '1_25' ~ "Sparse (1-25%) Canopy" ,
    prefire %in% '25_75' ~ "Open (25-75%) Canopy" ,
    prefire %in% '75_100' ~ "Dense (75-100%) Canopy"
  )) %>% 
  mutate(prefire_qual = as.factor(prefire_qual)) %>% 
  mutate(postfire_qual = case_when(
    postfire %in% c("0", '1_25') ~ "Sparse (1-25%) Canopy" ,
    postfire %in% '25_75' ~ "Open (25-75%) Canopy" ,
    postfire %in% '75_100' ~ "Dense (75-100%) Canopy"
  )) %>% 
  mutate(postfire_qual = as.factor(postfire_qual))

# predict the dependent variable values from the other variables in the data_testset; # of folds

k = 5 # Folds

# sample from 1 to k, nrow times (the number of observations in the canopy data set)
canopy$id <- sample(1:k, nrow(canopy), replace = TRUE)
list <- 1:k

# prediction and testset data_test frames that we add to with each iteration over
# the folds

prediction <- data.frame()
testsetCopy <- data.frame()

#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)

for (i in 1:k){
  # remove rows with id i from data_testframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(canopy, id %in% list[-i])
  testset <- subset(canopy, id %in% c(i))
  
  # run a random forest model
  mymodel <- randomForest(prefire_qual~ min_2000_2006 + avg_2000_2006 + max_2000_2006, data = trainingset, ntree = 500)
  
  # remove response
  temp <- as.data.frame(predict(mymodel, testset[c(-11)]))
  #temp <- as.data.frame(predict(mymodel, testset[c(-11)], type = "class"))
  #temp.boosting <- predict.boosting(mymodel, testset)
  #temp <- as.data.frame(temp.boosting$class)
  #temp <- as.data.frame(predict(mymodel, testset[c(-11)]))
  
  # append this iteration's predictions to the end of the prediction data_test frame
  prediction <- rbind(prediction, temp)
  
  
  # append this iteration's test set to the test set copy data frame
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[c(11)]))
  
  progress.bar$step()
}

# add predictions and actual dependent variables values
result <- cbind(prediction, testsetCopy[,1])
names(result) <- c("Predicted", "Actual")
result$Difference <- result$Actual == result$Predicted

# As an example use % correct classification 
table(result$Difference)
# % Accuracy
sum(result$Difference)/length(result$Difference)
