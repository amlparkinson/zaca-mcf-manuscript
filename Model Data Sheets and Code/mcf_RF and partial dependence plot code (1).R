
library(randomForest)
library(tidyverse)
mcf<- read_csv("Zaca MCF variables for model.csv")



# Chri's code fr amped up binomial model -----------------------------------------------------------------------------------------------------------

#The actual BLR model
blr_temp <- glm(cbind(canopy_loss_success, canopy_loss_failure) ~ 
                  prefire + sr_new + TPI_five + tmax, 
                  family = binomial(link = "logit"), data = mcf)

predict(blr_temp)
#You can predict values with the model using the predict() function. This enables you to 
#graph the fit line. You'll need to create a data frame with the values you want to input 
#for each predictor variable. You may want to set all of the variables except one to a 
#constant value.

#random forest code---------------------------------------------------
for (i in 1:3)
{
  
# divide data into equal # of observations --------------------
  
  mcf_high <- mcf[which(mcf$mortality_qual == "High"),]
  mcf_low <- mcf[which(mcf$mortality_qual == "Low"),]
  mcf_moderate <- mcf[which(mcf$mortality_qual == "Moderate"),]
  mcf_none <- mcf[which(mcf$mortality_qual == "None"),]
  
  # pull random observations from the high class and separate
  x_high <- sample(1:nrow(mcf_high), 156 , replace = FALSE)
  polygon_data_high_train <- mcf_high[x_high,]
  polygon_data_high_test <- mcf_high[-x_high,]
  # pull random observations from the low class and separate
  x_low <- sample(1:nrow(mcf_low), 156 , replace = FALSE)
  polygon_data_low_train <- mcf_low[x_low,]
  polygon_data_low_test <- mcf_low[-x_low,]
  # pull random observations from the moderate class and separate
  x_moderate <- sample(1:nrow(mcf_moderate), 156 , replace = FALSE)
  polygon_data_moderate_train <- mcf_moderate[x_moderate,]
  polygon_data_moderate_test <- mcf_moderate[-x_moderate,]
  # pull random observations from the none class and separate
  x_none <- sample(1:nrow(bc1_none), 156 , replace = FALSE)
  polygon_data_none_train <- mcf_none[x_none,]
  polygon_data_none_test <- mcf_none[-x_none,]
  
  
  # combine all random observations
  train <- do.call("rbind", list(polygon_data_high_train, polygon_data_low_train, polygon_data_moderate_train, polygon_data_none_train))
  # recombine classes that were not pulled
  #test <- do.call("rbind", list(polygon_data_high_test, polygon_data_low_test, polygon_data_moderate_test, polygon_data_none_test))
  
  rfm<-randomForest(mortality_qual~sr+drainage_assoication+rain+tmax+TPI_five+dem_10m_zaca+flow_accumulation+sr+slope+TWI,
                    train, ntree=500, importance=T, nscale=0)
  rfm
  
  #plot to visualize variable importance
  importance(rfm)                  
  varImpPlot(rfm)
  varUsed(rfm) #shows how often a varible is used in the model
  
  #predcit test data
  pp<-predict(rfm, test)
  
  #test data accuracy
  tabb<- table(pp, test$mortality_qual)
  tabb
  acc<-1-sum(diag(tabb))/sum(tabb)
  acc #% missclassified
  (1-acc)*100 #model accuracy
  
  test_accuracy <-(1-acc)*100 #model accuracy
  print(test_accuracy)
}

# explain random forest------------------------------------------------
install.packages("randomForestExplainer")
install.packages("colorspace")
library(colorspace)
library(randomForestExplainer)

explain_forest(rfm, interactions=T, data=mcf)

# partial dependence plot code -----------------------------------------

a<-partialPlot(rfm, train, sr, "None")
b<-partialPlot(rfm, train, sr, "Low")
c<-partialPlot(rfm, train, sr, "Moderate")
d<-partialPlot(rfm, train, sr, "High", ylim=c(-0.65, 0.65))


lines(a, col="blue")
lines(b, col="red")
lines(c, col="green")

a<-partialPlot(rfm, train, rain, "None")
b<-partialPlot(rfm, train, rain, "Low")
c<-partialPlot(rfm, train, rain, "Moderate")
d<-partialPlot(rfm, train, rain, "High", ylim=c(-0.7, 0.7))


lines(a, col="blue")
lines(b, col="red")
lines(c, col="green")

a<-partialPlot(rfm, train, temp, "None")
b<-partialPlot(rfm, train, temp, "Low")
c<-partialPlot(rfm, train, temp, "Moderate")
d<-partialPlot(rfm, train, temp, "High", ylim=c(-0.3, 0.3))


lines(a, col="blue")
lines(b, col="red")
lines(c, col="green")

a<-partialPlot(rfm, train, TWI, "None")
b<-partialPlot(rfm, train, TWI, "Low")
c<-partialPlot(rfm, train, TWI, "Moderate")
d<-partialPlot(rfm, train, TWI, "High", ylim=c(-0.2, 0.2))


lines(a, col="blue")
lines(b, col="red")
lines(c, col="green")

a<-partialPlot(rfm, train, tpi, "None")
b<-partialPlot(rfm, train, tpi, "Low")
c<-partialPlot(rfm, train, tpi, "Moderate")
d<-partialPlot(rfm, train, tpi, "High", ylim=c(-0.5, 0.4))


lines(a, col="blue")
lines(b, col="red")
lines(c, col="green")

a<-partialPlot(rfm, train, vpd_jan, "None")
b<-partialPlot(rfm, train, vpd_jan, "Low")
c<-partialPlot(rfm, train, vpd_jan, "Moderate")
d<-partialPlot(rfm, train, vpd_jan, "High", ylim=c(-0.25, 0.25))


lines(a, col="blue")
lines(b, col="red")
lines(c, col="green")

a<-partialPlot(rfm, train, vpd_Aug, "None")
b<-partialPlot(rfm, train, vpd_Aug, "Low")
c<-partialPlot(rfm, train, vpd_Aug, "Moderate")
d<-partialPlot(rfm, train, vpd_Aug, "High", ylim=c(-0.2, 0.2))


lines(a, col="blue")
lines(b, col="red")
lines(c, col="green")

a<-partialPlot(rfm, train, cwd, "None")
b<-partialPlot(rfm, train, cwd, "Low")
c<-partialPlot(rfm, train, cwd, "Moderate")
d<-partialPlot(rfm, train, cwd, "High", ylim=c(-0.2, 0.2))


lines(a, col="blue")
lines(b, col="red")
lines(c, col="green")

a<-partialPlot(rfm, train, southwestness, "None")
b<-partialPlot(rfm, train, southwestness, "Low")
c<-partialPlot(rfm, train, southwestness, "Moderate")
d<-partialPlot(rfm, train, southwestness, "High", ylim=c(-0.2, 0.25))


lines(a, col="blue")
lines(b, col="red")
lines(c, col="green")


a<-partialPlot(rfm, train, area_d, "None")
b<-partialPlot(rfm, train, area_d, "Low")
c<-partialPlot(rfm, train, area_d, "Moderate")
d<-partialPlot(rfm, train, area_d, "High")


a<-partialPlot(rfm, train, Veg_Assoc, "None")
b<-partialPlot(rfm, train, Veg_Assoc, "Low")
c<-partialPlot(rfm, train, Veg_Assoc, "Moderate")
d<-partialPlot(rfm, train, Veg_Assoc, "High")

