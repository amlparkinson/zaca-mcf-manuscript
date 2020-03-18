# load packages and data -----------------------------------------------------

library(randomForest)
library(tidyverse)
library(here)

# load data -----------------------------------------------------------------

#shade fraction time series data
canopy_sf <- read_csv(here::here("Shade Fraction Data Sheets and Code", "Final Zaca MCF SF_MainData.csv")) %>% 
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
  mutate(postfire_qual = as.factor(postfire_qual)) %>% 
  dplyr::mutate(mortality_qualitative= fct_relevel(mortality_qualitative, levels=c("None", "Low", "Moderate", "High"))) %>% 
  mutate(other_mortality = fct_relevel(other_mortality, levels = c("None", "Low", "Moderate", "High", "Type Converted")))

# green vegation time series data
canopy_gv <- read_csv(here::here("Shade Fraction Data Sheets and Code", "Zaca MCF variables for model_plus GV.csv")) %>% 
  mutate(prefire_qual = case_when(
    prefire %in% '1_25' ~ "Sparse (1-25%) Canopy" ,
    prefire %in% '25_75' ~ "Open (25-75%) Canopy" ,
    prefire %in% '75_100' ~ "Dense (75-100%) Canopy"
  )) %>% 
  dplyr::mutate(mortality_qual= fct_relevel(mortality_qual, levels=c("None", "Low", "Moderate", "High"))) %>% 
  rename(mortality_qualitative = mortality_qual)

# sample size --------------------------------------------------------------------------------------

# sample size will be the same for shade fraction and gv fractions (and dnbr when thats added)
mortality_sample_size <- canopy_sf %>% 
  group_by(mortality_qualitative) %>% 
  count()

# visualize the data -----------------------------------------------------------------------------

ggplot(canopy_sf, aes(x= mortality_qualitative, y = shade_2018)) +
         geom_jitter()

ggplot(canopy_sf, aes(x= other_mortality, y = shade_2018)) +
  geom_jitter()

ggplot(canopy_gv, aes(x= mortality_qualitative, y = gv_2018)) +
  geom_jitter()

sf_gv_df <- merge(canopy_sf, canopy_gv, by = "OBJECTID") %>% 
  select(mortality_qualitative.x, other_mortality, gv_2018, gv_2008, shade_2018, shade_2008,new_model_class) %>% 
  mutate(new_model_class = as.factor(new_model_class))# 1 = type converted, 0 = not type converted 


ggplot(sf_gv_df, aes(x = shade_2018, y = gv_2018)) +
  geom_point(aes(color=new_model_class))
ggplot(sf_gv_df, aes(x = shade_2008, y = gv_2008)) +
  geom_point(aes(color=new_model_class))



# compare shade fraction and GV assessment of burn severity to visual assessment of mortality -------------------

# cant do yet since the shade fraction and gv fractions are still in quantitative values instead of categorical values
tabb <- table (bc$Mortality_Qualitative, bc$dnbr_qualitative)
acc<-1-sum(diag(tabb))/sum(tabb) # % missclassified
test_accuracy <-(1-acc)*100 #model accuracy






















# RF model:prefire ----------------------------------------------------------------

# split data into training and test data 
x <- sample(735, 500)
train <- merged_df [x, ]
test <- merged_df [-x, ]

# split data into training and test data with equal observations ----------

dense <- merged_df[which(merged_df$prefire_qual == "Dense (75-100%) Canopy"),]
open <- merged_df[which(merged_df$prefire_qual == "Open (25-75%) Canopy"),]
sparse <- merged_df[which(merged_df$prefire_qual == "Sparse (1-25%) Canopy"),]

# dense
x_dense <- sample(1:nrow(dense), 80 , replace = F)
dense_train <- dense[x_dense, ]
dense_test <- dense[-x_dense, ]
# open
x_open <- sample(1:nrow(open), 80 , replace = F)
open_train <- open[x_open, ]
open_test <- open[-x_open, ]
# sparse
x_sparse <- sample(1:nrow(sparse), 80 , replace = F)
sparse_train <- sparse[x_sparse, ]
sparse_test <- sparse[-x_sparse, ]

# combine all random observations
train <- do.call("rbind", list(dense_train, open_train, sparse_train))
# recombine classes that were not pulled
test <- do.call("rbind", list(dense_test, open_test, sparse_test))


# RF 
rfm<-randomForest(prefire_qual~ min_2000_2006 + avg_2000_2006 + max_2000_2006 + dem_10m_zaca + TPI_five + 
                    TWI + sr_new + slope + southness, train, ntree=500, importance=T, nscale=0)
                  

#rfm <- randomForest(prefire_qual ~ shade_2000 + shade_2001 + shade_2002 + shade_2003 +
#                    shade_2004 + shade_2005 + shade_2006 + min_2000_2006, 
#                   train, ntree=500, importance=T, nscale=0)

rfm

#plot to visualize variable importance
importance(rfm)                  
varImpPlot(rfm)
varUsed(rfm) #shows how often a varible is used in the model

#predcit test data
pp<-predict(rfm, test)

#test data accuracy
tabb<- table(pp, test$prefire_qual)
tabb
acc<-1-sum(diag(tabb))/sum(tabb)
acc #% missclassified
(1-acc)*100 #model accuracy


# RF model: post-fire -----------------------------------------------------

# split data into training and test data 
x <- sample(735, 500)
train <- canopy [x, ]
test <- canopy [-x, ]

# RF 

rfm <- randomForest(postfire_qual ~ shade_2008 + shade_2009 + shade_2010 + shade_2011 +
                      shade_2013 + shade_2014 + shade_2015 + shade_2016 + shade_2017 + shade_2018, 
                    train, ntree=500, importance=T, nscale=0)
rfm <- randomForest(postfire_qual ~ shade_2018, train, ntree=500, importance=T, nscale=0)

rfm

#plot to visualize variable importance
importance(rfm)                  
varImpPlot(rfm)
varUsed(rfm) #shows how often a varible is used in the model

#predcit test data
pp<-predict(rfm, test)

#test data accuracy
tabb<- table(pp, test$postfire_qual)
tabb
acc<-1-sum(diag(tabb))/sum(tabb)
acc #% missclassified
(1-acc)*100 #model accuracy



# visualize data distribition ---------------------------------------------

ggplot (merged_df, aes(x= prefire_qual, y= shade_2000)) +
  geom_jitter()
