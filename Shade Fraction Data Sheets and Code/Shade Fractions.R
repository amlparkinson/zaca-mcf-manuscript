# data and libraries -------------------------------------------------------------------------------------------------------
library(tidyverse)
library(car)
sf <-read_csv("Final Zaca MCF SF_MainData.csv") %>% 
  mutate(mortality_qualitative = fct_relevel(mortality_qualitative, levels = "None", "Low", "Moderate", "High")) %>% 
  mutate(other_mortality = fct_relevel(other_mortality, levels = "None", "Low", "Moderate", "High", "Type X=Converted"))


# code to compare plot SF values to their representative polygon's SF values ------------------------------------------------------------
conifer_stands_shade<- read.csv(file.choose(), header=T)
model <- lm(stand~plot, data=conifer_stands_shade)
summary(model)

plot(conifer_stands_shade$plot~conifer_stands_shade$stand, 
             xlab="Average Polygon Shade Fraction", ylab="Plot Shade Fraction", 
             main="Shade Fraction of the Plots vs Their Corresponding MCF Stand")
abline(model)
r2<-"Adjusted R2=0.765"
legend(0.405, 0.68, r2)


# sig difference between stands 2008 vs 2018 SF values? --------------------------------------------------------------------------------------

# 4 mortality classes
ggplot(sf, aes(x=mortality_qualitative, y=SF_change_2008_2018)) +
  geom_jitter()+
  labs(x='Canopy Loss', y='2008 vs 2018 SF')

# 5 mortality classes 
ggplot(sf, aes(x=other_mortality, y=SF_change_2008_2018)) +
  geom_jitter()+
  labs(x='Canopy Loss', y='2008 vs 2018 SF')

kruskal.test(sf$SF_change_2008_2018~sf$mortality_qual)
pairwise.wilcox.test(sf$SF_change_2008_2018,sf$mortality_qual, p.adj="holm")

# arrange order of categories in Mortality qual class------------------------------------
mortality <- c(rep("None", 251), rep("Low", 165), rep("Moderate", 156), rep("High", 64), rep("Type Converted", 99))
mortality1 <- factor(mortality, levels=c("None", "Low", "Moderate", "High", "Type Converted"))


# box plot of SF and mortality level for each year-------------------------------------------------------------------------------------------------
 
plot(sf$shade_2008~sf$other_mortality,
      xlab='Canopy Loss', ylab='2008 Shade Fraction', ylim=c(0.38, 0.85))

plot(sf$shade_2009~sf$other_mortality,
     xlab='Canopy Loss', ylab='2009 Shade Fraction', ylim=c(0.38, 0.85))

plot(sf$shade_2010~sf$other_mortality,
     xlab='Canopy Loss', ylab='2010 Shade Fraction', ylim=c(0.38, 0.85))

plot(sf$shade_2011~sf$other_mortality,
     xlab='Canopy Loss', ylab='2011 Shade Fraction', ylim=c(0.38, 0.85))

plot(sf$shade_2013~sf$other_mortality,
     xlab='Canopy Loss', ylab='2013 Shade Fraction', ylim=c(0.38, 0.85))

plot(sf$shade_2014~sf$other_mortality,
     xlab='Canopy Loss', ylab='2014 Shade Fraction', ylim=c(0.38, 0.85))

plot(sf$shade_2015~sf$other_mortality,
     xlab='Canopy Loss', ylab='2015 Shade Fraction', ylim=c(0.38, 0.85))

plot(sf$shade_2016~sf$mortality_qualitative,
     xlab='Canopy Loss', ylab='2016 Shade Fraction', ylim=c(0.38, 0.85))

plot(sf$shade_2017~sf$other_mortality,
     xlab='Canopy Loss', ylab='2017 Shade Fraction', ylim=c(0.38, 0.85))

plot(sf$shade_2018~sf$other_mortality,
     xlab='Canopy Loss', ylab='2018 Shade Fraction', ylim=c(0.38, 0.85))

# one plot to show change in sf btw classes over the years----------------------------------------------------------

postfire_SF_by_mortality <- read_csv("meadian_avg_postfire_SF.csv")

ggplot(data=postfire_SF_by_mortality, aes(x=Year, y=Median_SF, color=Mortality)) +
  geom_line() +
  labs(x="Year", y="Median Post-fire Shade Fraction")

ggplot(data=postfire_SF_by_mortality, aes(x=Year, y=Average_SF, color=Mortality)) +
  geom_line() +
  labs(x="Year" , y="Average Post-fire Shade Fraction")


#Kruskal wallis test ---------------------------------------------------------------
kt<- kruskal.test(sf$shade_2008~sf$mortality_qualitative)
print(kt)

# TukeyHSD(kt) # cannot use with nonparametric anova
pairwise.wilcox.test(sf$shade_2008, sf$other_mortality, p.adj="holm")
pairwise.wilcox.test(sf$shade_2009, sf$other_mortality, p.adj="holm")
pairwise.wilcox.test(sf$shade_2010, sf$other_mortality, p.adj="holm")
pairwise.wilcox.test(sf$shade_2011, sf$other_mortality, p.adj="holm")
pairwise.wilcox.test(sf$shade_2013, sf$other_mortality, p.adj="holm")
pairwise.wilcox.test(sf$shade_2014, sf$other_mortality, p.adj="holm")
pairwise.wilcox.test(sf$shade_2015, sf$other_mortality, p.adj="holm")
pairwise.wilcox.test(sf$shade_2016, sf$other_mortality, p.adj="holm")
pairwise.wilcox.test(sf$shade_2017, sf$other_mortality, p.adj="holm")
pairwise.wilcox.test(sf$shade_2018, sf$other_mortality, p.adj="holm") #holm was default. Was it the correct adjustment to use??


# check assumptions of all stand dataset --------------------------------------------------------------------------------------------------------

shapiro.test(sf$mortality_level) # y-variable, but it is non-numeric so didnt work
shapiro.test(sf$shade_2000) # data is NOT normally distributed
shapiro.test(sf$shade_2001) # data is normally distributed
shapiro.test(sf$shade_2002) # data is NOT normally distributed
shapiro.test(sf$shade_2003) # data is normally distributed
shapiro.test(sf$shade_2004) # data is NOT normally distributed
shapiro.test(sf$shade_2005) # data is NOT normally distributed
shapiro.test(sf$shade_2006) # data is NOT normally distributed
shapiro.test(sf$shade_2008) # data is NOT normally distributed
shapiro.test(sf$shade_2009) # data is NOT normally distributed
shapiro.test(sf$shade_2010) # data is NOT normally distributed
shapiro.test(sf$shade_2011) # data is NOT normally distributed
shapiro.test(sf$shade_2013) # data is NOT normally distributed
shapiro.test(sf$shade_2014) # data is NOT normally distributed
shapiro.test(sf$shade_2015) # data is normally distributed
shapiro.test(sf$shade_2016) # data is NOT normally distributed
shapiro.test(sf$shade_2017) # data is NOT normally distributed
shapiro.test(sf$shade_2018) # data is NOT normally distributed


qqnorm(sf$shade_2000)
qqline(sf$shade_2000)

leveneTest(sf$shade_2000~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2001~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2002~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2003~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2004~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2005~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2006~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2008~sf$mortality_qualitative) # p-value < 0.05 (passes assumption)
leveneTest(sf$shade_2009~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2010~sf$mortality_qualitative) # p-value > 0.05 (passes assumption)
leveneTest(sf$shade_2011~sf$mortality_qualitative) # p-value < 0.05 (passes assumption)
leveneTest(sf$shade_2013~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2014~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2015~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2016~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2017~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)
leveneTest(sf$shade_2018~sf$mortality_qualitative) # p-value < 0.05 (fails assumption)

sf_pivot <- sf %>% 
  pivot_longer(shade_2000:shade_2018,
               names_to = 'sf_year',
               values_to = 'sf_values') 

summary_table <- sf_pivot %>% 
  group_by(mortality_qualitative, sf_year) %>% 
  summarize (mean = mean(sf_values, na.rm=T),
             sd = sd(sf_values, na.rm=T),
             se = sd((sf_values) / sqrt(n()), na.rm=T),
             var =var(sf_values),
             sample_size = n())


  

