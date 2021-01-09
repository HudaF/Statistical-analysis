setwd("D:/bilal's books 8/Lie Detector/Project 2")
install.packages("corrplot")
library(ggplot2)
library(corrplot)
require(gridExtra)
library(MuMIn)

#read pollution data
#We have multiple files containing different information 
#We will process the data and combine relevant columns with socio economic data to 
#get one final dataframe on which we will be running our models
pm <- read.csv("pm25.csv")
ozone <- read.csv("ozone.csv")
hap <- read.csv("hap.csv")

#The pm 2.5 data is only for the year 2017 so we will drop the Year column
pm
#Picking relevant data
pm <- pm[c(2,5)]
pm

#Similarly ozone data is only for the year 2017 so we will drop the Year column and the
#irrelevant columns

#ozone <- ozone[-c(3,8,9,10,11)]
ozone
#Picking relevant data
ozone <- ozone[c(2,5)]
ozone

#hap data doesn't have the year column but it is for the year 2017
hap
#Picking relevant data
hap <- hap[c(2,3,5)]
hap

#We pick two variables from pm2.5 data ~ Country name which we will merge on
#and median pm2.5 levels for each country we could have chosen 
#mean but that would have had accumulated the outliers as well
#Therefore we choose median values
str(pm)
summary(pm)
names(pm)[names(pm) == 'Median..µg.m3.'] <- "Median pm2.5" 
#There are 195 observations 
#The mean of  pm2.5 median values for all countries is 25.09
#The median of this data is 20.40
Med<-pm$`Median pm2.5`
var(Med)
sd(Med)
range(Med)
#The variance for Average (µg/m3) is 287.1 and sd is 16.94
#range is from 5.8 to 94.5


#Similarly for ozone we chose the same columns as pm 2.5 data
str(ozone)
summary(ozone)
names(ozone)[names(ozone) == 'Median..ppb.'] <- "Median ppb" 
#There are 195 observations 
#The mean of the median ozone level is 49.69
#The median of this data is 49.10
Medppb<-ozone$`Median ppb`
var(Medppb)
sd(Medppb)
range(Medppb)
#The variance of this data is 102.07
#The sd of this data is 10.10
#the range of this data is from 26.2 to 71.6



#For proportion of household air pollution we chose location population and median
str(hap)
summary(hap)
names(hap)[names(hap) == 'Population'] <- "Population exposed to hap"
names(hap)[names(hap) == 'Median'] <- "Median proportion exposed to hap"
#There are 195 observations 
#The mean population is 3.904e+07 and the median is 8.949e+06
#The median of median of this household air pollution data is 0.130 and the mean is 0.317
pop<-hap$`Population exposed to hap`
var(pop)
sd(pop)
range(pop)
#The variance of population exposed is 2.128567e+16
#The sd for population exposed is 145896107
#the range is from 44878 to 1412480390
medExp<-hap$Median
var(medExp)
sd(medExp)
range(medExp)
#The variance of median exposed is 0.13
#The sd of median exposed is 0.36
#The range is from 0.00 to 0.99


data <- pm
count2 <- list()
data <- merge(data,ozone,by="Location")  
data <- merge(data,hap,by="Location")  
str(data)

#Loading economic data
econ <- read.csv('economic.csv')
names(econ)[names(econ) == 'Country.Name'] <- "Location"
econ
str(econ)

hexp<-econ$HealthExpenditurePerCapita
var(hexp)
sd(hexp)
range(hexp)
#the variance for Health expenditure per capita is 3450930
#the sd for health expenditure per capita us 1857.668
#the range for health expenditure per capita is from 19.43 to 10246.14

totgdp <- econ$GDP
var(totgdp)
sd(totgdp)
range(totgdp)
#the variance for Health expenditure per capita is 9.661255e+25
#the sd for health expenditure per capita us 9.829168e+12
#the range for health expenditure per capita is from 3.750402e+08 to 8.100000e+13

agr <- econ$AgricultureForestryFisheryValue
var(agr)
sd(agr)
range(agr)
#the variance for Health expenditure per capita is 1.953117e+23
#the sd for health expenditure per capita us 441940795692
#the range for health expenditure per capita is from 2.711784e+07 to 3.100000e+12


df <- merge(data,econ,by="Location")  
str(df)
#We have 142 entries after joining
#We have in total 10 variables

#Remove COuntry Code
df <- df[-c(6)]
str(df)
#So we are left with 9 variables

#Finding correlation
df.cor <- cor(df[-c(1)])
corrplot(df.cor,tl.cex=0.6,cl.cex=0.6)


#We make a heatmap
mat <- data.matrix(df[-c(1)])
mat
nba_heatmap <- heatmap(mat, Rowv=NA, Colv=NA,  scale="column", margins=c(5,10))



#Histogram of continuous variables
#We initially potted histograms of all the data we saw that they were right skewed because the values
#were very large
#therefore we log transformed the data except proportion of people exposed to hap
#because it was already between 0 and 1

log_df <- df
str(log_df)
log_df[-c(1,5)] <- log(log_df[-c(1,5)])
log_df


a1 <- ggplot(data=log_df, aes(log_df$`Median pm2.5`)) + geom_histogram(col="black", 
  fill="red",alpha = .2,bins=10)+labs(x='Median pm2.5',y='Frequency',title='Histogram of log of Median pm2.5')


a2 <- ggplot(data=log_df, aes(log_df$`Median ppb`)) + geom_histogram(col="black", 
fill="cyan",alpha = .2,bins=10)+labs(x='Median Ozone ppb',y='Frequency',title='Histogram of log of Median Ozone ppb')

a3 <- ggplot(data=log_df, aes(log_df$Population)) + geom_histogram(col="black", 
fill="brown",alpha = .2,bins=10)+labs(x='Population',y='Frequency',title='Histogram of log of Population of each country')

a4 <- ggplot(data=log_df, aes(log_df$`Median proportion exposed to hap`)) + geom_histogram(col="black", 
fill="orange",alpha = .2,bins=10)+labs(x='Median Proportion',y='Frequency',title=
'Histogram of Median proportion exposed to hap')

grid.arrange(a1,a2,a4,nrow=2,ncol=2)


a5 <- ggplot(data=log_df, aes(log_df$GDPPerEmployedPerson)) + geom_histogram(col="black", 
                                                                             fill="Red",alpha = .2,bins=10)+labs(x='GDPPerEmployedPerson',y='Frequency',title=
                                                                                                                   'Histogram of log of GDPPerEmployedPerson')

a6 <- ggplot(data=log_df, aes(log_df$GDP)) + geom_histogram(col="black", 
  fill="yellow",alpha = .2,bins=10)+labs(x='Total GDP',y='Frequency',title=
  'Histogram of log of Total GDP for each country')

a7 <- ggplot(data=log_df, aes(log_df$AgricultureForestryFisheryValue)) + geom_histogram(col="black", 
  fill="green",alpha = .2,bins=10)+labs(x='AgricultureForestryFisheryValue',y='Frequency',title=
  'Histogram of log of AgricultureForestryFisheryValue')


a8 <- ggplot(data=log_df, aes(log_df$HealthExpenditurePerCapita)) + geom_histogram(col="black", 
        fill="orange",alpha = .2,bins=10)+labs(x='HealthExpenditurePerCapita',y='Frequency',title=
        'Histogram of log of HealthExpenditurePerCapita')

grid.arrange(a5,a6,a7,a8,nrow=2,ncol=2)



##Box plot of GDP against pollution
#pollution variables vs GDP boxplots
p1 <- ggplot(data=log_df, aes(log_df$`Median pm2.5`,log_df$GDP, group = 1)) + geom_boxplot(col=c('black'),
            fill=c('cyan'))+labs(y='GDP',x='Median PM2.5',
                                    title='Log Median PM2.5 vs Log GDP')

p2 <- ggplot(data=log_df, aes(log_df$`Median ppb`,log_df$GDP, group = 1)) + geom_boxplot(col=c('black'),
           fill=c('blue'))+labs(y='GDP',x='Median ppb',
                      title='Log Median ppb vs Log GDP')

p3 <- ggplot(data=log_df, aes(log_df$`Median proportion exposed to hap`,log_df$GDP, group = 1)) + geom_boxplot(col=c('black'),
            fill=c('red'))+labs(y='GDP',x='Median proportion',
            title='Proportion exposed to hap vs GDP')

grid.arrange(p1, p2,p3, ncol=3)


#pollution variables vs AFF Value boxplots
p4 <- ggplot(data=log_df, aes(log_df$`Median pm2.5`,log_df$AgricultureForestryFisheryValue, group = 1)) + geom_boxplot(col=c('black'),
            fill=c('cyan'))+labs(y='GDP',x='AFF Value',
            title='Log Median PM2.5 vs Log AFF Value')

p5 <- ggplot(data=log_df, aes(log_df$`Median ppb`,log_df$AgricultureForestryFisheryValue, group = 1)) + geom_boxplot(col=c('black'),
        fill=c('blue'))+labs(y='GDP',x='AFF Value', title='Log Median ppb vs Log AFF Value')

p6 <- ggplot(data=log_df, aes(log_df$`Median proportion exposed to hap`,log_df$AgricultureForestryFisheryValue, group = 1)) + geom_boxplot(col=c('black'),
      fill=c('red'))+labs(y='GDP',x='AFF Value',
      title='Proportion exposed to hap vs Log AFF Value')

grid.arrange(p4, p5,p6, ncol=3)


#pollution variables vs HealthExpenditurePerCapita boxplots
p7 <- ggplot(data=log_df, aes(log_df$`Median pm2.5`,log_df$HealthExpenditurePerCapita, group = 1)) + geom_boxplot(col=c('black'),
     fill=c('cyan'))+labs(y='GDP',x='Health Expenditure',
     title='Log Median PM2.5 vs Log Health Expenditure')

p8 <- ggplot(data=log_df, aes(log_df$`Median ppb`,log_df$HealthExpenditurePerCapita, group = 1)) + geom_boxplot(col=c('black'),
      fill=c('blue'))+labs(y='GDP',x='Health Expenditure', title='Log Median ppb vs Log Health Expenditure')

p9 <- ggplot(data=log_df, aes(log_df$`Median proportion exposed to hap`,log_df$HealthExpenditurePerCapita, group = 1)) + geom_boxplot(col=c('black'),
       fill=c('red'))+labs(y='GDP',x='Health Expenditure',
       title='Proportion exposed to hap vs Log Health Expenditure')

grid.arrange(p7, p8,p9, ncol=3)

log_df

#Regression model
#We apply linear regression because after applying the log transform almost all of our
#features have become normally distributed
#We study three different set of relationships
#First of all the relationship between GDP and pollution variables
#Secondly the relationship between Agriculture Fishery value and pollution variables
#Thirdly the relationship between Health Expenditure and pollution variables

#Relationship between GDP and pollution variables

log_df$Median..�g.m3.
model <- lm(GDP~Median..�g.m3.+`Median ppb`+`Median proportion exposed to hap`,data=log_df,na.action = "na.fail")
summary(model)

#Adjusted R-squared:  0.336
#The Median ppb and Median proportion exposed to hap seem to be significant at a significance level of 0.05
##The Median pm2.5 doesn't seem to be significant at 0.05 level
#p-value: 6.871e-13


#Coefficients:
#                                     Estimate    Std. Error t value   Pr(>|t|)    
#(Intercept)                         13.7606         3.5167   3.913     0.000143 ***
#  `Median pm2.5`                        -0.5840     0.3074  -1.900     0.059547 .  
#`Median ppb`                         3.5029         1.0005   3.501     0.000625 ***
#  `Median proportion exposed to hap`  -2.2477       0.5035  -4.465     1.65e-05 ***

plot(model,col='blue',pch=16)
#The residual vs Fitted model seems to be random there is no increasing or decereasing pattern
#Similarly, the scale location plot also seems to be random hence homoscedasticity holds
#The residuals vs leverage plot shows there are no outliers
# Finally the normal qq plot shows a very slight deviation from normality but overall it fits the line



library("ggplot2")

gdpozone <- ggplot(log_df,aes(y=log_df$GDP,x=log_df$`Median ppb`))+labs(y= "GDP", x = "ozone(ppb)")+geom_point()+geom_smooth(method="lm")
gdpozone

gdphap <- ggplot(log_df,aes(y=log_df$GDP,x=log_df$`Median proportion exposed to hap`))+labs(y= "GDP", x = "population proportion exposed to HAP")+geom_point()+geom_smooth(method="lm")
gdphap

#We use the dredge function for model selection
dredge(model)

#when we do this, we get a model selection table
#In each row is a form of the model
#explanatory variables names are provided as column names followed by likelihood based measures of model performance
#for each row, if there are numbers in the column of an explanatory variable, this means that the variable was 
#included in the model assessed
#since the model with the lowest AIC (or AICc, which is AIC corrected for small sample sizes) value is the best model. 
#Models with AIC/AICc values that are very close to each other are considered equivalent as long as the 
#delate value is less than 2
#The lowest AIC value is of the model containing Mdn ppb and Mdn proportion exposed to hap


#Relationship between Agriculture Fishery value and pollution variables

model.1 <- lm(AgricultureForestryFisheryValue~Median..�g.m3. +`Median ppb`+`Median proportion exposed to hap`,data=log_df,na.action = "na.fail")
summary(model.1)

#Adjusted R-squared:  0.01889 
#The Median ppb is very close to 0.05 significance level with p-value 0.052037
#So we can't say with certainty that if it is significant at 0.05 level or not
##The Median pm2.5 and Median proportion exposed don't seem to be significant at 0.05 level
# p-value: 0.1317

#Coefficients:
#                                     Estimate Std. Error t value   Pr(>|t|)    
#(Intercept)                        14.74214    3.76097   3.920     0.000139 ***
#`Median pm2.5`                       -0.27316    0.32877  -0.831   0.407481    
#`Median ppb`                        2.09699    1.07002   1.960     0.052037 .  
#`Median proportion exposed to hap` -0.09794    0.53842  -0.182     0.855923 

plot(model.1,col='blue',pch=16)
#The residual vs Fitted model seems to be random there is no increasing or decereasing pattern
#Similarly, the scale location plot also seems to be random hence homoscedasticity holds
#The residuals vs leverage plot shows there are no outliers
# Finall the normal qq plot shows that the distribution is normal


agohap <- ggplot(log_df,aes(y=log_df$AgricultureForestryFisheryValue,x=log_df$`Median proportion exposed to hap`))+labs(y= "Agriculture, forestry, and fishing value ", x = "population proportion exposed to HAP")+geom_point()+geom_smooth(method="lm")
agohap

agozone <- ggplot(log_df,aes(y=log_df$AgricultureForestryFisheryValue,x=log_df$`Median ppb`))+labs(y= "Agriculture, forestry, and fishing value ", x = "ozone(ppb)")+geom_point()+geom_smooth(method="lm")
agozone

dredge(model.1)

#when we do this, we get a model selection table
#In each row is a form of the model
#explanatory variables names are provided as column names followed by likelihood based measures of model performance
#for each row, if there are numbers in the column of an explanatory variable, this means that the variable was included in the model assessed
#since the model with the lowest AIC (or AICc, which is AIC corrected for small sample sizes) value is the best model. 
#Models with AIC/AICc values that are very close to each other are considered equivalent as long as the delate value is less than 2
#The lowest AIC value is of the model containing 
#Mdn ppb and Mdn pm2.5
#The second lowest AIC value is of Mdn ppb and Mdn proportion exposed to hap


#Relationship between Health Expenditure and pollution variables

model.2 <- lm(HealthExpenditurePerCapita~Median..�g.m3. +`Median ppb`+`Median proportion exposed to hap`,data=log_df,na.action = "na.fail")
summary(model.2)

#Adjusted R-squared:  0.7915 
#The Median ppb, Median pm2.5 and Median proportion exposed are significant at 0.05 level
#p-value: < 2.2e-16

#Coefficients:
#                                     Estimate      Std. Error  t value     Pr(>|t|)    
#(Intercept)                          3.3170         1.5497     2.140       0.034079 *  
#  `Median pm2.5`                        -1.0279     0.1355    -7.588     4.38e-12 ***
#  `Median ppb`                         1.6897       0.4409    3.833     0.000192 ***
#  `Median proportion exposed to hap`  -2.8342       0.2219  -12.775     < 2e-16 ***


plot(model.2,col='blue',pch=16)
#The residual vs Fitted model seems to be random there is no increasing or decereasing pattern
#However towards the we can see a sort of a very slight increase in values
#Similarly, the scale location plot also seems to be random hence homoscedasticity holds
#The residuals vs leverage plot shows there are no outliers
# Finall the normal qq plot shows that the distribution is normal and normality holds


healthozone <- ggplot(log_df,aes(y=log_df$HealthExpenditurePerCapita,x=log_df$`Median ppb`))+labs(y= "Health Expenditure per Capita", x = "ozone(ppb)")+geom_point()+geom_smooth(method="lm")
healthozone

healthpm <- ggplot(log_df,aes(y=log_df$HealthExpenditurePerCapita,x=log_df$Median..�g.m3.))+labs(y= "Health Expenditure per Capita", x = "PM2.5(??g/m3)")+geom_point()+geom_smooth(method="lm")
healthpm

healthhap <- ggplot(log_df,aes(y=log_df$HealthExpenditurePerCapita,x=log_df$`Median proportion exposed to hap`))+labs(y= "Health Expenditure per Capita", x = "population proportion exposed to HAP")+geom_point()+geom_smooth(method="lm")
healthhap

dredge(model.2)

#when we do this, we get a model selection table
#In each row is a form of the model
#explanatory variables names are provided as column names followed by likelihood based measures of model performance
#for each row, if there are numbers in the column of an explanatory variable, this means that the variable was included in the model assessed
#since the model with the lowest AIC (or AICc, which is AIC corrected for small sample sizes) value is the best model. 
#Models with AIC/AICc values that are very close to each other are considered equivalent as long as the delate value is less than 2
#The lowest AIC value is of the model containing 
# all three variables Mdn ppb, Mdn pm2.5 and Mdn proportion exposed to hap


#Principal Component Analysis
install.packages('ade4')
install.packages('vegan')
install.packages('gclus')
install.packages('ape')
library(ade4)
library(vegan)
library(gclus)
library(ape)

#How do we determine if we have a linear relationship between variables or not?
#we can use the decorana function (detrended correlational analysis)
#If value of the longest gradient is close or larger than 4, then we use unimodal methods (CA, CCA)
#If not, then we use linear methods (PCA, RDA)
log_df
summary(log_df)
#We use the log transformed data
decorana(log_df[-c(1)])
#We can see the values of longest gradient is less than 4 therefore we use PCA

?decorana
source("evplot.R")
source("cleanplot.pca.R")
source("PCA.R")
source("CA.R")

#We use PCA based on a correlation matrix 
#Because the units are in different scale
#Proportion is between 0 to 1 where as the rest have been log transformed
#We exclude Location because it is categorical
#We also exclude GDP per Employed Person because we already have GDP

log_df.pca <- rda(log_df[-c(1,9)], scale=TRUE) # Argument scale=TRUE calls for a standardization of the variables, i.e. correlation matrix
log_df.pca


summary(log_df.pca) # Default scaling 2
#The inertia is 7
#The larger the eigen value the important the axes and the more variation explained

#Importance of components:
#                        PC1    PC2    PC3     PC4     PC5      PC6     PC7
#Eigenvalue            3.0577 2.2401 1.2097 0.27483 0.15348 0.053990 0.01022
#Proportion Explained  0.4368 0.3200 0.1728 0.03926 0.02193 0.007713 0.00146
#Cumulative Proportion 0.4368 0.7568 0.9296 0.96890 0.99083 0.998540 1.00000

summary(log_df.pca, scaling=1)


#Inertia: in vegan’s language, this is the general term for “variation” in the data
#In PCA, the “inertia” is either the sum of the variances of the variables (PCA on a covariance matrix) or
#in this case (PCA on a correlation matrix), the sum of the diagonal values of the correlation matrix, 
#i.e. the sum of all correlations of the variables with themselves, corresponds to number of variables

#Eigenvalues: symbolized lj, these are measures of the importance (variance) of the axes. 
#They can be expressed as Proportions Explained, or proportions of variation accounted for, by dividing them by the total inertia.

#Scaling: “Scaling” refers to the way ordination results are projected in the reduced space 
#for graphical display. There is no single way to optimally display objects and variables 
#together in a PCA biplot, i.e. a plot showing two types of results, here the sites and the variables.
#Two main types of scaling are used:

#Scaling 1 = distance biplot: the eigenvectors are scaled to unit length.
#Distances among objects in the biplot are approximations of their Euclidean distances in multidimensional space. 
#This means that distances between datapoint on the plot are accurate but angles between variables in the biplot do not reflect their correlation

#Scaling 2 = Scaling 2= correlation biplot: each eigenvector is scaled to the square root of 
#its eigenvalue. Distances between datapoints or sites or rows in the biplot are not 
#approximations of their Euclidean distances in multidimensional space. The angles between descriptors 
#in the biplot reflect their correlations

####Species scores: coordinates of the arrow heads of the variables. 
#For historical reasons, response variables are always called “species” in vegan, 
#no matter what they represent.

####Site scores: coordinates of the sites in the ordination diagram. 
#Objects are always called “Sites” in vegan output file


# Plots using vegan's biplot.rda
windows(title="PCA biplots - environment - biplot.rda", 12, 6) 
#if you are using Windows, x11 should be changed to windows. If you are using Mac, use "quartz"

par(mfrow=c(1,2))
biplot(log_df.pca, scaling=1, main="PCA - scaling 1")
biplot(log_df.pca, main="PCA - scaling 2")	# Default scaling = 2


#In scaling = 1, distances in multivariate space between sites are accurate
#In scaling = 2, the angle and the length of the arrows which represent the variables in the dataset are accurately depicted
#The scaling 2 biplot shows that the variables are organized in groups. 

#There are 142 sites
#The scaling-2 biplot shows certain variables are associated with each other
#We can't see the variables in this plot so we use clean plot

# Plots using cleanplot.pca()
# A rectangular graphic window is needed for the two plots

windows(title="PCA biplots - environment - cleanplot.pca", 12, 6)
cleanplot.pca(log_df.pca)							# with site labels only (vegan's standard)
cleanplot.pca(log_df.pca, point=TRUE)	# with points for sites and arrowheads
cleanplot.pca(log_df.pca, ahead=0)			# ... and without arrowheads

# In scaling =1, the radius of this circle represents the length of the vector representing 
#a variable that would contribute equally to all the dimensions of the PCA space.
#for any given pair of axes, the variables that have vectors longer than this radius 
#make a higher contribution than average

#The variables:

#Population exposed to hap
#Agriculture Forest Fishery Value
#GDP
#HealthCare expenditure 
#Make higher contribution then average

#The variables Median ppb and Agriculture forest fishery value are closely associated
#Explain 

#PCA is not a statistical test, but a heuristic procedure.  
#It aims at representing the major features of the data along a reduced number of axes 
#hence, the expression “ordination in reduced space”

#In our lecture, we examined the eigenvalues (i.e. proportion of variance explained)
#and decided how many axes were worth representing and displaying on the basis of the amount of variance explained

#Rather than choosing arbritarily, we can use a few procedure to standardize selection of components
#One option is Kaiser–Guttman criterion) 
#It consists of computing the mean of all eigenvalues and interpreting only the axes whose eigenvalues are larger than that mean.

#To do this, we will need to pull out the eigenvalues
(ev <- log_df.pca$CA$eig)

# Apply Kaiser-Guttman criterion to select axes
ev[ev > mean(ev)] #pulling out eigenvalues that are larger than the mean eigenvalue

#PC1, PC2 and PC3 are greater than mean

#Another option is to compute the broken stick model

#This model which randomly divides a stick of unit length into the same number of pieces as there are PCA axes.
#The pieces are then put in order of decreasing length and compared to the eigenvalues.
#Only components with eigenvalues that are larger than the length of the corresponding piece of the stick are selected

#Building broken stick model

n <- length(ev) #findiing out the number of variables
bsm <- data.frame(j=seq(1:n), p=0) #creating an empty dataset
bsm$p[1] <- 1/n 
for (i in 2:n)
{
  bsm$p[i] = bsm$p[i-1] + (1/(n + 1 - i))
}
bsm$p <- 100*bsm$p/n
bsm

# Plot the output from Kaiser-Guttman and Broken stick model
windows(title="PCA eigenvalues")
par(mfrow=c(2,1))
barplot(ev, main="Eigenvalues", col="bisque", las=2)
abline(h=mean(ev), col="red")		# average eigenvalue
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
barplot(t(cbind(100*ev/sum(ev),bsm$p[n:1])), beside=TRUE, 
        main="% variance", col=c("bisque",2), las=2)
legend("topright", c("% eigenvalue", "Broken stick model"), 
       pch=15, col=c("bisque",2), bty="n")

# Same plots using a single function: evplot()
# Plot eigenvalues and % of variance for each axis
evplot(ev)

#PC1, PC2 and PC3 are the three axes that are greater than 
#average eignen value as per KaiserGuttman Criterion
#and Broken stick model both

# Combining clustering and ordination results

#We can compare a cluster analysis and an ordination to explain or confirm the differences between groups of sites.
#Here, I have used two ways of combining these results
#The first differentiates clusters of sites by colours on the ordination plot
#the second overlays a dendrogram on the plot

# Clustering the objects using the environmental data: Euclidean 
# distance after standardizing the variables, followed by Ward clustering
env.w <- hclust(dist(scale(log_df[-c(1)])), "ward")
# Cut the dendrogram to yield 4 groups
gr <- cutree(env.w, k=4)
grl <- levels(factor(gr))

# Get the site scores, scaling 1
sit.sc1 <- scores(log_df.pca, display="wa", scaling=1)
#We use scaling 1 because we need accurate distances

# Plot the sites with cluster symbols and colours (scaling 1)
windows(title="Ordination and clustering")
p <- plot(log_df.pca, display="wa", scaling=1, type="n", 
          main="PCA correlation + clusters")
abline(v=0, lty="dotted")
abline(h=0, lty="dotted")
for (i in 1:length(grl))
{
  points(sit.sc1[gr==i,], pch=(14+i), cex=2, col=i+1)
}

text(sit.sc1, row.names(log_df[-c(1)]), cex=0.7, pos=3)
# Add the dendrogram
ordicluster(p, env.w, col="dark grey")
# Add legend interactively
legend(locator(1), paste("Cluster", c(1:length(grl))), pch=14+c(1:length(grl)), 
       col=1+c(1:length(grl)), pt.cex=2)

#We can see that there are groups of four clusters are being formed
#The points are very close together 
