#Your dataset is about crime rates across US states. 
#The variables in the your dataset are the following:
# Crime Rate (number of offences per million people)
#Youth (proportion of the youth in the population is similar to national average (average), 
    #lower than the national average (low) or higher than the national average (high)
#YouthUnemployment (proportion of unemployed youth is similar to the national average 
    #(average), lower than the national average (low) or 
    #higher than the national average (high)).
#You are expected to answer the following questions:
#1) Is there any relationship between the proportion of youth in the population 
    #and proportion of youth unemployment across the US states?
#2) Is there any relationship between the proportion of youth unemployed 
    #in each state and the crime rate?

setwd("D:/bilal's books 8/Lie Detector/Project")
data <- read.csv('stcp-Rdataset-Crime_modified.csv')
library(ggplot2)
#structure of the data
str(data)
unique(data$Youth)
unique(data$YouthUnemployment)
#There are three variables
#CrimeRate which is numerical
#Youth which is a factor and has 3 levels (Average,High,Low)
#YouthUnemployment which is a factor and also has 3 levels (Average,High,Low)
#Total number of rows are 47

#summary of the data
summary(data)
#The mean crime rate is 102.8
#The median is 103
#The min value is 45.5
#The max value is 161.8

sd(data$CrimeRate)
#The standard deviation of the data is 28.89327
range(data$CrimeRate)
#The range of the data is 45.5 - 161.8

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(data$Youth) #High
getmode(data$YouthUnemployment) #Average

ggplot(data=data, aes(data$Youth)) + geom_bar(col='black',
fill=c('cyan','pink','green')) + labs( x='Youth Proportion Category',y='Frequency',
  title='Barplot of proportion of youth')
#The bar plot shows that the number of states having proportion of youth higher than average 
#is more than the rest.
#The number of states having proportion of youth lower than average is the least.
#Average:15
#High   :19 
#Low    :13

ggplot(data=data, aes(data$YouthUnemployment)) + geom_bar(col='black',
fill=c('cyan','pink','green')) + labs( x='Youth Unemployment Proportion Category',y='Frequency',
title='Barplot of Proportion of Youth Unemployment')
#The bar plot shows that the number of states having proportion of unemployed youth equal to national
#average is more than the rest.
#The number of states having proportion of unemployed youth higher than average is the least.
#Average:19
#High   :11 
#Low    :17 

ggplot(data=data, aes(data$CrimeRate)) + geom_histogram(col="black", 
        fill="red",alpha = .2,bins=10)+labs(x='Crime Rate',y='Frequency',title='Histogram of Crime Rate')
#hist(data$CrimeRate)

#The crime rate looks normally distributed with a very slight skew on the right

#confirming normality:
shapiro.test(data$CrimeRate) #p-value = 0.8508 so is normally distributed.

ggplot(data=data, aes(x="",data$CrimeRate)) + geom_boxplot(col='black', 
fill='cyan')+labs(x='',y='Crime Rate',title='Boxplot of Crime Rate')
#boxplot(data$CrimeRate, horizontal = TRUE,  
#        main="Crime Rate", col = "blue")

#The box plot of crime rate shows there are no outliers



ggplot(data=data, aes(data$Youth,data$CrimeRate)) + geom_boxplot(col=c('black'),
fill=c('cyan','pink','green'))+labs(x='Youth Proportion Category',y='Crime Rate',
title='Boxplot of Crime Rate vs Youth Proportion Category')
#The box plot of youth vs crime rate shows that the spread
#of crime rate compared to the proportion of youth in the states is more or less the same
#where the mean of crime rates in states of having lower average of youths is slightly
#lower than rest of the areas but the overall crime rate has more spread in this category.


ggplot(data=data, aes(data$YouthUnemployment,data$CrimeRate))  + geom_boxplot(col=c('black'),
  fill=c('cyan','pink','green'))+labs(x='Youth Unemployment Category',y='Crime Rate',
title='Boxplot of Crime Rate vs Youth Unemployment Category')
#The box plot of YouthUnemployment vs crime rate shows that the 
#mean of crime rates for states having average proportion of youth unemployment 
#is higher than the rest.
#The plot of lower proportion of youthUnemployment has three outliers. Two outliers are 
#at the upper end and one outlier is at the lower end of the plots.
#The mean crime rate for states having higher proportion of youthUnmployment 
#and the mean crime rate for states having lower proportion of youthUnemployment is same


#1) Is there any relationship between the proportion of youth in the population 
#and proportion of youth unemployment across the US states?

#We use chi-squared test if we want to find if there are patterns 
#between two sets of categorical variables


#when performing a Chi-sq test in R
#1) The input data is in the form of a table that contains the count value of the variables in the observation.

#2) We use chisq.test function to perform the chi-square test of independence in the native stats package in R.
# For this test, the function requires the contingency table to be in the form of a matrix. 
#Depending on the form of the data, to begin with, this can need an extra step, either combining vectors into a matrix or cross-tabulating the counts among factors in a data frame. 

#3)We use read.table and as.matrix to read a table as a matrix. 
#While using this, be careful of extra spaces at the end of lines. 
#Also, for extraneous characters on the table, as these can cause errors.


#Our null hypothesis is that there is no relationship between the proportion of youth in the population 
#and proportion of youth unemployment across the US states

#Our alternative hypothesis is that there is a relationship between the proportion of youth in the population 
#and proportion of youth unemployment across the US states

#1) Single random sample
#2) Large sample size.
# If a chi squared test is conducted on a sample with a smaller size, then the chi squared test will yield an inaccurate inference. 
#3)Adequate expected cell counts. 
#A common rule is 5 or more in all cells of a 2-by-2 table, 
#and 5 or more in 80% of cells in larger tables, but no cells with zero expected count.
#4)The observations are always assumed to be independent of each other. 

#Note: not sure if we Can do a chi-square because it's saying 
#approximation may be incorrect

#Plus 3 of the cells have count less than 5
#Plus sample size is small as if there are only 47 entries

z<-table(data$Youth, data$YouthUnemployment)
z
chisq.test(z, correct=FALSE)
#Note: Found a kind of solution on the internet
chisq.test(z, correct=FALSE,simulate.p.value = TRUE)

?chisq.test
#Since the p-value is greater than 0.05 therefore we donot reject the null hypothesis
#at the 95% confidence level and we can say that there is no relationship
# between the proportion of youth in the population 
#and proportion of youth unemployment across the US states

  
#2) Is there any relationship between the proportion of youth unemployed 
#in each state and the crime rate?

#We will use Anova to find out this relationship

#Our null hypothesis is the average crime rate 
#for all groups of proportions of youth Unemployed in each state is same.

#Our alternate hypothesis is the average crime rate 
#for all groups of proportions of youth Unemployed in each state is not same.


#One way analysis of variance extends the t-test by allowing us to compare continuous data between more than two groups. 

#Here are the asusmptions of a one-way analysis of variance
#1) Independence of observations – this is an assumption of the model that simplifies the statistical analysis.
#2) Normality – the data or the distributions of the residuals are normal.
#3) Equality (or "homogeneity") of variances, called homoscedasticity — the variance of data in groups should be the same.

#Each of the observations are from different states therefore they are assumed to be independent
bartlett.test(CrimeRate~YouthUnemployment,data=data)
#The null hypothesis of bartlett test is that all k population variances are equal
#The alternative is that at least two are different
#Since the p-value is 0.9491 we donot reject the null hypothesis and we say variances are equal

#The boxplot of Crime Rate vs Youth Unemployment Category shows that the overall spread is more or less same
#Furthermore, Crime Rate is normally distributed. 
#So the assumptions for anova hold 

crime.aov<- aov(CrimeRate~YouthUnemployment, data)
class(crime.aov)
#class.aov has the class "aov" and "lm"

typeof(crime.aov)
#the type of tyres.aov is a list

names(crime.aov)
#crime.aov contains different informations (approximately 13) 
#We can access each of them using tyres.aov$information_name 
#The information_name can be coefficients, residuals, effects, rank, model etc

summary(crime.aov)
#Since the p-value is 0.139 hence there is no difference in the average crime rate 
#for all groups of proportions of youth Unemployed in each state. 
#We donot reject the null hypothesis at a significance level of 0.05.

########################################################################################

#xbar+- z* psd/sqrt(n)
sd.p=function(x){sd(x)*sqrt((length(x)-1)/length(x))}
psd = sd.p(data$CrimeRate)
psd
xbar = mean(data$CrimeRate)
z = 1.96
n=length(data$CrimeRate)
n
error = z* psd/sqrt(n)
error + xbar
error - xbar
error


#alpha value
library(psych)
install.packages("psych")
cronbach.alpha(data)

plot(density(data$CrimeRate))



