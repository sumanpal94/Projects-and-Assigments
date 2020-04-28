data = read.csv("D:/Suman/PGDBA/ISI/SSD/Assignment1/ml_friendly_bike_detection.csv")                  ## Reading dataset from csv file ##
data=data[data$count != 0,]
data=data[data$count < 11,]

summary(data)                                ## Summary of dataset ##


## Barplot for variable "count" and Histogram for variable "MaxTemp"
histogramy=hist(data$MaxTemp)
histogramx = hist(data$count)


## Frequency distribution table for both variables
FrequencytableX=table( data$count )                      
FrequencytableY=data.frame("Class Intervals" = as.character(paste(histogramy$breaks[1:9],histogramy$breaks[2:10], sep= "-"))
                           ,"Frequency"= histogramy$counts)
print(FrequencytableX)
print(FrequencytableY)

## Appropriate measures of central tendency, dispersion, skewness and kurtosis.
## For variable "count"
library(psych)
describe(data$count)

meanX = mean(data$count)
medianX = median(data$count)
varianceX = var(data$count)
library(e1071)
kurtosisX = kurtosis(data$count)
skewnessX = skewness(data$count)



## For variable "MaxTemp"
library(psych)
describe(data$MaxTemp)

meanY = mean(data$MaxTemp)
medianY = median(data$MaxTemp)
varianceY = var(data$MaxTemp)
kurtosisY = kurtosis(data$MaxTemp)
skewnessY = skewness(data$MaxTemp)


## Box Plot
boxplot(data$MaxTemp ~ data$count , data = data)


library(MASS)
library(fitdistrplus)
## Fitting probability distribution in variable "count"

pfitX=fitdist(data$count,'geom')


## Fitting probability distribution in variable "MaxTemp"
normy=fitdist(data$MaxTemp,'norm')
logisy=fitdist(data$MaxTemp,'logis')


##  GOODNESS OF FIT
##  FOR X

xx=FrequencytableX

E_geom = dgeom( 1:10,prob  =  pfitX$estimate )
expectedfreq = (sum(xx) *E_geom)


chisquarev = sum((xx-expectedfreq)^2/(expectedfreq))
pchisq ( chisquarev , df=8 )

plot(expectedfreq)


## GOODNESS OF FIT
## FOR Y
plot(normy)
plot(logisy)

num_of_samples = 1410

##Normal Distribution
y <- rnorm(num_of_samples, mean =   normy$estimate[1], sd=  normy$estimate[2]  )

breaks = seq(-20,20,by=5)
temp.cut = cut(y,breaks,right = F)
temp.freq = table(temp.cut)
cbind(temp.freq)

hist(y)
result = ks.test(data$MaxTemp, y)
result$p.value

## Logistic Distribution
z = rlogis(num_of_samples, location =logisy$estimate[1], scale =  logisy$estimate[2])

breaks = seq(-20,20,by=5)
temp.cut = cut(z,breaks,right = F)
temp.freq = table(temp.cut)
cbind(temp.freq)

hist(z)
result = ks.test(data$MaxTemp, z)
result$p.value

#Linear Regression of MaxTemp on MinTemp
yonx=lm(data$MaxTemp ~ data$MinTemp)

#Scatter plot and fitted line
plot(data$MinTemp, data$MaxTemp) 
abline(lm(data$MaxTemp ~ data$MinTemp))

#ANOVA Table and coefficient of Determination
anova(yonx)
summary(yonx)$r.squared 

#Residuals Plot
plot(yonx, which=1, col=c("blue")) 


plot(data$MinTemp , rstudent(yonx), 
     ylab="Studentized Residuals", xlab="MinTemp", 
     main="Studentized residuals") 


library(olsrr)

#Studentized Residuals Plot
ols_plot_resid_stud(yonx)

#Cook's Distance
ols_plot_cooksd_chart(yonx)

