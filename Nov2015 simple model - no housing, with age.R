library(ggplot2)
library(lavaan)
library(semPlot)
library(semTools)
library(foreign)



#Read in CSV of data without duplicates#
WICData2 <-read.csv("C:/Users/cschonberg/Desktop/WIC fellowship/November data analysis/11-15 models with age.csv", header = TRUE, sep = ',')
WICData2<-data.frame(WICData2)


model3<- '
#measurement model
homeliteracyenvironment =~ Q55r_HLEReadingFreq + Q57ar_HLEStorytelling + Q57br_HLEPreliteracy + Q57e_HLEPastMonthLibrary + Q57f_HLENumBooks + Q96a_parentEducation
childcareenrollmentdifficulty =~ Q97r_matEmploy + Q47_childcareDifficulty + Q48a_childcareCostTooHigh + Q48b_childcareNoSpace + Q48c_childcareBadHoursOrLoc + Q48d_childcarePoorQuality + Q48e_childcareNoTransport
#regressions
Q49_isEnrolledPreschool ~ Q96a_parentEducation + childcareenrollmentdifficulty + homeliteracyenvironment + Q97r_matEmploy + childAge
Q97r_matEmploy ~ Q96a_parentEducation 
#covariances
Q47_childcareDifficulty ~~ Q48a_childcareCostTooHigh'

#nonrobust
fit <- sem(model3, data=WICData2, fixed.x=FALSE)
summary(fit, standardized=TRUE, fit.measures=TRUE)
varTable(fit)
fitMeasures(fit)
mardiaKurtosis(WICData2)


#robust statistics
fit <- sem(model3, data=WICData2, meanstructure=TRUE,std.lv=TRUE, estimator="MLM")
parameterEstimates(fit,ci=FALSE,standardized=TRUE)
summary(fit, standardized=TRUE, fit.measures=TRUE)
varTable(fit)
fitMeasures(fit)
mardiaKurtosis(WICData2)

