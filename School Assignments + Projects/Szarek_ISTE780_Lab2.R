rm(list=ls())

Auto= read.csv("/Users/emanuel.s/Documents/ISTE 780/Lab2/Auto.csv", header=T, na.strings = "?")
#(a) Produce a scatterplot matrix, which includes all of the variables in the data set.
Auto = subset(Auto, horsepower!="?") #Removing the "?" in Horsepower so that cor() will work
plot(Auto)
attach(Auto)


#(b) Compute the matrix of correlations between the variables using the function cor() .
#You will need to exclude the name variable, which is qualitative.
cor(Auto[,-9])

#c) Use the lm() function to perform a multiple linear regression with mpg as the
#response and all other variables except name as the predictors. Use the summary() function to print the results. Comment on the output. For instance:
lm.fit=lm(mpg~.-name, data=Auto)
summary(lm.fit)

#  i. Is there a relationship between the predictors and the response?
#The F-Statistic is the most useful with multiple predictors, helps us find if any of the predictors are relevant or useful to the response
#and since we see an F-statistic much farther than 1, 252.4, we can go ahead and assume that there is in fact
#a relationship between the predictors and the response.

#  ii. Which predictors appear to have a statistically significant relationship to the response?
#By looking at the P-Values we see if the predictors are important/Statistically significant.
#The following predictors have a P-value lower than .05, displacement, weight, year and origin, this indicates
#that they are indeed statistically significant.

#  iii. What does the coefficient for the year variable suggest?
#The coefficient for year is 0.750773, this means that every time the year of the car goes up by 1 the amount
#of miles per gallon the car gets, goes up by 0.750773.  This is a positive contributor for increasing MPG with years, thus newer cars get more MPG.

#(d) Use the plot() function to produce diagnostic plots of the linear regression fit. 
par(mfrow=c(2,2))
plot(lm.fit)

#Comment on any problems you see with the fit. Do the residual plots suggest any unusually large outliers? 
#From the fit we can see that there is a non linear curve/relationship.
#The upper right hand corner looks like there may be a cluster of potential outliers.

#Does the leverage plot identify any observations with unusually high leverage?
#Based on the 4th plot, it looks like 14 is a point that would be considered high leverage due to how far away it is from the cluster

#(e) Use the * and : symbols to fit linear regression models with interaction effects.
#Do any interactions appear to be statistically significant?
summary(lm(mpg~year*weight, data=Auto)) 
#Due to the P-value being lower than .05 we can assume this interaction is statistically significant.
summary(lm(mpg~cylinders*horsepower, data=Auto)) 
#Due to the P-value being lower than .05 we can assume this interaction is statistically significant.
summary(lm(mpg~acceleration*weight, data=Auto)) 
#Due to the P-value being lower than .05 we can assume this interaction is statistically significant.

#(f) Try a few different transformations of the variables, such as log(X), X0.5, X2. Comment on your findings.
summary(lm(mpg~.+log(acceleration)-name,data=Auto))
#By logging acceleration we are able to get a P-value below .05 meaning, using the log of acceleration makes the
#predictor statistically significant and slightly raises both R squared and adjusted R squared

summary(lm(mpg~.+I(cylinders^2)-name,data=Auto))
#By squaring cylinders we are able to get a P-value below .05 meaning, squaring cylinders makes the
#predictor statistically significant and slightly raises both R squared and adjusted R squared

summary(lm(mpg~.+log(acceleration) + I(cylinders^2)-name,data=Auto))
#Using both Log(accelerationan) and squaring cylinders, both end up with a P=value below .05 making them significant
#Combining them also raises our R-squared and adjusted R-squared values making it a slightly better model then either of them alone

summary(lm(mpg~.+I(displacement^.05)-name,data=Auto))
#This model is even better than the previous 3

#PART II
library(ISLR)
attach(Carseats)
#(a) Fit a multiple regression model to predict Sales using Price, Urban, and US.
lm.fit = lm(Sales~Price+Urban+US, data=Carseats)
summary(lm.fit)
?Carseats
#(b) Provide an interpretation of each coefficient in the model. Be careful—some of the variables in the model are qualitative!
#Price has a coefficient of -0.054459, so for every time price charged of a competitor goes up by 1 dollar, 
#the amount of sales decreases by ~55 dollars.  

#UrbanYes has a coeffcient of -.021916, so compared to UrbanNo, being in an urban area reduces sales by ~22 dollars.

#USYes has a coefficent of 1.200573, so compared to USNo, being in the US increases sales by ~$1,200 dollars.  

#(c) Write out the model in equation form, being careful to handle the qualitative variables properly.
# Sales =13.043469 + (-0.054459*Price) + (-0.021916*Urban) + (1.200573*US) + Epsilon
# Because Urban and US are qualitative, 0 will be used as the base for not urban and not in the US,
# while 1 will be used if Urban is yes and it is in the US (USYes).  

#(d) For which of the predictors can you reject the null hypothesis H0: βj = 0?
#Due to the p-value being lower than .05 we reject the null in favor of the alternative for predictors Price and US

#(e) On the basis of your response to the previous question, fit a smaller model that
#only uses the predictors for which there is evidence of association with the outcome.
lm.fit = lm(Sales~Price+US, data=Carseats)
summary(lm.fit)

#(f) How well do the models in (a) and (e) fit the data?
#The residual error went down slightly for (e), this means the average error is ~$2,469 difference than actual value.
#Looking at the R-squared value, it looks like the R-squared stayed the same, but the Adjusted R-squared
#went up slightly for model (e).  Model (e) also has a higher F-statistic which is good and same p-value.  
#Thus model (e) would be better to use, although the change is very small.  

#(g) Using the model from (e), obtain 95% confidence intervals for the coefficient(s).
confint(lm.fit)
#95% chance that the intercept will reside within the interval of 11.79.. and 14.27...
