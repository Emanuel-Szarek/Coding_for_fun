rm(list=ls())
#Part A
college <- read.csv("/Users/emanuel.s/Documents/ISTE 780/College.csv")

#Part B
fix(college)
rownames (college )=college [,1]
fix (college )
college =college [,-1] 
fix (college)

#Part C
attach(college)
summary(college)
Private = as.factor(Private)
pairs(college[,2:10]) #does not work with first column

plot(Private, Outstate)


Elite =rep ("No",nrow(college ))
Elite [college$Top10perc >50]=" Yes" 
Elite =as.factor(Elite)
college =data.frame(college ,Elite)
summary(Elite)
plot(Outstate, Elite)
par(mfrow=c(2,2))

hist(P.Undergrad)
hist(Room.Board)
hist(Books)
hist(Grad.Rate)

#(d) i. Which of the predictors are quantitative, and which are qualitative?
#Private and Elite are qualitative, everything else is quantitative

#ii. What is the range of Enroll? What are the mean and standard deviation?
range(Enroll)
#35 6392
mean(Enroll)
#779.973
sd(Enroll)
#929.1762


#iii. Now remove the 100th through 200th observations. 
#What are the range, mean, and standard deviation of Enroll in the subset of the data that remains?
Enroll_RemovedRows = Enroll[-c(100:200)]
range(Enroll_RemovedRows)
#35 6392
mean(Enroll_RemovedRows)
#823.0695
sd(Enroll_RemovedRows)
#972.2258