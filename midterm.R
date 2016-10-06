#### BIOS 526 Midterm
#### Xinyi Zhao        

#install.packages("lme4")

### Part 1

setwd("C:/Users/zhaohexu/Dropbox/Academic/Courses/Fall 2015/BIOS 526")
nls <- read.csv("NLS.csv")
head(nls)
str(nls)
nls <- nls[order(nls$id, nls$age),]
nls <- within(nls, edu <- relevel(edu, ref = "HS or less")) # reference group of education is HS or less

## Summary statistics

summary(nls)

# number of subjects
unique(nls$id) 

# gender
temp1 <- subset(nls, subset = gender != "NA")
summary(temp1)
uni <- unique(temp1$id)
a <- length(uni)
last <- c()

for (i in 1:a) {
  temp11<-subset(temp1, id==uni[i])
  if (dim(temp11)[1] > 1) {
    last.temp<-temp11[dim(temp11)[1],]
  }
  else {
    last.temp<-temp11
  }
  last<-rbind(last, last.temp)
}

head(last)
table(last$gender) # 1=male, 2=female
prop.table(table(last$gender))

# education
temp2 <- subset(nls, subset = edu != "NA")
summary(temp2)
uni <- unique(temp2$id)
b <- length(uni)
last <- c()

for (i in 1:b) {
  temp22<-subset(temp2, id==uni[i])
  if (dim(temp22)[1] > 1) {
    last.temp<-temp22[dim(temp22)[1],]
  }
  else {
    last.temp<-temp22
  }
  last<-rbind(last, last.temp)
}

head(last)
table(last$edu)
prop.table(table(last$edu))
# HS or less = did not complete high school
# HS or GED = high school diploma or GED
# College or AD = Associate¡¯s or Bachelor¡¯s degree
# Graduate or Prof = Graduate or professional degree

# income
tapply(nls$income, nls$age, mean, na.rm=TRUE)
tapply(nls$income, nls$age, sd, na.rm=TRUE)

## Missing data pattern
tapply(nls$income, nls$age, summary, na.rm=TRUE) # by age
tapply(nls$income, nls$gender, summary, na.rm=TRUE) # by gender
table(nls$gender)
tapply(nls$income, nls$edu, summary, na.rm=TRUE) # by edu
table(nls$edu)

## Univariate association

# crude association plot
par(mfrow=c(1, 2))
plot(income~age, data=nls, xlab="Age (Years)", ylab="Previous Year's Income (Dollars)")
boxplot(income~age, xlab="Age (Years)", ylab="Previous Year's Income (Dollars)", data=nls)

# standard model
# crude model
par(mfrow=c(1, 1))
nls$age0 <- nls$age-20 # center data
fit1 = lm(income ~ age0, data = nls)
summary(fit1)
confint(fit1)
plot(fit1)

# adjusted model
fit2 = lm(income ~ age0 + factor(gender), data = nls) # adjust for gender
summary(fit2)
confint(fit2)

fit3 = lm(income ~ age0 + factor(edu), data = nls) # adjust for edu
summary(fit3)
confint(fit3)

fit4 = lm(income ~ age0 + factor(edu) + factor(gender), data = nls) # adjust for gender and edu
summary(fit4)
confint(fit4)
par(mfrow=c(1, 2))
plot(fit4)

## transform response variable

hist(nls$income, xlab = "Previous Year's Income (Dollars)", main = "    ")

# log transformation
fit5<-lm(log(income) ~ age0 + factor(edu) + factor(gender), data = nls) 
# Not applicable for multiples 0 values of income

# square-root transformation
s <- sqrt(nls$income)
hist(s, xlab = "Square Root of Income", main = "    ")

fit6<-lm(sqrt(income) ~ age0 + factor(edu) + factor(gender), data = nls) 
summary(fit6)
par(mfrow=c(1, 2))
plot(fit6)

# cube-root transformation
nls$c <- (nls$income)^(1/3)
hist(nls$c, xlab = "Cube Root of Income", main = "    ")

fit7<-lm(c ~ age0 + factor(edu) + factor(gender), data = nls) 
summary(fit7)
confint(fit7)
par(mfrow=c(1, 2))
plot(fit7)

## Part 2

# spline model

# number of knots
library(splines)
results = NULL
for (d in seq(1,10, by = 1) ){
  print (d)
  fit = lm (sqrt(income)~bs (age0, df = d) + factor(edu) + factor(gender), data = nls)
  H =  hatvalues (fit)
  results = rbind (results, 
                   c(d,  	AIC (fit), BIC(fit),  
                     mean((fit$fitted - sqrt(nls$income))^2 / (1-H)^2 ),
                     mean ((fit$fitted - sqrt(nls$income))^2) / (1 - mean(H))^2 ))
}

par(mfrow = c(1,2))
plot(results[,2]~results[,1], xlab = "# Knots", ylab = "AIC", main="AIC")
plot(results[,3]~results[,1], xlab = "# Knots", ylab = "BIC", main="BIC")
# error when calculate CV and GCV?

# Find knots
par(mfrow=c(1, 1))
plot(sqrt(income) ~ age, data=nls, xlab="Age (Years)", ylab="Square Root of Income")
sp1 <- (nls$age0-2)*as.numeric(nls$age0 >= 2)
sp2 <- (nls$age0-7)*as.numeric(nls$age0 >= 7)

# Final model
library(lme4)
fit10 <- lmer(sqrt(income) ~ age0 + factor(edu) + factor(gender) + sp1 + sp2 + (1|id), data = nls)
summary(fit10)
confint(fit10)
plot(fit10)
AIC(fit10)
BIC(fit10)

57.7520^2 # baseline income
49.8698292^2 # lower limit
65.638280^2 # upper limit

# 95% CI

# age 22 to 27
m <- 10.5212+1.7388
se <- sqrt(0.7885^2 + 0.9721^2 + 2*(-0.971)*0.7885*0.9721)
m - 1.96*se
m + 1.96*se

# age after 27
m <- 10.5212+1.7388-8.8239
se <- sqrt(0.7885^2 + 0.9721^2 + 0.6797^2 + 2*(-0.971)*0.7885*0.9721 +
           2*0.334*0.7885*0.6797 + 2*(-0.504)*0.9721*0.6797)
m - 1.96*se
m + 1.96*se

# use interaction terms to test if there is a particular sub-population most affected by the recession
# interaction between age and education

nls$edu1 <- as.numeric(nls$edu == "College or AD")
nls$edu2 <- as.numeric(nls$edu == "Gradute or Prof")
nls$edu3 <- as.numeric(nls$edu == "HS or GED")

fit11 <- lmer(sqrt(income) ~ age0 + factor(gender) + sp1 + sp2*factor(edu) + (1|id), data = nls)
summary(fit11)

fit11.1 <- lmer(sqrt(income) ~ age0 + edu2 + edu3 + factor(gender) + sp1 + sp2*edu1 + (1|id), data = nls)
summary(fit11.1)
anova(fit10, fit11.1)

fit11.2 <- lmer(sqrt(income) ~ age0 + edu1 + edu3 + factor(gender) + sp1 + sp2*edu2 + (1|id), data = nls)
summary(fit11.2)
anova(fit10, fit11.2)

fit11.3 <- lmer(sqrt(income) ~ age0 + edu1 + edu2 + factor(gender) + sp1 + sp2*edu3 + (1|id), data = nls)
summary(fit11.3)
confint(fit11.3)
anova(fit10, fit11.3)

# interaction between age and gender
fit12 <- lmer(sqrt(income) ~ age0 + factor(edu) + sp1 + sp2*factor(gender) + (1|id), data = nls)
summary(fit12)
anova(fit10, fit12)

# Intraclass correlation
1507/(1507+1618)