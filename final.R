#### BIOS 526 Final
#### Xinyi Zhao  

setwd("C:/Users/zhaohexu/Dropbox/Academic/Courses/Fall 2015/BIOS 526/final")

fi <- read.csv("Final.csv")
head(fi); str(fi)
fi <- na.omit(fi)
fi <- fi[order(fi$School),]
names(fi)<-c("school","sex","age","treat","Z")

# multilevel data; outcome=Z, predictor=treat (main exposure), sex, age

###### summary statistics ######

### individual-based
summary(fi)
mean(fi$age); sd(fi$age) # age
table(fi$sex); prop.table(table(fi$sex))*100 # sex (0=boy, 1=girl)
table(fi$Z); prop.table(table(fi$Z))*100 # diarrhea
table(fi$treat); prop.table(table(fi$treat))*100 # treat

### school-based
length(unique(fi$school)) # number of unique schools
test1 <- subset(fi, subset=treat==1)
l1 <- length(unique(test1$school)) 
test2 <- subset(fi, subset=treat==0)
l2 <- length(unique(test2$school)) 
l1; l1/(l1+l2)*100 # number (%) of unique schools with the program
l2; l2/(l1+l2)*100 # number (%) of unique schools without the program

###### univariate associations ######

# Z ~ treat (main exposure)
fit1.1<-glm(Z~factor(treat),family=binomial(link="logit"), data=fi)
summary(fit1.1); exp(confint(fit1.1))
exp(-2.93209) # OR

# Z ~ sex
fit1.2<-glm(Z~factor(sex),family=binomial(link="logit"), data=fi)
summary(fit1.2); exp(confint(fit1.2))
exp(0.11953) # OR

# Z ~ age
summary(fi$age)
fi$agec <- fi$age-7 # center age
fit1.3<-glm(Z~agec,family=binomial(link="logit"), data=fi)
summary(fit1.3); exp(confint(fit1.3))
exp(-0.07509) # OR

###### multivariate associations ######

#### standard model ####

# start with the full model
# Z ~ treat + sex + age + treat*sex + treat*age
fit1.4<-glm(Z~factor(treat)*factor(sex)+factor(treat)*agec,family=binomial(link="logit"), data=fi)
summary(fit1.4)
AIC(fit1.4); BIC(fit1.4)
# both two interactions are significant --do not drop
# use this predictor sets to fit random intercept model and GEE

#### random intercept model ####

library(lme4)
# Z ~ treat + sex + age + treat*sex + treat*age
fit2.1<-glmer(Z~factor(treat)*factor(sex)+factor(treat)*agec+(1|school),family=binomial(link="logit"), data=fi)
summary(fit2.1); confint(fit2.1)

#### GEE model ####

library(gee)
# Z ~ treat + sex + age + treat*sex + treat*age
fit3.1<-gee(Z~factor(treat)*factor(sex)+factor(treat)*agec,id=school,family=binomial(link="logit"), data=fi,corstr="unstructured")
# the model is invalid due to divergence of estimates

### try other models --

# drop treat*sex
# Z ~ treat + sex + age + treat*age
fit3.2<-gee(Z~factor(treat)*agec+factor(sex),id=school,family=binomial(link="logit"), data=fi,corstr="unstructured")
# divergence --invalid

# drop treat*age
# Z ~ treat + sex + age + treat*sex
fit3.3<-gee(Z~factor(treat)*factor(sex)+agec,id=school,family=binomial(link="logit"), data=fi,corstr="unstructured")
summary(fit3.3)
# convergence --valid
# interaction significant, but age non-significant --drop age

# Z ~ treat + sex + treat*sex
fit3.4<-gee(Z~factor(treat)*factor(sex),id=school,family=binomial(link="logit"), data=fi,corstr="unstructured")
# divergence --invalid
# cannot drop age

# so the final model is: Z ~ treat + sex + age + treat*sex
gf<-gee(Z~factor(treat)*factor(sex)+agec,id=school,family=binomial(link="logit"), data=fi,corstr="unstructured")
summary(gf)

## 95% CI

# treat
exp(-2.37945718) # OR
exp(-2.37945718-1.96*0.2253457) # lower limit
exp(-2.37945718+1.96*0.2253457) # upper limit

# sex
exp(0.34785130) # OR
exp(0.34785130-1.96*0.1356297) # lower limit
exp(0.34785130+1.96*0.1356297) # upper limit

# age
exp(-0.07524955) # OR
exp(-0.07524955-1.96*0.0473460) # lower limit
exp(-0.07524955+1.96*0.0473460) # lower limit

# treat*sex
exp(-1.21626551) # OR
exp(-1.21626551-1.96*0.5390791) # lower limit
exp(-1.21626551+1.96*0.5390791) # lower limit

# OR for girl
exp(-2.37945718-1.21626551)

#### random intercept model (refit) ####

# Z ~ treat + sex + age + treat*sex
rf<-glmer(Z~factor(treat)*factor(sex)+agec+(1|school),family=binomial(link="logit"), data=fi)
summary(rf)

## 95% CI

# treat
exp(-2.56108) # OR
exp(-2.56108-1.96*0.25861) # lower limit
exp(-2.56108+1.96*0.25861) # upper limit

# sex
exp(0.19338) # OR
exp(0.19338-1.96*0.10475) # lower limit
exp(0.19338+1.96*0.10475) # upper limit

# age
exp(-0.08020) # OR
exp(-0.08020-1.96*0.02963) # lower limit
exp(-0.08020+1.96*0.02963) # upper limit

# treat*sex
exp(-1.02586)
exp(-1.02586-1.96*0.41458) # lower limit
exp(-1.02586+1.96*0.41458) # upper limit

# OR for girl
exp(-2.56108-1.02586) # OR
se <- sqrt(0.25861^2 + 0.41458^2 + 2*0.25861*0.41458*(-0.507))
exp(-2.56108-1.02586 - 1.96*se) # lower limit
exp(-2.56108-1.02586 + 1.96*se) # upper limit