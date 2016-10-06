# Lecture 5

# Read in data
# Data: http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/vlbw.html
vlbw <- read.csv("vlbw.csv", stringsAsFactors = F)
# Subset data to be more manageable for class
vlbw <- vlbw[complete.cases(vlbw), c("bwt", "gest", "sex", "twn", "pneumo")]


#####################
# One sample t-tests
#####################
# Graphically display age:
hist(vlbw$gest, xlab = "Gestational age", main = "Histogram of 
  gestational age")

# Perform a t-test for
# H0: mu = 39
# H1: mu != 39
# at alpha = 0.05
t_age <- t.test(x = vlbw$gest, mu = 39)
#Print results
t_age
# Check names
names(t_age)
# Extract p-value
pval <- t_age$p.value


#####################
# Two sample t-tests
#####################
# Does mean gestational age differ between male and female low birthweight infants?
# First, subset gestation by male and female
age_female <- vlbw$gest[vlbw$sex == "female"]
age_male <- vlbw$gest[vlbw$sex == "male"]
# Perform two sample t-test for
# H0: mu1 = mu2
# H1: mu1 != mu2
t_agesex <- t.test(age_female, age_male, alternative = "less")


# In-class question:
# Perform a t-test for
# H0: mu1 = mu2
# H1: mu1 < mu2


#####################
# One sample test of proportion
#####################
# First make a table of values
table_pneumo <- table(vlbw$pneumo)
table_pneumo
table_pneumo <- matrix(c(127, 518), ncol = 2)

#Is the proportion of pneumothorax in low birthweight infants different than 
# 6.3%?
prop.test(table_pneumo, p = 0.063)


#####################
# Two sample test of proportion
#####################
# Get a table of twin status by pneumothorax
table_pneumo <- table(twin = vlbw$twn, pneumo = vlbw$pneumo)
table_pneumo
# Reformat results
table_pneumo <- matrix(c(6, 17, 36, 115), ncol = 2)
colnames(table_pneumo) <- c("Pneumo", "No pneumo")
rownames(table_pneumo) <- c("Not twin", "Twin")
# Test whether the proportion of pneumothorax differs between twins and singleton
  #births
prop.test(table_pneumo)


#####################
# Chi-squared test
#####################
# Is sex associated with being a twin in low birthweight infants?
chsq_surgery <- chisq.test(vlbw$sex, vlbw$twn)
names(chsq_surgery)

# In-class question:
# Extract out the p-value from the chi-squared test
chsq_surgery$p.value


#####################
# Relative risk/ risk ratio and odds ratio
#####################
install.packages("epitools")
library(epitools)
epitab(table_pneumo, method = "riskratio")
epi_pneumo <- epitab(table_pneumo, method = "oddsratio")
epi_pneumo

# Get the result table
names(epi_pneumo)
epi_pneumo_out <- epi_pneumo$tab
colnames(epi_pneumo_out)

#Extract the odds ratio and confidence interval
epi_pneumo_out[2,5]


#####################
# Power calculations
#####################
# Compute power when 
# - The difference in means is 0.1
# - The standard deviation is 1
# - We want 90% power
power.t.test(delta = 0.1, power = 0.9, type = "two.sample", alternative = 
  "two.sided")


#####################
# Survival data
#####################
library(survival)
hunger <- read.csv("Hunger Games survival analysis data set - Sheet1.csv",
                   stringsAsFactors = F)
surv_hunger <- Surv(time = hunger$survival_days,event = rep(1, nrow(hunger)))
plot(survfit(surv_hunger ~ hunger$career),
     main = "74th annual Hunger Games - survival estimates",
     xlab = "Days", ylab = "Proportion surviving", col = c(1, 2))
legend(c("topright"), legend = c("Career tribute", "Not career tribute"),
       col= c(1, 2), lty = 1)
