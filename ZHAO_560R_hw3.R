####
####
# Xinyi Zhao, MPH Candidate
# Homework 3
# BIOS 560R
####
####



#### Part 1. Exploratory data analysis


# Set the working directory

#setwd("H:/Spring 2015/R/homework")

# Read the data

support <- read.csv("support2.csv", stringsAsFactors = FALSE)
head(support)

# Install packages

install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("gridExtra")
install.packages("RColorBrewer")
install.packages("reshape2")
install.packages("plyr")
install.packages("Rcpp")

# Load the package

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(RColorBrewer)
library(reshape2)
library(plyr)
library(Rcpp)

# Create a new variable log(charges)

support$log_charges <- log(support$charges)

# Plot the data

sct <- ggplot(support, aes(x=age, y=log_charges)) + 
       # Add scatter point
       geom_point(size=3.5, shape=20) + 
       # Add axis labels
       ylab("Charges (log dollars)") + xlab("Age (years)") + 
       # Add title
       ggtitle("Association between hospital charges and age") +
       # Set the fontsize of text
       theme(axis.title = element_text(size = 15.5)) + 
       theme(plot.title = element_text(size = 19)) + 
       theme(axis.text = element_text(size = 13)) +
       # Add a smooth line
       stat_smooth(mapping = NULL, data = NULL, geom = "smooth", position = "identity", 
                 method="auto", formula = y ~ x, se = TRUE, fullrange = FALSE, 
                 level = 0.95, na.rm = FALSE, size=1)
sct




#### Part 2. Estimating association with in-hospital death


### 1. Univariate logistic regression


## Question a --Build simple logistic model

# (1) Association between in-hospital death and age

glm_age <- glm(hospdead ~ age, data = support, family = "binomial")
glm_age
summary(glm_age)$coef

# (2) Association between in-hospital death and sex

glm_sex <- glm(hospdead ~ sex, data = support, family = "binomial")
glm_sex
summary(glm_sex)$coef

# (3) Association between in-hospital death and coma score

glm_scoma <- glm(hospdead ~ scoma, data = support, family = "binomial")
glm_scoma
summary(glm_scoma)$coef

# (4) Association between in-hospital death and number of comorbidities

glm_nco <- glm(hospdead ~ num.co, data = support, family = "binomial")
glm_nco
summary(glm_nco)$coef

# (5) Association between in-hospital death and diabetes status

glm_dia <- glm(hospdead ~ diabetes, data = support, family = "binomial")
glm_dia
summary(glm_dia)$coef

# (6) Association between in-hospital death and years of education

glm_edu <- glm(hospdead ~ edu, data = support, family = "binomial")
glm_edu
summary(glm_edu)$coef


## Question b --Extract odds ratio and confidence intervals

# (1) Association between in-hospital death and age

# Point Estimate of OR
OR_age <- exp(glm_age$coef[2])
OR_age

# 95% Confidence interval of OR
con_age <- confint(glm_age)
OR_age_CI <- exp(con_age[2, ])
OR_age_CI

# (2) Association between in-hospital death and sex

# Point Estimate of OR
OR_sex <- exp(glm_sex$coef[2])
OR_sex

# 95% Confidence interval of OR
con_sex <- confint(glm_sex)
OR_sex_CI <- exp(con_sex[2, ])
OR_sex_CI

# (3) Association between in-hospital death and coma score

# Point Estimate of OR
OR_scoma <- exp(glm_scoma$coef[2])
OR_scoma

# 95% Confidence interval of OR
con_scoma <- confint(glm_scoma)
OR_scoma_CI <- exp(con_scoma[2, ])
OR_scoma_CI

# (4) Association between in-hospital death and number of comorbidities

# Point Estimate of OR
OR_nco <- exp(glm_nco$coef[2])
OR_nco

# 95% Confidence interval of OR
con_nco <- confint(glm_nco)
OR_nco_CI <- exp(con_nco[2, ])
OR_nco_CI

# (5) Association between in-hospital death and diabetes status

# Point Estimate of OR
OR_dia <- exp(glm_dia$coef[2])
OR_dia

# 95% Confidence interval of OR
con_dia <- confint(glm_dia)
OR_dia_CI <- exp(con_dia[2, ])
OR_dia_CI

# (6) Association between in-hospital death and years of education

# Point Estimate of OR
OR_edu <- exp(glm_edu$coef[2])
OR_edu

# 95% Confidence interval of OR
con_edu <- confint(glm_edu)
OR_edu_CI <- exp(con_edu[2, ])
OR_edu_CI



### 2. Multivariate logistic regression


## Question a --Build multiple logistic model

glm_multi <- glm(hospdead ~ age + sex + scoma + num.co + diabetes + edu, data = support, family = "binomial")
glm_multi
summary(glm_multi)$coef


## Question b --Extract odds ratio and confidence intervals


# (1) Association between in-hospital death and age, adjusted for sex, coma score, 
# number of comorbidities, diabetes status and years of education

# Point Estimate of OR
OR_multi_age <- exp(glm_multi$coef[2])
OR_multi_age

# 95% Confidence interval of OR
OR_multi_CI <- exp(confint(glm_multi))
OR_multi_CI_age <- OR_multi_CI[2, ]
OR_multi_CI_age


# (2) Association between in-hospital death and sex, adjusted for age, coma score, 
# number of comorbidities, diabetes status and years of education

# Point Estimate of OR
OR_multi_sex <- exp(glm_multi$coef[3])
OR_multi_sex

# 95% Confidence interval of OR
OR_multi_CI_sex <- OR_multi_CI[3, ]
OR_multi_CI_sex


# (3) Association between in-hospital death and coma score, adjusted for age, sex, 
# number of comorbidities, diabetes status and years of education

# Point Estimate of OR
OR_multi_scoma <- exp(glm_multi$coef[4])
OR_multi_scoma

# 95% Confidence interval of OR
OR_multi_CI_scoma <- OR_multi_CI[4, ]
OR_multi_CI_scoma


# (4) Association between in-hospital death and number of comorbidities, adjusted for
#age, sex, coma score, diabetes status and years of education

# Point Estimate of OR
OR_multi_nco <- exp(glm_multi$coef[5])
OR_multi_nco

# 95% Confidence interval of OR
OR_multi_CI_nco <- OR_multi_CI[5, ]
OR_multi_CI_nco


# (5) Association between in-hospital death and diabetes status, adjusted for
#age, sex, coma score, number of comorbidities and years of education

# Point Estimate of OR
OR_multi_dia <- exp(glm_multi$coef[6])
OR_multi_dia

# 95% Confidence interval of OR
OR_multi_CI_dia <- OR_multi_CI[6, ]
OR_multi_CI_dia


# (6) Association between in-hospital death and years of education, adjusted for
#age, sex, coma score, number of comorbidities and diabetes status

# Point Estimate of OR
OR_multi_edu <- exp(glm_multi$coef[7])
OR_multi_edu

# 95% Confidence interval of OR
OR_multi_CI_edu <- OR_multi_CI[7, ]
OR_multi_CI_edu



### 3. Combine the results to a dataframe


# Create a matrix with column names 

death_OR <- matrix(nrow = 12, ncol = 5)
colnames(death_OR) <- c("Var", "OR", "LB", "UB", "Type")

# Fill in relevent information for each column

death_OR[, "Var"] <- c("Age", "Age", "Male sex", "Male sex", "Coma score", "Coma score", 
                       "Comorbidities", "Comorbidities", "Diabetes", "Diabetes", "Education", "Education")

death_OR[, "OR"] <- c(OR_age, OR_multi_age, OR_sex, OR_multi_sex, OR_scoma, OR_multi_scoma, 
                             OR_nco, OR_multi_nco, OR_dia, OR_multi_dia, OR_edu, OR_multi_edu)

death_OR[, "LB"] <- c(OR_age_CI[1], OR_multi_CI_age[1], OR_sex_CI[1], OR_multi_CI_sex[1], 
                                     OR_scoma_CI[1], OR_multi_CI_scoma[1], OR_nco_CI[1], OR_multi_CI_nco[1], 
                                     OR_dia_CI[1], OR_multi_CI_dia[1], OR_edu_CI[1], OR_multi_CI_edu[1])

death_OR[, "UB"] <- c(OR_age_CI[2], OR_multi_CI_age[2], OR_sex_CI[2], OR_multi_CI_sex[2], 
                                     OR_scoma_CI[2], OR_multi_CI_scoma[2], OR_nco_CI[2], OR_multi_CI_nco[2], 
                                     OR_dia_CI[2], OR_multi_CI_dia[2], OR_edu_CI[2], OR_multi_CI_edu[2])

death_OR[, "Type"] <- c("Univariate", "Multivariate", "Univariate", "Multivariate", 
                        "Univariate", "Multivariate", "Univariate", "Multivariate", 
                        "Univariate", "Multivariate", "Univariate", "Multivariate")

# Change the type to data frame

death_OR <- data.frame(death_OR)
death_OR

# Verify the class of death_OR

class(death_OR)




#### Part 3. Plotting results


# Create a color vector

cols <- c("slateblue", "darkred")


# Create a theme for the plot
              
              # Set the fontsize of the strip
theme_plot <- theme(strip.text = element_text(size = 18)) +
              # Set the background of the strip
              theme(strip.background = element_rect(size = 0, colour="grey50")) +
              # Remove the legend
              theme(legend.position = "none") + 
              # Remove the title of x-axis
              theme(axis.title.x = element_blank()) + 
              # Set the grid line
              theme(panel.grid.major = element_line(colour = "grey85", size=0)) + 
              theme(panel.grid.minor.y = element_line(colour = "grey95", size=1)) +
              # Set the text of axis
              theme(axis.text.x = element_text(size = 18, color="black", angle = 20, hjust = 1, vjust = 1)) +
              theme(axis.text.y = element_text(size = 18, color="black")) +
              # Set the panel background
              theme(panel.background = element_rect(fill = 'white', colour="grey30")) +
              # Set the axis ticks
              theme(axis.ticks = element_line(size = 1, colour="black")) +
              theme(axis.ticks.length = unit(0.25, "cm")) +
              # Set the axis line
              theme(axis.line = element_line(size = 0.5, colour="grey30")) +
              # Set the fontsize of the title
              theme(plot.title = element_text(size = 18)) +
              # Set the fontsize and color of y-axis label
              theme(axis.title.y = element_text(size = 18, color="black"))


# Transform the OR's and their CI's into numeric data

death_OR$OR <- as.numeric(as.vector(death_OR$OR))
death_OR$LB <- as.numeric(as.vector(death_OR$LB))
death_OR$UB <- as.numeric(as.vector(death_OR$UB))


# Make Type and Var variables into factors to rearrange their orders in the plot

death_OR$Type <- factor(death_OR$Type, levels=c("Univariate", "Multivariate"))
death_OR$Var <- factor(death_OR$Var, levels=c("Age", "Male sex", "Education", "Coma score", "Comorbidities", "Diabetes"))


# Create the plot

plot_OR <- ggplot(death_OR, aes(y=OR, x=Type, colour=Type)) +
           # Add points for OR's
           geom_point(size=3.5) + 
           # Plot by facet
           facet_wrap(~Var, ncol=2, scale="free") +
           # Add bars for confidence intervals
           geom_errorbar(aes(ymin=LB, ymax=UB), width=0, size=1) +
           # Set the color of the bars
           scale_color_manual(values=cols) + 
           # Add a label for y-axis
           ylab("Odds ratio") + 
           # Add a horizontal line
           geom_hline(aes(yintercept=1), color="gray60",linetype="dashed", size=1)+
           # Add a title
           ggtitle("Covariates associated with in-hospital death") + 
           # Apply the theme to this plot
           theme_plot 

plot_OR




#### Bonus


### Plot a histogram for the distribution of age among males and females


# Get the mean of each sex group

mspt <- ddply(support, "sex", summarise, age.mean=mean(age))
mspt

# Create a theme for the plot

           # Set the grid line
theme_a <- theme(panel.grid.major = element_line(colour = "white", size=0, linetype="dashed")) + 
           theme(panel.grid.minor = element_line(colour = "grey80", size=0, linetype="dotted")) +
           # Set the text of axis
           theme(axis.text.x = element_text(size = 12, color="black", angle = 30, hjust = 1, vjust = 1)) +
           theme(axis.text.y = element_text(size = 12, color="black")) +
           # Set the panel background
           theme(panel.background = element_rect(fill = 'grey20', colour="grey90")) +
           # Set the axis ticks
           theme(axis.ticks = element_line(size = 1, colour="black")) +
           theme(axis.ticks.length = unit(0.3, "cm")) +
           # Set the axis line
           theme(axis.line = element_line(size = 2.5, colour="grey")) +
           # Set the fontsize of the title
           theme(plot.title = element_text(size = 17, face = "italic")) +
           # Set the fontsize and color of y-axis label
           theme(axis.title = element_text(size = 15, color="black")) +
           # Set the legend
           theme(legend.background = element_rect(colour = "grey20")) +
           theme(legend.key = element_rect(colour = "grey80")) +
           theme(legend.key = element_rect(fill = "yellow")) +
           # Set the color of plot background
           theme(plot.background = element_rect(fill = "grey80"))

## Plot a histogram for the distribution of age among males and females

plot_a <- ggplot(support, aes(x = age, fill=sex)) + 
          # Create interleaved histograms by group
          geom_histogram(binwidth=2, colour="grey30", position="dodge") +
          # Plot vertical lines for means of each group
          geom_vline(data=mspt, aes(xintercept=age.mean), size=1, color="yellow", linetype="dashed") +
          # Add a title
          ggtitle("Distribution of age among sex, with means") +
          # Apply the theme to the plot
          theme_a
plot_a
