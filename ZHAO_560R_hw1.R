####
####
# Xinyi Zhao, MPH Candidate
# Homework 1
# BIOS 560R
####
####


### Part 1. Data exploration and summary

## Question a

# (1) Set the working directory

##setwd("D:/Courses/Spring 2015/BIOS 560R")

# (2) Read the data

fev <- read.csv("FEV.csv", stringsAsFactors = FALSE)
head(fev)

## Question b

# Create an R vector info_fev

info_fev <- c(nrow(fev), ncol(fev))
info_fev

## Question c

#(1) Create column vectors of the data frame

variable <- c("age", "fev", "height")
variable

number_observation <- as.integer(c(length(fev$age), length(fev$fev), length(fev$height)))
number_observation
class(number_observation)

minimum <- round(c(min(fev$age, na.rm = TRUE), 
                   min(fev$fev, na.rm = TRUE), 
                   min(fev$height, na.rm = TRUE)), digits=2)
minimum

maximum <- round(c(max(fev$age, na.rm = TRUE), 
                   max(fev$fev, na.rm = TRUE), 
                   max(fev$height, na.rm = TRUE)), digits=2)
maximum

mean <- round(c(mean(fev$age, na.rm = TRUE), 
                mean(fev$fev, na.rm = TRUE), 
                mean(fev$height, na.rm = TRUE)), digits=2)
mean

standard_deviation <- round(c(sd(fev$age, na.rm = TRUE), 
                              sd(fev$fev, na.rm = TRUE), 
                              sd(fev$height, na.rm = TRUE)), digits=2)
standard_deviation

number_missing <- as.integer(c(length(which(is.na(fev$age))), 
                               length(which(is.na(fev$fev))), 
                               length(which(is.na(fev$height)))))
number_missing
class(number_missing)

#(2) Use the column vectors to create the data frame

summ_df <- data.frame(variable, number_observation, minimum, maximum, 
                      mean, standard_deviation, number_missing)
summ_df

## Question d

# Create a table tab_sex_smoke

tab_sex_smoke <- round(prop.table(table(fev$sex,fev$smoke), 1)*100, digits=1)
tab_sex_smoke

## Question e

#(1) Create a new variable age_group

age_group <- vector(length = length(fev$age))

age_g13 <- which(fev$age > 13)
age_9_13 <- which(fev$age >= 9 & fev$age <= 13)
age_l9 <- which(fev$age < 9)

age_group[age_g13] <- "Over 13"
age_group[age_9_13] <- "9-13"
age_group[age_l9] <- "Under 9"

head(age_group)

#(2) Add the new variable to FEV dataframe

fev$age_group <- age_group
head(fev)

#(3) Create cells, row names and column names for the matrix

rnames <- c("Under 9", "9-13", "Over 13")
cnames <- c("smoker", "nonsmoker")
cells <- round(c(mean(fev$fev[which(fev$age_group == "Under 9" & 
                                    fev$smoke == "current smoker")], na.rm = T),
                 mean(fev$fev[which(fev$age_group == "Under 9" & 
                                    fev$smoke == "non-current smoker")], na.rm = T),
                 mean(fev$fev[which(fev$age_group == "9-13" & 
                                    fev$smoke == "current smoker")], na.rm = T),
                 mean(fev$fev[which(fev$age_group == "9-13" & 
                                    fev$smoke == "non-current smoker")], na.rm = T),
                 mean(fev$fev[which(fev$age_group == "Over 13" & 
                                    fev$smoke == "current smoker")], na.rm = T),
                 mean(fev$fev[which(fev$age_group == "Over 13" & 
                                    fev$smoke == "non-current smoker")], na.rm = T)), digits=2)

#(4) Use the created cells and names to create the matrix

mean_fev <- matrix(cells, nrow = 3, ncol = 2, byrow=TRUE, dimnames=list(rnames, cnames))
mean_fev

## Question f

#(1) Create a dataframe of only female current smokers with FEV < 2.5

fev_smoke0 <- fev[fev$sex == "female" & fev$fev < 2.5 & fev$smoke == "current smoker", ]
fev_smoke0

#(2) Order the dataframe to get the fev_smoke that we need

fev_smoke <- fev_smoke0[order(fev_smoke0$age, fev_smoke0$height),]
fev_smoke


### Part 2. Plotting

# Combine the following plots

layout(matrix(c(1, 2, 3, 3), 2, 2, byrow=TRUE))

## Plot 1

#(1) Change the shape and size of the plot

par(pin=c(2.4, 2.4))

#(2) Create a vector for the color

col_sex <- vector(length = length(fev$sex))
col_sex[fev$sex == "female"] <- "red"
col_sex[fev$sex == "male"] <- "blue"

#(3) Create plot

plot(x = fev$age, y = fev$fev, xlab = "Age (years)", ylab = "FEV (liters)", 
     pch=19, cex = 1.2, main = "Changes in FEV with age", col = col_sex) 

#(4) Add lowess lines

lowess_fev_male <- lowess(x = fev$age[which(fev$sex=="male")], 
                          y = fev$fev[which(fev$sex=="male")])

lines(x = lowess_fev_male$x, y = lowess_fev_male$y, col = "blue")

lowess_fev_female <- lowess(x = fev$age[which(fev$sex=="female")], 
                            y = fev$fev[which(fev$sex=="female")])

lines(x = lowess_fev_female$x, y = lowess_fev_female$y, col = "red")

#(5) Add legend

legend("topleft", legend = c("female", "male"), pch = 19, 
       cex = 1.2, col=c("red", "blue"), bg="transparent")


## Plot 2

#(1) Make the order of age groups the same as that of the example plot

fev$age_group <- factor(fev$age_group, levels=c("Under 9", "9-13", "Over 13"))

#(2) Get the proportion of smokers in each age group

test <- table(fev$age_group, fev$smoke)
test

res <- test[, 1] / (test[, 1] + test[, 2])
res

#(3) Change the shape and size of the plot

par(pin=c(2.4, 2.4))

#(4) Create the bar plot

barplot(res, col="green", ylim=c(0,1), axes=F, main="Smokers by age category",
        xlab="Age (years)", ylab="Proportion current smokers", las = 2)

#(5) Change the scale and range of y-axis

axis(side=2, at=c(0,0.5,1))


## Plot 3

#(1) Change the shape and size of the plot
par(pin=c(5.9, 2.4))

#(2) Create a vector for the color

col_age <- vector(length = length(fev$age_group))
col_age[fev$age_group == "Over 13"] <- "green"
col_age[fev$age_group == "9-13"] <- "red"
col_age[fev$age_group == "Under 9"] <- "black"

#(3) Create a vector for the symbol

pch_age <- vector(length = length(fev$age_group))
pch_age[fev$age_group == "Over 13"] <- 3
pch_age[fev$age_group == "9-13"] <- 2
pch_age[fev$age_group == "Under 9"] <- 1

#(4) Create the plot

plot(x = fev$height, y = fev$fev, xlab = "Height (inches)", ylab = "FEV (liters)", 
     pch=pch_age, col = col_age, ylim=c(0, 8), 
     main = "Changes in FEV with height by age category") 

#(5) Add horizontal lines for the means of each group

abline(h = mean(fev$fev[which(fev$age_group == "Under 9")]), 
       col = "black", lty=2)
abline(h = mean(fev$fev[which(fev$age_group == "9-13")]), 
       col = "red", lty=2)
abline(h = mean(fev$fev[which(fev$age_group == "Over 13")]), 
       col = "green", lty=2)

#(6) Add legends

legend("topleft", legend = c("Under 9", "9-13", "Over 13"), title = "Age category", 
        pch = c(1, 2, 3), col=c("black", "red", "green"), bg="transparent", bty="n")

#(7) Add text for the lines

mtext("Mean FEV over 13",las=1, line=0.5,col="green", side=4, 
      at=mean(fev$fev[which(fev$age_group == "Over 13")]))
mtext("Mean FEV 9 - 13",las=1, line=0.5,col="red", side=4, 
      at=mean(fev$fev[which(fev$age_group == "9-13")]))
mtext("Mean FEV under 9",las=1, line=0.5,col="black", side=4, 
      at=mean(fev$fev[which(fev$age_group == "Under 9")]))




### Bonus

layout(matrix(1, 1, byrow=TRUE))

# Create a vector for the color

col_age <- vector(length = length(fev$age_group))
col_age[fev$age_group == "Over 13"] <- "green"
col_age[fev$age_group == "9-13"] <- "red"
col_age[fev$age_group == "Under 9"] <- "black"

# Create a vector for the symbol

pch_age <- vector(length = length(fev$age_group))
pch_age[fev$age_group == "Over 13"] <- 3
pch_age[fev$age_group == "9-13"] <- 2
pch_age[fev$age_group == "Under 9"] <- 1

# Create the plot

plot(x = fev$height, y = fev$fev, xlab = "Height (inches)", ylab = "FEV (liters)", 
     pch=pch_age, col = col_age, ylim=c(0, 8), 
     main = "Changes in FEV with height by age category") 

# Add segments for min and max height of each age group

segments(min(fev$height[which(fev$age_group == "Under 9")]), 2,
         min(fev$height[which(fev$age_group == "Under 9")]), 6, col = "black", lty=2)

segments(min(fev$height[which(fev$age_group == "9-13")]), 2,
         min(fev$height[which(fev$age_group == "9-13")]), 6, col = "red", lty=2)

segments(min(fev$height[which(fev$age_group == "Over 13")]), 2,
         min(fev$height[which(fev$age_group == "Over 13")]), 6, col = "green", lty=2)

segments(max(fev$height[which(fev$age_group == "Under 9")]), 2,
         max(fev$height[which(fev$age_group == "Under 9")]), 6, col = "black", lty=2)

segments(max(fev$height[which(fev$age_group == "9-13")]), 2,
         max(fev$height[which(fev$age_group == "9-13")]), 6, col = "red", lty=2)

segments(max(fev$height[which(fev$age_group == "Over 13")]), 2,
         max(fev$height[which(fev$age_group == "Over 13")]), 6, col = "green", lty=2)

# Add legends

legend("topleft", legend = c("Under 9", "9-13", "Over 13"), title = "Age category", 
        pch = c(1, 2, 3), col=c("black", "red", "green"), bg="transparent", bty="n")
