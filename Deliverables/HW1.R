#CSC-487 Homework 1
#Matthew Cronce

#Problem 1
#a
su = read.delim("Su_raw_matrix.txt")

#b
#separate the Liver_2.CEL column
l2row = as.vector(su[,8])

l2mean = mean(l2row)
l2sd = sd(l2row)

#c
su_means = colMeans(su)
su_sums = colSums(su)

#Problem 2
#a
ra = rnorm(10000, mean = 0, sd = 0.2)
hist_ra = hist(ra, plot = TRUE, xlim = c(-5,5))

#b
rb = rnorm(10000, mean = 0, sd = 0.5)
hist_rb = hist(rb, plot = TRUE, xlim = c(-5,5))

#The histogram of rb has a wider spread of values compared to ra because of
#the higher standard deviation value in rb. ra has a high concentration of
#values around the mean because of the lower standard deviation value of ra

#Problem 3
library(ggplot2)

#a
dat <- data.frame(cond = factor(rep(c("A", "B"), each=200)),
  rating = c(rnorm(200), rnorm(200, mean=.8)))

#b
# Overlaid histograms
ggplot(db, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

#c
# Interleaved histograms
ggplot(db, aes(x=rating, fill=cond)) + geom_histogram(binwidth=.5, position="dodge")

#d
# Density plots
ggplot(db, aes(x=rating, colour=cond)) + geom_density()

#e
# Density plots with semitransparent fill
ggplot(db, aes(x=rating, fill=cond)) + geom_density(alpha=.3)

#f
#diabetes is the whole csv file
diabetes <- read.delim("diabetes_train.csv", sep=',')

#db is the data frame for plotting
db <- data.frame(diabetes, rating = diabetes$mass, cond = diabetes$class)

#Problem 4
passengers = read.delim("titanic.csv", sep=',')
library(tidyr)
library(tidyverse)

#a
#This line removes rows with missing values and displays
#the summary of the data afterwards
passengers %>% drop_na() %>% summary()

#b
#This line filters passengers by only rows that have
#a "male" attribute in the Sex column
passengers %>% filter(Sex == "male")

#c
#This line orders the entries by the Fare attribute
#in descending order
passengers %>% arrange(desc(Fare))

#d
#This line makes a new attribute called FamSize and
#sets it to the value of Parch plus SibSp 
passengers %>% mutate(FamSize = Parch + SibSp)

#e
#This line takes the average off the fare for females
#and males, and totals the number of survivors for
#females and males
passengers %>% group_by(Sex) %>% summarise(meanFare = mean(Fare), numSurv = sum(Survived))

#Problem 5
sq = quantile(as.vector(diabetes$skin), probs = c(.1, .3, .5, .6))
sq
