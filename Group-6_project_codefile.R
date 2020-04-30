install.packages('rcompanion')
install.packages('party')
install.packages('rpart.plot')
install.packages('rpart')
install.packages("corrplot")
library(corrplot)
library(rcompanion)
library(dbplyr)
library(party)
library(desc)
library(dplyr)
library(e1071)
library(fitdistrplus)
library(ggplot2)
library(plyr)
library(prob)
library(reshape2)
library(tidyverse)
library(tidyselect)
library(rpart)
library(rpart.plot)
library(party)

#used to set working directory
setwd("E:/northeastern university/courses/prob and stats/project")

#____________________________________________________________________________________________________
#Used to omit null value data from the dataset and save it as a new file

#credit <- read.csv("credit_card.csv", header = TRUE, sep = ',')
#fltr_data <- na.omit(credit)
#write.csv(fltr_data, 'final_data.csv')
#____________________________________________________________________________________________________

#To read data
fltr_data <- read.csv("final_data.csv")

#To transform LIMIT BALANCE attribute
fltr_data <- mutate(fltr_data, LIMIT_BAL=(LIMIT_BAL)/100)
fltr_data1 <- mutate(fltr_data, SEX=as.character(SEX), MARRIAGE=as.character(MARRIAGE))
head(fltr_data)

#To plot histogram of LIMIT BALANCE
ggplot(fltr_data, aes(LIMIT_BAL)) +
  geom_histogram(bins = 50, color = 'Black', fill = 'steelblue')

#To normalize LIMIT BALANCE attribute and plot NORMAL HISTOGRAM
LIMIT_BAL <- log(fltr_data$LIMIT_BAL)
plotNormalHistogram(LIMIT_BAL, xlab='limit balance')
skewness <- skewness(LIMIT_BAL)
skewness
kurtosis <- kurtosis(LIMIT_BAL)
kurtosis
summary(LIMIT_BAL)
#########################################################################################
#To understand the conversion of categorical data into numerical data
# Gender (1 = male; 2 = female).
# Education (1 = graduate school; 2 = university; 3 = high school; 4 = others).
# Marital status (1 = married; 2 = single; 3 = others).
#########################################################################################


#selecting required columns
fltr_data2 <- dplyr::select(fltr_data1, SEX, AGE, EDUCATION, MARRIAGE, LIMIT_BAL, defaulters)
head(fltr_data2)

fltr_data_n <- dplyr::select(fltr_data, SEX, AGE, EDUCATION, MARRIAGE, LIMIT_BAL, defaulters)


#filtering data entries with defaulters
defaulter_data <- dplyr::filter(fltr_data2, defaulters ==1)
head(defaulter_data)

#Categorizing data based on gender, education and maritial status

#Category 1 :: male , graduate or university, single or other
category_1 <- dplyr::filter(fltr_data2, SEX ==1)
category_1 <- dplyr::filter(category_1, EDUCATION == 1 | EDUCATION ==2)
category_1 <- dplyr::filter(category_1, MARRIAGE == 2 | MARRIAGE ==3)
category_1D <- dplyr::filter(category_1, defaulters==1)

#Category 2 :: female , graduate or university, single or other
category_2 <- dplyr::filter(fltr_data2, SEX ==2)
category_2 <- dplyr::filter(category_2, EDUCATION == 1 | EDUCATION ==2)
category_2 <- dplyr::filter(category_2, MARRIAGE == 2 | MARRIAGE ==3)
category_2D <- dplyr::filter(category_2, defaulters==1)

#Category 3 :: male , high school and others, single or other
category_3 <- dplyr::filter(fltr_data2, SEX ==1)
category_3 <- dplyr::filter(category_3, EDUCATION == 3 | EDUCATION ==4)
category_3 <- dplyr::filter(category_3, MARRIAGE == 2 | MARRIAGE ==3)
category_3D <- dplyr::filter(category_3, defaulters==1)

#Category 4 :: female , high school and others, single or other
category_4 <- dplyr::filter(fltr_data2, SEX ==2)
category_4 <- dplyr::filter(category_4, EDUCATION == 3 | EDUCATION ==4)
category_4 <- dplyr::filter(category_4, MARRIAGE == 2 | MARRIAGE ==3)
category_4D <- dplyr::filter(category_4, defaulters==1)

#Category 5 :: male , graduate or university, married
category_5 <- dplyr::filter(fltr_data2, SEX ==1)
category_5 <- dplyr::filter(category_5, EDUCATION == 1 | EDUCATION ==2)
category_5 <- dplyr::filter(category_5, MARRIAGE == 1)
category_5D <- dplyr::filter(category_5, defaulters==1)

#Category 6 :: female , graduate or university, married
category_6 <- dplyr::filter(fltr_data2, SEX ==2)
category_6 <- dplyr::filter(category_6, EDUCATION == 1 | EDUCATION ==2)
category_6 <- dplyr::filter(category_6, MARRIAGE == 1)
category_6D <- dplyr::filter(category_6, defaulters==1)

#Category 7 :: male , high school and others, married
category_7 <- dplyr::filter(fltr_data2, SEX ==1)
category_7 <- dplyr::filter(category_7, EDUCATION == 3 | EDUCATION ==4)
category_7 <- dplyr::filter(category_7, MARRIAGE == 1)
category_7D <- dplyr::filter(category_7, defaulters==1)

#Category 8 :: female , high school and others, married
category_8 <- dplyr::filter(fltr_data2, SEX ==2)
category_8 <- dplyr::filter(category_8, EDUCATION == 3 | EDUCATION ==4)
category_8 <- dplyr::filter(category_8, MARRIAGE == 1)
category_8D <- dplyr::filter(category_8, defaulters==1)

#To create a table with total people in each category with their respective number of defaulters along with their probabilities

#to create column with total count
category_count <- c(nrow(category_1), nrow(category_2), nrow(category_3), nrow(category_4),nrow(category_5), nrow(category_6), nrow(category_7),nrow(category_8))
category_count <- melt(category_count)

#to create column wth defaulter count
category_defaulter_count <- c(nrow(category_1D), nrow(category_2D), nrow(category_3D), nrow(category_4D),nrow(category_5D), nrow(category_6D), nrow(category_7D),nrow(category_8D))
category_defaulter_count <- melt(category_defaulter_count)

#To rename column headers
colnames(category_count) <- c('Total')
colnames(category_defaulter_count) <- c('defaulters')

#to create category column
category <- c('category 1', 'category 2', 'category 3','category 4', 'category 5', 'category 6', 'category 7', 'category 8')

#To bind all the columns together
category_count <- cbind( category, category_count, category_defaulter_count)
category_count <- transform(category_count, Probabilty = defaulters /Total)
category_count
#___________________________visualization section_______________________________

#the primary aim of the histogram is to depict the number of defaulters and the total number of people in each category 

#to create histogram of categorical data obtained
clus.col1 <- do.call(rbind, category_count)
barplot(clus.col1 ,beside = TRUE,
        main="Grouped Column Chart" ,
        legend.text = c("Defaulters", 'Total'),
        names.arg = category,
        col = c(' steel blue', 'black'))

Hist_cat_def <- ggplot(data = category_count) + 
  geom_col(aes(x =category, y= Probabilty), fill= 'black')
Hist_cat_def

#cullen and frey graph
descdist(LIMIT_BAL)

#Goodness of fit test
fit_ln <- fitdist(fltr_data$LIMIT_BAL, "lnorm")
fit_exp <- fitdist(fltr_data$LIMIT_BAL, "exp")
plot.legend <- c("norm")
fit_ln <- fitdist(LIMIT_BAL, "norm")
summary(fit_ln)


##density plot
denscomp(list(fit_ln), legendtext = plot.legend, xlab = 'Limit Balance', xlegend = 'topleft')

## cdf plot
cdfcomp (list(fit_ln), legendtext = plot.legend, xlab = 'Limit Balance')

##qq-plot
qqnorm(fltr_data$LIMIT_BAL)

#pp- plot
m <- mean(fltr_data$LIMIT_BAL)
s <- sd(fltr_data$LIMIT_BAL)
n <- nrow(fltr_data)
p <- (1 : n) / n - 0.5 / n
ggplot(fltr_data) + geom_point(aes(x = p, y = sort(pnorm(LIMIT_BAL, m, s))))

#scatter plot

ggplot(data = defaulter_data) + 
  geom_point(mapping = aes(x = AGE, y = LIMIT_BAL, color = SEX)) + xlim(20,80) + geom_vline(xintercept = 30) + geom_hline(yintercept = 2000)

ggplot(data = defaulter_data) + 
  geom_point(mapping = aes(x = AGE, y = LIMIT_BAL, color = MARRIAGE)) + xlim(20,80) + geom_vline(xintercept = 35) + geom_hline(yintercept = 2000)

#box plot
#comparing category1 and category 3(highly educated men doing default v/s men with less education doing default)
par(mfrow=c(1,1))
boxplot(category_1$LIMIT_BAL,category_3$LIMIT_BAL,
        main="multiple box plots for comparision",
        names=c("Category 1","Category 3"),
        xlab="",ylab="",col="orange",border = "brown",notch=TRUE,horizontal = TRUE)

#correlation plot
M <- cor(fltr_data)
head(round(M,2))
corrplot(M,method ="circle")
#red cirlce represents negative correlations, blue represents positive correlations


#statistical analysis
#______________________________________________________________________________________
#TOH Z distribution #two tailed
p0 <- nrow(category_1D)/nrow(fltr_data2)
sample1_data <- sample_n (fltr_data2, 5000)
sample1 <- dplyr::filter(sample1_data, SEX ==1)
sample1 <- dplyr::filter(sample1, EDUCATION == 1 | EDUCATION ==2)
sample1 <- dplyr::filter(sample1, MARRIAGE == 2 | MARRIAGE ==3)
sample1 <- dplyr::filter(sample1, defaulters==1)
p1 <- nrow(sample1)/nrow(sample1_data)
n <- nrow(sample1_data)
z.test <- function(p0, p1, n)
{
  z= ((p1-p0)/sqrt(p1*(1-p1)/n))
  return(z)
}
z.test(p0,p1,n)

# TOH for mean(s)  left tailed
sample_m <- sample_n(fltr_data2, 2000)
sample_f <- sample_n(fltr_data2,3000)
mean_sample_m <- mean(sample_m[sample_m$SEX==1, ]$LIMIT_BAL)
mean_sample_f <- mean(sample_f[sample_f$SEX==2, ]$LIMIT_BAL)

sd_m <- sd(fltr_data[fltr_data$SEX==1, ]$LIMIT_BAL)
sd_m
sd_f <- sd(fltr_data[fltr_data$SEX==2, ]$LIMIT_BAL)
sd_f

ztest_means <- function(x1, x2, sd_m, sd_f)
{
  z_m = (x1-x2)/sqrt(((sd_m^2)/2000)+((sd_f^2)/3000))
  return(z_m)
}
ztest_means(mean_sample_m, mean_sample_f, sd_m, sd_f)


#To get confidence interval of proportions
#Comparing the sample data of 1000 and 1300 individuals (from category 1 i.e, married males with university or 
#higher education level) with 95% confidence level for each of the sample data

#to calculate defaulter probability of sample data 1
sample_test1_a <- sample_n(category_5,1000)
sample_test1_ad <- dplyr::filter(sample_test1_a, defaulters==1)
prob_1a <- (nrow(sample_test1_ad)/nrow(sample_test1_a))

#to calculate defaulter probability of sample data 2
sample_test1_b <- sample_n(category_5, 1300)
sample_test1_bd <- dplyr::filter(sample_test1_b, defaulters==1)
prob_1b <- (nrow(sample_test1_bd)/nrow(sample_test1_b))

error <- qnorm(0.975)*sqrt( (  (prob_1a*(1-prob_1a))/1000) + (  (prob_1b*(1-prob_1b))/1300))

left <- round((prob_1a - prob_1b - error), 4)
left
right <- round((prob_1a - prob_1b + error), 4)
right
#___________________________________________________________________________________________________________________
#Advanced analysis

# performing analysis Using decision tree to identify the trend of individuals doing default based on there alloted limit balance, age, sex and maritial status
str(fltr_data_n)
fltr_data_n$Def <- factor(fltr_data_n$defaulters)

#partitioning data into training and validation
set.seed(123)
pd <- sample(2,nrow(fltr_data_n),replace = TRUE,prob = c(0.8,0.2))
train <- fltr_data_n[pd==1,]
validate <- fltr_data_n[pd==2,]

#decision tree with party

tree <- ctree(Def~AGE+SEX+MARRIAGE+LIMIT_BAL,data=train,controls = ctree_control(mincriterion = 0.9,minsplit = 100))
tree
plot(tree)

#prediction
predict(tree,validate,type="prob")

#decision tree with rpart

tree1 <- rpart(Def~LIMIT_BAL+AGE+EDUCATION,train)

#prediction
predict(tree1,validate)

#misclassification error
#training data
tab <- table(predict(tree),train$Def)
print(tab)
1-sum(diag(tab))/sum(tab)

#validation data
testpred <- predict(tree,newdata=validate)
tab <- table(testpred,validate$Def)
print(tab)
1-sum(diag(tab))/sum(tab)

