#install.packages("data.table")
library(data.table)
#install.packages("stargazer")
library(stargazer)
#install.packages("stargazer")
library(stargazer)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("pwr")
library(pwr)
#install.packages("plm")
library(plm)
#install.packages("AER")
library(AER)
#install.packages("MatchIt")
library(MatchIt)

#clear environment
rm(list=ls());gc()


# SET YOUR WORKING DIRECTORY HERE
setwd("/Users/geyues/Desktop/ABT/Project")

#load dataset
MyData = fread(input='data.csv', verbose = F)
MyData <- MyData[-c(1, 2), ]

#clean data
MyData$treatment<-with(MyData,ifelse(Q7==2|Q8==2|Q9==2|Q10==2|Q11==2|Q12==2,1,0))
names(MyData)[names(MyData) == 'Q13'] <- 'willingness'
MyData <- MyData[!(MyData$willingness == "" | is.na(MyData$willingness)), ]

#1.Analyze the willingness of control group and treatment group
#plain OLS: y~X
ols<-lm(willingness~treatment,data=MyData)

stargazer(ols,
          se=list(
            sqrt(diag(vcovHC(ols,
                             method="arellano",
                             type="HC1")))),
          title=
            "OLS Regression: The Effect of Adding a Reading Time next to Title on Willingness to Read the Whole Article",
          type="text",
          column.labels = c("plain"),
          model.numbers = F)


#2. Analyze the willingness of different ethnicity
MyData$ethnicity[MyData$Q3 == 1] <- "White or Caucasion"
MyData$ethnicity[MyData$Q3 == 2] <- "Black or African American"
MyData$ethnicity[MyData$Q3 == 3] <- "Asian"
MyData$ethnicity[MyData$Q3 == 4] <- "Hispanic or Latino"
MyData$ethnicity[MyData$Q3 == 5] <- "Multiracial or Biracial"
MyData$ethnicity[MyData$Q3 == 6] <- "Other"
MyData$ethnicity[MyData$Q3 == 7] <- "Prefer not to answer"

#Have an overview at the percentage of ethnicity
proportions <- table(MyData$ethnicity)/length(MyData$ethnicity)
percentages <- proportions*100
View(percentages)

#Run OLS regression
MyData$ethnicity <- as.factor(MyData$ethnicity)
ols1<-lm(willingness~treatment*ethnicity,data=MyData)
stargazer(ols1,
          se=list(
            sqrt(diag(vcovHC(ols1,
                             method="arellano",
                             type="HC1")))),
          title=
            "OLS Regression: The Effect of Interaction of Adding a Reading Time and Ethnicity on Willingness to Read the Whole Article",
          type="text",
          column.labels = c("plain"),
          model.numbers = F)

#3. Analyze the willingness of different reading frequency
MyData$frequency[MyData$Q5 == 1] <- "Less than 1 hour"
MyData$frequency[MyData$Q5 == 2] <- "1-2 hours"
MyData$frequency[MyData$Q5 == 3] <- "2-4 hours"
MyData$frequency[MyData$Q5 == 4] <- "4-6 hours"
MyData$frequency[MyData$Q5 == 5] <- "More than 6 hours"

#Have an overview at the percentage of frequency
proportions1 <- table(MyData$frequency)/length(MyData$frequency)
percentages1 <- proportions1*100
View(percentages1)

#Run OLS regression
MyData$frequency <- as.factor(MyData$frequency)
ols2<-lm(willingness~treatment*frequency,data=MyData)
stargazer(ols2,
          se=list(
            sqrt(diag(vcovHC(ols1,
                             method="arellano",
                             type="HC1")))),
          title=
            "OLS Regression: The Effect of Interaction of Adding a Reading Time and Reading Frequency on Willingness to Read the Whole Article",
          type="text",
          column.labels = c("plain"),
          model.numbers = F)


