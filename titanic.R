library(tidyverse)
library(psych)

setwd('C:/Users/Alissa Hayes/Desktop/Springboard')

titanic <- read.csv('titanic3.csv')

glimpse(titanic)

#1: Port of embarkation
table(titanic$embarked)

titanic$embarked[nchar(titanic$embarked) == 0] <- "S"

#2: Age
sum(is.na(titanic$age))
mean(titanic$age, na.rm = TRUE)
titanic$age[is.na(titanic$age)] <- mean(titanic$age, na.rm = TRUE)

  ##Standard deviation would have worked better to reduce the impact of the small number of elderly passengers

#3: Lifeboat
table(titanic$boat)
titanic$boat <- gsub(" ", NA, titanic$boat)
sum(is.na(titanic$boat))

#4: Cabin
titanic$cabin <- as.character(titanic$cabin)
titanic$has_cabin_number <- ifelse(titanic$cabin == "", 0, 1)
  
write.csv(titanic, file = "titanic_clean.csv")
