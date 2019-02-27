library(tidyverse)
library(ryouready)

setwd('C:/Users/Alissa Hayes/Desktop/Springboard')

Quotes <- read.csv('Quote File.csv')
summary(Cancels)
Quotes<- Quotes %>% 
  filter(Bind.Count == 1)

XML <- read.csv('revised2 xml.csv')
summary(XML)
cancel_file <- read.csv('cancels2.csv')

#Join together with lead and cancel data

Cancels <- inner_join(Quotes, XML, by = c("PHONE_NBR" = "phone"))
cancel_file$PHONE_NBR <- as.character(cancel_file$PHONE_NBR)
Cancels2 <- left_join(Cancels, cancel_file, by = "PHONE_NBR")


#Examine Data
glimpse(Cancels2)

count_na(Cancels2, along = 2)
#Most columns have 0 missing values. The currently insured column has 38 missing values. Cancelled information has missing
#values because not all policies have cancelled. 

#Create cancelled column (1 or 0 column)
Cancels2 <-  Cancels2 %>% 
  mutate(Cancelled = ifelse(!is.na(CANCELLATION_EFF_DATE) & lag<365, 1,0))
glimpse(Cancels2)

#Keep only needed variables:
Cancels3 <- Cancels2 %>% 
  select(State, Zip.Code, First.Quote.Date, Last.Quote.Date, EMPLY_EMAIL_ADDR_TXT.x, LQ_FB_PREM_AMT, customer_age,
         marital_status, education, no_of_vehicles, currently_insured.x, gender, risk_profile, number_of_claims, number_of_accidents,
         license_status, policy_expiration_date.x, years_continuous_coverage, months_continuous_coverage, previous_policy_type,
         cost_of_coverage, bi_limit, vehicle_ownership, lead_seller, CANCELLATION_EFF_DATE,
         CANCELLATION_REASON, DATE_CANX_PROCESSED, lag, Cancelled)

#Format Columns:
Cancels3$Last.Quote.Date <- as.Date(Cancels3$Last.Quote.Date, format = "%y-%b-%d")
Cancels3$First.Quote.Date <- as.Date(Cancels3$First.Quote.Date, format = "%y-%b-%d")
Cancels3$DATE_CANX_PROCESSED <- as.Date(Cancels3$DATE_CANX_PROCESSED, format = "%m/%d/%y")
Cancels3$CANCELLATION_EFF_DATE <- as.Date(Cancels3$CANCELLATION_EFF_DATE, format = "%m/%d/%y")
Cancels3$Cancelled <- as.factor(Cancels3$Cancelled)
Cancels3$cost_of_coverage <- as.integer(Cancels3$cost_of_coverage)
Cancels3$currently_insured.x <- as.factor(Cancels3$currently_insured.x)
Cancels3$LQ_FB_PREM_AMT <- as.integer(Cancels3$LQ_FB_PREM_AMT)


#Derived Variable: Cost Per vehicle

Cancels3 <- Cancels3 %>% 
  mutate(cost_per_vehicle = LQ_FB_PREM_AMT/no_of_vehicles)

#Create binary lag variables
Cancels3 <- Cancels3 %>% 
  mutate(lag_0_60 = ifelse(lag<=60, 1, 0), lag_61_90 = ifelse(lag>60 & lag <=90, 1, 0),
         lag_91_180 = ifelse(lag>90 & lag<=180, 1, 0),
         lag_181_364 = ifelse(lag>180 & lag<365, 1, 0))


#How many unique: length(unique(variable))

unique <- sapply(Cancels3, function(x) length(unique(x)))

#How many missing

Cancels3 <- as.dataframe(gsub(" ", NA, Cancels3))


missing <- colSums(is.na(Cancels3)) 


#Use ggplot2 to come up with some plots: 
##Plot 1: Cancelelled vs. Not cancelled. Distinguish (maybe with shape or something else to show which lag group)
##Plot 2-?: Group predictor variables together: Demographic, prior policy, etc. Have a separate plot for each group which could
  ##help explain which predictor variables are the most correlated with cancellations

