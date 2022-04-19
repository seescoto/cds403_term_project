####
#sofia escoto, term project
#cds 403 spring 2022
###

library(tidyverse)
library(readxl)
library(naniar) #replace_with_na()
library(caret) #create data partition
library(C50) #decision trees
library(gmodels) #crosstable

#read in data
data <- read_excel('usa_foreign_workers_salary.xlsx')

date <- '%m/%d/%y'
#edit - remove case number, change variable formats to correct 
d <- data %>% 
  select(-CASE_NUMBER) %>% 
  mutate(CASE_RECEIVED_DATE = as.Date(CASE_RECEIVED_DATE, format = date),
         DECISION_DATE = as.Date(DECISION_DATE, format = date),
         PREVAILING_WAGE_SUBMITTED = as.numeric(PREVAILING_WAGE_SUBMITTED),
         PAID_WAGE_SUBMITTED = as.numeric(PAID_WAGE_SUBMITTED),
         EXPERIENCE_REQUIRED_NUM_MONTHS = as.numeric(EXPERIENCE_REQUIRED_NUM_MONTHS),
         WORK_POSTAL_CODE = as.numeric(WORK_POSTAL_CODE),
         PREVAILING_WAGE_PER_YEAR = as.numeric(PREVAILING_WAGE_PER_YEAR))

#look at the wages,,, are they all in the same unit ??
d %>% distinct(PREVAILING_WAGE_SUBMITTED_UNIT)
d %>% 
  filter(PREVAILING_WAGE_SUBMITTED_UNIT == 'hour') %>% 
  select(PREVAILING_WAGE_SUBMITTED, PREVAILING_WAGE_SUBMITTED_UNIT, PREVAILING_WAGE_PER_YEAR)


d %>% 
  filter(PREVAILING_WAGE_SUBMITTED_UNIT == 'bi-weekly') %>% 
  select(PREVAILING_WAGE_SUBMITTED, PREVAILING_WAGE_SUBMITTED_UNIT, PREVAILING_WAGE_PER_YEAR)

#not all the same - lets make a new column that will give them all equal footing
d <- d %>% 
  mutate(calculated_prev_wage_py = case_when( 
    #change the calculated value to their wage per year based off how often they get paid
    PREVAILING_WAGE_SUBMITTED_UNIT == 'bi-weekly' ~ 24 * PREVAILING_WAGE_SUBMITTED,
    PREVAILING_WAGE_SUBMITTED_UNIT == 'moonth' ~ 12 * PREVAILING_WAGE_SUBMITTED,
    PREVAILING_WAGE_SUBMITTED_UNIT == 'hour' ~ 40 * 4 * 8 * PREVAILING_WAGE_SUBMITTED,
    PREVAILING_WAGE_SUBMITTED_UNIT == 'week' ~ 48 * PREVAILING_WAGE_SUBMITTED,
    #anything else means the unit is 'year' so we can just go with the regular value
    TRUE ~ PREVAILING_WAGE_SUBMITTED))

#do the same thing with paid wage
d <- d %>% 
  mutate(calculated_paid_wage_py = case_when(
    PAID_WAGE_SUBMITTED_UNIT == 'bi-weekly' ~ 24 * PAID_WAGE_SUBMITTED,
    PAID_WAGE_SUBMITTED_UNIT == 'moonth' ~ 12 * PAID_WAGE_SUBMITTED,
    PAID_WAGE_SUBMITTED_UNIT == 'hour' ~ 40 * 4 * 8 * PAID_WAGE_SUBMITTED,
    PAID_WAGE_SUBMITTED_UNIT == 'week' ~ 48 * PAID_WAGE_SUBMITTED,
    #anything else means the unit is 'year' so we can just go with the regular value
    TRUE ~ PAID_WAGE_SUBMITTED))

#now we can take out those columns
d <- d %>% 
  select(-c(PREVAILING_WAGE_SUBMITTED, PREVAILING_WAGE_SUBMITTED_UNIT, 
            PAID_WAGE_SUBMITTED, PAID_WAGE_SUBMITTED_UNIT))

d2 <- d %>% #no date columns
  select(-c(DECISION_DATE, CASE_RECEIVED_DATE, EMPLOYER_NAME))

#combine status into either certified (accepted) or denied or withdrawn
d2 <- d2 %>% 
  mutate(CASE_STATUS = case_when(CASE_STATUS == 'certified-expired' ~ 'certified',
                                 CASE_STATUS == 'certified-withdrawn' ~ 'certified',
                                 TRUE ~ CASE_STATUS)) #if else, leave it be


#as factor for all character features
d2[sapply(d2, is.character)] <- lapply(d2[sapply(d2, is.character)], 
                                       as.factor)

#take out features that have wayy too many factors
d2 <- d2 %>% 
  select(-c(JOB_TITLE, WORK_CITY, COLLEGE_MAJOR_REQUIRED, PREVAILING_WAGE_SOC_CODE,
            PREVAILING_WAGE_SOC_TITLE))


##### 
#decision tree 

#split into training and testing
validation_index <- createDataPartition(d2$CASE_STATUS, p=0.80, list=FALSE)
test <- d2[-validation_index,]
train <- d2[validation_index,]


mod1 <- C5.0(train[-1], as.factor(train$CASE_STATUS))

mod1
summary(mod1)

#big tendency to overfit so lets look at the testing set 

status_pred <- predict(mod1, test)
CrossTable(test$CASE_STATUS, status_pred, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual status', 'predicted status'))
confusionMatrix(test$CASE_STATUS, status_pred)

#pretty good but positive predictive value for denied is super low and 
#negative predictive value for accepted (certified) is super high
#lets try to fix that by boosting

mod_boost <- C5.0(train[-1], train$CASE_STATUS, trials = 10)
mod_boost
summary(mod_boost)

status_pred2 <- predict(mod_boost, test)
CrossTable(test$CASE_STATUS, status_pred2, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual status', 'predicted status'))
confusionMatrix(test$CASE_STATUS, status_pred2)

#a little better positive predictive value for denied and
#negative predictive value for accepted/certified


#lets increase cost of guessing denied wrong
matrix_dimensions <- list(c('certified', 'denied', 'withdrawn'),
                          c('certified', 'denied', 'withdrawn'))
names(matrix_dimensions) <- c('predicted', 'actual')
matrix_dimensions %>% 

error_cost <- matrix(c(5, 0, 1, 0, 5, 1), nrow = 2))
, dimnames = matrix_dimensions)
error_cost

matrix_dimensions[1]
