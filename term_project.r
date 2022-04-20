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

#edit - remove case number, change variables to correct format 
d <- data %>% 
  select(-CASE_NUMBER) %>% 
  mutate(PREVAILING_WAGE_SUBMITTED = as.numeric(PREVAILING_WAGE_SUBMITTED),
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
    PREVAILING_WAGE_SUBMITTED_UNIT == 'month' ~ 12 * PREVAILING_WAGE_SUBMITTED,
    PREVAILING_WAGE_SUBMITTED_UNIT == 'hour' ~ 40 * 48 * PREVAILING_WAGE_SUBMITTED,
    PREVAILING_WAGE_SUBMITTED_UNIT == 'week' ~ 48 * PREVAILING_WAGE_SUBMITTED,
    #anything else means the unit is 'year' so we can just go with the regular value
    TRUE ~ PREVAILING_WAGE_SUBMITTED))

#do the same thing with paid wage
d <- d %>% 
  mutate(calculated_paid_wage_py = case_when(
    PAID_WAGE_SUBMITTED_UNIT == 'bi-weekly' ~ 24 * PAID_WAGE_SUBMITTED,
    PAID_WAGE_SUBMITTED_UNIT == 'month' ~ 12 * PAID_WAGE_SUBMITTED,
    PAID_WAGE_SUBMITTED_UNIT == 'hour' ~ 40 * 48 * PAID_WAGE_SUBMITTED,
    PAID_WAGE_SUBMITTED_UNIT == 'week' ~ 48 * PAID_WAGE_SUBMITTED,
    TRUE ~ PAID_WAGE_SUBMITTED))

#now we can take out those columns + dates
d2 <- d %>% 
  select(-c(PREVAILING_WAGE_SUBMITTED, PREVAILING_WAGE_SUBMITTED_UNIT, 
            PAID_WAGE_SUBMITTED, PAID_WAGE_SUBMITTED_UNIT, DECISION_DATE, 
            CASE_RECEIVED_DATE, EMPLOYER_NAME))


#combine status into either certified (accepted) or denied or withdrawn
#since certified withdrawn is a separate category but denied withdrawn isnt
#im placing all withdrawn values with denied
d2 <- d2 %>% 
  mutate(CASE_STATUS = case_when(CASE_STATUS == 'certified-expired' ~ 'certified',
                                 CASE_STATUS == 'certified-withdrawn' ~ 'certified',
                                 CASE_STATUS == 'withdrawn' ~ 'denied',
                                 TRUE ~ CASE_STATUS)) #if else, leave it be


#as factor for all character features so r can work with them
d2[sapply(d2, is.character)] <- lapply(d2[sapply(d2, is.character)], 
                                       as.factor)

#take out features that have way too many factors
d2 <- d2 %>% 
  select(-c(JOB_TITLE, WORK_CITY, COLLEGE_MAJOR_REQUIRED, PREVAILING_WAGE_SOC_CODE,
            PREVAILING_WAGE_SOC_TITLE))



##### 
#decision tree 

#split into training and testing
validation_index <- createDataPartition(d2$CASE_STATUS, p=0.80, list=FALSE)
test <- d2[-validation_index,]
train <- d2[validation_index,]

#test %>% filter(CASE_STATUS == 'denied') %>% count()


mod1 <- C5.0(train[-1], as.factor(train$CASE_STATUS))

mod1
summary(mod1)

#big tendency to overfit so lets look at the testing set 

status_pred <- predict(mod1, test)
CrossTable(test$CASE_STATUS, status_pred, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('predicted status', 'actual status'))
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
matrix_dimensions <- list(c('certified', 'denied'),
                          c('certified', 'denied'))
names(matrix_dimensions) <- c('predicted', 'actual')

error_cost <- matrix(c(0, 5,
                       1, 0), 
                     nrow = 2, byrow = TRUE, dimnames = matrix_dimensions)
error_cost


status_cost <- C5.0(d2[-1], d2$CASE_STATUS, costs = error_cost)
status_cost_pred <- predict(status_cost, test) 
CrossTable(test$CASE_STATUS, status_cost_pred, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual', 'predicted'))
confusionMatrix(status_cost_pred, test$CASE_STATUS)

#specificity (true negative over all negative)
#much less in this model, good if we want to be conservative about who will get in
#but not so much if we want to be optimistic or even if we want to have the best
#overall accuracy

#i can't get a cost to fix the massive difference in the number of classifications
#i'm just going to balance the classes

#get current props
prop.table(table(d2$CASE_STATUS))
#huge class imbalance which isn't good for decision trees because they can be easily 
#overfitted and biased 

#lets balance classes with upsampling

#set.seed(13)
balanced <- upSample(d2, d2$CASE_STATUS)
balanced <- balanced %>% 
  select(-Class) 

prop.table(table(balanced$CASE_STATUS))
#much better

#splitting into testing and training
vi <- createDataPartition(balanced$CASE_STATUS, p=0.80, list=FALSE)
test1 <- balanced[-vi,]
train1 <- balanced[vi,]


#no boosting, no weights
modb1 <- C5.0(train1[-1], train1$CASE_STATUS)
summary(modb1)


status_predb1 <- predict(modb1, test1)
CrossTable(test1$CASE_STATUS, status_predb1, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('predicted status', 'actual status'))
confusionMatrix(test1$CASE_STATUS, status_predb1)
#eh, it's alright. Thinks are much more balanced (not some values really high and
#others really low) but I'm sure i could do better

#with boosting
mod_boostb1 <- C5.0(train1[-1], train1$CASE_STATUS, trials = 10)
mod_boostb1
summary(mod_boostb1)

status_predb2 <- predict(mod_boostb1, test1)
CrossTable(test1$CASE_STATUS, status_predb2, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual status', 'predicted status'))
confusionMatrix(test1$CASE_STATUS, status_predb2)
#nice!!!! good sensitivity, specificity, everything is really good
#error went down from > 16 to < 12


