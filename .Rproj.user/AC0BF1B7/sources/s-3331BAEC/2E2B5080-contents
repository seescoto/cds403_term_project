#term project - balanced classes
#sofia escoto 

prop.table(table(d2$CASE_STATUS))
#huge class imbalance which isn't good for decision trees because they can be easily 
#overfitted and biased 

#lets balance classes with upsampling

set.seed(13)

d3 <- d2
balanced <- upSample(d3, d3$CASE_STATUS)
balanced <- balanced %>% select(-Class)

prop.table(table(balanced$CASE_STATUS))


vi <- createDataPartition(balanced$CASE_STATUS, p=0.80, list=FALSE)
test1 <- balanced[-vi,]
train1 <- balanced[vi,]


#no boosting, no weights
modb1 <- C5.0(train1[-1], as.factor(train1$CASE_STATUS))
summary(modb1)


status_predb1 <- predict(modb1, test1)
CrossTable(test1$CASE_STATUS, status_predb1, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('predicted status', 'actual status'))
confusionMatrix(test1$CASE_STATUS, status_predb1)


#boosting
mod_boostb1 <- C5.0(train1[-1], train1$CASE_STATUS, trials = 10)
mod_boostb1
summary(mod_boostb1)

status_predb2 <- predict(mod_boostb1, test1)
CrossTable(test1$CASE_STATUS, status_predb2, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual status', 'predicted status'))
confusionMatrix(test1$CASE_STATUS, status_predb2)
