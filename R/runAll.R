# runAll.R

source('utils.R')
source('models.R')
#source('postprocess.R')

train <- raw_train()
test <- raw_test()

simple.names <- c('seasonal_naive', 'medians')
simple.nums <- c(0,0)
for(k in 1:2){
  print(paste('Predicting on model:', simple.names[k]))
  pred <- individual_forecast(train, test, simple.names[k])
  names(pred)[which(names(pred)=="predSales")] = "Sales"
  simple.nums[k] <- write.submission(pred)
}
pred <- make.average(simple.nums)
print('This is the shifted average of simple models.')
# keep the number, because this goes into the final model
sub.nums <- write.submission(pred)

# time-series linear model without extra regressors
pred <- individual_forecast(train, test, 'tslm_mod', model.type="plus", xtra_reg = F)
names(pred)[which(names(pred)=="predSales")] = "Sales"
s.num <- write.submission(pred)
sub.nums <- c(sub.nums, s.num)

# time-series linear model with extra regressors
pred <- individual_forecast(add_features_all(train), add_features_all(test), 'tslm_mod', 
                            model.type="plus", xtra_reg = T)
names(pred)[which(names(pred)=="predSales")] = "Sales"
s.num <- write.submission(pred)
sub.nums <- c(sub.nums, s.num)

# This is the final result.
pred <- make.average(sub.nums)
final.num <- write.submission(pred)
print(paste0('The final average is submission', final.num, '.csv'))
