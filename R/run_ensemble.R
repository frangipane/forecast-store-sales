# run_ensemble.R

source('utils.R')
source('models.R')
source('tools.R')
#source('postprocess.R')

train <- raw_train()
test <- raw_test()

#################
# first get validation estimate of RMSPE

temp = subtrain_val(train)
tr = temp$tr
val = temp$val
rm(temp)

# 1 seasonal_naive (guessing using sales from ~365 days prior)
pred1 <- individual_forecast(tr, val, 'seasonal_naive')
pred1 = pred1[order(Store, Date),]
#"RMSPE: 0.192210257980582"

# 2 August2015_kluge (guessing using sales from ~14 days prior)
# pred2 <- individual_forecast(tr, val, 'August2015_kluge')
# pred2 = pred2[order(Store, Date),]

# 3 medians
pred3 <- individual_forecast(tr, val, 'medians')
pred3 = pred3[order(Store, Date),]
#"RMSPE: 0.142309871772377"

# 4 stlf_svd with ets, grouping stores by Assortment
pred4 <- grouped_forecast(tr, val, 'Assortment', 'stlf_svd', model.type="ets", n.comp=12)
pred4 = pred4[order(Store, Date),]
#"RMSPE: 0.193264404334467"

# 5 stl decomposition, arima without regressors
pred5 <- individual_forecast(tr, val, 'stlm_mod', model.type="arima")
pred5 = pred5[order(Store, Date),]
#"RMSPE: 0.195846708551007"

# 6 time-series linear model with extra regressors
pred6 <- individual_forecast(add_features_all(tr), add_features_all(val), 'tslm_mod', 
                            model.type="plus", xtra_reg = T)
pred6 = pred6[order(Store, Date),]
# "RMSPE: 0.143514547328967"

# This is the final result.
# sum averaging

# check
all.equal(paste(pred1$Store, pred1$Date),
          paste(pred3$Store, pred3$Date),
          paste(pred4$Store, pred4$Date),
          paste(pred5$Store, pred5$Date),
          paste(pred6$Store, pred6$Date))

predSales_avg = (  pred1$predSales + 
                     pred3$predSales + 
                     pred4$predSales + 
                     pred5$predSales + 
                     pred6$predSales)/5

myensemble = data.table(Store = val$Store, Date=val$Date, Sales=val$Sales, predSales=predSales_avg)
setkey(myensemble, Store)

myensemble_rmspe_per_store = RMSPE_per_store(myensemble)
RMSPE(truth=as.vector(myensemble$Sales), pred=as.vector(myensemble$predSales))
# RMSPE: 0.1404405

#################
# sum averaging of just medians and tslm model
predSales_avg2 = (  pred3$predSales + pred6$predSales)/2

myensemble2 = data.table(Store = val$Store, Date=val$Date, Sales=val$Sales, predSales=predSales_avg2)
setkey(myensemble2, Store)

myensemble_rmspe_per_store2 = RMSPE_per_store(myensemble2)
RMSPE(truth=as.vector(myensemble2$Sales), pred=as.vector(myensemble2$predSales))
# RMSPE: 0.1317683


#################
# validation on August 2014 dates?

tr = train[(Date < as.Date("2014-08-01"))]
val = train[(Date >= as.Date("2014-08-01")) & (Date <= as.Date("2014-09-17"))]
salesperstore = val[ , .(total=sum(Sales)), by=Store]
dropStore = salesperstore[total==0, Store]
val = val[Store != dropStore]

# 3 medians
pred3_2014 <- individual_forecast(tr, val, 'medians')
pred3_2014 = pred3_2014[order(Store, Date),]
#"RMSPE: 0.142309871772377"
# "RMSPE: 0.133164909811087"

# 6 time-series linear model with extra regressors
pred6_2014 <- individual_forecast(add_features_all(tr), add_features_all(val), 'tslm_mod', 
                             model.type="plus", xtra_reg = T)
pred6_2014 = pred6_2014[order(Store, Date),]
# "RMSPE: 0.143514547328967"
# "RMSPE: 0.159746703571181"

# 6 time-series linear model without extra regressors
pred6_2014_0 <- individual_forecast(add_features_all(tr), add_features_all(val), 'tslm_mod', 
                                  model.type="plus", xtra_reg = T)
pred6_2014_0 = pred6_2014[order(Store, Date),]

predSales_avg2014 = (  pred3_2014$predSales * pred6_2014$predSales)^(1/2)
myensemble2014 = data.table(Store = val$Store, Date=val$Date, Sales=val$Sales, 
                            predSales=predSales_avg2014)
setkey(myensemble2014, Store)

myensemble_rmspe_per_store2014 = RMSPE_per_store(myensemble2014)
RMSPE(truth=as.vector(myensemble2014$Sales), pred=as.vector(myensemble2014$predSales))
# "RMSPE: 0.1324014

#################
## Run models for final submission

# 1 seasonal_naive (guessing using sales from ~365 days prior)
pred <- individual_forecast(train, test, 'seasonal_naive')
names(pred)[which(names(pred)=="predSales")] = "Sales"
pred = pred[order(Store, Date),]
sub.nums <- write.submission(pred)

# 2 August2015_kluge (guessing using sales from ~14 days prior)
# pred <- individual_forecast(train, test, 'August2015_kluge')
# names(pred)[which(names(pred)=="predSales")] = "Sales"
# pred = pred[order(Store, Date),]
# sub.nums <- c(sub.nums, s.num)

# 3 medians
pred <- individual_forecast(train, test, 'medians')
names(pred)[which(names(pred)=="predSales")] = "Sales"
pred = pred[order(Store, Date),]
s.num <- write.submission(pred)
sub.nums <- c(sub.nums, s.num)

# 4 stlf_svd with ets, grouping stores by Assortment
pred <- grouped_forecast(train, test, 'Assortment', 'stlf_svd', model.type="ets", n.comp=12)
names(pred)[which(names(pred)=="predSales")] = "Sales"
pred = pred[order(Store, Date),]
s.num <- write.submission(pred)
sub.nums <- c(sub.nums, s.num)

# 5 stl decomposition, arima without regressors
pred <- individual_forecast(train, test, 'stlm_mod', model.type="arima")
names(pred)[which(names(pred)=="predSales")] = "Sales"
pred = pred[order(Store, Date),]
s.num <- write.submission(pred)
sub.nums <- c(sub.nums, s.num)

# 6 time-series linear model with extra regressors
pred <- individual_forecast(add_features_all(train), add_features_all(test), 'tslm_mod', 
                             model.type="plus", xtra_reg = T)
names(pred)[which(names(pred)=="predSales")] = "Sales"
pred = pred[order(Store, Date),]
s.num <- write.submission(pred)
sub.nums <- c(sub.nums, s.num)

# 7 This is the final result.
final <- make.average(sub.nums)
final.num <- write.submission(final)
print(paste0('The final average is submission', final.num, '.csv'))
# submission6.csv

#################
# make submission using just sum average of medians and tslm model
final <- make.average(c(2,5))
final.num <- write.submission(final)
print(paste0('The final average is submission', final.num, '.csv'))
# submission7.csv

#################
# make submission using just product average of medians and tslm model
pred_med = reload.submission(2)
pred_med = pred_med[order(pred_med$Id),]

pred_tslm = reload.submission(5)
pred_tslm = pred_tslm[order(pred_tslm$Id),]

final <- data.frame(Id = pred_med$Id, Sales = sqrt(pred_med$Sales*pred_tslm$Sales))
final$Sales[is.na(final$Sales)] = 0

final.num <- write.submission(final)
print(paste0('The final average is submission', final.num, '.csv'))
# submission8.csv

final = read.csv("../submissions/submission8.csv")
#################
# 
# 2 August2015_kluge (guessing using sales from ~14 days prior)
pred <- individual_forecast(add_features_all(train), add_features_all(test), 'August2015_kluge')
combo = merge(final, pred[,.(Id, predSales)], by = "Id", all = T)

combo = as.data.frame(combo)
combo$combo_avg = (combo$Sales^2*combo$predSales)^(1/3) # take product average including kluge column
combo <- within(combo, {combo_avg = ifelse(is.na(combo_avg), Sales, combo_avg)})
combo$combo_avg[is.na(combo_avg)] =  combo$Sales[is.na(combo_avg)] # replace NAs with old product average (of just medians and tslm)

final = data.frame(Id = combo$Id, Sales = combo$combo_avg)
final.num <- write.submission(final)
## submission9.csv

# names(pred)[which(names(pred)=="predSales")] = "Sales"
# pred = pred[order(Store, Date),]
# sub.nums <- c(sub.nums, s.num)