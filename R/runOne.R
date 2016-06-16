# runOne.R

source('utils.R')
source('models.R')

source('tools.R')
#source('postprocess.R')

train <- raw_train()
test <- raw_test()
  
temp = subtrain_val(train)
tr = temp$tr
val = temp$val
rm(temp)

pred0 <- grouped_forecast(tr, val, 'StoreType', 'stlf_svd', model.type="ets", n.comp=12)
# "RMSPE: 0.19358315370747"

pred <- grouped_forecast(tr, val, 'Assortment', 'stlf_svd', model.type="ets", n.comp=12)
#"RMSPE: 0.193264404334467"

pred2 <- grouped_forecast(tr, val, 'Assortment', 'stlf_svd', model.type="arima", n.comp=12)
# "RMSPE: 0.196492984324033"

pred3 <- grouped_forecast(tr, val, 'Assortment', 'stlf_svd', model.type="arima", n.comp=12,
                          freq=7, s.win=7)
#"RMSPE: 0.265987078375413"

pred4 <- individual_forecast(tr, val, 'stlm_mod', model.type="arima")
# "RMSPE: 0.195846708551007"

pred5 <- individual_forecast(tr, val, 'tslm_mod', model.type="plus", xtra_reg = F)
# "RMSPE: 0.153008870885237"

pred6 <- individual_forecast(add_features_all(tr), add_features_all(val), 'tslm_mod', 
                             model.type="plus", xtra_reg = T)
# "RMSPE: 0.143514547328967"

pred7 <- individual_forecast(tr, val, 'seasonal_naive')
# "RMSPE: 0.192210257980582"

pred8 <- individual_forecast(tr, val, 'medians')
# "RMSPE: 0.142309871772377"

# Make predictions using one model
# pred <- individual_forecast(train, test, 'tslm_mod', model.type="basic")
# snum <- write.submission(pred)
# temp = RMSPE_per_store(pred)
# temp
# 
# setkey(pred, Store)
# plot(pred[.(292),Date], pred[.(292),Sales])
# lines(pred[.(292),Date], pred[.(292),predSales])
# 
# plot(pred[.(909),Date], pred[.(909),Sales])
# lines(pred[.(909),Date], pred[.(909),predSales])
# 
# plot(pred[.(782),Date], pred[.(782),Sales])
# lines(pred[.(782),Date], pred[.(782),predSales])
# 
# plot(pred[.(550),Date], pred[.(550),Sales])
# lines(pred[.(550),Date], pred[.(550),predSales])
# 
# plot(pred[.(733),Date], pred[.(733),Sales])
# lines(pred[.(733),Date], pred[.(733),predSales])
# 
# pred2 <- grouped_forecast(tr, val, 'Assortment', 'stlf_svd', model.type="ets", n.comp=12,
#                           freq=365, s.win=)
# 
# fit <- stl(ts(tr[.(782),Sales], frequency=14), s.window=7, robust=TRUE)
# plot(fit)
# fit2 <- stl(ts(tr[.(1),Sales], frequency=14), t.window=15, s.window=7, robust=TRUE)
# plot(fit2)
