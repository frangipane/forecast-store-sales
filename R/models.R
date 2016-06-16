## models.R


grouped_forecast <- function(train, test, group=c('Assortment','StoreType'), fname, ...){
  # Iterates over each group (Assortment or StoreType) and calls a model 
  # function to make forecasts on each store, using grouped information.
  #
  # args:
  #  train - a data.table containing the data from train.csv, or part of it
  #  test - a data.table like that returned by raw_test()
  #  group - string, one of "Assortment" or "StoreType"
  #  fname - a string specifying which model function to call
  #
  # returns:
  #  a data frame corresponding to the test parameter, but with all of the 
  #  predictions in the Sales field
  
  FNAMES <- c('stlf_svd')
  
  if(fname %in% FNAMES){
    f <- get(fname)
  }else{
    stop(fname,' not legal forecast option')
  }
  ## impute first
  if(fname %in% c("stlf_svd")) {
    train = impute_missing_dates(train)
  }
  
  stores = raw_store()
  train = merge(train, stores[ , .(Store, StoreType, Assortment)], by="Store")
  test = merge(test, stores[ , .(Store, StoreType, Assortment)], by="Store")
  
  test.dates = unique(test$Date)
  ndates.test = length(test.dates) # number of dates to predicted in test
  stores.test = unique(test$Store) # stores in test
  
  ## forecast per store, grouped by StoreType or Assortment
  count = 0
  if(group=="StoreType") {
    test.StoreTypes = unique(test$StoreType)
    
    fcs_list = vector(mode="list", length=length(test.StoreTypes))
    for(storetype in test.StoreTypes) {
      print(paste0("StoreType: ", storetype))
      count = count + 1
      
      ## create train and test matrices of sales (# dates) x (# stores + 1)
      train.g = sales_matrix(train[StoreType == storetype])
      test.g = as.data.frame(matrix(0, ncol=ncol(train.g), nrow=ndates.test))
      names(test.g) = names(train.g)
      test.g[ ,1] = test.dates
      
      # predictions for all the stores in train
      pred = f(train=train.g, test=test.g, ...)
      
      # predictions for just the stores in test
      pred.test = pred[ , c(1,which(names(pred) %in% stores.test))]
      
      # tidy data frame of pred_submit with columns Store, Date, predSales
      pred.tidy = data.table(gather(pred.test, key="Store", value="predSales", -1))
      pred.tidy$Store = as.integer(levels(pred.tidy$Store))[pred.tidy$Store]
      pred.tidy = merge(pred.tidy, test, by=c("Store","Date"))
      
      fcs_list[[count]] = pred.tidy
    }
  } else if(group=="Assortment") {
    test.Assortments = unique(test$Assortment)
    
    fcs_list = vector(mode="list", length=length(test.Assortments))
    for(assort in test.Assortments) {
      print(paste0("Assortment: ", assort))
      count = count + 1
      
      ## create train and test matrices of sales of dimension: (# dates) x (# stores)
      train.g = sales_matrix(train[Assortment == assort])
      test.g = as.data.frame(matrix(0, ncol=ncol(train.g), nrow=ndates.test))
      names(test.g) = names(train.g)
      test.g[ ,1] = test.dates
      
      # predictions for all the stores in train
      pred = f(train=train.g, test=test.g, ...)
      
      # predictions for just the stores in test
      pred.test = pred[ , c(1,which(names(pred) %in% stores.test))]
      
      # tidy data frame of pred_submit with columns Store, Date, predSales
      pred.tidy = data.table(gather(pred.test, key="Store", value="predSales", -1))
      pred.tidy$Store = as.integer(levels(pred.tidy$Store))[pred.tidy$Store]
      pred.tidy = merge(pred.tidy, test, by=c("Store","Date"))
      
      fcs_list[[count]] = pred.tidy
    }
  }
   fcs = rbindlist(fcs_list)
   
   if('Sales' %in% names(test)){
     print(paste("RMSPE:",
                 RMSPE(truth=as.vector(fcs$Sales), pred=as.vector(fcs$predSales))))
   }
   return(fcs)
}

individual_forecast <- function(train, test, fname, ...){
  # Iterates over each store and calls a model function to make forecasts
  # on each.
  #
  # args:
  #  train - a data.table containing the data from train.csv, or part of it
  #  test - a data.table like that returned by raw_test()
  #  fname - a string specifying which model function to call
  #
  # returns:
  #  a data frame corresponding to the test parameter, but with all of the 
  #  predictions in the Sales field
  
  FNAMES <- c('tslm_mod',
              'stlm_mod',
              'seasonal_naive',
              'medians',
              'August2015_kluge',
              'August2014_kluge')
  
  if(fname %in% FNAMES){
    f <- get(fname)
  }else{
    stop(fname,' not legal forecast option')
  }
  #if('Sales' %in% names(test)){
  #  test <- subset(test, select=-Weekly_Sales)
  #}
  
  ## impute first
  if(fname %in% c("seasonal_naive")) train = impute_missing_dates(train)
  
  ## forecast per store
  fcs_list = vector(mode="list", length=length(unique(test$Store)))
  count = 0
  for(store in unique(test$Store)) {
    count = count + 1
    fcs_list[[count]] = f(x=train[.(store)], test=test[.(store)], ...)
  }
  fcs = rbindlist(fcs_list)
  fcs = fcs[!is.na(fcs$predSales), ]
  
  if('Sales' %in% names(test)){
    print(paste("RMSPE:",
                RMSPE(truth=as.vector(fcs$Sales), pred=as.vector(fcs$predSales))))
  }
  return(fcs)
}


tslm_mod = function(x, freq=14, model.type=c("basic","plus"), test = NULL,
                    xtra_reg = F) {
  # Model with linear regression including trend and seasonality ("basic") and
  # optionally including regressors ("plus")
  #
  # args:
  # x - a data.table containing a column "Sales" and columns named in xreg
  # freq - an integer, the number of data points per season
  # model.type - string, either "basic" (no extra regressors) or "plus" (include regressors)
  # test - a data.table, if supplied, return predictions for Sales of test
  # xtra_reg - bool, include engineered features in regression? (only for model.type=="plus")
  # 
  # returns:
  # tslm model if test is NULL.  Otherwise, data.table of predictions on test
  
  Sales <- ts(x$Sales, frequency = freq, start=c(1, 1))
  if(model.type == "basic") {
    model <- tslm(Sales ~ trend + season)
  } else if (model.type == "plus") {
    # vector of regressors (column names in x) not including trend and season
    xreg = c("DayOfWeek", "Open", "Promo", "StateHoliday", "SchoolHoliday")
    
    #Sales <- ts(x$Sales, frequency = freq)
    DayOfWeek <- x$DayOfWeek
    Open <- x$Open
    Promo <- x$Promo
    StateHoliday <- x$StateHoliday
    SchoolHoliday <- x$SchoolHoliday
    
    if(xtra_reg) {
      # include extra engineered regressors
      xreg_xtra = c("Xmas", "preClose_short", "preClose_long2", "preClose_long1", "postClose")
      Xmas <- x$Xmas
      preClose_short <- x$preClose_short
      preClose_long2 <- x$preClose_long2
      preClose_long1 <- x$preClose_long1
      postClose <- x$postClose
      
      # formula for tslm model
      formula = as.formula(paste0("Sales ~ ",
                                  paste(c("trend", "season", xreg, xreg_xtra), collapse = "+")))
    } else {
      # formula for tslm model
      formula = as.formula(paste0("Sales ~ ",
                                  paste(c("trend", "season", xreg), collapse = "+")))
    }
    model <- tslm(formula)
  }

  if(!is.null(test)) {
    # return predictions on new data
    fc = forecast(model, newdata=test)
    return(data.table(test, predSales = as.numeric(fc$mean)))
  } else {
    # return model (fit)
    return(model)
  }
}


stlm_mod = function(x, test=NULL, freq=14, s.window=7, 
                    model.type=c("ets", "arima"), incl.xreg=FALSE) {
  # STL decomposition with method = 'ets' or 'arima' for fitting the seasonally
  # adjusted series
  #
  # args:
  # x - a data.table containing a column "Sales"
  # test - a data.table, if supplied, return predictions for Sales of test
  # freq - an integer, the number of data points per season
  # incl.xreg - bool, include x regressors if model.type = "arima"
  #
  # returns:
  # stlm model if test is NULL.  Otherwise, data.table of predictions on test
  Sales = ts(x$Sales, frequency = freq)
  
  if(!is.null(test)) {
    # return predictions on new data
    if(!incl.xreg) {
      # ets model or arima w/o xreg
      fc = stlf(Sales, h = nrow(test), method = model.type, s.window = s.window)
    } else {
      # arima with xreg
      fc = stlf(Sales, method = model.type, s.window = s.window, h=nrow(test),
                xreg = model_matrix(x), newxreg = model_matrix(test), robust=F)
    }
    return(data.table(test, predSales = as.numeric(fc$mean)))
    
  } else {
    
    # return model (fit)
    if(model.type == "ets") {
      model = stlm(Sales, method = "ets", s.window = s.window)
    } else if (model.type == "arima") {
      if(incl.xreg) {
        xreg = model_matrix(x)
        model = stlm(Sales, method = "arima", s.window = s.window, xreg = xreg)
      } else {
        model = stlm(Sales, method = "arima", s.window = s.window)
      }
    }
    return(model)
  }
}

seasonal_naive <- function(x, test, freq=365, shift=2){
  # Computes seasonal naive forecasts based on sales from the
  # same day one year ago (day - 365 + shift), where the shift is applied to 
  # retrieve sales matching the same DayOfWeek (rather than exact date a year ago)
  #
  # args:
  # x - a data.table containing a column "Sales"
  # test - a data.table
  #
  # returns:
  #  data.table of predictions on test 
  
  h <- nrow(test)
  idxs2014 = (nrow(x) - (freq:1) + shift)[1:h]
  idxs2013 = (nrow(x) - ((freq*2):1) + shift + 1)[1:h]
  sales2014 = x[idxs2014, Sales]
  sales2013 = x[idxs2013, Sales]
  pairwise_max_sales = mapply(function(x,y) {max(x,y)}, 
                              sales2013, sales2014,
                              SIMPLIFY = T)
  
  # need to impute missing values (if dayofweek!=7 and historicSales==0, replace
  # with median)
  return(data.table(test, predSales = pairwise_max_sales))
}


August2015_kluge <- function(x, test){
  # Computes seasonal naive forecasts based on 14-day seasonality.
  # From Aug 1 to the first Sunday in Aug, straightforwardly obtain
  # sales from the previous 14 days.  
  # Then, starting from the 1st Monday in Aug, repeat the 2-week-long cycle beginning
  # 3 weeks before the 1st Sunday. (Pattern does not work well for the few stores 
  # that are always open.)
  #
  # args:
  # x - a data.table containing a column "Sales"
  # test - a data.table
  #
  # returns:
  #  data.table of predictions on test 
  
  h <- nrow(test)
  
  testdate.start = test[1,Date]
  
  ## if flag_closure contains any closures > 1 in the cycle_dates period of train, or
  ## if flag_closure contaings any closures > 1 in the test period,
  ## do not perform kluge.
  
  # extract dates for two-week cycle used for forcasting
  cycle_dates = testdate.start + c(1)*days(1) - c(21:8)*days(1)
  
  train_has_closures = (any(x[Date %in% c(cycle_dates[1] - c(20:1)*days(1), cycle_dates), 
                             flag_closure] > 1) | 
                          sum(x[Date %in% cycle_dates,Sales])==0)
  
  test_has_closures = (any(test[, flag_closure] > 1) | 
                         ((rle(test$Open)$values[1] == 0) & (rle(test$Open)$lengths[1] > 1)))
  
  if(train_has_closures | test_has_closures) {
    # don't make fit
    return(data.table(test, predSales = NA))
  } else {
    # forecast for Aug1 based on training set Sale from 2 weeks prior
    fc_Aug1 = x[Date == (testdate.start - c(14)*days(1)), Sales]
    
    # extract sales from two-week cycle for forecasting test Sales
    cycle_sales = rep(x[Date %in% cycle_dates, Sales], length.out = h-1)
    
    return(data.table(test, predSales = c(fc_Aug1, cycle_sales)))
  }
}

August2014_kluge <- function(x, test){
  # Computes seasonal naive forecasts based on 14-day seasonality.
  # From Aug 1 to the first Sunday in Aug, straightforwardly obtain
  # sales from the previous 14 days.  
  # Then, starting from the 1st Monday in Aug, repeat the 2-week-long cycle beginning
  # 3 weeks before the 1st Sunday. (Pattern does not work well for the few stores 
  # that are always open.)
  #
  # args:
  # x - a data.table containing a column "Sales"
  # test - a data.table
  #
  # returns:
  #  data.table of predictions on test 
  
  h <- nrow(test)
  
  testdate.start = test[1,Date]
  
  ## if flag_closure contains any closures > 1 in the cycle_dates period of train, or
  ## if flag_closure contaings any closures > 1 in the test period,
  ## do not perform kluge.
  
  # extract dates for two-week cycle used for forcasting
  cycle_dates = testdate.start + c(2)*days(1) - c(21:8)*days(1)
  
  train_has_closures = (any(x[Date %in% c(cycle_dates[1] - c(20:1)*days(1), cycle_dates), 
                             flag_closure] > 1) | 
                          sum(x[Date %in% cycle_dates,Sales])==0)
  test_has_closures = (any(test[, flag_closure] > 1) | 
                         ((rle(test$Open)$values[1] == 0) & (rle(test$Open)$lengths[1] > 1)))
  
  if(train_has_closures | test_has_closures) {
    # don't make fit
    return(data.table(test, predSales = NA))
  } else {
    # forecast for Aug1 based on training set Sale from 2 weeks prior
    fc_Aug1 = x[Date == (testdate.start - c(14)*days(1)), Sales]
    fc_Aug2 = x[Date == (testdate.start - c(13)*days(1)), Sales]
    
    # extract sales from two-week cycle for forecasting test Sales
    cycle_sales = rep(x[Date %in% cycle_dates, Sales], length.out = h-2)
    
    return(data.table(test, predSales = c(fc_Aug1, fc_Aug2, cycle_sales)))
  }
}

medians = function(x, test) {
  # args:
  # x - a data.table containing a column "Sales"
  # test - a data.table
  #
  # returns:
  #  data.table of predictions on test 
  median_sales = group_medians(x)
  
  pred = merge(test, median_sales,
               by=c("Store", "DayOfWeek", "Promo","SchoolHoliday"), 
               all.x=T)[order(Date)]
  #print(pred)
  pred[ , predSales := med_Sales]
  pred[ , med_Sales := NULL]
  pred[is.na(predSales), predSales := 0]
  return(pred)
}

stlf_svd <- function(train, test, model.type, n.comp, freq=14, s.win=7,incl.xreg=FALSE ){
  # Replaces the training data with a rank-reduced approximation of itself,
  # then forecasts each store using stlf() from the forecast package.
  # That function performs an STL decomposition on each series, seasonally
  # adjusts the data, non-seasonally forecasts the seasonally adjusted data,
  # and then adds in the naively extended seasonal component to get the
  # final forecast.
  #
  # args:
  # train - A matrix of Sales values from the training set of dimension
  #         (number of days in training data) x (number of stores in the group +1)
  # test - An all-zeros matrix of dimension:
  #       (number of days in test data) x (number of stores in the group +1)
  #       The forecasts are written in place of the zeros.
  # model.type - one of 'ets' or 'arima', specifies which type of model to
  #        use for the non-seasonal forecast
  # n.comp - the number of components to keep in the singular value
  #         decomposition that is performed for preprocessing
  #
  # returns:
  #  the test(forecast) data frame with the forecasts filled in 
  #  (number of days in test data) x (number of stores in the group +1)
  horizon <- nrow(test)
  train <- preprocess.svd(train, n.comp) 
  for(j in 2:ncol(train)){
    s <- ts(train[, j], frequency=freq)
    if(model.type == 'ets'){
      fc <- stlf(s, 
                 h=horizon, 
                 s.window=s.win, 
                 method='ets',
                 ic='bic', 
                 opt.crit='mae')
    }else if(model.type == 'arima' & !incl.xreg){
      fc <- stlf(s, 
                 h=horizon, 
                 s.window=s.win, 
                 method='arima',
                 ic='bic')
    }else if(model.type == 'arima' & incl.xreg){
      fc <- stlf(s, 
                 h=horizon, 
                 s.window=s.win, 
                 method='arima',
                 ic='bic',
                 xreg = model_matrix(x),
                 newxreg = model_matrix(test))
    }else{
      stop('Model type must be one of ets or arima.')
    }
    pred <- as.numeric(fc$mean)
    test[, j] <- pred
  }
  test
}

preprocess.svd <- function(train, n.comp){
  # Replaces the training data with a rank-reduced approximation of itself.
  # This is for noise reduction. The intuition is that characteristics
  # that are common across stores (within the same group) are probably
  # signal, while those that are unique to one store may be noise.
  #
  # args:
  # train - A data frame of Sales values from the training set of dimension
  #         (number of days in training data) x (number of stores + 1), where
  #         the 1st column is a Date column
  # n.comp - the number of components to keep in the singular value
  #         decomposition
  #
  # returns:
  #  the rank-reduced approximation of the training data
  
  #train[is.na(train)] <- 0
  n.comp = min(n.comp, floor((ncol(train)-1)/2))
  z <- svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
  s <- diag(z$d[1:n.comp])
  train[, 2:ncol(train)] <- z$u %*% s %*% t(z$v)
  train
}