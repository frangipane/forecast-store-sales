## tools.R
source('utils.R')

subtrain_val = function(train, val_size = 48, start = NULL, end = NULL) {
  # Further split the training set for all stores in train into a smaller training set 
  # and validation set.
  #
  # args:
  #  val_size - An integer, the number of days counting back from the 
  #    latest date in train[.(1)] to be used as the validation set
  #  start - (optional) String specifying start date for validation set
  #    in the form of %Y-%m-%d.  Training subset ends the day before 
  #    start parameter.
  #  end - (optional) String specifying end date for validation set
  #    in the form of %Y-%m-%d.  If start is specified and end is not, then
  #    end defaults to the last date of the training set
  #
  # returns:
  #  tr - the subsetted training set
  #  val - the subsetted validation set
  
  i = 1 #store 1
  if(is.null(start)) {
    start = (train[.(i), .(Date), mult = "last"] - val_size + 1)$Date
    tr = train[Date < start]
    val = train[Date >= start]
  } else if (is.null(end)) {
    start = as.Date(start)
    tr = train[Date < start]
    val = train[Date >= start]
  } else {
    start = as.Date(start)
    tr = train[Date < start]
    val = train[(Date >= start) & (Date <= as.Date(end))]
  }
  list(tr = tr, val = val)
}

get_subtrain_val = function(i, val_size = 48, start = NULL, end = NULL) {
  # Further split the training set for stores in vector i into a smaller training set 
  # and validation set.
  #
  # args:
  #  i - a vector of store ids (integers)
  #  val_size - An integer, the number of days counting back from the 
  #    latest date in train[.(i)] to be used as the validation set
  #  start - (optional) String specifying start date for validation set
  #    in the form of %Y-%m-%d.  Training subset ends the day before 
  #    start parameter.
  #  end - (optional) String specifying end date for validation set
  #    in the form of %Y-%m-%d.  If start is specified and end is not, then
  #    end defaults to the last date of the training set
  #
  # returns:
  #  tr - the subsetted training set for store i
  #  val - the subsetted validation set for store i
  if(is.null(start)) {
    start = (train[.(i), .(Date), mult = "last"] - val_size + 1)$Date
    tr = train[.(i)][Date < start]
    val = train[.(i)][Date >= start]
    #tr = subset(train[.(i)], Date <= train[.(i), .(Date), mult = "last"] - val_size)
    #val = subset(train_one, Date > train[.(i), .(Date), mult = "last"] - val_size)
  } else if (is.null(end)) {
    tr = train[.(i)][Date < as.Date(start)]
    val = train[.(i)][Date >= as.Date(start)]
  } else {
    tr = train[.(i)][Date < as.Date(start)]
    val = train[.(i)][(Date >= as.Date(start)) & (Date <= as.Date(end))]
  }
  list(tr = tr, val = val)
}

get_date_index = function(x, datei) {
  # Get the index of the row in x that corresponds to Date == datei
  # (assumes that each row has a unique date)
  #
  # args:
  # x - a data.table with column "Date"
  # datei - string, in "%y-%m-%d" format
  #
  # returns:
  # idx - an integer index of a row in the data.table
  idx = which(x[, Date] == as.Date(datei))
  idx
}

plot_ts = function(x, start=NULL, end=NULL, title="", ...) {
  # Plot sales time-series for a window of time
  #
  # args:
  # x - a data.table with column "Sales" and "Date"
  # start - (optional) string, start date in "%y-%m-%d" format
  # end - (optional) string, end date in "%y-%m-%d" format
  # title - string, title to give plot
  par(mfrow=c(1,1))
  if(!is.null(start) & !is.null(end)) {
    # get row indices corresponding to start and end dates
    s_idx = get_date_index(x, start)
    e_idx = get_date_index(x, end)
    
    print(plot(x[s_idx:e_idx, Date], x[s_idx:e_idx, Sales], type='l', 
               ylab="sales", xlab="time", main=title, ...))
  } else {
    # if no start and end date are provided, plot entire series
    print(plot(x[,Date], x[,Sales], type='l', ylab="sales", xlab="time", 
               main=title, ...))
  }
}

plot_DayOfWeek_overlay = function(x, start="2013-01-01", end="2013-05-01", title="") {
  # Plot time series of Sales and color-coded DayOfWeek label per timepeoint
  #
  # args:
  # x - a data.table with column "Sales" and "Date"
  # start - string, start date in "%y-%m-%d" format
  # end - string, end date in "%y-%m-%d" format
  # title - string, title to give plot
  s_idx = get_date_index(x, start)
  e_idx = get_date_index(x, end)
  
  plot_ts(x, start=start, end=end, title=title)
  print(points(x[s_idx:e_idx, Date], x[s_idx:e_idx, Sales], 
               pch=as.character(x$DayOfWeek[s_idx:(s_idx+6)]), 
               cex=1.25, font=3, col=1:7))
}

plot_fit = function(fit, x, start=NULL, end=NULL, title="") {
  # Plot fitted model on top of time-series used to train model
  #
  # args:
  # fit - model returned by a fit to data.table x, e.g. via a call to tslm
  # x - a data.table with column "Sales" and "Date"
  # start - string, start date in "%y-%m-%d" format
  # end - string, end date in "%y-%m-%d" format
  # title - string, title to give plot
  par(mfrow=c(1,1))
  if(is.null(start) & is.null(end)) {
    # if no start/end dates provided, default to first 100 dates in x
    start = x[1, Date]
    end = x[min(100, nrow(x)), Date]
  }
  s_idx = get_date_index(x, start)
  e_idx = get_date_index(x, end)
  
  plot_ts(x, start=start, end=end, title=title)
  print(lines(x[s_idx:e_idx, Date], fitted(fit)[s_idx:e_idx], col="red", lty=2))
  
#   if (class(fit)=="lm") {
#      print(lines(x[s_idx:e_idx, Date], fitted(fit)[s_idx:e_idx], col="red", lty=2))
#   } else if(class(fit)=="stlm") {
#      # plot the fit to the seasonally adjusted series
#      print(lines(x[s_idx:e_idx, Date], fitted(fit$model)[s_idx:e_idx], col="red", lty=2))
#   }
}

summarize_fit = function(fit, x, title="",...) {
  # Summarize fit to a time series
  #
  # args:
  # fit - a time series model
  # x - data.table, the data used to fit the time series model
  # title - string, title for plots (e.g. the model name used for the fit)
  print(summary(fit))
  plot_fit(fit, x, title=paste0("(fit) ", title),...)
  tsdisplay(residuals(fit), main=paste0("residuals (fit): ", title))
}

summarize_forecast = function(fc, x, title="",...) {
  # Summarize forecast for validation set
  #
  # args:
  # fc - object of class "forecast" over the time period in x
  # x - data.table, validation data containing true sales
  plot_forecast(fc, x, title=paste0("(forecast) ", title),...)
  tsdisplay(x$Sales - fc$mean, main=paste0("residuals (forecast): ", title))
  print(paste0("RMSPE: ", RMSPE(truth = as.vector(x$Sales), pred = as.vector(fc$mean))))
}

get_forecast = function(fit, xnew=NULL, h=48, ...) {
  # return forecast for time series model
  #
  # args:
  # fit - time series model
  # h - horizon to forecast
  # xnew - any data needed for the model, e.g. regressors in the horizon
  #
  # returns:
  # fc - object of class "forecast"
  if(is.null(xnew)) {
    # model does not require external regressors
    fc = forecast(fit, h=h, ...)
  } else {
    # model requires regressors from the period of the horizon.
    #if(!is.null(modeltype)) { }
    
    # the horizon length, is set by the number of timepoints in xnew
    fc = forecast(fit, h=nrow(xnew), newdata=xnew, ...)
  }
  fc
}

plot_forecast = function(fc, x=NULL, title="") {
  # Plot the forecast.  If x is provided, overlay plot with the true data.
  #
  # args:
  # fc - object of class "forecast"
  # x - (optional) data.table including with column "Sales" containing
  #     true daily sales over the forecast period and column "Date"
  
  if(!is.null(x)) {
    if(nrow(x) != length(fc$mean)) {
      stop("nrow(x) != forecast horizon")
    }
    plot_ts(x, title=title)
    print(lines(x[ , Date], fc$mean, col="red", lty=2))
  } else {
    h = length(fc$mean)
    print(plot(seq(h), fc$mean, type='l', ylab="sales", xlab="time", main=title))
  }
}

fit_and_forecast = function(x, val, model=c("tslm", "stlm"), model.type = NULL, ...) {
  # Fit model to data, then forecast on time period over val set
  #
  # args:
  # x - data.table, used for fitting model
  # val - data.table, used for forecast and evaluation of RMSPE
  # modeltype - string, the name of the model to use
  # start - (optional) string, start date in "%y-%m-%d" format for 
  #         plot of fit
  # end - (optional) string, end date in "%y-%m-%d" format for 
  #         plot of fit
  
  if (model == "tslm") {
    # model.type = c("basic", "plus")
    fit = tslm_mod(x, model.type = model.type)
    summarize_fit(fit, x, title = paste(model, model.type), ...)
    fc = forecast(fit, newdata=val, ...)
  } else if (model == "stlm") {
    # model.type = c("ets", "arima")
    fit = stlm_mod(x, model.type = model.type, incl.xreg=T)
    summarize_fit(fit$model, x, title = paste(model, model.type), ...)
    fc = forecast(fit, h=nrow(val), xreg=model_matrix(val), ...)
  }
  summarize_forecast(fc, val, title=paste(model, model.type))
}

RMSPE_per_store = function(pred) {
  # Calculate RMSPE per store ID
  #
  # args:
  # pred - a data.table containing columns Id, Sales, and predSales (the forecasted sales)
  #
  # returns:
  # a data.table with columns Id and RMSPE, sorted in descending order of RMSPE
  
  rmse_by_store = pred[ , .(rmspe = RMSPE(truth=as.vector(Sales), pred=as.vector(predSales))),
        by = Store][order(-rmspe)]
}

plot_frequency = function(x, store) {
  # Find and plot frequencies (i.e. the seasonal period) per store, 
  # disregarding closed dates.
  #
  # args:
  # x - data.table containing columns Store and Sales
  # store - data.table containing columns Store, StoreType, and Assortment
  #
  # returns:
  # freqs - a data.table with two columns: Store, freqs
  
  freq_per_store = x[Open==1, .(freq=findfrequency(Sales)), by=Store]
  freqs = merge(freq_per_store, store[.(Store, StoreType, Assortment)], by="Store")
  print(ggplot(freqs, aes(freq, fill=StoreType)) + 
          geom_histogram(binwidth=1) +
          xlim(0, 30) + 
          ggtitle("frequency"))
  print(ggplot(freqs, aes(freq, fill=Assortment)) + 
          geom_histogram(binwidth=1) +
          xlim(0, 30) +
          ggtitle("frequency"))
  return(freqs)
}

plot_closures = function(x) {
  # calculate run lengths of consecutive closed days per store, and
  # plot histogram of closure lengths for all stores
  #
  # args:
  # x - a data.table with columns: Store, Open
  
  rle_per_store = closure_runlengths(x)
  rles = unlist(rle_per_store)
  summary(rles)
  par(mfrow=c(1,1))
  hist(rles[rles>2], right=F, breaks=seq(186), 
       main="lengths of contiguous store closures \n (closures longer than 2 days)",
       xlab="store closure length")
}

## in training set:
#     a   b   c (Assortment)
# a 381   0 221
# b   7   9   1
# c  77   0  71
# d 128   0 220
# (StoreType)

## in test set:
#     a   b   c
# a 264   0 197
# b   2   9   1
# c  45   0  44
# d 112   0 182

get_imputed = function() {
  # Loads the imputed data as a data.table object
  tr_imp <- fread(file.path(paths$data, 'tr_imp.csv'), stringsAsFactors = T)
  tr_imp[, ':=' (Sales = as.numeric(Sales),
                DayOfWeek = as.factor(DayOfWeek),
                Date = as.Date(Date),
                SchoolHoliday = as.factor(SchoolHoliday),
                StateHoliday = as.factor(StateHoliday))]
  tr_imp <- tr_imp[order(Store,Date)]
  setkey(tr_imp, Store)
}

get_medians = function() {
  # Loads the store data as a data.table object
  medians <- fread(file.path(paths$data, 'medians.csv'), stringsAsFactors = T)
  medians[, ':=' (DayOfWeek = as.factor(DayOfWeek),
                 SchoolHoliday = as.factor(SchoolHoliday),
                 med_Sales = as.numeric(med_Sales))]
  medians <- medians[order(Store, DayOfWeek, Promo)]
  setkey(medians, Store)
}

plot_ts_around_closures = function(x) {
  # plot example time series around lengthy closures (>2) for stores with such closures
  x = add_features_all(x)
  x = x[(flag_closure > 7) & (flag_closure < 18)]
  
  for (i in 1:floor(nrow(x)/4)) {
    store = x$Store[i]
    start = as.character(max(x$Date[i] - c(20)*days(1), as.Date("2013-01-01")))
    end = as.character(x$Date[i] + c(x$flag_closure[i] + 20)*days(1))
    print(plot_ts(tr[.(store)], start = start, end=end, 
                  title=paste0("store: ", as.character(store), ", date: ", as.character(x$Date[i]))))
  }
}
