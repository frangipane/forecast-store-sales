## utils.R

library("data.table")
library("zoo")
library("lubridate")
library("forecast")
library("ggplot2")
library("lmtest")
library("plyr")
library('tseries')
library('tidyr')

paths = list(data='../data',
             submit='../submissions',
             r='../R')

raw_train <- function() {
  # Loads the training data as a data.table object
  train <- fread(file.path(paths$data, 'train.csv'), stringsAsFactors = T)
  train[, ':=' (Sales = as.numeric(Sales),
                DayOfWeek = as.factor(DayOfWeek),
                Date = as.Date(Date),
                SchoolHoliday = as.factor(SchoolHoliday),
                StateHoliday = as.factor(StateHoliday))]
#                SchoolHoliday = as.integer(levels(SchoolHoliday))[SchoolHoliday])]
  train <- train[order(Store,Date)]
  setkey(train, Store)
  train
}

raw_test <- function() {
  # Loads the test data as a data.table object
  test <- fread(file.path(paths$data, 'test.csv'), stringsAsFactors = T)
  test[, ':=' ( DayOfWeek = as.factor(DayOfWeek),
                Date = as.Date(Date), 
                SchoolHoliday = as.factor(SchoolHoliday),
                StateHoliday = as.factor(StateHoliday))]
#                SchoolHoliday = as.integer(levels(SchoolHoliday))[SchoolHoliday])]
  test <- test[order(Store,Date)]
  setkey(test, Store)
  
  #Competition admin Florian Knauer says the missing Open values for store 622 
  # should be 0: https://www.kaggle.com/c/rossmann-store-sales/forums/t/17048/putting-stores-on-the-map?page=2."
  test[is.na(Open), Open := 0]
  
  test
}

raw_store <- function() {
  # Loads the store data as a data.table object
  store <- fread(file.path(paths$data, 'store.csv'), stringsAsFactors = T)
  setkey(store, Store)
}

add_features <- function(x) {
  # Add engineered features
  #
  # args:
  # x - data.table for a single store
  #
  # returns:
  # x - data.table with added feature columns

  ## increasing sequence of integers between two weeks before Dec 23
  Xmas = function(z) {
    pulseLen = 14 # pulse length of pre-Xmas run, consider extending it (see store 1, 2014)
    pulseEnd = 23 # Date to end Xmas (monotonic increasing) pulse, inclusive
    z[ , Xmas := as.integer(0)]
    z[ ((month(Date)==12) & (day(Date) <= pulseEnd) & (day(Date) > (pulseEnd-pulseLen))), 
       Xmas := seq(1, pulseLen)]
    return(z)
  }
  
  ## helper function for adding __Closure features.
  ## add a column flagging closure length for the start of each closure period
  flag_closure = function(z) {
    rls = rle(z[,Open])
    nruns = length(rls$values)
    
    # row indices of each new change of Open status, i.e. from 0->1 or 1->0
    idx_change_open = (cumsum(rls$lengths)+1)[1:(nruns-1)]
    
    # vector of row indices for start of closures, i.e. the changes from 1->0
    idx_closure = idx_change_open[rls$values[2:nruns]==0]
    
    if(length(idx_closure)!=0) {
      # values of run lengths for closures
      if(rls$values[1]==0) {
        rl_closure = rls$lengths[rls$values == 0][-1]
      } else {
        rl_closure = rls$lengths[rls$values == 0]
      }
      
      # flag closure (zero if open, otherwise, the run length of closure on start-date of closure)
      flag = rep(0, nrow(z))
      flag[idx_closure] = rl_closure
      return(data.table(z, flag_closure = as.integer(flag)))
    } else {
      # the store is closed only on the first day (the first row)
      return(data.table(z, flag_closure = 0))
    }
  }
  
  ## add 
  closure_pulse = function(z) {
    z = flag_closure(z) # add flag_closure column
    
    # pre-closure:
    # 1 two-day long pulse immediately before short closures (1 < flag_closure <= 4)
    pulseLen = 2
    z[ , preClose_short := as.integer(0)]
    if(length(z[ (flag_closure > 1) & (flag_closure <= 4), Date]) > 0) {
      close_dates = z[ (flag_closure > 1) & (flag_closure <= 4), Date]
      pulse_dates = sort(do.call(c, lapply(close_dates, function(x) {x - c(1:pulseLen)*days(1)})))
      z[Date %in% pulse_dates, preClose_short := seq(pulseLen)]
    }
    
    # 1 five-day long pulse immediately before longer closures (flag_closure >= 7)
    pulseLen = 5
    z[ , preClose_long2 := as.integer(0)]
    if(length(z[ flag_closure >= 7, Date]) > 0) {
      close_dates = z[ flag_closure >= 7, Date]
      pulse_dates = sort(do.call(c, lapply(close_dates, function(x) {x - c(1:pulseLen)*days(1)})))
      z[Date %in% pulse_dates, preClose_long2 := seq(pulseLen)]
    }
    
    # 1 seven-day long pulse before longer closures (flag_closure >= 7), 
    # beginning 12 days in advance of closure
    pulseLen = 7
    adv_days = 12
    z[ , preClose_long1 := as.integer(0)]
    if(length(z[ flag_closure >= 7, Date]) > 0) {
      pulse_start = z[ flag_closure >= 7, Date] - days(adv_days)
      pulse_dates = sort(do.call(c, lapply(pulse_start, function(x) {x + c(0:(pulseLen-1))*days(1)})))
      z[Date %in% pulse_dates, preClose_long1 := seq(pulseLen)]
    }
    
    # post-closure
    # 1 four-day long pulse immediately after a longer closure (flag_closure >= 7)
    pulseLen = 4
    z[ , postClose := as.integer(0)]
    if(length(z[ flag_closure >= 7, Date]) > 0) {
      pulse_start = z[ flag_closure >= 7, Date] + days(z[flag_closure >= 7, flag_closure])
      pulse_dates = sort(do.call(c, lapply(pulse_start, function(x) {x + c(0:(pulseLen-1))*days(1)})))
      z[Date %in% pulse_dates, postClose := seq(pulseLen)]
    }
    
    return(z)
  }
  
  x = Xmas(x)
  if(any(x$Open == 0, na.rm=T)) {
    x = closure_pulse(x)
  } else {
    # some stores have 0 closures
    x[ , ':=' (flag_closure = 0, 
               preClose_short= 0, 
               preClose_long2 =0, 
               preClose_long1 =0,
               postClose =0)]
  }
  x
}

add_features_all = function(x) {
  # Add engineered features to all stores
  #
  # args:
  # x - data.table for multiple stores
  #
  # returns:
  # x - data.table with added feature columns
  count = 0
  x_list = vector(mode="list", length=length(unique(x$Store)))
  for(store in unique(x$Store)) {
    count = count + 1
    x_list[[count]] = add_features(x[.(store)])
  }
  x = rbindlist(x_list)
  print(nrow(x))
  setkey(x, Store)
}

RMSPE = function(truth, pred) {
  # Calculate the evaluation metric for Rossmann Kaggle, the
  # Root Mean Square Percentage Error:
  # RMSPE = sqrt{(1/N) SUM_i[ ((y_i - y_pred_i)/y_i)^2 ]},
  # where y_i = sales at time i, y_pred_i are predicted sales at time i,
  # and N is the number of time points predicted.  Cases where y_i = 0 are
  # not included in the calculation.
  #
  # args:
  #  truth - A vector or data frame with single column containing true sales
  #       (number of time points)  ( x optional (1) )
  #  pred - A vector or data frame with single column containing predicted sales
  #       (number of time points)  ( x optional (1) )
  #
  # returns:
  #  The root mean square percentage error for the provided sample
  
  if(!is.vector(truth) | !is.vector(pred)) stop("One of the inputs is not a vector.")
  notZero = (truth!=0)
  truth = truth[notZero]
  pred = pred[notZero]
  sqrt(sum(((truth-pred)/truth)^2)/length(pred))
}


model_matrix = function(x, freq=14, 
                       xreg = c("Open", "Promo", "StateHoliday", "SchoolHoliday")) {
  # Create a model matrix of regressors
  #
  # args:
  # x - a data.table containing a column "Sales" and columns named in xreg
  # freq - an integer, the number of data points per season
  # xreg - vector of regressors (column names in x) not including trend and season
  
  # formula to create model matrix
  formula.mat = as.formula(paste0("~", paste(xreg, collapse = "+")))
  mod.matrix = model.matrix(formula.mat, x[ , xreg, with=FALSE])
  #mod.matrix = cbind(mod.matrix, Sales = x$Sales) # add in Sales column
  
  # leave out intercept
  mod.matrix[,-1]
}

write.submission <- function(pred){
  # Writes a valid submission to paths$submit.
  #
  # args:
  #  pred - a data table with predictions in the Sales field
  #
  # returns:
  #  the submission number used
  
  subs <- dir(paths$submit)
  subs <- grep('submission[0-9]+(.csv)(.zip|.gz)?', subs, value=TRUE)
  nums <- gsub('submission','', gsub('(.csv)(.zip|.gz)?','', subs))
  if(length(nums) == 0){
    submission.number <- 1
  }else{
    submission.number <- max(as.numeric(nums)) + 1
  }
  ss <- data.frame(Id = pred$Id, Sales = pred$Sales)
  
  submit.path = paste0(paths$submit, 
                       '/submission', 
                       submission.number,
                       '.csv')
  print(paste('Writing to:', submit.path))
  write.csv(ss, file = submit.path, quote=FALSE, row.names=FALSE)
  submission.number
}

reload.submission <- function(submit.num){
  # Reloads a previously saved submission
  #
  # args:
  #  submit.num - the number of the submission
  #
  # returns:
  #  the saved submission as a data frame (with Id and Sales fields)
  submit.path <- paste0(paths$submit, '/submission', submit.num, '.csv')
  read.csv(submit.path)
}

sample.submission <- function(){
  # Loads the sample submission, which is used in writing predictions
  ss <- read.csv(paste0(paths$data, '/sample_submission.csv'))
}

make.average <- function(submissions, wts=NULL){
  # Averages previously saved submissions.
  #
  # args:
  #  submissions - a vector of submission numbers
  #  wts - optional vector of weights for submissions
  #
  # returns:
  #  a data frame with the weighted average of the Sales fields
  #  from the submissions as its Sales field
  if(is.null(wts)){
    wts <- rep(1, length(submissions))
  }
  pred <- sample.submission()
  for(k in 1:length(submissions)){
    sub.k <- reload.submission(submissions[k])
    sub.k = sub.k[order(sub.k$Id), ]  # shouldn't be necessary, submissions should be ordered already
    pred.k <- wts[k] * sub.k$Sales
    pred$Sales <- pred$Sales + pred.k
  }
  pred$Sales <- pred$Sales/sum(wts)
  pred
}

impute_missing_dates = function(x, imputeZeros=F) {
  # replaces missing dates in training set (particularly targeting missing block in 
  # 2nd half of 2014 for 180 stores that have 184 missing dates--note, 1 store also has 1 
  # missing date) with comparable dates from previous year
  #
  # args:
  # x - data.table, the full training set
  #
  # returns:
  # x - data.table with missing dates imputed
  #
  
  # first calculate medians, used for adjusting imputations (notably for
  # store 348, which has overlapping closures in consecutive years)
  medians = group_medians(x)
  
  # create continuous sequence of dates (using store 1 to determine how many
  # since it is complete)
  ndates = nrow(x[.(1)]) - 1
  
  seq_DayOfWeek = x[.(1), DayOfWeek]
  seq_Promo = x[.(1), Promo]
  seq_SchoolHoliday = x[.(1), SchoolHoliday]
  seq_Open = x[.(1), Open]
  seq_dates = data.table(Date=as.Date("2013-01-01")+ c(0:ndates)*days(1))
  
  dates_per_store = table(x[,Store])
  incomplete_stores = names(dates_per_store)[which(dates_per_store != length(seq_dates))]
  incomplete_stores = as.integer(incomplete_stores)
  
  imputed_list = vector(mode="list", length=length(incomplete_stores))
  count = 0
  for(store in incomplete_stores) {
    count = count + 1
    
    # left outer join
    all_dates = merge(seq_dates, x[.(store), ], by="Date", all.x = T)

    if(imputeZeros) {
      # impute zeros
      all_dates[is.na(Sales), Sales := 0]
    } else {
      # impute previous year's sales for all but store 988
      if(store != 988) {
        dates_for_imputing = all_dates[is.na(Sales), Date] - c(365)*days(1) + 1
        all_dates[is.na(Sales), Sales := all_dates[Date %in% dates_for_imputing, Sales]]
        # sometimes there are a few rare, remaining NA's when no medians or data
        # exist for the historical period:
        all_dates[is.na(Sales), Sales := median(Sales, na.rm=T)]
      } else if (store == 988) {
        # Store 988 is missing 2013-01-01, so the above method does not work.
        all_dates[is.na(Sales), Sales := 0]
      }
    }
    # fill in NA's with correct store id and DayOfWeek.
    # fill-ins for Promo and SchoolHoliday are coarse approximations ~ assume same
    # as for store #1.
    # Also assume all StateHolidays equal 0 so that medians can be used to fill in
    # (better than leaving med_Sales = NA).
    NA_idxs = which(is.na(all_dates[ , Promo]))
    all_dates[NA_idxs, ':=' (Store = store, 
                             DayOfWeek = seq_DayOfWeek[NA_idxs],
                             Promo = seq_Promo[NA_idxs],
                             SchoolHoliday = seq_SchoolHoliday[NA_idxs],
                             Open = seq_Open[NA_idxs],
                             StateHoliday = "0")]
    
    imputed_list[[count]] = all_dates
  }
  imputed = rbindlist(imputed_list)
  imputed = rbind(imputed, x[!.(incomplete_stores)])
  
  # replace imputations for stores where Sales == 0 but DayOfWeek != 7 and
  # StateHoliday==0 with medians (for not missing medians) for that particular
  # Store, DayOfWeek, Promo, SchoolHoliday
  imputed <- merge(imputed, medians,
                     by=c("Store", "DayOfWeek", "Promo","SchoolHoliday"), 
                     all.x=T)
  imputed[Sales==0 & DayOfWeek!=7 & StateHoliday==0, 
          Sales :=  ifelse(!is.na(med_Sales), med_Sales, median(Sales, na.rm=T))]
  imputed = imputed[order(Store,Date)]
  #imputed[, med_Sales := NULL]
  
  setkey(imputed, Store)
  return(imputed)
}

group_medians = function(x) {
  # args:
  # x - data.table, the full training set (not imputed) OR data.table for a subset of stores
  #
  # returns:
  # x - data.table, median sales per Store, DayOfWeek, Promo, and SchoolHoliday
  
  # remove rows where Sales == 0 (mostly days when closed, but 54 days when open),
  # as well as StateHoliday == b or c (not present in test set)
  x = x[!(Sales == 0 | (StateHoliday %in% c('b','c')))]
  
  medians = x[ , .(med_Sales = median(as.numeric(Sales))), 
               by = .(Store, DayOfWeek, Promo, SchoolHoliday)][order(Store, DayOfWeek, Promo)]
#   medians = ddply(x, .(Store, DayOfWeek, Promo, SchoolHoliday), 
#                   summarize, med_Sales = median(Sales))
  setkey(medians, Store)
  medians
}

closure_runlengths = function(x) {
  # calculate run lengths of consecutive closed days per store
  #
  # args:
  # x - a data.table with columns: Store, Open
  #
  # returns
  # rle_per_store - list of run lengths of closed days per store
  rle_closed = function(x) {
    rl = rle(x)
    return(rl$lengths[rl$values==0])
  }
  rle_per_store = dlply(x, .(Store), function(x) rle_closed(x$Open))
  return(rle_per_store)
}

sales_matrix = function(x) {
  # transform a long-form data.table into a matrix of sales of dimensions
  # (# dates) x (# sales).  this assumes each store in the long-form data.table has
  # the same number of dates
  #
  # args:
  # x - data.table with columns Store, Date, Sales
  #
  # returns:
  # sales_mat - data frame with first column = Date, followed by one column per Store
  sales_mat = as.data.frame(spread(x[,.(Store, Date, Sales)], key = Store, value=Sales))
  #sales_mat = data.matrix(sales_mat)
  
  if(anyNA(sales_mat)) print('NAs generated')
  sales_mat
}