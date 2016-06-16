# explore ensemble averaging

predSales_mavg = (pred0$predSales * 
                    pred$predSales * 
                    pred2$predSales * 
                    pred4$predSales * 
                    pred6$predSales * 
                    pred7$predSales * 
                    pred8$predSales)^(1/7)

allpred = data.table(Store = val$Store, Date=val$Date, Sales=val$Sales, predSales=predSales_mavg,
                     pred0 = pred0$predSales, pred = pred$predSales, pred2=pred2$predSales, 
                     pred4 =pred4$predSales, pred6=pred6$predSales, pred7= pred7$predSales,
                     pred8=pred8$predSales)
allpred[is.nan(predSales)]
## can see there are problems with pred0 and pred2.  remove them from averaging for ensembles.

#################################
# product averaging
predSales_mavg = (  pred$predSales * 
                      pred4$predSales * 
                      pred6$predSales * 
                      pred7$predSales * 
                      pred8$predSales)^(1/5)

myensemble_m = data.table(Store = val$Store, Date=val$Date, Sales=val$Sales, predSales=predSales_mavg)
setkey(myensemble_m, Store)

temp2 = RMSPE_per_store(myensemble_m)
RMSPE(truth=as.vector(myensemble_m$Sales), pred=as.vector(myensemble_m$predSales))
# RMSPE: 0.1400766


#################################
# product averaging WITHOUT seasonal naive
predSales_mavg2 = (  pred$predSales * 
                      pred4$predSales * 
                      pred6$predSales * 
                      pred8$predSales)^(1/4)

myensemble_m2 = data.table(Store = val$Store, Date=val$Date, Sales=val$Sales, 
                           predSales=predSales_mavg2)
setkey(myensemble_m2, Store)

temp2_2 = RMSPE_per_store(myensemble_m2)
RMSPE(truth=as.vector(myensemble_m2$Sales), pred=as.vector(myensemble_m2$predSales))
# RMSPE: 0.1476929

#################################
# product averaging with simple predictions down-weighted
predSales_mavg3 = (  pred$predSales * 
                      pred4$predSales * 
                      pred6$predSales * 
                      (pred7$predSales*pred8$predSales)^(1/2))^(1/4)

myensemble_m3 = data.table(Store = val$Store, Date=val$Date, Sales=val$Sales, 
                           predSales=predSales_mavg3)
setkey(myensemble_m3, Store)

temp2_3 = RMSPE_per_store(myensemble_m3)
RMSPE(truth=as.vector(myensemble_m3$Sales), pred=as.vector(myensemble_m3$predSales))
# RMSPE: 0.1461436


#################################
# sum averaging

predSales_avg = (  pred$predSales + 
                   pred4$predSales + 
                   pred6$predSales + 
                   pred7$predSales + 
                   pred8$predSales)/5

myensemble = data.table(Store = val$Store, Date=val$Date, Sales=val$Sales, predSales=predSales_avg)
setkey(myensemble, Store)

temp = RMSPE_per_store(myensemble)
RMSPE(truth=as.vector(myensemble$Sales), pred=as.vector(myensemble$predSales))
# RMSPE: 0.1404405

## removing store 292 gives RMSPE: 0.1386893

#################################
# sum averaging with simple predictions down-weighted

predSales_avg2 = (  pred$predSales + 
                     pred4$predSales + 
                     pred6$predSales + 
                     (pred7$predSales + pred8$predSales)/2)/4

myensemble2 = data.table(Store = val$Store, Date=val$Date, Sales=val$Sales, 
                         predSales=predSales_avg2)
setkey(myensemble2, Store)

temp = RMSPE_per_store(myensemble2)
RMSPE(truth=as.vector(myensemble2$Sales), pred=as.vector(myensemble2$predSales))
# RMSPE: 0.1458726

#################################

# plot some of the fits

plotfit = function(x) {
  plot(myensemble[.(x),Date], myensemble[.(x),Sales])
  lines(myensemble[.(x),Date], myensemble[.(x),predSales])
}

plotfit(292)
plotfit(909)
plotfit(782)
plotfit(876)
plotfit(770)
plotfit(169)
plotfit(286)
plotfit(563)
plotfit(722)
plotfit(839)
