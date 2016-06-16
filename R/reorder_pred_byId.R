# reorder_pred_byId.R

s1 = reload.submission(1)
s2 = reload.submission(2)
s3 = reload.submission(3)
s4 = reload.submission(4)
s5 = reload.submission(5)
s6 = reload.submission(6)


s1 = s1[order(s1$Id),]
s2 = s2[order(s2$Id),]
s3 = s3[order(s3$Id),]
s4 = s4[order(s4$Id),]
s5 = s5[order(s5$Id),]
s6 = s6[order(s6$Id),]

all.equal(s1$Id, s2$Id, s3$Id, s4$Id, s5$Id, s6$Id)

ens_pred = cbind(s1$Sales, s2$Sales, s3$Sales, s4$Sales, s5$Sales, s6$Sales)
ens_avg = data.frame(Id=s1$Id, Sales=rowMeans(ens_pred))
write.csv(ens_avg,"../submissions/submission7.csv", row.names=F, quote=F)

###############################
# removing august kluge
ens_pred = cbind(s1$Sales, s3$Sales, s4$Sales, s5$Sales, s6$Sales)
ens_avg = data.frame(Id=s1$Id, Sales=rowMeans(ens_pred))
write.csv(ens_avg,"../submissions/submission8.csv", row.names=F, quote=F)

################################

tr = train[(Date < as.Date("2014-08-01"))]
# stores not missing dates
rows_per_store_tr = tr[,.N,by=Store]
comp_stores = rows_per_store_tr[N>546]$Store
tr = tr[.(comp_stores)]

val = train[(Date >= as.Date("2014-08-01")) & (Date <= as.Date("2014-09-17"))]
rows_per_store_val = val[,.N,by=Store]

kluge = individual_forecast(tr, val, 'August2014_kluge')
setkey(kluge, Store)

plotfit = function(x) {
  plot(kluge[.(x),Date], kluge[.(x),Sales])
  lines(kluge[.(x),Date], kluge[.(x),predSales])
}

kluge = individual_forecast(add_features_all(tr), add_features_all(val), 'August2014_kluge')
setkey(kluge, Store)