superpc.plot.lrtest<- function(object.lrtestcurv){
  plot(object.lrtestcurv$threshold, object.lrtestcurv$lrtest,xlab="Threshold",ylab="Likelihood ratio statistic",type="b")


}
  
