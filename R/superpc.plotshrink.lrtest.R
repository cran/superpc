superpc.plotshrink.lrtest<- function(object.lrtestred){
  
  n.components<- object.lrtestred$n.components
  
  if(n.components==1) {par(mfrow=c(1,1))}
  
  if(n.components>1) {par(mfrow=c(2,2))}
  
  plot(object.lrtestred$shrinkages, object.lrtestred$lrtest,xlab="Shrinkage amount",ylab="Likelihood ratio test statistic",type="b")
  if(n.components==1){  axis(3,at=object.lrtestred$shrinkages, labels=as.character(object.lrtestred$num.features))}
  
  
  if(n.components>1)
    for(ii in 1:n.components){
      plot(object.lrtestred$shrinkages, object.lrtestred$num.features[,ii],xlab="Shrinkage amount",ylab="Number of features",type="b", log="y")
      title(paste("Component ",as.character(ii),sep=""))
    }
  
}
