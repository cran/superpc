superpc.predict.red.cv <- function(fitred, fitcv, data, threshold, n.shrinkage=30, sign.wt="both"){

 # try reduced predictor on cv folds, via prevalidation

                           
  this.call=match.call()

  type=fitred$type

  n.components=fitred$n.components


  n.fold<-length(fitcv$folds)

  shrinkages<- fitred$shrinkages
  n.shrinkage<-length(shrinkages)
  cur.vall<- array(NA,c(n.shrinkage,ncol(data$x),n.components))

  for(j in 1:n.fold){
    cat(j,fill=TRUE)
    fit.temp<-list(feature.scores=fitcv$featurescores.fold[,j], type=type)
    ii<-fitcv$folds[[j]]
    
    data1<-list(x=data$x[,-ii],y=data$y[-ii],status=data$status[-ii])
    data2<-list(x=data$x[,ii],y=data$y[ii],status=data$status[ii])
    junk<- superpc.predict.red(fit.temp, data1,data2, threshold, n.shrinkage=n.shrinkage, n.components=n.components,compute.lrtest=FALSE, sign.wt=sign.wt)
    cur.vall[,ii,]<-junk$v.test
  }

  lrtest.shrink<-rep(NA,n.shrinkage)

  for(i in 1:n.shrinkage){
    if(type=="survival"){
      require(survival)
      junk<- coxph(Surv(data$y, data$status) ~cur.vall[i,,])$loglik
      lrtest.shrink[i]=2*(junk[2]-junk[1])
    }
    else{
      junk<- summary(lm(data$y~cur.vall[i,,]))
      if(!is.null(junk$fstat)){lrtest.shrink[i]<-junk$fstat[1]}
    }

  }


  return(list(shrinkages=shrinkages, lrtest.shrink=lrtest.shrink, num.features=fitred$num.features, n.components=n.components, v.preval.red=cur.vall, sign.wt=sign.wt, type=type,call=this.call))
}
