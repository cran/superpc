superpc.predict.red.cv <- function(fitred, fitcv, data, threshold, num.reduced.models=30, sign.wt="both"){

 # try reduced predictor on cv folds, via full cross-validation
  # works on single component, indiaqted by component.number in fitred

                           
  this.call=match.call()

  type=fitred$type

  n.components=fitred$n.components
  component.number=fitred$component.number


  n.fold<-length(fitcv$folds)

  shrinkages<- fitred$shrinkages
  num.reduced.models<-length(shrinkages)
  cur.vall<- array(NA,c(num.reduced.models,ncol(data$x),n.components))

 lrtest.reduced<-matrix(NA,nrow=n.fold,ncol=num.reduced.models)

  for(j in 1:n.fold){
    cat(j,fill=TRUE)
    fit.temp<-list(feature.scores=fitcv$featurescores.fold[,j], type=type)
    ii<-fitcv$folds[[j]]
    
    data1<-list(x=data$x[,-ii],y=data$y[-ii],censoring.status=data$censoring.status[-ii])
    data2<-list(x=data$x[,ii],y=data$y[ii],censoring.status=data$censoring.status[ii])
    junk<- superpc.predict.red(fit.temp, data1,data2, threshold, num.reduced.models=num.reduced.models, n.components=n.components, component.number=component.number, compute.lrtest=TRUE, sign.wt=sign.wt)
 lrtest.reduced[j,]=junk$lrtest.reduced
  }

 mean.na <- function(x) {
            mean(x[!is.na(x)])
        }
        se.na <- function(x) {
            val = NA
            if (sum(!is.na(x)) > 0) {
                val = sqrt(var(x[!is.na(x)])/sum(!is.na(x)))
            }
            return(val)
        }

   llr= apply(log(lrtest.reduced), 2, mean.na)
        se.llr = apply(log(lrtest.reduced), 2, se.na)
        lrtest.reduced.lower = exp(llr - se.llr)
        lrtest.reduced.upper = exp(llr + se.llr)
        lrtest.reduced <- exp(llr)


return(list(shrinkages=shrinkages, lrtest.reduced=lrtest.reduced,  lrtest.reduced.lower= lrtest.reduced.lower, lrtest.reduced.upper=lrtest.reduced.upper,  n.components=n.components,component.number=component.number, num.features=fitred$num.features,  sign.wt=sign.wt, type=type,call=this.call))
}
