superpc.predict.red <- function(fit, data, data.test, threshold, n.components=1, n.shrinkage=20, compute.lrtest=TRUE, sign.wt="both"){

  # try reduced predictor on test set

  
  soft.thresh<- function(x,tt){ sign(x)*(abs(x)-tt)*(abs(x)>tt)}


  this.call<- match.call()
  

  type=fit$type

  lrtest.shrink<- rep(NA,n.shrinkage)
  
  cur.vall<- array(NA,c(n.shrinkage,ncol(data$x),n.components))
  cur.vall.test<- array(NA, c(n.shrinkage,ncol(data.test$x),n.components))
  corr.with.full<-matrix(NA,nrow=n.shrinkage, ncol=n.components)
  which.features <- abs(fit$feature.scores) > threshold
  x.sml <- data$x[which.features, ]
  x.svd <- mysvd(x.sml, n.components=n.components)
  cur.v <- scale(t(data$x[which.features, ]) %*%x.svd$u, center=FALSE,scale=x.svd$d)

# flip the sign of the latent factors, if a coef is neg

result<-superpc.fit.to.outcome(fit, data, cur.v, print=FALSE)
if(fit$type=="survival"){coef=result$coef}
if(fit$type=="regression"){coef=result$coef[-1]}

 cur.v<-scale(cur.v, center=FALSE,scale= sign(coef))
##

  sc<-cor(t(data$x), cur.v)

  # don't shrink all of the way to zero 

  maxshrink=max(abs(sc))

  if(sign.wt=="positive"){ maxshrink=max(abs(sc[sc>0]))}
  
  if(sign.wt=="negative"){ maxshrink=max(abs(sc[sc<0]))}

  shrinkages<- seq(0,maxshrink,length=n.shrinkage+1)
  shrinkages= shrinkages[-(n.shrinkage+1)]

  num.features<-matrix(NA,nrow=n.shrinkage, ncol=n.components)
  
  feature.list<-vector("list", n.shrinkage)


  for(i in 1:n.shrinkage){
    cat(i)
    sc2<- soft.thresh(sc,shrinkages[i])
    if(sign.wt=="positive"){sc2[sc2<0]<-0}
    if(sign.wt=="negative"){sc2[sc2>0]<-0}
    nonzero<-sc2!=0
    

    num.features[i,]<- apply(nonzero,2,sum)
    
    junk=vector("list",n.components)
    for(ii in 1:n.components){
      junk[[ii]]<- (1:nrow(data$x))[nonzero[,ii]]
    }
    feature.list[[i]]=junk

    for(ii in 1:n.components){
      cur.vall[i,,ii]<-apply(t(scale(t(data$x[nonzero[,ii],,drop=FALSE]), center=FALSE,scale=1/sc2[nonzero[,ii],ii])),2,sum)
      cur.vall.test[i,,ii]<-apply(t(scale(t(data.test$x[nonzero[,ii],,drop=FALSE]), center=FALSE,scale=1/sc2[nonzero[,ii],ii])),2,sum)
    }}
  cat("",fill=TRUE)

  if(compute.lrtest){ 
    for(i in 1:n.shrinkage){
      if(type=="survival"){
        require(survival)
        junk<- coxph(Surv(data.test$y, data.test$status) ~cur.vall.test[i,,])$loglik
        lrtest.shrink[i]=2*(junk[2]-junk[1])
      }
      else{
        junk<- summary(lm(data.test$y~cur.vall.test[i,,]))
        if(!is.null(junk$fstat)){lrtest.shrink[i]<-junk$fstat[1]}
      }
    }
  }
  
  for(ii in 1:n.components){
    corr.with.full[,ii]=cor(t(cur.vall[,,ii]),cur.v[,ii])
  }

  return(list(shrinkages=shrinkages, lrtest.shrink=lrtest.shrink, corr.with.full=corr.with.full,
              num.features=num.features, feature.list=feature.list, import=sc, v.test=cur.vall.test,
              n.components=n.components, sign.wt=sign.wt, type=type,call=this.call))
  
}



