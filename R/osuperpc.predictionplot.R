superpc.predictionplot <- function (train.obj, data, data.test,  threshold, n.components=3,  n.class=2, shrinkage=NULL, call.win.metafile=FALSE)
{
  



BIG=1000
  
this.call=match.call()
  
if(n.components>3){
     stop("n.components cannot be bigger than 3")
}

if(is.null(shrinkage)){ fit.cts=superpc.predict(train.obj, data, data.test, threshold=threshold, n.components=n.components, prediction.type="continuous")


  pred.cts=superpc.fit.to.outcome(train.obj, data.test, fit.cts$v.pred[,1:n.components])$results 


pred.cts.1df=superpc.fit.to.outcome(train.obj, data.test, fit.cts$v.pred.1df)$results

                               }
else  {

     fit.cts=superpc.predict.red(train.obj, data, data.test, threshold=threshold, n.components=n.components,  shrinkage=shrinkage)


pred.cts=superpc.fit.to.outcome(train.obj, data.test,fit.cts$v.test[,,1:n.components])$results


pred.cts.1df=superpc.fit.to.outcome(train.obj, data.test, fit.cts$v.test.1df[1,])$results

   }



if(call.win.metafile){win.metafile()}


if(train.obj$type=="survival"){

if(n.components==1){layout(matrix(c(1,2),1,2, byrow = TRUE),width=c(.70,.30),heights=c(1,1))}

if(n.components==2){layout(matrix(c(1,4,2,5,3,6),3,2, byrow = TRUE),width=c(.70,.30),heights=c(.34,.33,.33))}

if(n.components==3){layout(matrix(c(1,5,2,6,3,7,4,8),4,2, byrow = TRUE),width=c(.70,.30),heights=rep(.25,4))}


if(is.null(shrinkage)){
  fit.groups<- superpc.predict(train.obj, data, data.test, threshold=threshold, n.components=n.components, prediction.type="discrete", n.class=n.class)


pred.groups=superpc.fit.to.outcome(train.obj,  data.test, fit.groups$v.pred)$results


pred.groups.1df=superpc.fit.to.outcome(train.obj,  data.test, fit.groups$v.pred.1df)$results


}
 else{
   
 fit.groups<- superpc.predict.red(train.obj, data, data.test, threshold=threshold, n.components=n.components, shrinkage=shrinkage, prediction.type="discrete", n.class=n.class)


pred.groups=superpc.fit.to.outcome(train.obj,  data.test,fit.groups$v.test[,,1])$results

pred.groups.1df=superpc.fit.to.outcome(train.obj, data.test, fit.groups$v.test.1df[1,])$results


}


lastc=2+(n.class-1)
par(mar=c(5,4,2,0))
for(i in 1:n.components){
if(is.null(shrinkage)){
plot(survfit(Surv(data.test$y,data.test$censoring.status)~fit.groups$v.pred[,i]), col=2:lastc, xlab="time", ylab="Prob survival")
}
else{
 plot(survfit(Surv(data.test$y,data.test$censoring.status)~fit.groups$v.test[1,,i]), col=2:lastc, xlab="time", ylab="Prob survival")
}

if(n.class==2){legend(.7*max(data.test$y), .8,lty=c(1,1),col=2:lastc,c("low score","high score"))}

if(n.class==3){legend(.7*max(data.test$y), .8,lty=c(1,1,1),col=2:lastc,c("low score","medium score", "high score"))}

title(main=paste("Component",as.character(i),sep=" "))
}

# if  number of  components >1, plot combined predictor curves

if(n.components>1){
  if(is.null(shrinkage)){
plot(survfit(Surv(data.test$y,data.test$censoring.status)~fit.groups$v.pred.1df), col=2:lastc, xlab="time", ylab="Prob survival")
}
else{
 plot(survfit(Surv(data.test$y,data.test$censoring.status)~fit.groups$v.test.1df[1,]), col=2:lastc, xlab="time", ylab="Prob survival")
}

if(n.class==2){legend(.7*max(data.test$y), .8,lty=c(1,1),col=2:lastc,c("low score","high score"))}

if(n.class==3){legend(.7*max(data.test$y), .8,lty=c(1,1,1),col=2:lastc,c("low score","medium score", "high score"))}

  title(main=" Combined 1 degree of freedom predictor")
}



coefs=cbind(pred.cts$coef,pred.groups$coef)
se=cbind(sqrt(diag(pred.cts$var)), sqrt(diag(pred.groups$var)))
relrisk=exp(coefs)
relrisk[relrisk>BIG] =1/0
pvalue.univ=2*(1-pnorm(abs(coefs/se)))


lograt.stat=2*c(pred.cts$loglik[2]-pred.cts$loglik[1], pred.groups$loglik[2]-pred.groups$loglik[1])
if(n.components==1) {lograt.stat[2]=2*c(pred.groups$loglik[2]-pred.groups$loglik[1])}

pvalue=c(NA,NA)
pvalue[1]=1-pchisq(lograt.stat[1],1)
pvalue[2]=1-pchisq(lograt.stat[2],n.class-1)



res=NULL
rownames=NULL

for(i in 1:n.components){
temp=rbind(coefs[i,], se[i,],relrisk[i,], pvalue[i,])

#for group column, only p-value is menaingful
temp[1:(nrow(temp)-1),2]=NA

res=rbind(res,temp)
if(n.components==1){rownames=c(rownames,"coef","se","relrisk", "pvalue") }

if(n.components>1){rownames=c(rownames,paste("coef",i,sep=""), ,paste("se",i,sep=""), ,paste("relrisk",i,sep=""), paste("pvalue",i,sep="")) }
}
res=round(rbind(res,lograt.stat, pvalue),2)


browser()

rownames=c(rownames,"LR stat","pvalue")

#par(mar=c(5,1,2,2))
par(mar=c(1,1,1,0))
par(cex=.8)
leftcol= 0
midcol=.3
rightcol=.6
nrows=nrow(res)
plot(0,0,xlim=c(0,1),ylim=c(0,1),type="n",axes=F,xlab="",ylab="")
xc=c(midcol,rightcol)
if(n.components==1){ yinc=.03}
if(n.components==2){ yinc=.07}
if(n.components==3){ yinc=.11}
yc=1-(1:nrows)*yinc
#yc[(length(yc)-1)]= yc[(length(yc)-1)]-yinc/3
#yc[(length(yc))]= yc[(length(yc))]-yinc/3

# write out first block of results
for(j in 1:4){
 text(leftcol,yc[j],rownames[j], pos=4, col=4)
}
text(midcol,1, "Linear", pos=4, col=4)
text(rightcol,1, "Grouped", pos=4, col=4)
for(j in 1:4){
for(i in 1:2){
  text(xc[i],yc[j],labels=as.character(res[j,i]),  pos=4)
}}

if(n.components>1){
# write out 2nd block of results
plot(0,0,xlim=c(0,1),ylim=c(0,1),type="n",axes=F,xlab="",ylab="")  
 for(j in 5:8){
 text(leftcol,yc[j-3],rownames[j], pos=4, col=4)
}
text(midcol,1, "Linear", pos=4, col=4)
text(rightcol,1, "Grouped", pos=4, col=4)
for(j in 5:8){
for(i in 1:2){
  jj=j-3
  text(xc[i],yc[jj],labels=as.character(res[j,i]),  pos=4)
}}}

 if(n.components>2){
# write out 3rd block of results
plot(0,0,xlim=c(0,1),ylim=c(0,1),type="n",axes=F,xlab="",ylab="")  
 for(j in 9:12){
 text(leftcol,yc[j-6],rownames[j], pos=4, col=4)
}
text(midcol,1, "Linear", pos=4, col=4)
text(rightcol,1, "Grouped", pos=4, col=4)
for(j in 9:12){
for(i in 1:2){
  jj=j-6
  text(xc[i],yc[jj],labels=as.character(res[j,i]),  pos=4)
}}}

if(n.components>1){
  # write out combined 1df predictor results
#  for(i in 1:(n.components-1)){
#  plot(0,0,type="n",axes=F,xlab="",ylab="")
#}
 plot(0,0,xlim=c(0,1),ylim=c(0,1),type="n",axes=F,xlab="",ylab="") 
  text(midcol,1, "Linear", pos=4, col=4)
  text(rightcol,1, "Grouped", pos=4, col=4)
  
  lograt.stat=2*c(pred.cts.1df$loglik[2]-pred.cts.1df $loglik[1], pred.groups.1df$loglik[2]-pred.groups.1df$loglik[1])
  pvalue=c(NA,NA)
pvalue[1]=1-pchisq(lograt.stat[1],1)
pvalue[2]=1-pchisq(lograt.stat[2],1)
  res=rbind(lograt.stat, pvalue)
  res=round(res,2)
  rownames=c("LR stat","pvalue")
  nrows=2
  yc=1-(1:nrows)*yinc
  for(j in 1:nrows){
    text(leftcol,yc[j],rownames[j], pos=4, col=4)
  }
  for(j in 1:nrows){
    for(i in 1:2){
  text(xc[i],yc[j],labels=as.character(res[j,i]),  pos=4)
}}
par(cex=1)

}}

if(train.obj$type=="regression"){
layout(matrix(c(1,2),1,2, byrow = TRUE),width=c(.6,.40),heights=c(1,1))

par(mar=c(5,4,2,1))

plot(data.test$y, fit.cts$v.pred.1df, xlab="outcome",ylab="predicted outcome")


res=round(t(summary(pred.cts)$coef),3)

rownames=c("coef", "se","T stat","pvalue")

par(mar=c(5,1,2,1))

par(cex=.7)
leftcol= 0
midcol=.21
midcol2= .42
rightcol=.63
rightcol2=.84
nrows=nrow(res)
plot(0,0,xlim=c(0,1),ylim=c(0,1),type="n",axes=F,xlab="",ylab="")
xc=c(midcol,midcol2,rightcol, rightcol2)
yinc=.05
yc=1-(1:nrows)*yinc

for(j in 1:nrows){
 text(leftcol,yc[j],rownames[j], pos=4, col=4)
}
   
text(midcol,1, "Intcpt", pos=4, col=4)

if(n.components==1){
text(midcol2,1, "Comp", pos=4, col=4)
}
if(n.components==2){
text(midcol2,1, "Comp1", pos=4, col=4)
text(rightcol,1, "Comp2", pos=4, col=4)
}
if(n.components==3){
text(midcol2,1, "Comp1", pos=4, col=4)
text(rightcol,1, "Comp2", pos=4, col=4)
text(rightcol2,1, "Comp3", pos=4, col=4)
}

for(j in 1:nrows){
for(i in 1:ncol(res)){
  text(xc[i],yc[j],labels=as.character(res[j,i]),  pos=4)
}}
ylast=yc[nrows]

 junk=summary(pred.cts.1df)$fstat

fstat=junk[1]
pvalue=1-pf(fstat,junk[2],junk[3])
 

text(leftcol,ylast-2*yinc, "F-stat",pos=4,col=4)
text(midcol2,ylast-2*yinc, round(fstat,5),pos=4)
text(leftcol,ylast-3*yinc, "pvalue",pos=4, col=4)
text(midcol2,ylast-3*yinc, round(pvalue,5),pos=4)
par(cex=1)
}


if(call.win.metafile){dev.off()}


}


