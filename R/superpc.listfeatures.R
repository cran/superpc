superpc.listfeatures<- function(data, train.obj, fit.red, 
num.features=NULL, component.number=1){

ii=component.number
total.num=sum(abs(fit.red$import[,ii])>0)

if(is.null(num.features)){ num.features=total.num}

if(num.features< 1 | num.features > total.num){

    stop("Error: num.features   argument out of range")

}

featurenames.short<- substring(data$featurenames,1,40)


ii=component.number

oo=rank(abs(fit.red$import[,ii]))> nrow(data$x)-num.features

res<-cbind(round(fit.red$import[oo,ii],3), round(train.obj$feature.scores[oo],3),
#round(fit.red$wt[oo,ii],3),
 featurenames.short[oo])

collabs=c("Importance-score", "Raw-score" , "Name")

if(!is.null(data$featureid)){
  res=cbind(res, data$featureid[oo])
 collabs=c(collabs, "ID")
}

o<-order(-abs(fit.red$import[oo,ii]))
res<-res[o,]
dimnames(res)<-list(NULL,collabs)

return(res)
}
