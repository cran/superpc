superpc.listfeatures<- function(data, train.obj, fitred, component.number, shrinkage){

if( shrinkage < min(fitred$shrinkages) |  shrinkage > max(fitred$shrinkages)){
    stop("Error: shrinkage value out of range")
}

temp<- abs(shrinkage- fitred$shrinkages)

ii<-(1:length(temp))[temp==min(temp)]

featurenames.short<- substring(data$featurenames,1,40)

oo=fitred$feature.list[[ii]][[component.number]]

res<-cbind(round(fitred$import[oo,component.number],3), round(train.obj$feature.scores[oo],3), featurenames.short[oo])



o<-order(-abs(fitred$import[oo,component.number]))
res<-res[o,]
dimnames(res)<-list(NULL,c("Importance-score", "Raw-score", "Name"))
return(res)
}
