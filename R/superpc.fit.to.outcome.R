superpc.fit.to.outcome<- function(fit, data.test,score, print=TRUE){


type=fit$type

 if(type=="survival"){
   require(survival)
   result<-coxph(Surv(data.test$y, data.test$status)~score)
}

 else{
   result<-lm(data.test$y~score)
}


if(print){print(summary(result))}

return(result)
}

