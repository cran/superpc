superpc.train<-
  function (data, type=c("survival","regression")){
    
# computes feature scores for supervised pc analysis
    
  this.call <- match.call()
 type <- match.arg(type)

  
  
  if (is.null(data$status) & type=="survival") {
 stop("Error: survival specified but censoring status is null")
  }
  
  if (type=="survival") {
    feature.scores <- coxfunc(data$x, data$y, data$status)$tt
   }
  else {
    feature.scores <- cor.func(data$x, data$y)$tt
  }

  
  junk <- list( feature.scores=feature.scores, 
               type=type, 
               call = this.call)

  
  class(junk) <- "superpc"
  return(junk)

}

