superpc.predict <- function (object, data, newdata, threshold, n.components=1,  prediction.type=c("continuous", "discrete",
                                                                                  "nonzero"), n.class=2)
{
  
#thresholds the feature scores at "threshold", computes svd based on "data", and then predicts based
#  "newdata"
  
  this.call <- match.call()

  prediction.type <- match.arg(prediction.type)

  which.features <- (abs(object$feature.scores) >= threshold)
  x.sml <- data$x[which.features, ]
  n.pc <- n.components
                                

  x.sml.svd <- mysvd(x.sml, n.components=n.components)



  
  if (prediction.type=="nonzero") {
    if (!is.null(data$featurenames)) {
      out<- data$featurenames[which.features]
    }
    else {
      out<- (1:nrow(data$x))[which.features]
    }
  }

  if (prediction.type=="continuous" | prediction.type=="discrete") {
    cur.v <-scale( t(newdata$x[which.features,]) %*%x.sml.svd$u,  center=FALSE,scale=x.sml.svd$d)

     cur.v0 <-scale( t(data$x[which.features,]) %*%x.sml.svd$u,  center=FALSE,scale=x.sml.svd$d)

#here we obtain the regression coefs of y on the latent factors
# and flip the sign of the factors if the coef is negative

result<-superpc.fit.to.outcome(object, data, cur.v0, print=FALSE)

if(object$type=="survival"){coef=result$coef}
if(object$type=="regression"){coef=result$coef[-1]}



    if (prediction.type=="continuous") {
      out<-scale(cur.v, center=FALSE,scale= sign(coef))
    }
    else if (prediction.type=="discrete") {

     out<-scale(cur.v, center=FALSE,scale= sign(coef))


      for(j in 1:ncol(out)){
        out[,j]<-cut(out[,j],n.class,labels=FALSE)
      }}
  }
  junk <- list(v.pred=out, u = x.sml.svd$u, d = x.sml.svd$d, 
               which.features = which.features, 
               n.components = n.pc, 
               call = this.call, prediction.type=prediction.type)

  return(junk)
}
