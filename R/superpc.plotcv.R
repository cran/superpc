superpc.plotcv<-
  
  function (object,  smooth=TRUE, smooth.df=10, ...) 
{

  scor=object$scor.preval

  if(smooth){
    for(j in 1:nrow(scor)){
      scor[j,]=smooth.spline(object$th, scor[j,],df=smooth.df)$y
    }}
  
  ymax=max(scor,qchisq(.95,nrow(scor)))

  matplot(object$th, t(scor), xlab = "Threshold", 
          ylab = "Likelihood ratio test statistic", ylim=c(0,ymax))
  matlines(object$th, t(scor), ...)

  for(j in 1:nrow(scor)){
    abline(h=qchisq(.95,j),lty=2,col=j)
  }

}

