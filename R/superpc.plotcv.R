superpc.plotcv=
function (object, smooth = TRUE, smooth.df = 6, ...)
{
    scor = object$scor.preval
k=nrow(scor)
    if (smooth) {
        for (j in 1:nrow(scor)) {
         if(is.null(smooth.df)){
               junk=smooth.spline(object$th, scor[j, ])
               scor[j, ] = predict(junk,object$th)$y
              }
            if(!is.null(smooth.df)){
             junk=smooth.spline(object$th, scor[j, ], df=smooth.df)
            scor[j, ] =predict(junk,object$th)$y
             }
        }
    }
    ymax = max(scor, qchisq(0.95, nrow(scor)))
    matplot(object$th, t(scor), xlab = "Threshold", ylab = "Likelihood ratio test statistic",
        ylim = c(0, ymax), lty=rep(1,k))
    matlines(object$th, t(scor), lty=rep(1,k), ...)


    for (j in 1:k) {
        abline(h = qchisq(0.95, j), lty = 2, col = j)
    }
}


