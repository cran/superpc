\name{superpc.cv}
\alias{superpc.cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Cross-validation for  supervised principal components}
\description{
This function uses a form of cross-validation  to  estimate the optimal feature  threshold in supervised principal components
}
\usage{
superpc.cv(fit, data, n.threshold = 20,  n.fold = NULL, folds = NULL,   n.components = 3, min.features = 5, max.features = nrow(data$x),  compute.fullcv =  TRUE,
                 compute.preval = TRUE, xl.mode = c("regular",
                 "firsttime", "onetime", "lasttime"), xl.time = NULL,
                 xl.prevfit = NULL)
 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{ Object returned by superpc.train }
  \item{data}{ Data object of form described in superpc.train documentation }
  \item{n.threshold}{Number of thresholds to consider. Default 20.}
  \item{n.fold}{ Number of cross-validation folds. default is around 10
(program pick a convenient value based on the sample size }
  \item{folds}{List of indices  of cross-validation folds (optional)   }
  \item{n.components}{ Number of cross-validation components to use: 1,2 or 3. }
 \item{min.features}{ Minimum number of features to include, in determining
range for threshold. Default 5. }
  \item{max.features}{ Maximum number of features to include, in determining
range for threshold. Default is total number of features in the dataset  }
  \item{compute.fullcv}{ Should full cross-validation  be done? }
  \item{compute.preval}{ Should full pre-validation  be done? }
 \item{xl.mode}{Used by Excel interface only}
 \item{xl.time}{Used by Excel interface only}
 \item{xl.prevfit}{Used by Excel interface only}
}
\details{
 This function uses a form of cross-validation  to  estimate the optimal feature  threshold in supervised principal components. To avoid prolems with
fitting Cox models to samll validation datastes, it uses the "pre-validation"
approach of Tibshirani and Efron (2002)
}
\value{
 list(threshold = th, nonzero = nonzero, scor = out,
        scor.preval = out.preval, folds = folds, featurescores.folds = featurescores.folds,
        v.preval = cur2, type = type, call = this.call)

  \item{threshold }{Vector of thresholds considered}
  \item{nonzero}{Number of features exceeding each value of the threshold}
 \item{scor.preval}{Likelihood ratio scores from pre-validation}
  \item{scor}{Full CV scores}
 \item{folds}{Indices of CV folds used}
\item{featurescores.folds}{Feature scores for each fold}
\item{v.preval}{The pre-validated predictors}
\item{type }{problem type}
 \item{call }{calling sequence}
}
\author{ Eric Bair and Robert Tibshirani  }
\note{ ~~further notes~~ }


\seealso{}
\examples{
set.seed(332)
x<-matrix(rnorm(1000*40),ncol=40)
y<-10+svd(x[1:60,])$v[,1]+ .1*rnorm(40)
censoring.status<- sample(c(rep(1,30),rep(0,10)))

featurenames <- paste("feature",as.character(1:1000),sep="")
data<-list(x=x,y=y, censoring.status=censoring.status, featurenames=featurenames)


a<- superpc.train(data, type="survival")
aa<-superpc.cv(a,data)
}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line
