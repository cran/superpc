\name{superpc.predict.red}
\alias{superpc.predict.red}
\title{Feature selection for supervised principal components }
\description{ 
Forms  reduced models involving a subset of the features, by
soft-thresholding of correlations between the  features and the
supervised principal component predictor.
}
\usage{
superpc.predict.red(fit, data, data.test, threshold, n.components = 1, n.shrinkage = 20, compute.lrtest = TRUE, sign.wt = "both")
}
\arguments{
  \item{fit}{ Object returned by superpc.train}
  \item{data}{ Training data object, of form described in superpc.train dcoumentation }
  \item{data.test}{ Test data object; same form as train  }
  \item{threshold}{ Feature score threshold; usually estimated from superpc.cv}
  \item{n.components}{ Number of principal components to use.
Should be 1,2 or 3. }
  \item{n.shrinkage}{ Number of shrinkage values to consider. Default 20. }
  \item{compute.lrtest}{ Should the likelihood ratio test be computed? Default TRUE  }
  \item{sign.wt}{Allowable signs for feature weights. Can be "both",
"positive",  or "negative". Default: "both"  }
}
\details{
}
\value{
  ~Describe the value returned
  If it is a LIST, use
\item{shrinkages}{Shrinkage values used}
\item{lrtest.shrink}{ Likeihood ratio tests for reduced models }
\item{corr.with.full }{ Correlation of each reducted predictor with full
superivsed PC predictor}
\item{num.features}{Number of features used in each shrunken model}
\item{features.list}{ Indiced of features used}
\item{import}{Importance scores for features}
\item{v.test}{Outcome predictor from reduced models. Array of
n.shrinkage by (number of test observations) by n.components}
\item{n.components }{Number of supervised  principal component used}
\item{sign.wt}{Allowable signs for feature weights}
\item{type }{Type of outcome}
\item{call}{calling sequence}
}
\references{ ~put references to the literature/web site here ~ }
\author{ Eric Bair and Robert Tibshirani }
\note{ ~~further notes~~ }


\seealso{}
\examples{


set.seed(332)
#generate some data

x<-matrix(rnorm(1000*40),ncol=40)
y<-10+svd(x[1:60,])$v[,1]+ .1*rnorm(40)
ytest<-10+svd(x[1:60,])$v[,1]+ .1*rnorm(40)
status<- sample(c(rep(1,30),rep(0,10)))
status.test<- sample(c(rep(1,30),rep(0,10)))

featurenames <- paste("feature",as.character(1:1000),sep="")
data<-list(x=x,y=y, status=status, featurenames=featurenames)
data.test<-list(x=x,y=ytest, status=status.test, featurenames= featurenames)



a<- superpc.train(data, type="survival")

fit<- superpc.predict(a, data, data.test, threshold=1.0, n.components=1, prediction.type="continuous")

fit.red<- superpc.predict.red(a,data, data.test, .6)

superpc.fit.to.outcome(a, data.test, fit.red$v.test[5,,])
}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line

