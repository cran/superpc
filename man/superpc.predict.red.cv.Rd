\name{superpc.predict.red.cv}
\alias{superpc.predict.red.cv}
\title{Cross-validation of feature selection for supervised principal components }
\description{
Applies superpc.predict.red to cross-validation folds generates in superpc.cv.
Uses the output to evaluate reduced models, and compare them to the
full supervised principal components predictor.
}
\usage{
superpc.predict.red.cv(fitred, fitcv, data, threshold, n.shrinkage = 30, sign.wt = "both")
}
\arguments{
  \item{fitred}{ Output of superpc.predict.red}
  \item{fitcv}{ Output of superpc.cv }
  \item{data}{ Training data object }
  \item{threshold}{Feature score threshold; usually estimated from superpc.cv}
  \item{n.shrinkage}{Number of shrinkage values to consider. Default 20.}
  \item{sign.wt}{ Allowable signs for feature weights. Can be "both",
"positive",  or "negative". Default: "both" }
}
\details{
}
\value{
\item{shrinkages}{Shrinkage values used}
\item{lrtest.shrink}{ Likeihood ratio tests for reduced models }
% superivsed PC predictor}
\item{num.features}{Number of features used in each shrunken model}
\item{n.components }{Number of supervised  principal components used}

\item{v.preval.red}{Outcome predictor from reduced models. Array of
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
aa<-superpc.cv(a, data)

fit<- superpc.predict(a, data, data.test, threshold=1.0, n.components=1, prediction.type="continuous")

fit.red<- superpc.predict.red(a,data, data.test, .6)

fit.redcv<- superpc.predict.red.cv(fit.red, aa,  data,  .6)


}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line
