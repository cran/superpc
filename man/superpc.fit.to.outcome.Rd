\name{superpc.fit.to.outcome}
\alias{superpc.fit.to.outcome}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Fit predictive model using outcome of supervised principal components }
\description{
 Fit predictive model using outcome of supervised principal components, via
either coxph (for surival data) or lm (for regression data)
}
\usage{
superpc.fit.to.outcome(fit, data.test, score, print=TRUE)
}
\arguments{
  \item{fit}{ Object returned by superpc.train }
  \item{data.test}{ Data object for prediction. Same form as data object documented in superpc.train.}  
  \item{score}{Supervised principal component score, from superpc.predict  }
\item{print}{Should a summary of the fit be printed? Default TRUE}
}
\details{
}
\value{
Returns summary of coxph or lm fit
}
\references{ ~put references to the literature/web site here ~ }
\author{ Eric Bair and Robert Tibshirani }
\note{ ~~further notes~~ }

\examples{
set.seed(332)
#generate some data

x<-matrix(rnorm(1000*20),ncol=20)
y<-10+svd(x[1:30,])$v[,1]+ .1*rnorm(20)
ytest<-10+svd(x[1:30,])$v[,1]+ .1*rnorm(20)
status<- sample(c(rep(1,17),rep(0,3)))
status.test<- sample(c(rep(1,17),rep(0,3)))



featurenames <- paste("feature",as.character(1:1000),sep="")
data<-list(x=x,y=y, status=status, featurenames=featurenames)
data.test<-list(x=x,y=ytest, status=status.test, featurenames= featurenames)



a<- superpc.train(data, type="survival")

fit<- superpc.predict(a, data, data.test, threshold=1.0, n.components=1, prediction.type="continuous")

superpc.fit.to.outcome(a, data, fit$v.pred)
}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line

