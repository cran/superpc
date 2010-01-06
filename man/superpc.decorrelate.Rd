\name{superpc.decorrelate}
\alias{superpc.decorrelate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Decorrelate features  with respect to competing predictors}
\description{
Fits a linear model to the  features as a function of some competing predictors.
Replaces the features by the residual from this fit.
These "decorrelated" features are then used in the superpc model building process, to
explicitly look for predictors that are independent of the competing predictors.
Useful for example, when the competing predictors are clinical predictors like stage, grade etc.
}
\usage{
superpc.decorrelate(x, competing.predictors) 
}
\arguments{
  \item{x}{ matrix of features. Different features  in different  rows, one observation per column}
\item{competing.predictors}{List of one or more competing predictors.  Discrete predictors should be factors}
}
\value{
Returns   lm (linear model) fit of rows of x on compeiting predictors
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
censoring.status<- sample(c(rep(1,17),rep(0,3)))
censoring.status.test<- sample(c(rep(1,17),rep(0,3)))
competing.predictors=list(pred1=rnorm(20), pred2=as.factor(sample(c(1,2),replace=TRUE,size=20)))
featurenames <- paste("feature",as.character(1:1000),sep="")

# decorrelate x
foo<-superpc.decorrelate(x,competing.predictors)

xnew<-t(foo$res)

# now use xnew in superpc

data<-list(x=xnew,y=y, censoring.status=censoring.status, featurenames=featurenames)

a<- superpc.train(data, type="survival")

# etc. Remember to decorrelate test data in the same way, before making predictions.
}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line

