\name{superpc.plotcv}
\alias{superpc.plotcv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot output from superpc.cv}
\description{
Plots pre-validation results from plotcv, to aid in choosing best threshold
}
\usage{
superpc.plotcv(object, smooth = TRUE, smooth.df = 10, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ Object returned by superpc.cv }
  \item{smooth}{ Should plot be smoothed? Default TRUE. }
  \item{smooth.df}{ Degrees of fredom for smooth.spline, default 10.}
  \item{\dots}{ Additional plotting args to be passed to matplot }
}
\details{
}
\value{
}
\author{ Eric Bair and Robert Tibshirani}


\seealso{}
\examples{
set.seed(332)
x<-matrix(rnorm(1000*40),ncol=40)
y<-10+svd(x[1:60,])$v[,1]+ .1*rnorm(40)
status<- sample(c(rep(1,30),rep(0,10)))

featurenames <- paste("feature",as.character(1:1000),sep="")
data<-list(x=x,y=y, status=status, featurenames=featurenames)


a<- superpc.train(data, type="survival")
aa<-superpc.cv(a,data)

superpc.plotcv(aa)

}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line
