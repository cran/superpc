\name{superpc.lrtest.curv}
\alias{superpc.lrtest.curv}
\title{ Compute values of likelihood ratio test  from supervised  principal components fit }
\description{
Compute values of likelihood ratio test  from supervised  principal components fit 
}
\usage{
superpc.lrtest.curv(object, data, newdata, n.components = 1, threshold = NULL, n.threshold = 20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{ Object returned by superpc.train }
\item{data}{ List of training data, of form described in superpc.train documentation}
 \item{newdata}{ List of test data; same form as training data }
  \item{n.components}{ Number of principal components to compute. Should be 1,2 or 3. }
 \item{threshold}{ Set of thresholds for scoresL default is n.threshold values equally spaced over the range of the feature scores}
  \item{n.threshold}{ Number of thresholds to use; default 20. Should be 1,2 or 3. }
}
\details{
}
\value{
  If it is a LIST, use
  \item{lrtest }{Values of likelihood ratio test statistic}
  \item{comp2 }{Description of 'comp2'}
 \item{threshold}{ Thresholds used}
\item{num.features}{Number of features exceeding threshold}
\item{type}{Type of outcome variable}
\item{call}{calling sequence}
}
\references{ ~put references to the literature/web site here ~ }
\author{ Eric Bair and Robert Tibshirani }

\note{ ~~further notes~~ }


\seealso{}
\examples{
set.seed(332)
#generate some data

x<-matrix(rnorm(1000*20),ncol=20)
y<-10+svd(x[1:30,])$v[,1]+ .1*rnorm(20)
ytest<-10+svd(x[1:30,])$v[,1]+ .1*rnorm(20)
censoring.status<- sample(c(rep(1,17),rep(0,3)))
censoring.status.test<- sample(c(rep(1,17),rep(0,3)))

featurenames <- paste("feature",as.character(1:1000),sep="")
data<-list(x=x,y=y, censoring.status=censoring.status, featurenames=featurenames)
data.test<-list(x=x,y=ytest, censoring.status=censoring.status.test, featurenames= featurenames)


a<- superpc.train(data, type="survival")

fit<- superpc.predict(a, data, data.test, threshold=1.0, n.components=1, prediction.type="continuous")


aa<- superpc.lrtest.curv(a, data, data.test)
superpc.plot.lrtest(aa)
}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line
