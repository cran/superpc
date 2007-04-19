\name{superpc.rainbowplot}
\alias{superpc.rainbowplot}
\title{ Make rainbow plot of superpc and compeiting predictors}
\description{
Makes a heatmap display of  outcome predictions from superpc,
 along with expected survival time, and  values of competing predictors
}
\usage{
 superpc.rainbowplot(data, pred, sample.labels,  competing.predictors, call.win.metafile=FALSE)
}

\arguments{
 \item{data}{ List of (test) data, of form described in superpc.train documentation}
  \item{pred}{Superpc score from superpc.predict or  superpc.predict.red}
  \item{sample.labels}{Vector of sample labels of test data}
\item{competing.predictors}{List of competing predictors to be plotted}
\item{call.win.metafile}{Used only by Excel interface call to function}
}

\details{Any censored survival times are estimated by E(T|T>C), where
$C$ is the observed  censoring  time and the Kaplan-Meier estimate from
the training set is used to estimate the expectation.
}

\value{}
\author{ Eric Bair and Robert Tibshirani}


\seealso{}
\examples{
set.seed(332)
x<-matrix(rnorm(1000*40),ncol=40)
y<-10+svd(x[1:60,])$v[,1]+ 5*rnorm(40)
censoring.status<- sample(c(rep(1,30),rep(0,10)))

ytest<- 10+svd(x[1:60,])$v[,1]+ 5*rnorm(40)
censoring.status.test<- sample(c(rep(1,30),rep(0,10)))


competing.predictors.test=list(pred1=rnorm(40), pred2=as.factor(sample(c(1,2),replace
=TRUE,size=40)))

featurenames <- paste("feature",as.character(1:1000),sep="")
data<-list(x=x,y=y, censoring.status=censoring.status, featurenames=featurenames)

data.test=list(x=x,y=ytest, censoring.status=censoring.status.test, featurenames=featurenames)

sample.labels=paste("te",as.character(1:40),sep="")

a<- superpc.train(data, type="survival")
pred=superpc.predict(a,data,data.test,threshold=.25, n.components=1)$v.pred


superpc.rainbowplot(data,pred, sample.labels,competing.predictors=competing.predictors.test)

}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line
