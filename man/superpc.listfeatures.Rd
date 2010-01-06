\name{superpc.listfeatures}
\alias{superpc.listfeatures}
\title{ Return a list of the important predictors }
\description{
 Return a list of the important predictor
}
\usage{
superpc.listfeatures(data, train.obj, fit.red, fitred.cv = NULL,
num.features=NULL, component.number = 1)
}

\arguments{
  \item{data}{Data object }
  \item{train.obj}{Object returned by superpc.train}
  \item{fit.red}{Object returned by superpc.predict.red, applied to training set}
  \item{fitred.cv}{(Optional) object returned by superpc.predict.red.cv}
  \item{num.features}{Number of features to list. Default is all features.}
  \item{component.number}{Number of principal component (1,2, or 3) used to determine feature importance scores}
 }

\value{
Returns matrix of features and their importance scores, in order
of decreasing absolute value of importance score. The importance score
is the correlation of the reduced predictor and the full supervised PC
predictor. It also lists the raw score- for survival data, this is the Cox score
for that feature; for regression, it is the standardized regression coefficient.
If fitred.cv is supplied,  the function also reports
the average rank of the gene in the
cross-validation folds, and the proportion of times  that the gene is chosen
(at the given threshold) in the cross-validation folds.
}
\author{Eric Bair and Rob Tibshirani}


\examples{
#generate some data

x<-matrix(rnorm(1000*40),ncol=40)
y<-10+svd(x[1:60,])$v[,1]+ .1*rnorm(40)
ytest<-10+svd(x[1:60,])$v[,1]+ .1*rnorm(40)
censoring.status<- sample(c(rep(1,30),rep(0,10)))
censoring.status.test<- sample(c(rep(1,30),rep(0,10)))
featurenames <- paste("feature",as.character(1:1000),sep="")
data<-list(x=x,y=y, censoring.status=censoring.status, featurenames=featurenames)
data.test<-list(x=x,y=ytest, censoring.status=censoring.status.test, featurenames= featurenames)

a<- superpc.train(data, type="survival")

fit<- superpc.predict(a, data, data.test, threshold=1.0, n.components=1, prediction.type="continuous")

fit.red<- superpc.predict.red(a,data, data.test, .6)
superpc.listfeatures(data, a,  fit.red, num.features=20)

}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line
