\name{superpc.predict}
\alias{superpc.predict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Form principal components predictor from a trained superpc object}
\description{
Computes supervised principal components, using scores from "object"
}
\usage{
superpc.predict(object, data, newdata, threshold, n.components = 3, prediction.type = c("continuous", "discrete", "nonzero"), n.class = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ Obect returned by superpc.train }
  \item{data}{ List of training data, of form described in superpc.train documentation, }
  \item{newdata}{ List of test data; same form as training data }
  \item{threshold}{ Threshold for scores: features with abs(score)>threshold
are retained. }
  \item{n.components}{ Number of principal components to compute.
Should be 1,2 or 3. }
  \item{prediction.type}{ "continuous" for raw principal component(s);
"discrete" for principal component categorized in equal bins;
"nonzero" for indices of features that pass the threshold }
  \item{n.class}{ Number of classes into which predictor is binned
(for prediction.type="discrete" }
}
\details{
}
\value{
  \item{v.pred }{Supervised principal componients predictor}
  \item{u }{U matrix from svd of feature matrix x}
  \item{d }{singual values  from svd of feature matrix x}
 \item{which.features}{Indices of features exceeding threshold}
 \item{n.components}{Number of supervised  principal components requested}
\item{call}{calling sequence}
}
\author{ Eric Bair and Robert Tibshirani }



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

fit<- superpc.predict(a, data, data.test, threshold=1.0, n.components=1)

plot(fit$v.pred,ytest)

}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line
