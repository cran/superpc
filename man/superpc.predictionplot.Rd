\name{superpc.predictionplot}
\alias{superpc.predictionplot}
\title{ Plot outcome predictions from superpc}
\description{
Plots outcome predictions from superpc
}
\usage{
superpc.predictionplot(train.obj, data, data.test,  threshold, n.components=3,
  n.class=2, shrinkage=NULL,  call.win.metafile=FALSE)
 
}
\arguments{
  \item{train.obj}{ Object returned by superpc.train }
 \item{data}{ List of training data, of form described in superpc.train documen
tation, }
  \item{data.test}{ List of test data; same form as training data }
  \item{threshold}{ Threshold for scores: features with abs(score)>threshold
are retained. }
  \item{n.components}{ Number of principal components to compute.
Should be 1,2 or 3. }
\item{n.class}{Number of classes for survival stratification. Onply applicable
for survival data. Default 2.}
\item{shrinkage}{Shrinkage to be applied to feature loadings. Default is NULL
meaning no shrinkage}
\item{call.win.metafile}{Used only by Excel interface call to function}
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
censoring.status<- sample(c(rep(1,30),rep(0,10)))

featurenames <- paste("feature",as.character(1:1000),sep="")
data<-list(x=x,y=y, censoring.status=censoring.status, featurenames=featurenames)


a<- superpc.train(data, type="survival")

superpc.predictionplot(a,data,data,threshold=1)

}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line
