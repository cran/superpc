\name{superpc.train}
\alias{superpc.train}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Prediction by supervised principal components}
\description{
Does prediction of a quantitative  regression 
or survival outcome, by the supervised principal components
method.
}
\usage{
superpc.train(data, type = c("survival", "regression"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ Data object with components x- p by n matrix of features,
one observation per column; y- n-vector of outcome measurements;
status- n-vector of censoring status (1= died or event occurred, 0=survived, or event was censored), needed
for a censored survival outcome}
  \item{type}{Problem type:  "survival" for censored survival outcome, or "regression"
for simple quantitative outcome}
}
\details{ Compute wald scores for each feature (gene), for later use in 
superpc.predict and superpc.cv
}
\value{
 gene.scores=gene.scores, 
               type=type, 
               call = this.call
  \item{feature.scores }{Score for each feature (gene)}
  \item{type }{problem type}
 \item{call }{calling sequence}
}
\references{Bair E, Tibshirani R (2004) Semi-supervised methods to predict patient survival from gene expression data. PLoS Biol 2004 April; 2 (4): e108;
http://www-stat.stanford.edu/~tibs/superpc}
\author{ Eric Bair and Robert Tibshirani }


\examples{
#generate some example data
set.seed(332)
x<-matrix(rnorm(1000*40),ncol=40)
y<-10+svd(x[1:60,])$v[,1]+ .1*rnorm(40)
status<- sample(c(rep(1,30),rep(0,10)))

featurenames <- paste("feature",as.character(1:1000),sep="")
data<-list(x=x,y=y, status=status, featurenames=featurenames)


a<- superpc.train(data, type="survival")
}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line
