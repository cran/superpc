\name{superpc.predict.red}
\alias{superpc.predict.red}
\title{Feature selection for supervised principal components }
\description{ 
Forms  reduced models to approximate the
 supervised principal component  predictor.}

\usage{
superpc.predict.red(fit, data, data.test, threshold, n.components = 3, n.shrinkage= 20, shrinkages=NULL,compute.lrtest = TRUE, sign.wt="both",  prediction.type =
                 c("continuous", "discrete"), n.class = 2 )
}
\arguments{
  \item{fit}{ Object returned by superpc.train}
  \item{data}{ Training data object, of form described in superpc.train dcoumentation }
  \item{data.test}{ Test data object; same form as train  }
  \item{threshold}{ Feature score threshold; usually estimated from superpc.cv}
  \item{n.components}{ Number of principal components to  examine;
should equal 1,2, etc  up to the number of components used in training }
  \item{n.shrinkage}{ Number of shrinkage values to consider. Default 20. }
  \item{shrinkages}{ Shrinkage values to consider. Default NULL. }
  \item{compute.lrtest}{ Should the likelihood ratio test be computed? Default TRUE  }
\item{sign.wt}{Signs of feature weights allowed: "both", "pos", or "neg"}
\item{prediction.type}{Type of prediction: "continuous" (Default) or "discrete". In the latter,
superprc score is divided into n.class groups}
\item{n.class}{Number of groups for discrete predictor. Default 2. }
}
\details{
Soft-thresholding by each of the  "shrinkages"  values is applied to the PC loadings.
This reduce the number of features used in the model.
 The reduced predictor is then used  in place of the supervised PC predictor.
}

\value{
\item{shrinkages}{ Shrinkage values used }
\item{lrtest.reduced}{ Likelihood ratio tests for reduced models }
\item{num.features}{ Number of features used in each reduced model }
\item{feature.list}{ List of features used in each reduced model }
\item{coef}{Least squares coefficients for each reduced model} 
\item{import}{Importance scores for features}
\item{wt}{Weight for each feature, in constructing the reduced predictor}
\item{v.test}{Outcome predictor from reduced models. Array of
n.shrinkage by (number of test observations) }
\item{v.test.1df}{Outcome combined  predictor from reduced models. Array of
n.shrinkage by (number of test observations) }
\item{n.components}{ Number of principal components used}
\item{type }{Type of outcome}
\item{call}{calling sequence}
}
\references{ ~put references to the literature/web site here ~ }
\author{ Eric Bair and Robert Tibshirani }
\note{ ~~further notes~~ }


\examples{


set.seed(332)
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

fit.red<- superpc.predict.red(a,data, data.test, threshold=.6)
superpc.plotred.lrtest(fit.red)

}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line

