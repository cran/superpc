\name{superpc-internal}
\title{Internal superpc functions}
\alias{cor.func}
\alias{coxfunc}
\alias{coxscor}
\alias{coxstuff}
\alias{coxvar}
\alias{mysvd}
\description{Internal superpc functions}
\usage{
cor.func(x, y, fudge = median(sd)) 
coxfunc(x, y, status, fudge = median(sd)) 
coxscor(x, y, ic, offset = rep(0, length(y))) 
coxvar(x, y, ic, offset = rep(0, length(y)), coxstuff.obj = NULL) 
mysvd(x, n.components = NULL) 
}
\author{Eric Bair and Rob Tibshirani}
\details{
  These are not to be called by the user.
}
\keyword{internal}

