\name{superpc-internal}
\title{Internal superpc functions}
\alias{cor.func}
\alias{coxfunc}
\alias{coxscor}
\alias{coxstuff}
\alias{coxvar}
\alias{mysvd}
\alias{superpc.xl.fit.to.clin}
\alias{superpc.xl.get.threshold.range}
\alias{superpc.xl.listgenes.compute}
\alias{superpc.xl.decorrelate}
\alias{superpc.xl.decorrelate.test}
\description{Internal superpc functions}
\usage{
cor.func(x, y,  s0.perc) 
coxfunc(x, y, censoring.status,  s0.perc) 
coxscor(x, y, ic, offset = rep(0, length(y))) 
coxvar(x, y, ic, offset = rep(0, length(y)), coxstuff.obj = NULL) 
mysvd(x, n.components = NULL) 
superpc.xl.fit.to.clin(fit, data.test,score, pamr.xl.test.sample.labels, pamr.xl.clindata,pamr.xl.clinsamplabels, pamr.xl.clinprednames, pamr.xl.clinpredtype )
superpc.xl.get.threshold.range(train.obj)
superpc.xl.listgenes.compute(data, train.obj, fit.red, num.genes=NULL, component.number = 1)
superpc.xl.decorrelate(data, pamr.xl.train.sample.labels, pamr.xl.clindata, pamr.xl.clinsamplabels, pamr.xl.clinprednames, pamr.xl.clinpredtype)
superpc.xl.decorrelate.test(object.decorr, xtest, pamr.xl.train.sample.labels, pamr.xl.clindata, pamr.xl.clinsamplabels, pamr.xl.clinprednames, pamr.xl.clinpredtype)
}
\author{Eric Bair and Rob Tibshirani}
\details{
  These are not to be called by the user.
}
\keyword{internal}

