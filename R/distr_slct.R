#################################################################################
##
## Author:  Nat Goodman
## Created: 21-02-19
##
## Copyright (C) 2021 Nat Goodman.
## 
## distribution functions for selection with and without replacement; uniform and biased
## wraps base R and BiasedUrn functions in standard API
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
library(BiasedUrn);

########## drivers for all selection distributions
p_slct=function(x,m1,m2,n,odds=1,replace=FALSE,biased=(odds!=1),lower.tail=TRUE,precision=1E-7) {
  if (odds!=1&&!biased) stop("'odds' must be 1 when 'biased' is FALSE, not ",odds);
  if (!replace) {
    ## hypergeometric
    if (!biased) phypera(x,m1,m2,n,lower.tail)
    else phyperb(x,m1,m2,n,odds,lower.tail,precision)
  } else {
    ## binomial
    if (!biased) pbinoma(x,m1,m2,n,lower.tail)
    else pbinomb(x,m1,m2,n,odds,lower.tail)
  }
}
d_slct=function(x,m1,m2,n,odds=1,replace=FALSE,biased=(odds!=1),precision=1E-7) {
  if (odds!=1&&!biased) stop("'odds' must be 1 when 'biased' is FALSE, not ",odds);
  if (!replace) {
    ## hypergeometric
    if (!biased) dhypera(x,m1,m2,n,lower.tail)
    else dhyperb(x,m1,m2,n,odds,lower.tail,precision=1E-7)
  } else {
    ## binomial
    if (!biased) dbinoma(x,m1,m2,n,lower.tail)
    else dbinomb(x,m1,m2,n,odds,lower.tail)
  }
}
q_slct=function(p,m1,m2,n,odds=1,replace=FALSE,biased=(odds!=1),lower.tail=TRUE,precision=1E-7) {
  if (odds!=1&&!biased) stop("'odds' must be 1 when 'biased' is FALSE, not ",odds);
  if (!replace) {
    ## hypergeometric
    if (!biased) qhypera(p,m1,m2,n,lower.tail)
    else qhyperb(p,m1,m2,n,odds,lower.tail,precision)
  } else {
    ## binomial
    if (!biased) qbinoma(p,m1,m2,n,lower.tail)
    else qbinomb(p,m1,m2,n,odds,lower.tail)
  }
}
r_slct=function(nr,m1,m2,n,odds=1,replace=FALSE,biased=(odds!=1),precision=1E-7) {
  if (odds!=1&&!biased) stop("'odds' must be 1 when 'biased' is FALSE, not ",odds);
  if (!replace) {
    ## hypergeometric
    if (!biased) rhypera(nr,m1,m2,n)
    else rhyperb(nr,m1,m2,n,odds,precision=1E-7)
  } else {
    ## binomial
    if (!biased) rbinoma(nr,m1,m2,n)
    else rbinomb(nr,m1,m2,n,odds)
  }
}
odds_slct=function(mu,m1,m2,n,replace=FALSE,biased=TRUE,precision=1E-7) {
  if (!replace) {
    ## hypergeometric
    if (!biased) odds_hypera(mu,m1,m2,n)
    else odds_hyperb(mu,m1,m2,n,odds,precision=1E-7)
  } else {
    ## binomial
    if (!biased) odds_binoma(mu,m1,m2,n)
    else odds_binomb(mu,m1,m2,n,odds)
  }
}
mean_slct=function(m1,m2,n,odds=1,replace=FALSE,biased=(odds!=1),precision=1E-7) {
  if (odds!=1&&!biased) stop("'odds' must be 1 when 'biased' is FALSE, not ",odds);
  if (!replace) {
    ## hypergeometric
    if (!biased) mean_hypera(m1,m2,n)
    else mean_hyperb(m1,m2,n,odds,precision=1E-7)
  } else {
    ## binomial
    if (!biased) mean_binoma(m1,m2,n)
    else mean_binomb(m1,m2,n,odds)
  }
}
var_slct=function(m1,m2,n,odds=1,replace=FALSE,biased=(odds!=1),precision=1E-7) {
  if (odds!=1&&!biased) stop("'odds' must be 1 when 'biased' is FALSE, not ",odds);
  if (!replace) {
    ## hypergeometric
    if (!biased) mean_hypera(m1,m2,n)
    else mean_hyperb(m1,m2,n,odds,precision=1E-7)
  } else {
    ## binomial
    if (!biased) mean_binoma(m1,m2,n)
    else mean_binomb(m1,m2,n,odds)
  }
}


########## unbiased hypergeometric distribution
## wrapper for base R hyper functions with same API as hyperb
## the 'a' final letter is to distinguish from base R functions
phypera=function(x,m1,m2,n,lower.tail=TRUE) phyper(q=x,m=m1,n=m2,k=n,lower.tail=lower.tail)
dhypera=function(x,m1,m2,n) dhyper(x=x,m=m1,n=m2,k=n)
qhypera=function(p,m1,m2,n,lower.tail=TRUE) qhyper(p=p,m=m1,n=m2,k=n,lower.tail=lower.tail)
rhypera=function(nr,m1,m2,n) rhyper(nn=nr,m=m1,n=m2,k=n)

## odds included for compatibility with hyperb
odds_hypera=function(mu,m1,m2,n,precision=0.1) 1
## mean, var. formulas adapted from R ref manual
mean_hypera=function(m1,m2,n) {
  prob=m1/(m1+m2);
  n*prob;
}
var_hypera=function(m1,m2,n) {
  prob=m1/(m1+m2);
  var=n*prob*(1-prob)*(m1+m2-n)/(m1+m2-1);
}

########## biased hypergeometric distribution
##  technically, Fisher’s noncentral hypergeometric distribution
phyperb=function(x,m1,m2,n,odds=1,lower.tail=TRUE,precision=1E-7) {
  recycle(x,m1,m2,n,odds);
  mapply(pFNCHypergeo,x,m1,m2,n,odds,MoreArgs=list(lower.tail=lower.tail,precision=precision));
}
dhyperb=function(x,m1,m2,n,odds=1,precision=1E-7) {
  recycle(x,m1,m2,n,odds);
  mapply(dFNCHypergeo,x,m1,m2,n,odds,MoreArgs=list(precision=precision));
}
qhyperb=function(p,m1,m2,n,odds=1,lower.tail=TRUE,precision=1E-7) {
  recycle(p,m1,m2,n,odds);
  mapply(qFNCHypergeo,p,m1,m2,n,odds,MoreArgs=list(lower.tail=lower.tail,precision=precision));
}
rhyperb=function(nr,m1,m2,n,odds=1,precision=1E-7) {
  ## nr is special. sigh...
  ##   per eg, rnorm: if length(n)>1, number required is length(n)
  ##   have to spread this over the mapply iterations, with any extra at beginning
  if (length(nr)>1) nr=length(nr);
  mapl=recycle(m1,m2,n,odds);
  if (mapl>1) {
    ## spread nr over mapl iterations
    quotient=nr %/% mapl;
    remainder=nr %% mapl;
    nrl=rep(quotient,mapl);                                  # start with quotient spread evenly
    if (remainder>0) for (i in 1:remainder) nrl[i]=nrl[i]+1; # put remainder at beginning
    rl=mapply(rFNCHypergeo,nrl,m1,m2,n,odds,MoreArgs=list(precision=precision),SIMPLIFY=FALSE);
    ## interdigitate results
    r=numeric(nr);
    for(i in 1:mapl) r[seq(i,nr,by=mapl)]=rl[[i]][1:nrl[i]];
  } else {
    ## do it directly
    r=rFNCHypergeo(nr,m1,m2,n,odds,precision=precision);
  }
  r;
}
odds_hyperb=function(mu,m1,m2,n,precision=0.1) {
  recycle(mu,m1,m2,n);
  mapply(oddsFNCHypergeo,mu,m1,m2,n,MoreArgs=list(precision=precision));
}
mean_hyperb=function(m1,m2,n,odds=1,precision=1E-7) {
  recycle(m1,m2,n,odds);
  mapply(meanFNCHypergeo,m1,m2,n,odds,MoreArgs=list(precision=precision));
}
var_hyperb=function(m1,m2,n,odds=1,precision=1E-7) {
  recycle(m1,m2,n,odds);
  mapply(varFNCHypergeo,m1,m2,n,odds,MoreArgs=list(precision=precision));
}

########## unbiased binomial distribution
## wrapper for base R binom functions with same API as hyperb
## the 'a' final letter is to distinguish from base R functions
pbinoma=function(x,m1,m2,n,lower.tail=TRUE)
  pbinom(q=x,size=n,prob=m1/(m1+m2),lower.tail=lower.tail)
dbinoma=function(x,m1,m2,n) dbinom(x=x,size=n,prob=m1/(m1+m2))
qbinoma=function(p,m1,m2,n,lower.tail=TRUE)
  qbinom(p=p,size=n,prob=m1/(m1+m2),lower.tail=lower.tail)
rbinoma=function(nr,m1,m2,n) rbinom(n=nr,size=n,prob=m1/(m1+m2))

## odds included for compatibility with hyperb
odds_binoma=function(mu,m1,m2,n,precision=0.1) 1
## mean, var. formulas adapted from R ref manual
mean_binoma=function(m1,m2,n) {
  prob=m1/(m1+m2);
  n*prob;
}
var_binoma=function(m1,m2,n) {
  prob=m1/(m1+m2);
  var=n*prob*(1-prob);
}

########## biased binomial distribution
##   conceptually "silly" because bias simply changes prob
##   included for stylistic compqtibility with hyperb
pbinomb=function(x,m1,m2,n,odds=1,lower.tail=TRUE) {
  ## adjust m1 & prob from odds
  m1=m1*odds;
  prob=m1/(m1+m2);
  pbinom(q=x,size=n,prob=prob,lower.tail=lower.tail);
}
dbinomb=function(x,m1,m2,n,odds=1) {
  ## adjust m1 & prob from odds
  m1=m1*odds;
  prob=m1/(m1+m2);
  dbinom(x=x,size=n,prob=prob);
}
qbinomb=function(p,m1,m2,n,odds=1,lower.tail=TRUE) {
  ## adjust m1 & prob from odds
  m1=m1*odds;
  prob=m1/(m1+m2);
  qbinom(p=p,size=n,prob=prob,lower.tail=lower.tail)
}
rbinomb=function(nr,m1,m2,n,odds=1) {
  ## adjust m1 & prob from odds
  m1=m1*odds;
  prob=m1/(m1+m2);
  rbinom(n=nr,size=n,prob=prob);
}

odds_binomb=function(mu,m1,m2,n) {
  prob=mu/n;
  (m2*prob)/(m1*(1-prob));
}
## mean, var. formulas adapted from R ref manual
mean_binomb=function(m1,m2,n,odds=1) {
  ## adjust m1 & prob from odds
  m1=m1*odds;
  prob=m1/(m1+m2);
  n*prob;
}
var_binomb=function(m1,m2,n,odds=1) {
  ## adjust m1 & prob from odds
  m1=m1*odds;
  prob=m1/(m1+m2);
  var=n*prob*(1-prob);
}

