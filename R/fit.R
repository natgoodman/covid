#################################################################################
##
## Author:  Nat Goodman
## Created: created 20-05-06
##          from covid.R created circa 20-04-26
##          adapted from clapi/R/stats_hiddn.R created 20-01-20
##
## Copyright (C) 2020 Nat Goodman.
## 
## Data fitting (aka interpolation) functions
## Top-level 'fit' function in transform.R calls 'fitfun' to do the fitting
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Fit (aka interpolate) data frame ----
## drivers for data fitting methods. note top-level 'fit' in transform.R
## fitfun returns interpolation function
## smooth returns interpolated x,y values
## x is vector or 2-dimensional matrix-like object with same number of columns as y
## y is vector or 2-dimensional matrix-like object
## if clamp set, forces interpolated values to lie in clamp range
fitfun=function(x,y,method=cq(aspline,ispline,sspline,spline,loess,linear,approx,step),
                    args=list(),clamp=NULL,simplify=FALSE) {
  method=match.arg(method);
  if (method=='approx') method='linear';
  fun_=get0(paste0(method,'_'),mode='function');
  if (is.null(fun_)) stop(paste0('Bad news: function not found for method=',method,
                                 '. Should have been caught earlier!'));
  ## set default args. actual arg values override
  args=cl(
    switch(method,
           aspline=list(method='improved'),
           sspline=list(spar=0.5),
           spline=list(method='monoH.FC'),
           loess=list(span=0.75),
           linear=list(method='linear',yleft=0,rule=2),
           step=list(method='constant',yleft=0,rule=2),
           list()),
    args);
  if (is.vector(x)&&is.vector(y)) {
    nna=!(is.na(x)|is.na(y));
    fun=fitfun1(x[nna],y[nna],fun_,args,clamp);
    if (!simplify) fun=list(fun);
  } else {
    if (is.vector(x)) x=matrix(x)
    else if (!is_2d(x)) stop("'x' must be vector or 2-dimensional matrix-like object");
    if (is.vector(y)) y=matrix(y)
    else if (!is_2d(y)) stop("'y' must be vector or 2-dimensional matrix-like object");
    if (ncol(x)>1&&ncol(x)!=ncol(y))
      stop("When 'x' has multiple columns, it must have same number of columns as 'y'");
    ## compute 'mask' to remove NAs. code adapted from akima::aspline
    nax=apply(x,2,is.na);
    nay=apply(y,2,is.na);
    if (ncol(x)==1) nna=!(nax[,1]|nay) else nna=!(nax|nay);
    fun=sapply(seq_len(ncol(y)),function(j) {
      nna=nna[,j];
      if (length(which(nna))==0) function(xout) rep(NA,length(xout))
      else {
        x=if(ncol(x)==1) x[nna,1] else x[nna,j];
        y=y[nna,j];
        fitfun1(x,y,fun_,args,clamp);
      }
    });
    names(fun)=colnames(y);
    if (simplify&length(fun)==1) fun=fun[[1]];
  }
  fun;
}
## generate interpolation function for single x, y vectors
fitfun1=function(x,y,fun_,args,clamp=NULL) {
  x=as.numeric(x);
  fun=do.call(fun_,cl(x=x,y=y,args));
  ## add clamp to fun if necessary
  ## CAUTION: don't assign fun in block below else R generates recursive call. WRONG!!
  if (length(clamp)==1) function(x) pmax(clamp,fun(x))
  else if (length(clamp)>1) {
    clamp.lo=min(clamp);
    clamp.hi=max(clamp);
    function(x) pmin(clamp.hi,pmax(clamp.lo,fun(x)));
  } else fun;
}
## wrap fitfun for use inside plot functions
## x is vector or 2-dimensional matrix-like object with same number of columns as y
## y is vector or 2-dimensional matrix-like object
## simplify controls shape of result
##   'matrix', 'list' means return x,y as given type
##   TRUE means as simple as possible (vectors or data frames)
##   FALSE means 'list'
## by, length.out, relative passed to seqx to control interpolation values
##   default: interpolate at 1e3 evely spaced points
smooth=function(x,y,method,args=list(),clamp=NULL,by=NULL,length.out=NULL,relative=TRUE,
                simplify=cq(matrix,list,TRUE)) {
  if (method=='none') return(list(x=x,y=y));
  if (is.null(by)&&is.null(length.out)) if (relative) by=1e-3 else by=1;
  if (!is.logical(simplify)) simplify=match.arg(simplify)
  else simplify=if(simplify) 'TRUE' else 'list';
  fun=fitfun(x=x,y=y,method=method,args=args,clamp=clamp,simplify=FALSE);
  if (length(fun)==1) {
    f=fun[[1]];
    if (!is.vector(x)) x=x[,1];
    x.smooth=seqx(x,by,length.out,relative);
    y.smooth=f(x.smooth);
    switch(simplify,
           matrix={x.smooth=matrix(x.smooth); y.smooth=matrix(y.smooth)},
           list={x.smooth=list(x.smooth); y.smooth=list(y.smooth)});
  } else {
    if (is.vector(x)) x=matrix(x);
    x.smooth=seqx(x,by,length.out,relative);
    y.smooth=if(is.list(x.smooth))
               sapply(seq_along(fun),function(j) fun[[j]](x.smooth[[min(j,length(x.smooth))]]))
             else sapply(seq_along(fun),function(j) fun[[j]](x.smooth[,min(j,ncol(x.smooth))]));
    if (simplify=='TRUE') simplify='data.frame';
    switch(simplify,
           matrix=if (is.list(x.smooth)) {
                    nrow=max(sapply(x.smooth,length));
                    x.smooth=sapply(x.smooth,function(x) fill(x,nrow));
                    y.smooth=sapply(y.smooth,function(y) fill(y,nrow));
                  },
           list=if (!is.list(x.smooth)) {
                  ## code below from stackoverflow.com/questions/6819804. Thx!
                  x.smooth=lapply(seq_len(ncol(x.smooth)), function(j) x.smooth[,j]);
                  y.smooth=lapply(seq_len(ncol(y.smooth)), function(j) y.smooth[,j]);
                })
  }
  list(x=x.smooth,y=y.smooth);
}

## these functions wrap underlying data fitting (aka interpolation) methods with consistent API
## CAUTION: can't simply pass ... to approxfun, 'cuz aspline needs ..., too
##   can't simply split ... based on names(formals(fun)) 'cuz both functions have 'method'
##   could give aspline first dibs, remove its args from ..., then pass rest to approxfun
##     but solution here is simpler and works fine
##       FIX these: aspline, ispline, sspline
##       OK as is: spline, loess (but needs clamping), approx
## akima spline
## TODO: figure out yleft, yright, rule
aspline_=function(x,y,...) {
  fit.x=seqx(x,length.out=10,relative=TRUE);
  fit.y=suppressWarnings(akima::aspline(x,y,fit.x,...))$y;
  approxfun(fit.x,fit.y,rule=2);
}
## interpSpline
library(splines);
ispline_=function(x,y,...) {
  model=interpSpline(x,y,...);
  function(xout) suppressWarnings(predict(model,as.numeric(xout)))$y;
}
## smooth.spline
sspline_=function(x,y,...) {
  model=smooth.spline(x,y,...);
  function(xout) suppressWarnings(predict(model,as.numeric(xout))$y);
}
## splinefun
spline_=function(x,y,...) {
  f=suppressWarnings(splinefun(as.numeric(x),y,...));
  function(xout) suppressWarnings(f(as.numeric(xout)))
}
## loess
loess_=function(x,y,...) {
  model=loess(y~x,data=data.frame(x=as.numeric(x),y),...);
  function(xout) suppressWarnings(predict(model,data.frame(x=as.numeric(xout))));
}
## linear and step use approxfun with different args
linear_=step_=function(x,y,x.fit,...) {
  approxfun(x,y,...);
}
