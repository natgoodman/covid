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
## Note top-level 'fit' function in transform.R
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Fit (aka interpolate) data frame ----
## driver for data fitting methods. note top-level 'fit' in transform.R
## unit used for methods whose core function can only return interpolated results
##   (presently just aspline)
fit_fun=function(x,y,method=cq(aspline,ispline,sspline,spline,loess,linear,step),
                    args=NULL,unit=1) {
  method=match.arg(method);
  fun=get0(paste0(method,'_fun'),mode='function');
  if (is.null(fun)) stop(paste0('Bad news: function not found for method=',method,
                                       '. Should have been caught earlier!'));
  fit.x=seq(min(x),max(x),by=unit);
  if (is.null(args))
    args=
      switch(method,
             aspline=list(method='improved',fit.x=fit.x),
             sspline=list(spar=NULL),
             spline=list(method='monoH.FC'),
             loess=list(span=0.75),
             linear=list(method='linear',yleft=0,yright=1),
             step=list(method='constant',yleft=0,yright=1),
             list());
  args$x=x;
  fun=apply(y,2,function(col) do.call(fun,c(list(y=col),args)));
}
## these functions wrap underlying data fitting (aka interpolation) methods with consistent API
## CAUTION: can't simply pass ... to approxfun, 'cuz aspline needs ..., too
##   can't simply split ... based on names(formals(fun)) 'cuz both functions have 'method'
##   could give aspline first dibs, remove its args from ..., then pass rest to approxfun
##     but solution here is simpler and works fine
## TODO: some methods undershoot. eg, loess. clamp these
## akima spline
aspline_fun=function(x,y,fit.x,...) {
  fit.y=suppressWarnings(akima::aspline(x,y,fit.x,...))$y;
  approxfun(fit.x,fit.y,yleft=0,yright=0);
}
## interpSpline
library(splines);
ispline_fun=function(x,y,...) {
  x=as.numeric(x);
  model=interpSpline(x,y,...);
  function(xout) suppressWarnings(predict(model,as.numeric(xout)))$y;
}
## smooth.spline
sspline_fun=function(x,y,...) {
  model=smooth.spline(as.numeric(x),y,...);
  function(xout) suppressWarnings(predict(model,as.numeric(xout))$y);
}
## splinefun
spline_fun=function(x,y,...) {
  f=suppressWarnings(splinefun(as.numeric(x),y,...));
  function(xout) suppressWarnings(f(as.numeric(xout)))
}
## loess
loess_fun=function(x,y,...) {
  model=loess(y~x,data=data.frame(x=as.numeric(x),y),...);
  function(xout) suppressWarnings(predict(model,data.frame(x=as.numeric(xout))));
}
## linear and step use approxfun with different args
linear_fun=step_fun=function(x,y,fit=F,x.fit,...) {
  approxfun(x,y,...);
}
