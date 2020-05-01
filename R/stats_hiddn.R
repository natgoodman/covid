#################################################################################
##
## Author:  Nat Goodman
## Created: 20-01-20
##          from content from Projects/FreeCell/script/free_cell.R created ~July 2014
##
## Copyright (C) 2020 Nat Goodman.
## 
## Specialized stats functions for hiddn document
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## TODO: use assign_obj function
## constructor for stats_hiddn S3 object
stats_hiddn=
  function(frecl,frq.col,
           p.method='sspline',p.method.args=NULL,d.method='sspline',d.method.args=NULL,
           use.interp=FALSE,
           interp.score=NULL,break.p=NULL,interp.p=NULL) {
    if ('score' %notin% colnames(frecl)) stop("frecl data frame does not have a 'score' column");
    if (frq.col %notin% colnames(frecl))
      stop(paste0('frq=',frq.col,' not a column in frecl data frame'));
    score=frecl[,'score'];
    frq=frecl[,frq.col];
    ## initialize obj to all args and local vars.
    ## code adapted from stackoverflow.com/questions/11885207. thanks!
    obj=as.list(environment());
    class(obj)='stats_hiddn';
    ## cumulative probability ('p') function
    pf=obj$pf=make_p(score,frq,method=p.method,method.args=p.method.args);
    ## probability density ('d') function
    df=obj$df=make_d(pf,score,method=d.method,method.args=d.method.args);
    ## quantile ('q') function
    qf=obj$qf=make_q(pf,interp.p=interp.p,interval=c(0,max(score)));
    rf=obj$rf=make_r(qf);
    invisible(obj);
}
## plot method - plot density ('d') function. x is vector of scores
## works but not terribly useful.
## TODO: extend to handle multiple objects and do something like plotm_legend
plot.stats_hiddn=function(obj,x,...) {
  y=obj$df(x);
  plot(x,y,...);
}

##########
## TODO: fix comments throughout!!
##########
## probability distribution functions for observed, hidden, and all (estimated) scores
## used in doc_hidden.R
## like R's built-in distributions, has functions for
##   density, cumulative probability, quantiles, random with prefixes 'd', 'p', 'q', 'r'
## uses suffixes '.exact', .interp' for exact and interpolated versions
## use.interp controls whether interp functions used as defaults
##   and whether used as default
## score.interp is vector of score values used for interpolation
## make_p - construct cdf ('p function')
## score, frq from frecl data frame sorted by score w/ ties removed
## method is name of method - CAUTION: some not useful here
## method.args - additional args passed to method
##    note defaults for specific methods in code
##    TODO: decide if defaults reasonable
## interp controls whether to return 'exact' (or, at least, 'best') function
##   or list of 'exact' and 'interp'
## score.interp used for 'interp' functions and methods whose core function can only returned
##   interpolated results (presently just aspline)
make_p=
  function(score,frq,
           method=cq(asplinep,ispline,sspline,spline,loess,linear,step),method.args=NULL,
           make.interp=FALSE,interp.len=1000,interp.score=NULL) {
    method=match.arg(method);
    method.fun=get0(paste0(method,'_'),mode='function');
    if (is.null(method.fun)) stop(paste0('Bad news: function not found for method=',method,
                                         '. Should haave been caught earlier!'));
    if (is.null(interp.score)) interp.score=seq(min(score),max(score),len=interp.len);
    if (is.null(method.args))
      method.args=
        switch(method,
               asplinep=list(method='improved',x.interp=interp.score),
               sspline=list(spar=1),
               spline=list(method='monoH.FC'),
               loess=list(span=0.1),
               linear=list(method='linear',yleft=0,yright=1),
               step=list(method='constant',yleft=0,yright=1),
               list());
    method.args=c(list(x=score,y=frq),method.args);
    f=do.call(method.fun,method.args);
    ## some methods can under and over shoot - force result in range [0,1]
    f.exact=Vectorize(function(x) {
      if (x<min(score)) 0
      else if (x>max(score)) 1
      else {y=f(x); ifelse(y<0,0,ifelse(y>1,1,y));}
    })
    if (!make.interp) f.exact
    else list(exact=f.exact,interp=f_interp(f.exact,interp.score,yleft=0,yright=1));
  }
## make_d - construct density ('d') function
## pf is cdf ('p function')
##### Hmm.... why is dff$frq.obs(520) NA?
make_d=
  function(pf,score,
           method=cq(asplined,ispline,sspline,spline,loess,linear,step),method.args=NULL,
           make.interp=FALSE,interp.len=1000,interp.score=NULL) {
    method=match.arg(method);
    method.fun=get0(paste0(method,'_'),mode='function');
    if (is.null(method.fun)) stop(paste0('Bad news: function not found for method=',method,
                                         '. Should haave been caught earlier!'));
    if (is.null(interp.score)) interp.score=seq(min(score),max(score),len=interp.len);
    if (is.null(method.args))
      method.args=
        switch(method,
               asplined=list(method='improved',x.interp=interp.score),
               sspline=list(spar=0.75),
               spline=list(method='monoH.FC'),
               loess=list(span=0.75),
               linear=list(method='linear',yleft=0,yright=0),
               step=list(method='constant',yleft=0,yright=0),
               list());
    diff.pf=c(diff(pf(min(score):max(score))),0);
    method.args=c(list(x=min(score):max(score),y=diff.pf),method.args);
    f=do.call(method.fun,method.args);
    ## some methods can under shoot - force result >=0
    f.exact=Vectorize(function(x) {
      if (x<min(score)) 0
      else if (x>max(score)) 0
      ## else if (x>max(score)) NA
      else {y=f(x); ifelse(y<0,0,y);}
    });
    if (!make.interp) f.exact
    else list(exact=f.exact,interp=f_interp(f.exact,interp.score,yleft=0,yright=0));
  }
## make_q - construct quantile ('q') function
## pf is cdf ('p function')
## make.interp controls whether to return 'exact' function or list of 'exact' and 'interp'
## p.interp used for 'interp' function
## make_q=function(pf,interp=F,p.interp=seq(0.01,0.99,by=0.01),interval=c(0,max(parent(score)))) {
##   f.exact=Vectorize(function(p) {
##     if (p>0&p<1) uniroot(function(x) pf(x)-p,interval=interval)$root
##     else if (p<0|p>1) NaN else if (p==0) -Inf else Inf;
##   });
##   if (!interp) f.exact else list(f.exact=f.exact,f.interp=f_interp(f.exact,p.interp,rule=2));
## }
make_q=
  function(pf,make.interp=FALSE,interp.by=0.01,interp.p=NULL,interval=NULL,score=parent(score)) {
    if (is.null(interp.p)) interp.p=seq(interp.by,1-interp.by,by=interp.by);
    if (is.null(interval)) interval=c(0,max(score));
    f.exact=Vectorize(function(p) {
      if (p<0|p>1) NaN
      else if (p<pf(interval[1])) -Inf
      else if (p>pf(interval[2])) Inf
      else {uniroot(function(x) pf(x)-p,interval=interval)$root}
    });
    if (!make.interp) f.exact
    else list(exact=f.exact,interp=f_interp(f.exact,interp.p,rule=2));
  }


## make_r - construct random ('r') function
## qf is quantile ('q') function
## always exact - no sensible interp'ed version
make_r=function(qf) function(m) qf(runif(m))

## these functions wrap underlying interpolation methods with consistent API
## asplinep_, asplined_ differ in args to approxfun
## CAUTION: these function names conflict with same-named smoothing functions in util.R
## CAUTION: can't simply pass ... to approxfun, 'cuz aspline needs ..., too
##   can't simply split ... based on names(formals(fun)) 'cuz both functions have 'method'
##   could give aspline first dibs, remove its args from ..., then pass rest to approxfun
##     but solution here is simpler and works fine
asplinep_=function(x,y,x.interp,...) {
  y.interp=suppressWarnings(akima::aspline(x,y,x.interp,...))$y;
  approxfun(x.interp,y.interp,yleft=0,yright=1);
}
asplined_=function(x,y,x.interp,...) {
  y.interp=suppressWarnings(akima::aspline(x,y,x.interp,...))$y;
  approxfun(x.interp,y.interp,yleft=0,yright=0);
}
## interpSpline - doesn't seem useful here
library(splines);
ispline_=function(x,y,...) {
  model=interpSpline(x,y,...);
  function(xout) suppressWarnings(predict(model,xout))$y;
}
## smooth.spline
sspline_=function(x,y,...) {
  model=smooth.spline(x,y,...);
  function(xout) suppressWarnings(predict(model,xout)$y);
}
## splinefun
spline_=function(x,y,...) {
  suppressWarnings(splinefun(x,y,...));
}
## loess
loess_=function(x,y,...) {
  model=loess(y~x,data=data.frame(x,y),...);
  function(xout) suppressWarnings(predict(model,data.frame(x=xout)));
}
## linear and step use approxfun with different args
linear_=step_=function(x,y,interp=F,x.interp,...) {
  approxfun(x,y,...);
}
## generate interp function from exact
f_interp=function(f.exact,x.interp,...) {
  y.interp=f.exact(x.interp);
  approxfun(x.interp,y.interp,...);
}
## assign exact and interp functions to stat_hiddn object
## NOT USED!!
assign_fun2obj=function(obj,fun,use.interp) {
  obj.name=as.character(pryr::subs(obj));
  fun.name=as.character(pryr::subs(fun));
  names=c(obj.name,fun.name);
  parent.env=parent.frame(n=1);
  ## make sure all names valid
  bad=which(sapply(names,function(name) !exists(name,envir=parent.env)));
  if (any(bad)) stop(paste(sep=' ','object(s)',paste(collapse=', ',names(bad)),'not found'));
  ## make sure each arg has correct type
  obj=get(obj.name,envir=parent.env);
  if (!is_class(obj,'stats_hiddn'))
    stop(paste('object',obj.name,'must be a stats_hiddn object, not',class(obj)))
  fun=get(fun.name,envir=parent.env);
  if (!is_list(fun)||length(fun)!=2||!is_subset(names(fun),cq(exact,interp))||
      any(sapply(fun,function(fun) !is_class(fun,'function')))) 
    stop(paste('object',fun.name,"must be a list containing 2 functions with names 'exact' and 'interp'"));
  obj[[paste0(fun.name,'.exact')]]=fun$exact;
  obj[[paste0(fun.name,'.interp')]]=fun$interp;
  obj[[fun.name]]=if(use.interp) fun$interp else fun$exact
  assign(obj.name,obj,envir=parent.env);
  obj;
}
