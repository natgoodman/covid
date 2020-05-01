#################################################################################
##
## Author:  Nat Goodman
## Created: 20-01-14
##          from misig/util.R created 19-01-01
##          from repwr/R/util.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Utility functions for frecl
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################

## ---- Utility Functions ----
## generate name=value
paste_nv=function(name,value,sep='=') {
  name=as.character(pryr::subs(name));
  if (missing(value))
    if (exists(name,envir=parent.frame(n=1))) value=get(name,envir=parent.frame(n=1))
    else stop(paste('no value for',name,'in function call or parent environment'));
  paste(sep=sep,name,value); 
}
## generate list of name=value using values from parent environment. code adapted from base::rm
## IGNORE tells whether to ignore NULL and non-existant names
nvq=function(...,sep=' ',IGNORE=F) {
  dots=match.call(expand.dots=FALSE)$...
   if (length(dots) &&
     !all(vapply(dots,function(x) is.symbol(x) || is.character(x),NA,USE.NAMES=FALSE))) 
     stop("... must contain names or character strings");
  ## CAUTION: for some reason, doesn't work to use 'parent.frame(n=1)' inside sapply
  env=parent.frame(n=1);
  names=vapply(dots,as.character,"");
  values=sapply(names,function(name) {
    if (exists(name,envir=env)) get(name,envir=env)
    else if (!IGNORE) stop(paste('no value for',name,'in parent environment'));
  });
  ## values=sapply(names,function(name)
  ##   if (exists(name,envir=parent.frame(n=2))) get(name,envir=parent.frame(n=2))
  ##   else stop(paste('no value for',name,'in parent environment')));
  paste(collapse=sep,unlist(mapply(function(name,value)
    if (!is.null(value)|!IGNORE) paste(sep='=',name,value) else NULL,names,values)));
}
## NG 19-09-25: extend nvq for filenames. also to allow nested calls
## TODO: merge nvq, nvq_file
nvq_file=function(...,SEP=',',DOTS,PARENT=1,PRETTY=T,IGNORE=F) {
  if (missing(DOTS)) DOTS=match.call(expand.dots=FALSE)$...
  else if (missing(PARENT)) PARENT=2;
  if (length(DOTS) &&
     !all(vapply(DOTS,function(x) is.symbol(x) || is.character(x),NA,USE.NAMES=FALSE))) 
     stop("... must contain names or character strings");
  env=parent.frame(n=PARENT);
  names=vapply(DOTS,as.character,"");
  if (IGNORE) names=names[sapply(names,function(name) exists(name,envir=env))];
  values=sapply(names,function(name) {
    if (exists(name,envir=env)) {
      value=get(name,envir=env);
      if (PRETTY) value=pretty(name,value);
      value;
    }
    else if (!IGNORE) stop(paste('no value for',name,'in parent environment'));
  });
  paste(collapse=SEP,unlist(mapply(function(name,value)
    if (!is.null(value)|!IGNORE) paste(sep='=',name,value) else NULL,names,values)));
}
## tack id onto filebase if not NULL or NA
paste_id=function(base,id=NULL,sep='.') {
  ## test id this way to avoid running is.na when id=NULL 
  if (is.null(id)) return(base);
  if (is.na(id)) return(base);
  paste(sep=sep,base,id);
}  
## pretty print typical values of i, n, d, sd & m
pretty=function(name,value) {
  fun=switch(name,i=i_pretty,n=n_pretty,d=d_pretty,sd=sd_pretty,m=m_pretty,as.character);
  fun(value);
}
i_pretty=function(i) sprintf("%03i",i)
n_pretty=function(n) as.character(n);
d_pretty=function(d) sprintf('%3.2f',d);
sd_pretty=function(sd) sprintf('%3.2f',sd);
m_pretty=function(m) {
  exponent=floor(log10(m));
  significand=m/10^exponent;
  paste0(significand,'e',exponent);
}

## NG 19-01-31: CAUTION. doesn’t really work.
## NG 19-06-27: I think above CAUTION is wrong, at least partially
##   what it does is get value from parent frame. if not present,
##   searches from parent - this will be static
##   ----------
##   supposed to search dynamic environment tree but does static instead
##   seemed to work “back in the day” because params were global and static predecessor
##   of most functions is the global environment
##   ---------- 
## get value of variable from parent or set to default
## call with quoted or unquoted variable name
## if default missing, throws error if variable not found
parent=function(what,default) {
  what=as.character(pryr::subs(what));
  if (exists(what,envir=parent.frame(n=2))) return(get(what,envir=parent.frame(n=2)));
  if (!missing(default)) return(default);
  stop(paste(sep='',"object '",what,"' not found"));
}
## NG 19-06-27: I think this version does full dynamic lookup
parent_=function(what,default) {
  what=as.character(pryr::subs(what));
  n=2;
  repeat {
    env=parent.frame(n=n);
    if (n>100) stop(paste0('call stack too deep: n=',n));
    if (exists(what,envir=env,inherit=F)) return(get(what,envir=env));
    if (identical(env, globalenv())) break;
    n=n+1;
  }
  ## if fall out of loop, 'what' not found
  if (!missing(default)) return(default);
  stop(paste0("object '",what,"' not found"));
}

## get value of variable from param environment and assign to same named variable in parent
## call with quoted or unquoted variable names
## adapted from base::rm
## set params using par-like notation, eg, param(m=1e4)
param=function(...,list=character()) {
  dots=match.call(expand.dots=FALSE)$...
  parent.env=parent.frame(n=1);
  ## handle params with new values
  names=names(dots);
  if (!is.null(names)) {
    dots=sapply(seq_along(dots),function(i) {
      if (nchar(names[i])==0) return(dots[i]);
      ## set new value in param.env
      what=names[i];
      val=eval(dots[[i]],envir=parent.env);
      assign(what,val,envir=param.env);
      ## replace element in dots with name so it'll get returned
      what;
    })}
  if (length(dots) &&
      !all(vapply(dots,function(x) is.atomic(x)||is.symbol(x)||is.character(x),
                  NA,USE.NAMES=FALSE))) 
    stop("... must contain atomic data like names or character strings");
  names=vapply(dots,as.character,"");
  if (length(names)==0L) names=character();
  names=c(list,names);
  ## make sure all params valid
  bad=which(sapply(names,function(name) !exists(name,envir=param.env)));
  if (any(bad)) stop(paste(sep=' ','Invalid param(s):',paste(collapse=', ',names(bad))))
  retval=lapply(names,function(name) assign(name,get(name,envir=param.env),envir=parent.env));
  ## fix up return value
  if (length(retval)==1) unlist(retval)
  else {
    names(retval)=names;
    retval;
  }
}
## copy local variables to global - to simplify init
## NG 19-01-11: used in repwr, not in effit
assign_global=function() {
  env=parent.frame(n=1);
  sapply(ls(envir=env),function(what) assign(what,get(what,envir=env),envir=.GlobalEnv));
}
## copy local variables to new or existing param environment - to simplify init
init_param=function() {
  param.env=new.env(parent=emptyenv());
  parent.env=parent.frame(n=1);
  sapply(ls(envir=parent.env),
        function(what) assign(what,get(what,envir=parent.env),envir=param.env));
  assign('param.env',param.env,envir=.GlobalEnv);
}
assign_param=function() {
  parent.env=parent.frame(n=1);
  sapply(ls(envir=parent.env),
        function(what) assign(what,get(what,envir=parent.env),envir=param.env));
}
## copy variable to parent
## NG 19-01-11: not used in effit. used once upon a time in dofig to update fignum
assign_parent=function(what,value) {
  what=as.character(pryr::subs(what));
  if (missing(value)) value=get(what,envir=parent.frame(n=1));
  assign(what,value,envir=parent.frame(n=2));
}
## NG 18-10-24: wrap function - propogate locals and ... then call function
##   funfun are additional functions called by fun with ... args
## TODO: handle partial matching of ... params
## adapted from stackoverflow.com/questions/4124900
wrap_fun=function(fun,funfun=NULL,...) {
  env=parent.frame(n=1);
  x=ls(envir=env);
  fx=do.call(c,lapply(c(fun,funfun),function(fun) names(formals(fun))));
  args=sapply(x[x%in%fx],function(x) get(x,envir=env),simplify=F);
  dots=list(...);
  args=c(args,dots[names(dots)%in%fx]);
  do.call(fun,args);
}
  
## like match.arg but uses general matching and, if several.ok, returns 'em all
pmatch_choice=
  function(arg,choices,several.ok=T,none.ok=F,start=T,ignore.case=T,perl=F,fixed=F,invert=F) {
    ## m=startsWith(choices,arg);
    pat=if(start) paste0('^',arg) else arg;
    m=grep(pat,choices,ignore.case=ignore.case,perl=perl,value=T,fixed=fixed,invert=invert);
    if (length(m)==0&&!none.ok)
      stop(paste(sep=' ',"'arg' matched none of",paste(collapse=', ',choices),
           "but 'none.ok' is FALSE"));
    if (length(m)>1&&!several.ok)
      stop(paste(sep=' ',"'arg' matched several of",paste(collapse=', ',choices),
                 "but 'several.ok' is FALSE"));
    if (length(m)==0) NULL else m;
  }
## quote names in paramter list. code adapted from base::rm
cq=function(...) {
 dots=match.call(expand.dots=FALSE)$...
 if (length(dots) &&
     !all(vapply(dots,function(x) is.atomic(x)||is.symbol(x)||is.character(x),
                 NA,USE.NAMES=FALSE))) 
   stop("... must contain atomic data like names or character strings");
 return(vapply(dots,as.character,""));
}
## upper case first letter of word. like Perl's ucfirst
## from https://stackoverflow.com/questions/18509527/first-letter-to-upper-case/18509816
ucfirst=function(word) paste0(toupper(substr(word,1,1)),substr(word,2,nchar(word)));
  
## wrapper for smooth methods
## NG 19-12-31: extend for 'x', 'y' both matrices
##   'x' must be vector or 2-dimensional matrix-like object with same number of columns as y'
##   'y' must be vector or 2-dimensional matrix-like object
## NG 20-01-02: replace special-case spar, span by method.args - additional args passed to method
##    note defaults for aspline, spline, loess
smooth=
  function(x,y,method,length=100,
           method.args=
             switch(method,
                    aspline=list(method='improved'),
                    spline=list(spar=0.5),
                    loess=list(span=0.75),
                    list())) {
    if (is.vector(x)) x=as.matrix(x)
    else if (length(dim(x))!=2) stop("'x' must be vector or 2-dimensional matrix-like object");
    if (is.vector(y)) y=as.matrix(y)
    else if (length(dim(y))!=2) stop("'y' must be vector or 2-dimensional matrix-like object");
    if (ncol(x)>1&&ncol(x)!=ncol(y))
      stop("When 'x' has multiple columns, it must have same number of columns as 'y'");
    if (method=='none') return(list(x=x,y=y));
  
    x.smooth=apply(x,2,function(x) seq(min(x),max(x),length=length));
    y.smooth=do.call(cbind,lapply(seq_len(ncol(y)),function(j)
      if (ncol(x)==1) smooth_(x[,1],y[,j],x.smooth,method,method.args)
      else smooth_(x[,j],y[,j],x.smooth[,j],method,method.args)));
    list(x=x.smooth,y=y.smooth);
}
smooth_=function(x,y,x.smooth,method,method.args) {
  method.args=c(list(x,y,x.smooth),method.args);
  method.fun=switch(method,
                    aspline=aspline_,spline=spline_,loess=loess_,linear=approx_,approx=approx_,
                    stop(paste('Invalid smoothing method:',method)));
  y.smooth=do.call(method.fun,method.args);
  as.matrix(y.smooth);
}
## these functions wrap underlying smoothing methods with consistent API
## akima::aspline
aspline_=function(x,y,x.smooth,...) {
  y.smooth=if(all(is.na(y))) rep(NA,length(x.smooth))
    else if (length(which(!is.na(y)))==1) rep(y[which(!is.na(y))],length(x.smooth))
    else akima::aspline(x,y,x.smooth,...)$y;
}
## loess
loess_=function(x,y,x.smooth,...) {
  data=data.frame(x,y);
  ## fmla=as.formula('y~x');
  y.smooth=suppressWarnings(predict(loess(y~x,data=data,...),data.frame(x=x.smooth)));
  y.smooth;
}
## smooth.spline
## NG 18-11-07: remove NAs (same as akima::aspline) else smooth.spline barfs
spline_=function(x,y,x.smooth,...) {
  ## remove NAs. code adapted from akima::aspline
  ## CAUTION: must use '<-' not '=' or place assignment in extra parens ((na=is.na(y)))
  ##   see stackoverflow.com/questions/1741820 for explanation. gotta love R...
  if (any(na<-is.na(y))) x=x[!na]; y=y[!na];
  y.smooth=predict(smooth.spline(x,y,...),x.smooth)$y    
  y.smooth;
}
## approx - probably only for completeness
approx_=function(x,y,x.smooth,...) {
  y.smooth=if (all(is.na(y))) rep(NA,length(x.smooth))
    else if (length(which(!is.na(y)))==1) rep(y[which(!is.na(y))],length(x.smooth))
    else approx(x,y,x.smooth,...)$y;
  y.smooth;
}

## with case
## like 'with' but works on vectors. I use it inside apply(cases,1,function(case)...)
## note that plain 'with' works fine when applied to cases as a whole
## withcase=function(case,...) with(data.frame(t(case)),...)
## NG 19-06-27: this version doens't work and isn't used any more...
## withcase=function(case,...) {
##   case=data.frame(t(case),stringsAsFactors=FALSE);
##   assign('case',case,envir=parent.frame(n=1)); # so case will be data frame in called code
##   with(case,...);
## }  
## NG 19-06-27: this version might work...
## NG 19-07-15: BUG: clobbers 'case' vars in parent framr. sigh...
withrows=function(cases,case,expr) {
  var=as.character(pryr::subs(case));
  expr=pryr::subs(expr);
  env=parent.frame(n=1);
  lapply(1:nrow(cases),function(i) {
    case=cases[i,];
    ## assign case so it'll be data frame in called code
    assign(var,case,envir=env);         # so case will be visible in called code
    list2env(case,envir=env);           # assign vars from case
    eval(expr,envir=env);               # do it!
  })}
## NG 19-07-15: this version might work... famous last words :)
withrows=function(cases,case,expr) {
  var=as.character(pryr::subs(case));
  expr=pryr::subs(expr);
  parent=parent.frame(n=1);
  lapply(1:nrow(cases),function(i) {
    case=cases[i,];
    ## assign case so it'll be data frame in called code
    env=list2env(case,parent=parent); # assign vars from case
    assign(var,case,envir=env);         # so case will be visible in called code
    eval(expr,envir=env);               # do it!
  })}

## replace values in list by defaults. kinda like the way I set defaults in Perl
## code adapted from stackoverflow.com/questions/33004238, stackoverflow.com/questions/42207235
##   Thanks!
## default, actual both lists
fill_defaults=function(default,actual) {
  actual=actual[lengths(actual)!= 0];   # remove NULL elements from actual
  default[names(actual)]=actual;        # replace defaults elements by non-NULL actual
  default;
}
## test if arg is "real" list, not data frame
is_list=function(x) is.list(x)&&!is.data.frame(x);
## test blank field
is_blank=function(x) (x=='')|is.null(x)|is.na(x)
## test if x is subset of y. from stackoverflow.com/questions/26831041. thx!
is_subset=function(x,y) all(x %in% y)
is_superset=function(x,y) all(y %in% x)
## test if x is an object of givrn class
is_class(x,class) x %in% class(x)

## round up or down to nearest multiple of u. from https://grokbase.com/t/r/r-help/125c2v4e14/
round_up=function(x,u) ceiling(x/u)*u;
round_dn=function(x,u) floor(x/u)*u;
## x can be range or single number (lower bound)
round_rng=function(x,y,u) 
  if (missing(y)) c(round_dn(x[1],u),round_up(x[2],u)) else c(round_dn(x,u),round_up(y,u))

## pick n items from x approx evenly spaced
pick=function(x,n.want,n.min=1,rep.ok=FALSE,exclude=NULL) {
  x=x[x%notin%exclude];
  if (length(x)<n.min) stop('too few elements in x');
  if (length(x)<n.want&!rep.ok) x
  else {
    step=1/(n.want+1);
    probs=seq(step,by=step,len=n.want)
    unname(quantile(x,probs=probs,type=1))
  };
}

## repeat rows or columns of 2-dimensional matrix-like object. like rep
## like rep, ... can be times, length.out, or each
## based on StackOverflow https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
repr=function(x,...) {
  i=rep(seq_len(nrow(x)),...);
  x=x[i,,drop=F];
  rownames(x)=NULL;
  x;
}
repc=function(x,...) {
  j=rep(seq_len(ncol(x)),...);
  x=x[,j,drop=F];
  colnames(x)=NULL;
  x;
}
## not in - based on example in RefMan - more intutive than !%in%
"%notin%"=function(x,table) match(x,table,nomatch=0)==0
## between, near - to subset sim results. closed on bottom, open on top
between=function(x,lo,hi,tol=0) x>=lo-tol&x<hi+tol;
near=function(x,target,tol=.01) between(x,target-tol,target+tol)

## debugging functions
## TODO: BREAKPOINT is sooo feeble :(
## BREAKPOINT=browser;
BREAKPOINT=function(...,text="",condition=NULL,expr=TRUE,skipCalls=0L) {
  if (!expr) return();
  if (length(list(...))>0) print(paste(...));
  parent.env=parent.frame(n=1);
  with(parent.env,browser(skipCalls=5)); # skipCalls=5 empirically determined
  ## browser(text=text,condition=condition,skipCalls=skipCalls+1);
}
## traceback with args I like
tback=function(max.lines=2) traceback(max.lines=max.lines)
devs.close=function() for (dev in dev.list()) dev.off(dev)
## display color palette
pal=function(col,border="light gray",...) {
 n=length(col)
 plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),axes=FALSE,xlab="",ylab="",...)
 rect(0:(n-1)/n,0,1:n/n,1,col=col,border=border)
}
