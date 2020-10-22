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

## Generalize nvq in light of what I learned from edit
## subsumes paste_nv, nvq, nvq_file
## IGNORE can be logical or value to use for non-existant names and other errors
## PRETTY is function mapping name,value pairs to pretty values
## NV is escape hatch using standard evaluation
##   list of names or name,value pairs
## if NV and ... both present, NV entries come after dots entries
nv=function(...,SEP=' ',EQUAL='=',PRETTY=FALSE,IGNORE=FALSE,NV=list()) {
    dots=match.call(expand.dots=FALSE)$...;
    if (is.null(dots)&&length(NV)==0) return('');
    err.ok=if(is_logical(IGNORE)) IGNORE else TRUE;
    err.skip=is_logical(IGNORE);
    err.val=if(is_logical(IGNORE)) NA else IGNORE;
    names=names(dots);
    if (is.null(names)) names=rep('',length(dots));
    ## un-named dots are simple name=value 
    unnamed=(nchar(names)==0);
    if (!all(sapply(dots[unnamed],function(x) is_simple(x))))
      stop("unnamed elements of ... must be names or character strings");
    dots[unnamed]=sapply(dots[unnamed],as.name);
    ## use each dot as it's own name, eg, 'x' becomes 'x=x'
    names[unnamed]=sapply(dots[unnamed],as.character)
    parent.env=parent.frame(n=1);
    bad=NULL;
    values=sapply(seq_along(dots),function(i)
      tryCatch(eval(dots[[i]],parent.env), error=function(c) 
        if (err.ok) {bad<<-c(bad,i); err.val}
        else {
          c$message=paste0("Unable to evaluate '",deparse(dots[[i]]),"': ",c$message);
          c$call='nv';
          stop(c);
        }
        ));
    ## tack on names. values from NV
    ## names=c(names,sapply(NV,function(nv) if (length(nv)==1) '' else nv[1]));
    names=c(names,sapply(NV,function(nv) nv[1]));
    values=c(values,sapply(seq_along(NV),function(i) {
      nv=NV[[i]];
      if (length(nv)!=1) nv[-1]
      else {
        name=nv;
        tryCatch(get(name,parent.env),error=function(c) 
          if (err.ok) {bad<<-c(bad,i+length(dots)); err.val}
          else {
            c$message=paste0("Unable to evaluate '",name,"': ",c$message);
            c$call='nv';
            stop(c);
          })
      }}));
    if (!is.null(bad)&err.skip) {
      names=names[-bad];
      values=values[-bad];
    }
    values=sapply(seq_along(values),function(i) {
      name=names[i];
      value=unlist(values[i]);
      if (is.function(PRETTY)) value=sapply(value,function(value) pretty(name,value));
      if (length(value)>1) paste0('c(',paste(collapse=',',value),')') else value;
    });
    ## if (is.function(PRETTY)) values=PRETTY(names,values)
    paste(collapse=SEP,paste(sep=EQUAL,names,values));
}

## get value of variable from param environment and assign to same named variable in parent
## call with quoted or unquoted variable names
## adapted from base::rm
## set params using par-like notation, eg, param(m=1e4)
## TODO: 'list' should handle params with new values
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
  if (length(retval)==1) retval[[1]]
  else {
    names(retval)=names;
    retval;
  }
}
## TODO:  'list' arg of 'param' function should handle params with new values
## sample code
## assign to param.
## do it this way until 'list' arg of 'param' function can handle params with new values 
## assign(paste(sep='.','extra',what),fun,envir=param.env);

## copy local variables to new or existing param environment - to simplify init
init_param=function() {
  param.env=new.env(parent=emptyenv());
  parent.env=parent.frame(n=1);
  sapply(ls(envir=parent.env),
         function(what) assign(what,get(what,envir=parent.env),envir=param.env));
  assign('param.env',param.env,envir=.GlobalEnv);
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
## get function arguments - to remove args that will fail from ... and related
funargs=function(...) do.call(c,lapply(list(...),function(fun) names(formals(fun)) %-% '...' ))

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

## with case
## like 'with' but works on vectors. I use it inside apply(cases,1,function(case)...)
## note that plain 'with' works fine when applied to cases as a whole
## withcase=function(case,...) with(data.frame(t(case)),...)
## NG 19-07-15: this version might work... famous last words :)
withrows=function(cases,case,expr) {
  var=as.character(pryr::subs(case));
  expr=pryr::subs(expr);
  parent=parent.frame(n=1);
  lapply(1:nrow(cases),function(i) {
    case=cases[i,,drop=FALSE];
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
## combine lists, with elements in later lists overriding earlier
## can be used to replace or add elements in list. kinda like the way I set elements in Perl
## clc is same but preserves class of 1st argument
## cl=function(...) Reduce(function(...) cl_(...),list(...))
cl=function(...) {
  dots=list(...);
  names=names(dots);
  ## workaround R's handling of nchar applied to NULL
  if (is.null(names)) names=rep('',length(dots));
  dots=do.call(
    c,lapply(seq_along(dots),function(i)
      if(nchar(names[i])==0) {
        x=dots[i];
        if (!is.list(x[[1]])) x=list(x);
        x;
      } else {
        x=dots[i];
        names(x)=names[i];
        list(x);
      }));
  Reduce(function(...) cl_(...),dots);
}
## NG 20-08-06: handle un-named elements
## cl_=function(x,y) {
##   x=x[names(x) %-% names(y)];
##   c(x,y);
## }
cl_=function(x,y) {
  x[names(y)]=NULL;
  c(x,y);
}
clc=function(obj,...) {
  newobj=cl(obj,...);
  class(newobj)=class(obj);
  newobj;
}
## generate sequence from min(x) to max(x). similar to R's 'seq'
## x can be vector, matrix, data.frame, list of vectors
## when relative=FALSE
##   by, length.out are step-size and length as in R's seq
##     except that if both are set, length wins insted of being an error
## when relative=TRUE
##   by is fraction of range (max(x)-min(x))
##   length.out is number of elements between each elemnt of x
## CAUTION: when x is matrix-like, 'by' may cause varying column lengths! 'length' is safer
seqx=function(x,by=NULL,length.out=NULL,relative=FALSE,na.rm=TRUE) {
  if (all(sapply(c(by,length.out),is.null))) stop("One of 'by' or 'length.out' must be specified");
  ## is.list catches data.frames as well as 'real' lists
  if (is_2d(x)) apply(x,2,function(x) seqx_(x,by,length.out,relative,na.rm))
  else seqx_(x,by,length.out,relative,na.rm);
}
seqx_=function(x,by,length.out,relative,na.rm) {
  lo=min(x,na.rm=na.rm);
  hi=max(x,na.rm=na.rm);
  if (!relative) {
    if (!is.null(length.out)) seq(lo,hi,length.out=length.out) else seq(lo,hi,by=by);
  } else {
    if (!is.null(length.out)) {
      ## length.out is elements per interval
      ## code adapted from stackoverflow.com/questions/54449370. Thx!
      c(do.call(c,lapply(1:(length(x)-1),function(i) {
        lo=x[i]; hi=x[i+1]; delta=hi-lo; step=delta/length.out; seq(lo,hi-step,by=step)})),
        hi);
    } else {
      ## by is fraction of total range
      by=by*(hi-lo);
      seq(lo,hi,by=by);
    }}
}
## test if x is "real" list, not data frame
is_list=function(x) is.list(x)&&!is.data.frame(x)
## test if x is a vector - workaround for R's is.veector not working on Dates....
is_vector=function(x) is_date(x)||is.vector(x)
## test if x is named list -- all elements have names -- analogous to Perl hash
is_nlist=is_hash=function(x)
  is_list(x)&&!is.null(names(x))&&all(sapply(names(x),function(name) nchar(name)>0))
## test if x is unnamed list -- no elements have names
is_ulist=function(x)
  is_list(x)&&(is.null(names(x))||all(sapply(names(x),function(name) nchar(name)==0)))
## test if x is 2D matrix-like object
is_2d=function(x) (!is.null(dim(x)))&&(length(dim(x))==2)
## test blank field
is_blank=function(x) (x=='')|is.null(x)|is.na(x)
## test 'simple' value - eg, name or string
is_simple=function(x) is.symbol(x)||(is.character(x)&&length(x)<=1)
## test if x is "real" logical, not NA
is_logical=function(x) is.logical(x)&&!is.na(x)
## set predicates.
##   subset from stackoverflow.com/questions/26831041. thx!
##   psubset (proper subset) adapted from sets package
##   equal included ro stylistic consistency
is_subset=function(x,y) all(x %in% y)
is_superset=function(x,y) all(y %in% x)
is_psubset=function(x,y) is_subset(x,y)&(length(x)!=length(y))
is_psuperset=function(x,y) is_superset(x,y)&(length(x)!=length(y))
is_equalset=function(x,y) setequal(x,y)
## operator versions of above
"%<=%"=function(x,y) is_subset(x,y)
"%>=%"=function(x,y) is_superset(x,y)
"%<%"=function(x,y) is_psubset(x,y)
"%>%"=function(x,y) is_psuperset(x,y)
"%==%"=function(x,y) is_equalset(x,y)
## set operators
## intersect, difference, union
"%&%"=function(x,y) intersect(x,y)
"%-%"=function(x,y) setdiff(x,y)
"%+%"=function(x,y) union(x,y)
## symmetric difference. other implementations in stackoverflow.com/questions/19797954
symdiff=function(x,y) (x %-% y) %+% (y %-% x)
"%--%"=function(x,y) symdiff(x,y)
  
## test if x is an object of given class
is_class=function(x,class) class %in% class(x)
## test if x is Date
is_date=function(x) is_class(x,'Date')
is_POSIXdate=function(x) is_class(x,'POSIXt')
## convert date strings we encounter in covid to Dates
as_date=function(x) {
  ## R seems to convert dates to numeric when passed through sapply. sigh...
  dates=sapply(x,function(x) {
    if (is_date(x)||is.numeric(x)||is.na(x)) x
    else if (is_POSIXdate(x)) as.Date(x)
    else {
      if (is.character(x)) {
        ## if (endsWith(x,' UTC')) x=sub(' UTC$','',x); # for DOH >= 20-05-24
        if (startsWith(x,'20-')) format='%y-%m-%d'
        else if (startsWith(x,'2020-')) format='%Y-%m-%d'
        else if (startsWith(x,'2020')) format='%Y%m%d'
        else if (startsWith(x,'2019')) format='%Y%m%d'
        else if (endsWith(x,'/20')) format='%m/%d/%y'
        else stop('Unexpected date format: ',x);
        as.Date(x,format=format);
      }
      ## else BREAKPOINT('Unable to create date from ',x);
      else stop('Unable to create date from ',x);
    }});
  as.Date(dates,origin='1970-01-01');
}
## convert date to version strings we use, eg, 20-05-13
as_version=function(date) strftime(date,format='%y-%m-%d')
## day-of-week manipulation we use
Weekdays=cq(Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday);
dayofweek=Vectorize(function(day) which(Weekdays==day))
match_day=function(day) match.arg(ucfirst(day),Weekdays)
inc_day=function(day,i=0) Weekdays[((dayofweek(day)+i-1)%%7)+1]               

## upper/lower case fist character. like the Perl functions
ucfirst=function(x) {
  substr(x,1,1)=toupper(substr(x,1,1));
  x;
}
lcfirst=function(x) {
  substr(x,1,1)=tolower(substr(x,1,1));
  x;
}
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

## expand.grid for data frames. args can be data frames or vectors
## NULL args are skipped 
expand_df=function(...) {
  dots=list(...);
  dots=dots[sapply(dots,function(x) !is.null(x))];
  dots=lapply(seq_along(dots),function(i) {
    df=dots[[i]]
    if (!is.data.frame(df)) {
      df=as.data.frame(df,stringsAsFactors=FALSE);
      nm=names(dots)[i];
      colnames(df)=if(!is.null(nm)&&nchar(nm)>0) nm else paste0('X',i);
    }
    df;
  })
  ## line below from stackoverflow.com/questions/13640157. Thanks!
  Reduce(function(...) merge(..., by=NULL), dots)
}

## repeat rows or columns of 2-dimensional matrix-like object. like rep
## or repeat vector into matrix
## like rep, ... can be times, length.out, or each
## based on StackOverflow https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
repr=function(x,...) {
  if (is.null(dim(x))) x=matrix(x,nrow=1);
  i=rep(seq_len(nrow(x)),...);
  x=x[i,,drop=F];
  rownames(x)=NULL;
  x;
}
repc=function(x,...) {
  if (is.null(dim(x))) x=matrix(x,ncol=1);
  j=rep(seq_len(ncol(x)),...);
  x=x[,j,drop=F];
 ##  colnames(x)=NULL;
  x;
}
## fill matrix to desired number of rows or columns
## allow lower and uppr case param names for compatibility with my code and R
fillr=function(x,length.out,fill=NA,LENGTH.OUT=length.out,FILL=fill) {
  if (is.null(dim(x))) x=matrix(x,nrow=1);
  if (nrow(x)<LENGTH.OUT) {
    FILL=matrix(FILL,nrow=LENGTH.OUT-nrow(x),ncol=ncol(x));
    colnames(FILL)=colnames(x);
    x=rbind(x,FILL);
  }
  x;
}
fillc=function(x,length.out,fill=NA,LENGTH.OUT=length.out,FILL=fill) {
  if (is.null(dim(x))) x=matrix(x,ncol=1);
  if (ncol(x)<LENGTH.OUT) {
    FILL=matrix(FILL,nrow=nrow(x),ncol=LENGTH.OUT-ncol(x));
    x=cbind(x,FILL);
  }
  x;
}
fill=function(val,LENGTH.OUT,FILL=NA) {
  if (is.data.frame(val)) {
    if (nrow(val)<LENGTH.OUT) val[(nrow(val)+1):LENGTH.OUT,]=FILL
    else val=head(val,LENGTH.OUT);
  }
  else {
    if (length(val)<LENGTH.OUT) val[(length(val)+1):LENGTH.OUT]=FILL
    else val=head(val,LENGTH.OUT);
  }
  val;
};
strip=function(val,LENGTH.OUT=NA,FILL=NA) {
  ## get index of last real data
  if (is.na(FILL))
    i=if(is.data.frame(val)) apply(val,1,function(val) !all(is.na(val))) else !is.na(val)
  else
    i=if(is.data.frame(val)) apply(val,1,function(val) any(ifelse(is.na(val),TRUE,val!=FILL)))
      else ifelse(is.na(val),TRUE,val!=FILL);
  if (any(i)) {
    last=max(which(i));
    if (!is.na(LENGTH.OUT)) last=max(last,LENGTH.OUT);
    val=head(val,last);
  } else val=NULL;
  val;
};

## not in - based on example in RefMan - more intutive than !%in%
"%notin%"=function(x,table) match(x,table,nomatch=0)==0
## between, near - to subset sim results. closed on bottom, open on top
between=function(x,lo,hi,tol=0) x>=lo-tol&x<hi+tol;
near=function(x,target,tol=.01) between(x,target-tol,target+tol)

## debugging & software dev functions
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
## helpers to cope with flaky internet during covid crisis
plon=function(base='plot',file.png=filename('figure',base,suffix='png')) {
  if (exists('dev.png')) ploff();
  png(filename=file.png,height=8,width=8,units='in',res=200,pointsize=12);
  dev.png<<-dev.cur();
  file.png<<-file.png;
}
ploff=function(dev=dev.png,file=file.png) {
  dev.off(dev);
  system(paste('pjto',file));
}
sv=function() {save.image(); savehistory(); }
