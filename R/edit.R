#################################################################################
##
## Author:  Nat Goodman
## Created: 20-08-16
##
## Copyright (C) 2020 Nat Goodman.
## 
## Edit object data
## there are separate functions for editing places, ages, and dates
## (presently just places)
## TODO: add KEEP, DROP, EDIT args that separate edit functions
##
## Note top-level 'edit' functions in tranform.R call the functions here
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
###################################################################################
## edit places for one object data.frame
edit_places1=function(data,EXPR=list(),KEEP=NULL,DROP=NULL) {
  counts=edit_df1(data[,-1,drop=FALSE],EXPR,KEEP,DROP,'state');
  cbind(date=data[,1],counts);
}
## edit places for pop metadata
edit_popp=function(pop,EXPR=list(),KEEP=NULL,DROP=NULL) {
  if (is.null(pop)) pop=load_pop();
  edit_df1(pop,EXPR,KEEP,DROP,'state');
}
## edit columns for one data.frame of counts (counts from object or all of pop)
## 'total' is var that represents everything
edit_df1=function(data,EXPR=list(),KEEP=NULL,DROP=NULL,total='state',TARGNAME='places') {
  names=names(EXPR);
  do.call(cbind,lapply(seq_along(EXPR),function(i) {
    expr=EXPR[[i]];
    name=names[i];
    edit_chkvars(expr_vars(expr),name,colnames(data),TARGNAME);
    out=data.frame(x=eval(expr,data))
    colnames(out)=name;
    data<<-cbind(data,out);             # so later exprs can see this one
  }));
  if (!is.null(KEEP)&&!is.null(DROP)) stop('Illegal to specify both KEEP and DROP')
  if (!is.null(KEEP)) {
    KEEP=c(KEEP,names);
    if (total%in%colnames(data)) KEEP=c(total,KEEP);
    KEEP=unique(KEEP);
    edit_chkvars(KEEP,'KEEP',colnames(data),TARGNAME);
    data=data[,KEEP,drop=FALSE];
  }
  else if (!is.null(DROP)) {
    edit_chkvars(DROP,'DROP',colnames(data),TARGNAME);
    data=data[,colnames(data)%-%DROP,drop=FALSE];
  }
  data;
}

## edit ages for one doh object
edit_ages1=function(data,EXPR=list(),KEEP=NULL,DROP=NULL) {
  dates=data[[1]][1];
  data=lapply(data,function(data) data[,-1,drop=FALSE]);
  names=names(EXPR);
  lapply(seq_along(EXPR),function(i) {
    expr=EXPR[[i]];
    name=names[i];
    out=list(eval(expr,data));
    names(out)=name;
    data<<-c(data,out);             # so later dots exprs see this one
  });;
  if (!is.null(KEEP)&&!is.null(DROP)) stop('Illegal to specify both KEEP and DROP')
  if (!is.null(KEEP)) {
    KEEP=c(KEEP,names);
    if ('all'%in%names(data)) KEEP=c('all',KEEP);
    KEEP=unique(KEEP);
    data=data[unique(KEEP)];
  }
  else if (!is.null(DROP)) data=data[names(data)%-%DROP];
  ## put back dates
  data=lapply(data,function(counts) cbind(date=dates,counts));
  data;
}
## edit ages for pop metadata
edit_popa=function(pop,EXPR=list(),KEEP=NULL,DROP=NULL) {
  if (is.null(pop)) pop=load_pop();
  ## transpose then process as data frame
  tpop=as.data.frame(t(pop));
  tpop=edit_df1(tpop,EXPR,KEEP,DROP,'all','ages');
  ## transpose back
  pop=as.data.frame(t(tpop));
  pop; 
}
## parse dots and combine with explicit args. set EXPR, KEEP, DROP variables in parent
## edit_args=function(...,KEEP=NULL,DROP=NULL,SUM=list(),NEGATE=list(),SUB=list(),EXPR=list()) {
##  dots=match.call(expand.dots=FALSE)$...;
edit_args=function(dots,KEEP=NULL,DROP=NULL,SUM=list(),NEGATE=list(),SUB=list(),EXPR=list()) {
  DATE=NULL;
  if (!is.null(dots)) {
    names=names(dots);
    if (is.null(names)) names=rep('',length(dots));
    ## 'date' is special - move to DATE variable
    date=(('date'==tolower(names))|sapply(dots,function(dot) any(expr_vars(dot)=='date')))
    if (any(date))
      if (sum(date)==1) {
        DATE=dots[[which(date)]];
        dots=dots[!date];
        names=names(dots);
      }
      else stop("Can only have 1 'date' expression, not ",sum(date),': ',
                paste(collapse='; ',sapply(dots[date],expr2text)));
    ## remaining un-named dots are KEEPs or DROPs
    unnamed=(nchar(names)==0);
    udots=dots[unnamed];
    tdots=lapply(udots,expr_tokens);
    keep=sapply(tdots,function(tokens) tokens[[1]]!='`-`');
    drop=!keep;
    ## remove leading '-' from drops and process just like keeps
    parent=parent.frame(n=2);             # for eval'ing 'c'
    sapply(seq_along(tdots),function(i) {
      tdot=tdots[[i]];
      if (drop[i]) tdot=tail(tdot,n=-1);
      if (length(tdot)==1) var=tdot
      else if (tdot[1]=='cq') var=tail(tdot,n=-1)
      else if (tdot[1]=='c') {
        text=paste0('c(',paste(collapse=',',tail(tdot,n=-1)),')');
        expr=parse(text=text);
        var=eval(expr,parent);
      } else stop('Bad expression: ',dots[[i]]);
      if (keep[i]) KEEP<<-c(KEEP,var) else DROP<<-c(DROP,var);
    });
    ## remaining dots are named expressions
    dots=dots[!unnamed];
    names=names(dots);
    ## convert strings to names in dots, eg, "Walla Walla" to `Walla Walla`
    dots=lapply(dots,function(dot) {
      text=deparse(dot);
      text=gsub('"','`',text);
      parse(text=text);
    });
  } else names=NULL;
  ## move explict args into dots
  argnames=names(SUM);
  sums=lapply(seq_along(SUM),function(i) {
    vars=edit_arg1(SUM[[i]],argnames[i],'SUM')
    ## vars=fix_names(SUM[[i]]);
    text=paste(collapse='+',vars);
    parse(text=text);
  });
  negs=lapply(seq_along(NEGATE),function(i) {
    vars=edit_arg1(NEGATE[[i]],argnames[i],'NEGATE')
    ## vars=fix_names(NEGATE[[i]]);
    text=paste0('!(',paste(collapse='+',vars),')');
    parse(text=text);
  });
  subs=lapply(seq_along(SUB),function(i) {
    vars=edit_arg1(SUB[[i]],argnames[i],'SUB',2)
    ## vars=fix_names(SUB[[i]]);
    text=paste0(vars[1],'-',paste(collapse='+',tail(vars,n=-1)));
    parse(text=text);
  });
  dots=c(dots,sums,negs,subs);
  names(dots)=c(names,names(SUM),names(NEGATE),names(SUB));
  EXPR=c(dots,EXPR);
  ## assign EXPR, DATE, KEEP, DROP in parent
  parent=parent.frame(n=1);
  assign('EXPR',EXPR,parent);
  assign('DATE',DATE,parent);
  assign('KEEP',KEEP,parent);
  assign('DROP',DROP,parent);
}
## split args by target
edit_split=function(EXPR=list(),KEEP=NULL,DROP=NULL,places=NULL,ages=NULL) {
  ## set places.all, ages.all to universe of names for better error messages
  places.all=unique(c(places,places_all()));
  ages.all=unique(c(ages,ages_all()));
  if (length(EXPR)>0) {
    names=names(EXPR);
    targs=sapply(seq_along(EXPR),function(i) {
      expr=EXPR[[i]];
      targ=expr_targs(expr,places.all,ages.all);
      if (length(targ)>1)
        stop(paste0('Expression ',expr2text(expr),' contains variables for multiple targets: ',
                    paste(collapse=', ',targ)));
      ## add expr name to target names
      if (targ=='places') places.all<<-c(places.all,names[i])
      else if (targ=='ages') ages.all<<-c(ages.all,names[i]);
      targ;
    });
    expr.split=split(EXPR,targs);
  } else expr.split=list();
  keep.split=if(!is.null(KEEP)) split(KEEP,sapply(KEEP,var_targ)) else list();
  drop.split=if(!is.null(DROP)) split(DROP,sapply(DROP,var_targ)) else list();
  ## assign ARGS.<targ> in parent
  parent=parent.frame(n=1);
  args.targ=sapply(cq(places,ages),function(targ) {
    args=list(EXPR=expr.split[[targ]],KEEP=keep.split[[targ]],DROP=drop.split[[targ]]);
    if (all(sapply(args,is.null))) args=NULL;
    args},simplify=FALSE);
}
## re-state NEGATEs (!s) as subtractions
## 'total' is var that represents everything
edit_fixneg=function(EXPR=list(),total='state',vars.all=NULL) {
  EXPR=lapply(EXPR,function(expr) {
    text=expr2text(expr);
    if (substr(text,1,1)=='!') {
      if (total%notin%vars.all)
        stop("Cannot NEGATE: '",total,"' variable not in data; you might try SUB instead");
      text=substr(text,2,nchar(text));
      text=paste0(total,'-(',text,')');
      parse(text=text);
    }
    else expr;
  });
  EXPR;
}
edit_arg1=function(arg,argname,ARGNAME,min.size=1) {
  if (is.null(arg)) stop(ARGNAME,' entry ',argname,' is NULL');
  vars=fix_names(arg);
  if (length(vars)<min.size)
    stop(ARGNAME,' entry ',argname,' is too small; must contain at least ',min.size,' vars')
  vars;
}
edit_chkvars=function(vars,argname,valid,TARGNAME) {
  ## R may express symbols with back-ticks, eg `name`
  ## the only way I've found to handle this is to boldily remove the back-ticks. sigh...
  ## try first with vars as given
  bad=vars%-%valid;
  if (length(bad)>0) {
    ## now try with back-ticks removed
    vars=gsub("`","",bad);
    bad=vars%-%valid;
    if (length(bad)>0) 
      stop("Invalid ",TARGNAME," for ",argname,": ",paste(collapse=', ',bad),
           ".\nValid ",TARGNAME," are: ",paste(collapse=', ',valid));
  }
}

## core logic below from codetools package by Luke Tierney. Thx!
flatten_expr=function(expr) {
  if (typeof(expr)%in%cq(language,expression)) {
    if (class(expr)%in%c('call','(')) class(expr[[1]])=class(expr);
    unlist(lapply(as.list(expr),function(x) if (!missing(x)) flatten_expr(x)));
  } else expr;
}
expr_vars=function(expr) {
  flat=flatten_expr(expr);
  ## R handles symbols differently than most other things; single symbol NOT vector. sigh..
  if (is.symbol(flat)) as.character(flat)
  else as.character(flat[sapply(flat,class)%in%cq(name,character)]);
}
expr_targs=function(expr,places=places_all(),ages=ages_all()) {
  vars=expr_vars(expr);
  unique(unlist(var_targ(vars,places,ages)));
  ## unique(unlist(var_targs(vars,places,ages)));
}
expr_tokens=function(expr) as.character(flatten_expr(expr))

var_targ=function(vars,places=places_all(),ages=ages_all()) {
  sapply(vars,function(var) {
    var=gsub('`','',var);
    if (var%in%places) 'places'
    else if (var%in%ages) 'ages'
    else if (var=='date') 'dates'
    else 
      stop("Invalid var (not a place, age, or 'date'): ",var,
           ".\nValid places are ",paste(collapse=', ',places),
           ".\nValid ages are ",paste(collapse=', ',ages))
  });
}
## convert character strings to names. nop if already name. NOTE: returns list, not vector
fix_names=function(vars) sapply(vars,as.name)

## convert expr to string. Good ole R makes this tougher than it should be...
## this works for cases I'm seeing now, but who knows???
expr2text=function(expr) if (typeof(expr)=='language') deparse(expr) else as.character(expr)
