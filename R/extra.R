#################################################################################
##
## Author:  Nat Goodman
## Created: 20-08-16
##
## Copyright (C) 2020 Nat Goodman.
## 
## Adjust DOH data from recent weeks. This data is incomplete due to reporting
## and other delays. The solution is to add 'extra' data.
##
## The code here constructs lm models that serve as correction factors
## and packages the models in a function ('extrafun')
## 'extra' transform in tranform.R uses 'extrafun' to correct the data.
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
###################################################################################
extrafun=
  function(what='cases',objs=NULL,
           w.max=param(extra.wmax),nv.min=param(extra.nvmin),ex.min=param(extra.exmin),
           formula.type=cq('*',':',crossing,interaction),model.type=cq(split,joint),
           dates=NULL,places=NULL,ages=NULL) {
    if (is.null(objs)) {
      ## read objects
      versions.all=list_versions('doh',what);
      versions=if(is.null(versions)) versions.all else versions.all %&% versions;
      objs=lapply(versions,function(version) raw(what,'doh',version));
    } else {
      if (is_cvdat(objs)) objs=list(objs);
      versions=sort(sapply(objs,function(obj) obj$version))
    }
    vdates=as_date(versions);
    vmin=min(vdates);
    vmax=max(vdates);
    ws=seq_len(w.max);
    ys=min(ws):max(w.max,nv.min);
    formula.type=match.arg(formula.type);
    if (formula.type=='crossing') formula.type='*'
    else if (formula.type=='interaction') formula.type=':';
    model.type=match.arg(model.type);
    
    places.all=Reduce(intersect,lapply(objs,function(obj) colnames(obj$data$all)[-1]));
    places=if(is.null(places)) places.all else places.all %&% places;
    ages.all=Reduce(intersect,lapply(objs,function(obj) names(obj$data)))
    ages=if(is.null(ages)) ages.all else ages.all %&% ages;
    dates.all=unique(do.call(c,lapply(objs,function(obj) obj$data$all$date)))
    dates=if(is.null(dates)) dates.all else dates.all %&% dates;
    ## dates wth enough versions are vmin-1*7..vmax-6*7
    enuff=which(dates>=(vmin-7)&dates<=(vmax-nv.min*7));
    dates=dates[enuff];
    cases=expand.grid(place=places,age=ages,stringsAsFactors=FALSE);
    if (model.type=='split') extrafun_split(objs,cases,dates,ws,ex.min,formula.type)
    else extrafun_joint(objs,cases,dates,ws,ex.min,formula.type);
  }

extrafun_split=function(objs,cases,dates,ws,ex.min,formula.type) {
  funs=withrows(cases,case,{
    props=extra_props(objs=objs,dates=dates,ws=ws,place=place,age=age)
    wide=data.frame(date=dates,props);
    data=reshape(wide,varying=list(1+ws),dir='long',idvar='date',v.names='y',timevar='w');
    data$w=as.factor(data$w)
    data=data[is.finite(data$y),]       # remove NA, NaN, Inf
    if (nrow(data)==0) return(NA);
    ## dumb R requires that terms in formula not be singelton. sigh...
    terms=do.call(c,lapply(cq(date,w),
                           function(var) if (length(unique(data[,var]))>1) var else NULL));
    fmla=paste0('y~',paste(collapse=formula.type,terms));
    model=lm(as.formula(fmla),data=data);
    w.model=as.numeric(unique(data[,'w']));
    w.min=min(w.model);
    w.max=max(w.model);
    fun=function(date,w) {
      ## if w too big, return 1
      if (w>w.max) return(1);
      ## if w too small, return NA
      if (w<w.min) return(NA);
      p=suppressWarnings(predict(model,list(date=date,w=as.factor(w))));
      pmax(ex.min,pmin(1,p));         # clamp to [ex.min,1]
    }})
  names(funs)=paste(sep=';',cases$place,cases$age);
  fun1=function(date,place,age,w) {
    ## get correct function from funs
    fun=funs[[paste(sep=';',place,age)]];
    ## if no functon for these args, return NA
    if (is.function(fun)) fun(date,w) else NA;
  }
  fun=function(dates,places,ages,ws=NA,vdates=NA) {
    dates=as_date(dates);
    vdates=as_date(vdates);
    cases=expand.grid(date=dates,place=places,age=ages,w=ws,vdate=vdates,
                      stringsAsFactors=FALSE);
    cases$w=with(cases,ifelse((is.na(w)&!is.na(vdate)),(vdate-date)/7,w));
    ys=unlist(withrows(cases,case,fun1(date,place,age,w)));
    ## construct names from multivalued args
    names(ys)=extrafun_names(cases);
    ys;
  }
  fun;
}
extrafun_joint=function(objs,cases,dates,ws,ex.min,formula.type) {
  data=do.call(rbind,withrows(cases,case,{
    props=extra_props(objs=objs,dates=dates,ws=ws,place=place,age=age)
    wide=data.frame(date=dates,props);
    long=reshape(wide,varying=list(1+ws),dir='long',idvar='date',v.names='y',timevar='w');
    long$place=place;
    long$age=age;
    long;
  }));
  data$w=as.factor(data$w)
  data=data[is.finite(data$y),]       # remove NA, NaN, Inf
  if (nrow(data)==0) stop('No props data left after filtering bad values'); 
  ## dumb R requires that terms in formula not be singelton. sigh...
  terms=do.call(c,lapply(cq(date,place,age,w),
                         function(var) if (length(unique(data[,var]))>1) var else NULL));
  fmla=paste0('y~',paste(collapse=formula.type,terms));
  model=lm(as.formula(fmla),data=data)
  ## determine which cases are in model
  ## see stackoverflow.com/questions/5916854 for ways to implement test
  in.model=merge(cases,unique(data[,cq(place,age,w)]));
  in.model$w=as.numeric(in.model$w);
  in.model$in.model=TRUE;

  w.max=max(ws);
  fun=function(dates,places,ages,ws=NA,vdates=NA) {
    dates=as_date(dates);
    vdates=as_date(vdates);
    cases=expand.grid(date=dates,place=places,age=ages,w=ws,vdate=vdates,
                      stringsAsFactors=FALSE);
    cases$w=with(cases,ifelse((is.na(w)&!is.na(vdate)),(vdate-date)/7,w));
    cases$i=1:nrow(cases);             # for sorting after merges
    cases.model=merge(cases,in.model);
    cases.model$y=with(cases.model,{
      p=suppressWarnings(
        predict(model,list(date=date,place=place,age=age,w=as.factor(w))));
      pmax(ex.min,pmin(1,p));         # clamp to [ex.min,1]
    });
    ## merge back with cases to get ones not in model. y=NA for these cases
    cases=merge(cases,cases.model,all.x=TRUE);
    cases=cases[order(cases$i),];
    ## if w too big, set y=1
    cases$y=with(cases,ifelse(w<=w.max,y,1));
    ## construct names from multivalued args
    setNames(cases$y,extrafun_names(cases));
  }
  fun;
}
## returns data frame of 'props' by 'w' for one 'place' and 'age'
extra_props=function(objs,dates,ws,place,age) {
  versions=sort(sapply(objs,function(obj) obj$version))
  ## get data from objs
  data=lapply(objs,function(obj) {
    data=obj$data[[age]][,c('date',place)];
    data=data[data$date%in%dates,];
    colnames(data)[2]=obj$version;
    data});
  ## join on 'date' and format as data.frame
  counts=Reduce(function(...) merge(..., by='date',all=TRUE), data);
  dates=counts$date;
  vdates=as_date(versions);
  ## shift data so columns are delta-weeks (ie, number of versions covering date)
  ## first drop 'date' column - simplifies index arithmetic
  counts=counts[,-1];
  nv=ncol(counts);
  cw=do.call(rbind,
             lapply(1:nrow(counts),function(i) {
               row=if(i==1) counts[i,ws] else counts[i,-(1:(i-1))][,ws]
               setNames(row,ws)}
               ));
  cw=apply(cw,2,function(counts) ifelse(counts>0,counts,NA));
  ## make relative to final data
  finals=counts[,nv];
  props=apply(cw,2,function(counts) counts/finals);
  rownames(props)=as.character(dates);
  props;
}
## construct names for return value of extrafun from multivalued args
extrafun_names=function(cases) {
  mvargs=do.call(
    c,lapply(cq(date,place,age,w),
             function(arg) if (length(unique(cases[[arg]]))>1) arg else NULL));
  ## typically, 'date' is only real mv arg, but 'w' looks mv due to 'vdate' calculation
  ##   handle specially
  if (length(mvargs)==0) nms=NULL
  else if (length(mvargs)==1) nms=cases[[mvargs]]
  else if (identical(mvargs,cq(date,w))&(all((as.numeric(cases$vdate-cases$date)/7)==cases$w)))
    nms=cases$date
  else nms=Reduce(function(x,y) paste(sep=';',x,cases[[y]]),mvargs[-1],init=cases[[mvargs[1]]]);
  nms;
}
## initialize cached extrafuns
init_extra=
  function(what=cq(cases,deaths,admits),
           w.max=param(extra.wmax),nv.min=param(extra.nvmin),
           formula.type=cq('*',':',crossing,interaction),model.type=cq(split,joint),
           dates=NULL,places=NULL,ages=NULL) {
    what=match.arg(what,several.ok=TRUE);
    sapply(what,function(what) {
      fun=extrafun(what=what,w.max=w.max,nv.min=nv.min,
                   formula.type=formula.type,model.type=model.type,
                   dates=dates,places=places,ages=ages);
      ## assign to param.
      ## do it this way until 'list' arg of 'param' function can handle params with new values 
      assign(paste(sep='.','extra',what),fun,envir=param.env);
    });
    what;
  }
