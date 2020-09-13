#################################################################################
##
## Author:  Nat Goodman
## Created: created 20-05-06
##          from import.R created 20-05-02
##
## Copyright (C) 2020 Nat Goodman.
## 
## Transform imported data for analysis
##
# This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Data transformations ----
## parameters of transformed data set
##   data source
##   what - cases or deaths
##   raw or fit (aka interpolated or smoothed)
##   time unit - weekly or daily
##   cumulative vs incremental

## run standard transform pipeline
transform=function(what=cq(cases,deaths),datasrc=param(datasrc),version='latest') {
  objs=list();
  cases=expand.grid(what=what,datasrc=datasrc,version=version,stringsAsFactors=FALSE);
  objs=withrows(cases,case,{
    ## casename=paste(sep='.',what,datasrc);
    if (param(verbose)) print(paste('>>> transforming',nvq(what,datasrc,version)));
    within(list(), {
      raw=raw(what,datasrc,version);
      cum=cumulative(raw);
      cum2=cum2(raw);
      inc=incremental(raw);
      weekly=weekly(raw);
      daily=daily(raw);
      wklycum=weekly(cum);
      dlycum=daily(cum);
      wklyinc=weekly(inc);
      dlyinc=daily(inc);
      fitwklycum=fit(weekly(cum));
      fitdlycum=fit(daily(cum));
      fitwklyinc=fit(weekly(inc));
      fitdlyinc=fit(daily(inc));
    });
  })
  names(objs)=unlist(withrows(cases,case,
                              paste(collapse='.',c(what,datasrc,if(version!='latest') version))));
  invisible(objs);
}

## read raw imported data and return as standard 'pseudo-object'
raw=function(what=cq(cases,admits,deaths),datasrc=param(datasrc),version='latest') {
  what=match.arg(what);
  datasrc=match.arg(datasrc);
  if (what=='admits'&&datasrc!='doh') stop("Only have admits data for doh, not",datasrc);
  if (!is.null(version)&&version=='latest') version=latest_version(datasrc,what);
  data=load_data(whatv=what,datasrc=datasrc,version=version);
  obj=cvdat(data=data,datasrc=datasrc,what=what,version=version,fit='raw',extra=FALSE);
  clc(obj,switch(datasrc,
                  doh=list(unit=7,start.on='Sunday',center=FALSE,cumulative=FALSE),
                  ihme=list(unit=1,cumulative=FALSE),
                  jhu=list(unit=1,cumulative=TRUE),
                  nyt=list(unit=1,cumulative=TRUE),
                  trk=list(unit=1,cumulative=FALSE),
                  yyg=list(unit=1,cumulative=FALSE),
                  stop(paste('Bad news: unknown data source',datarc,
                             'should have been caught earlier'))
                 ));
}
## fit standard pseudo-object data
fit=function(obj,method='sspline',args=list(),fit.clamp=0,fit.unit=1) {
  data=obj$data;
  if (obj$datasrc!='doh') {
    dates=data[,1];
    fun=fitfun(x=as.numeric(dates),y=data[,-1,drop=FALSE],method=method,args=args,clamp=fit.clamp);
    dates=seq(min(dates),max(dates),by=fit.unit);
    data=data.frame(date=dates,sapply(fun,function(f) f(dates)));
  } else {
    ages=names(data);
    dates=data$all[,1];
    fun=lapply(data,function(data)
      fitfun(x=as.numeric(dates),y=data[,-1,drop=FALSE],method=method,args=args,clamp=fit.clamp));
    dates=seq(min(dates),max(dates),by=fit.unit);
    data=lapply(seq_along(obj$data),function(i) {
      data=data[[i]];
      fun=fun[[i]];
      data.frame(date=dates,sapply(fun,function(f) f(dates)));
    });
    names(data)=ages;
  }
  clc(obj,list(data=data,fit.fun=fun,fit=method,fit.args=args));
}
## convert pseudo-object data to cumulative. nop if already cumulative
cumulative=function(obj,week.end=FALSE) {
  if (obj$cumulative) return(obj);
  unit=obj$unit;
  obj$data=
    if(obj$datasrc!='doh') cum1(obj$data,unit,week.end)
    else sapply(obj$data,function(data) cum1(data,unit,week.end),simplify=FALSE);
  obj$cumulative=TRUE;
  obj;
}
cum1=function(data,unit,week.end) {
  data=data.frame(date=data[,1],apply(data[,-1,drop=FALSE],2,cumsum));
  if (unit==7&&week.end) {
    date.0=data$date[1]-1;
    ## adjust dates to end of week and insert row of 0s at beginning
    data$date=data$date+6;
    data.0=data.frame(date.0,repc(0,ncol(data)-1));
    colnames(data.0)=colnames(data)
    data=rbind(data.0,data)
  }
  data;
}
## convert pseudo-object data to incremental. nop if already incrmental
incremental=function(obj) {
  if (!obj$cumulative) return(obj);
  obj$data=
    if(obj$datasrc!='doh') inc1(obj$data)
    else sapply(obj$data,function(data) inc1(data),simplify=FALSE);
  obj$cumulative=FALSE;
  obj;
}
inc1=function(data) {
  ## use check.names=FALSE so R won't replace spaces in place names...
  data.frame(date=data[,1],rbind(data[1,-1],apply(data[,-1],2,diff)),check.names=FALSE)
}
## convert pseudo-object data to weekly and optionally center
weekly=function(obj,start.on='Sunday',center=TRUE) {
  if (obj$unit!=7) {
    start.on=match_day(start.on);
    cumulative=obj$cumulative;
    obj$data=
      if(obj$datasrc!='doh') wly1(obj$data,cumulative,start.on)
      else sapply(obj$data,function(data) wly1(data,cumulative,start.on),simplify=FALSE);
    obj=clc(obj,list(unit=7,start.on=start.on,center=FALSE));
  }
  if (center) obj=center(obj)
  obj;
}
wly1=function(data,cumulative,start.on) {
  dates=data$date;
  days=weekdays(dates);
  start=min(which(days==start.on));
  num.weeks=as.integer((dates[length(dates)]+1-dates[start])/7);
  weeks=seq(dates[start],by=7,length=num.weeks);
  if (cumulative) data=data[data$date %in% weeks,]
  else {
    data=subset(data,subset=(date>=min(weeks)&date<=(max(weeks+6))));
    byweek=split(data,cut(data$date,length(weeks),labels=weeks));
    data=data.frame(date=weeks,do.call(rbind,lapply(byweek,function(data)
      colSums(data[,-1,drop=FALSE]))));
  }
  rownames(data)=NULL;
  data;
}
## convert pseudo-object data to daily. nop if already daily
daily=function(obj,center=TRUE,method='linear',args=NULL) {
  if (obj$unit==1) return(obj);
  if (center) obj=center(obj,center=TRUE);
  cumulative=obj$cumulative;
  obj$data=
      if(obj$datasrc!='doh') dly1(obj$data,cumulative,center)
      else sapply(obj$data,function(data) dly1(data,cumulative,center),simplify=FALSE);
  obj$unit=1;
  obj=fit(obj,method=method,args=args,fit.unit=1);
  obj;
}
dly1=function(data,cumulative,center) {
  if (!cumulative) {
      data[,-1]=data[,-1]/7;            # scale data
      ## add data for first and last dates to anchor the fit
      mindate=if(center) min(data$date)-3 else min(data$date);
      maxdate=if(center) max(data$date)+3 else max(data$date)+6;
      minrow=data.frame(date=mindate,data[1,-1]);
      maxrow=data.frame(date=maxdate,data[nrow(data),-1]);
      data=rbind(minrow,data,maxrow);
    } else {
      ## stretch data to fill entire date range
      mindate=if(center) min(data$date)-3 else min(data$date);
      maxdate=if(center) max(data$date)+3 else max(data$date)+6;
      data$dates[1]=mindate
      data$dates[nrow(data)]=maxdate;
    }
  data;
  }
## center weekly data, ie, adjust dates to middle day of week. eg Wed for default Sun start
## revert to original dates if center=FALSE
center=function(obj,center=TRUE)  {
  if (obj$unit==1) stop('Can only center weekly objects');
  if (obj$center==center) return(obj);
  if (center) inc=3 else inc=-3;
  obj$data=
    if(obj$datasrc!='doh') ctr1(obj$data,inc)
    else sapply(obj$data,function(data) ctr1(data,inc),simplify=FALSE);
  clc(obj,list(report.on=inc_day(obj$start.on,inc),center=center));
}
ctr1=function(data,inc) {
  data$date=data$date+inc;
  data;
}
## add 'extra' counts to DOH objects to adjust for incomplete data near end
## fun computed by 'extrafun' (via 'init_extra') function in extra.R
extra=
  function(obj,fun=NULL) {
    if (obj$datasrc!='doh')
      stop("'extra' transform only makes sense for 'doh' objects, not ",obj$datasrc," objects");
    places=colnames(obj$data$all)[-1];
    ages=names(obj$data);
    ## R needs extra parens in is.null((fun=...)) to set 'fun' this way
    if (is.null(fun)&&is.null((fun=param(list=paste(sep='.','extra',obj$what))))) {
      fun=init_extra(what=obj$what);
    }
    vdate=as_date(obj$version);
    data=sapply(ages,function(age) extra1(fun,obj$data[[age]],places,age,vdate),
                simplify=FALSE);
    clc(obj,list(data=data,extra.fun=fun,extra=TRUE));
  }
extra1=function(fun,data,places,age,vdate) {
 data=do.call(rbind,lapply(1:nrow(data),function(i) {
    date=data[i,'date'];
    data=data[i,-1];
    props=fun(date,places,age,vdate=vdate);
    data=data.frame(date=date,round(data/props));
  }));
  data;
}
## lagged correlation
## TODO: not really a transformation but no place else to put it. move to better place
lag=function(obj1,obj2,name='state',lag.max=28,plot=FALSE) {
  if (obj1$unit!=obj2$unit) stop("Cannot compute lag for objects with different time units");
  lag.max=lag.max/obj1$unit;
  data1=obj1$data;
  data2=obj2$data;
  dates=as_date(data1$date %&% data2$date);
  data1=subset(data1,subset=date%in%dates);
  data2=subset(data2,subset=date%in%dates);
  lag=ccf(data1[,name],data2[,name],lag.max=lag.max,plot=plot);
}
## ---- Format slices of 'pseudo-object' as data.frame ----
## multiple ages for one place, or multiple places for one age
##   in multi-place mode, can hande special 'other' place
## TODO: not really a transform but no place else to put it. move to better place
data_obj=function(obj,places='state',ages=NULL,per.capita=FALSE,pop=NULL) {
  if (length(places)>1&&length(ages)>1)
    stop("Only one of 'places' or 'ages' can have multiple values");
  if (is.null(ages)&&obj$datasrc=='doh') ages=names(obj$data);
  if (!(is.null(ages)||identical(ages,'all'))&&obj$datasrc!='doh')
    stop("Can only select age groups from 'doh' objects, not ",obj$datasrc," objects");
  data=obj$data;
  if (length(ages)>1) {
    ## multiple ages for one place
    byage=data[ages];
    data=data.frame(date=byage[[1]]['date'],
                    do.call(cbind,sapply(byage,function(data) data[,places],simplify=FALSE)),
                    check.names=FALSE); 
  } else {
    ## multiple places for one age or from object w/o age groups
    if (obj$datasrc=='doh') data=data[[ages]];
    if ('other' %in% places) {
      places=places[places!='other'];
      other=colnames(data)%-%c(places,cq(date,state,notKing));
      data$other=rowSums(data[,other]);
      places=c(places,'other');
    }
    data=data[,c('date',places)]
  }
  if (per.capita) data=per_capita(data,pop,places,ages);
  data;
}
