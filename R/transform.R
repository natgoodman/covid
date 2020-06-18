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
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Data transformations ----
## parameters of transformed data set
##   data source
##   what - cases or deaths
##   raw or fit (aks interpolated or smoothed)
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
  data=load_data(whatv=what,datasrc=datasrc,version=version);
  raw=c(list(data=data,datasrc=datasrc,what=what,version=version,fit='raw'),
           switch(datasrc,
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
fit=function(obj,method='sspline',args=NULL,fit.unit=1) {
  obj$data=
    if(obj$datasrc!='doh') fit1(obj$data,method,args,fit.unit)
    else sapply(obj$data,function(data) fit1(data,method,args,fit.unit),simplify=FALSE);
  set_elements(list(fit=method,fit.args=args),obj);
}
fit1=function(data,method,args,fit.unit) {
  dates=data[,1];
  fun=fit_fun(x=dates,y=data[,-1,drop=FALSE],method=method,args=args,unit=fit.unit);
  dates=seq(min(dates),max(dates),by=fit.unit);
  data.frame(date=dates,do.call(data.frame,lapply(fun,function(f) f(dates))));
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
  obj$cumulative=TRUE;
  obj;
}
inc1=function(data) {
  data.frame(date=data[,1],rbind(data[1,-1],apply(data[,-1],2,diff)))
}
## convert pseudo-object data to weekly and optionally center
weekly=function(obj,start.on='Sunday',center=TRUE) {
  if (obj$unit!=7) {
    start.on=match_day(start.on);
    cumulative=obj$cumulative;
    obj$data=
      if(obj$datasrc!='doh') wly1(obj$data,cumulative,start.on)
      else sapply(obj$data,function(data) wly1(data,cumulative,start.on),simplify=FALSE);
    obj=set_elements(list(unit=7,start.on=start.on,center=FALSE),obj);
  }
  if (center) obj=center(obj)
  newobj;
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
  set_elements(list(report.on=inc_day(obj$start.on,inc),center=center),obj);
}
ctr1=function(data,inc) {
  data$date=data$date+inc;
  data;
}
## lagged correlation. not really a transformation but no place else to put it
## TODO: move to better place
lag=function(obj1,obj2,name='state',lag.max=28,plot=FALSE) {
  if (obj1$unit!=obj2$unit) stop("Cannot compute lag for objects with different time units");
  lag.max=lag.max/obj1$unit;
  data1=obj1$data;
  data2=obj2$data;
  dates=as_date(intersect(data1$date,data2$date));
  data1=subset(data1,subset=date%in%dates);
  data2=subset(data2,subset=date%in%dates);
  lag=ccf(data1[,name],data2[,name],lag.max=lag.max,plot=plot);
}

time_label=Vectorize(function(unit=cq(1,7,daily,weekly)) {
  unit=as.character(unit);
  unit=match.arg(unit);
  switch(unit,'1'='daily','7'='weekly',unit);
})
time_unit=Vectorize(function(label=cq(1,7,daily,weekly)) {
  label=as.character(label);
  label=match.arg(label);
  switch(label,daily=1,weekly=7,as.numeric(label));
})
cuminc_label=Vectorize(function(cum) if (cum) 'cumulative' else 'incremental')
datasrc_label=Vectorize(function(datasrc)
  switch(datasrc,doh='DOH',ihme='IHME',jhu='JHU',nyt='NYTimes',trk='CovidTrack',yyg='C19Pro',
         datasrc),
  USE.NAMES=FALSE)
fit_label=Vectorize(function(fit) if (is.na(fit)) 'raw' else fit)
name_label=function(name,val,SEP='&') {
  if (is.null(val)) val=NA;
  switch(name,
         datasrc=datasrc_label(val),
         unit=time_label(val),
         cumulative=cuminc_label(val),
         fit=fit_label(val),
         val);
}
paste_label=function(labels,SEP='&') {
  if (all(is.na(labels))) NULL else paste(collapse=SEP,unique(labels[!is.na(labels)]));
}
paste_title=function(name,labels,SEP='&') {
  if (all(is.na(labels))) NULL
  else {
    labels=unique(labels);
    if (name=='fit') {
      raw=if('raw' %in% labels) TRUE else FALSE;
      labels=labels[labels!='raw'];
    }
    term=paste(collapse=SEP,
          switch(name,
                 unit=ucfirst(labels),
                 cumulative=ucfirst(labels),
                 what=ucfirst(labels),
                 labels));
    prefix=switch(name,
                  datasrc='from',
                  fit='fitted to',
                  NULL);
    title=if(length(labels)!=0) paste(collapse=' ',c(prefix,term)) else NULL;
    if (name=='fit'&&raw) paste(collapse=SEP,c('raw',title)) else title;
  }
}
paste_legend=function(row,SEP=', ') {
  names=names(row);
  terms=unlist(sapply(names,function(name) {
    label=row[name];
    if (is.na(label)) NULL else label;
  }));
  paste(collapse=SEP,terms);
}
ltitle_label=function(name) {
  switch(name,
         unit='Interval',
         cumulative='Cum/Inc',
         what='What',
         datasrc='Source',
         version='Version',
         fit='Fit');
}
                 
## produce data frame of obj typically single-valued params
obj_attr=function(obj,names=cq(datasrc,what,unit,cumulative,version,fit),label=TRUE) {
  vals=with(obj,mget(names,ifnotfound=list(NULL)));
  if (label) vals=sapply(names,function(name) name_label(name,vals[[name]]),simplify=FALSE);
  xattr=as.data.frame(vals,stringsAsFactors=FALSE);
  rownames(xattr)=NULL;
  xattr;
}
objs_attr=function(objs,names=cq(datasrc,what,unit,cumulative,version,fit),label=TRUE)
  do.call(rbind,lapply(objs,function(obj) obj_attr(obj,names,label)));
  

