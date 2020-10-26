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

## read raw imported data and return as object
raw=function(what=cq(cases,admits,deaths),datasrc=param(datasrc),version='latest') {
  what=match.arg(what);
  datasrc=match.arg(datasrc);
  if (what=='admits'&&datasrc %notin% cq(doh,trk))
    stop("Only have admits data for doh and trk, not ",datasrc);
  if (!is.null(version)&&version=='latest') version=latest_version(datasrc,what);
  data=load_data(whatv=what,datasrc=datasrc,version=version);
  newobj=if(datasrc!='doh') cvdat else cvdoh;
  obj=newobj(data=data,datasrc=datasrc,what=what,version=version,
             fit=FALSE,roll=FALSE,extra=FALSE,edit=FALSE);
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
## fit object data
fit=function(obj,...) UseMethod('fit')
fit.cvdat=function(obj,method='sspline',args=list(),fit.clamp=0,fit.unit=1) {
  dates=dates(obj);
  counts=counts(obj);
  fun=fitfun(x=as.numeric(dates),y=counts,method=method,args=args,clamp=fit.clamp);
  dates=seq(min(dates),max(dates),by=fit.unit);
  data=data.frame(date=dates,sapply(fun,function(f) f(dates)));
  clc(obj,list(data=data,fit=method,fit.fun=fun,fit.args=args));
}
fit.cvdoh=function(obj,method='sspline',args=list(),fit.clamp=0,fit.unit=1) {
  dates=dates(obj);
  ages=ages(obj);
  x=as.numeric(dates);
  fun=lapply(ages,function(age) 
    fitfun(x=x,y=counts(obj,age),method=method,args=args,clamp=fit.clamp));
  dates=seq(min(dates),max(dates),by=fit.unit);
  data=lapply(fun,function(fun) data.frame(date=dates,sapply(fun,function(f) f(dates))));
  names(data)=ages;
  clc(obj,list(data=data,fit=method,fit.fun=fun,fit.args=args));
}
## use rolling mean to smooth object data
## right alignment only. fills bottom with partial means
roll=function(obj,...) UseMethod('roll')
roll.cvdat=function(obj,width=NULL) {
  if (is.null(width)) width=if(is_weekly(obj)) 3 else 7;
  obj$data=roll1(obj$data,width);
  clc(obj,list(roll=paste(width,if(is_weekly(obj)) 'weeks' else 'days')));
}
roll.cvdoh=function(obj,width=NULL) {
  if (is.null(width)) width=if(is_weekly(obj)) 3 else 7;
  obj$data=sapply(obj$data,function(data) roll1(data,width),simplify=FALSE);
  clc(obj,list(roll=paste(width,if(is_weekly(obj)) 'weeks' else 'days')));
}
roll1=function(data,width) {
  dates=data[,1];
  counts=data[,-1,drop=FALSE];
  len=nrow(counts);
  w1=width-1;
  counts=round(apply(counts,2,function(x) 
    c(sapply(1:w1,function(i) mean(x[1:i])),sapply(1:(len-w1),function(i) mean(x[i:(i+w1)])))));
  counts=as.data.frame(counts)
  data=cbind(date=dates,counts);
  data;
}
## convert object data to cumulative. nop if already cumulative
cumulative=function(obj,...) UseMethod('cumulative')
cumulative.cvdat=function(obj,week.end=FALSE) {
  if (is_cumulative(obj)) return(obj);
  unit=obj$unit;
  obj$data=cum1(obj$data,unit,week.end);
  obj$cumulative=TRUE;
  obj;
}
cumulative.cvdoh=function(obj,week.end=FALSE) {
  if (is_cumulative(obj)) return(obj);
  unit=obj$unit;
  obj$data=sapply(obj$data,function(data) cum1(data,unit,week.end),simplify=FALSE);
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
## convert object data to incremental. nop if already incrmental
incremental=function(obj,...) UseMethod('incremental')
incremental.cvdat=function(obj) {
  if (is_incremental(obj)) return(obj);
  obj$data=inc1(obj$data)
  obj$cumulative=FALSE;
  obj;
}
incremental.cvdoh=function(obj) {
  if (is_incremental(obj)) return(obj);
  obj$data=sapply(obj$data,function(data) inc1(data),simplify=FALSE);
  obj$cumulative=FALSE;
  obj;
}
inc1=function(data) {
  ## use check.names=FALSE so R won't replace spaces in place names...
  data.frame(date=data[,1],rbind(data[1,-1],apply(data[,-1],2,diff)),check.names=FALSE)
}
## convert object data to weekly and optionally center
weekly=function(obj,...) UseMethod('weekly')
weekly.cvdat=function(obj,start.on='Sunday',center=TRUE) {
  if (!is_weekly(obj)) {
    start.on=match_day(start.on);
    obj$data=wkly1(obj$data,obj$cumulative,start.on);
    obj=clc(obj,list(unit=7,start.on=start.on,center=FALSE));
  }
  if (center) obj=center(obj)
  obj;
}
weekly.cvdoh=function(obj,start.on='Sunday',center=TRUE) {
  if (!is_weekly(obj)) {
    start.on=match_day(start.on);
    obj$data=sapply(obj$data,function(data) wkly1(data,obj$cumulative,start.on),simplify=FALSE);
    obj=clc(obj,list(unit=7,start.on=start.on,center=FALSE));
  }
  if (center) obj=center(obj)
  obj;
}
wkly1=function(data,cumulative,start.on) {
  dates=data$date;
  days=weekdays(dates);
  start=min(which(days==start.on));
  num.weeks=as.integer((dates[length(dates)]+1-dates[start])/7);
  weeks=seq(dates[start],by=7,length=num.weeks);
  if (cumulative) data=data[data$date %in% weeks,]
  else {
    data=subset(data,subset=(date>=min(weeks)&date<=(max(weeks+6))));
    byweek=split(data,cut(data$date,length(weeks),labels=weeks));
    data=cbind(date=weeks,as.data.frame(do.call(rbind,lapply(byweek,function(data)
      colSums(data[,-1,drop=FALSE])))));
  }
  rownames(data)=NULL;
  data;
}
## convert object data to daily. nop if already daily
daily=function(obj,...) UseMethod('daily')
daily.cvdat=function(obj,center=TRUE,method='linear',args=NULL) {
  if (is_daily(obj)) return(obj);
  if (center) obj=center(obj,center=TRUE);
  obj$data=dly1(obj$data,obj$cumulative,center);
  obj$unit=1;
  fit(obj,method=method,args=args,fit.unit=1);
}
daily.cvdoh=function(obj,center=TRUE,method='linear',args=NULL) {
  if (is_daily(obj)) return(obj);
  if (center) obj=center(obj,center=TRUE);
  obj$data=sapply(obj$data,function(data) dly1(data,obj$cumulative,center),simplify=FALSE);
  obj$unit=1;
  fit(obj,method=method,args=args,fit.unit=1);
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
center=function(obj,...) UseMethod('center')
center.cvdat=function(obj,center=TRUE)  {
  if (!is_weekly(obj)) stop('Can only center weekly objects');
  if (obj$center==center) return(obj);
  if (center) inc=3 else inc=-3;
  obj$data=cntr1(obj$data,inc)
  clc(obj,list(report.on=inc_day(obj$start.on,inc),center=center));
}
center.cvdoh=function(obj,center=TRUE)  {
  if (!is_weekly(obj)) stop('Can only center weekly objects');
  if (obj$center==center) return(obj);
  if (center) inc=3 else inc=-3;
  obj$data=sapply(obj$data,function(data) cntr1(data,inc),simplify=FALSE);
  clc(obj,list(report.on=inc_day(obj$start.on,inc),center=center));
}
cntr1=function(data,inc) {
  data$date=data$date+inc;
  data;
}
## add 'extra' counts to DOH objects to adjust for incomplete data near end
## fun computed by 'extrafun' (via 'init_extra') function in extra.R
extra=function(obj,...) UseMethod('extra')
extra.cvdat=function(obj,fun=NULL,objs=NULL,incompatible.ok=param(incompatible.ok))
  stop("'extra' transform only makes sense for 'doh' objects, not ",obj$datasrc," objects")
extra.cvdoh=function(obj,fun=NULL,objs=NULL,incompatible.ok=param(incompatible.ok)) {
  places=places(obj);
  ages=ages(obj);
  if (is.null(fun)) {
    if (!is.null(objs)) {
      if (is_cvdat(objs)) objs=list(objs);
      objs=c(list(obj),objs);
    }
    fun=extrafun(what=obj$what,objs=objs,incompatible.ok=incompatible.ok);
  }
  vdate=vdate(obj);
  data=sapply(ages,function(age) extra1(fun,obj$data[[age]],places,age,vdate),
              simplify=FALSE);
  clc(obj,list(data=data,extra.fun=fun,extra=TRUE));
}
extra1=function(fun,data,places,age,vdate) {
  data=do.call(rbind,lapply(1:nrow(data),function(i) {
    date=data[i,1];
    counts=data[i,-1];
    props=fun(date,places,age,vdate=vdate);
    ## have to use cbind, not data.frame, so R won't convert spaces
    data=cbind(date=date,round(counts/props));
  }));
  data;
}
## edit object data
## there are separate functions for editing places, ages, and dates
## (presently just places)
## TODO: add KEEP, RM (or DROP), SUM, NEGATE args that avoid NSE
edit=function(obj,...) UseMethod('edit')
edit.cvdat=function(obj,...,
                    KEEP=NULL,RM=NULL,SUM=list(),NEG=list(),SUB=list(),DROP=RM,NEGATE=NEG,
                    EMPTY.OK=FALSE) {
  dots=match.call(expand.dots=FALSE)$...;
  ## parse dots and combine with explicit args. set EXPR, DATE, KEEP, DROP variables here
  edit_args(dots,KEEP=KEEP,DROP=DROP,SUM=SUM,NEGATE=NEGATE,SUB=SUB);
  ## split args by target (places, ages, date)
  args.targ=edit_split(EXPR,KEEP,DROP,places(obj),ages(obj));
  parent=parent.frame(n=1);
  if (!is.null(args.targ$places)) obj=do.call(edit_places_,c(list(obj),args.targ$places));
  if (length(args.targ$ages)!=0) obj=do.call(edit_ages_,c(list(obj),args.targ$ages));
  if (!is.null(DATE)) {
    ## adapted from Advanced R by Wickham http://adv-r.had.co.nz/Computing-on-the-language.html
    dates=dates(obj);
    r=eval(DATE,data.frame(date=dates),parent);
    obj=edit_dates_(obj,r)
  }
  if ((length(ages(obj))==0)&&!EMPTY.OK)
    stop('No ages left after edit and EMPTY.OK is FALSE');
  if ((length(places(obj))==0)&&!EMPTY.OK)
    stop('No places left after edit and EMPTY.OK is FALSE');
  if ((length(dates(obj))==0)&&!EMPTY.OK)
    stop('No dates left after edit and EMPTY.OK is FALSE');
  obj;
  clc(obj,list(edit=TRUE));
}
## edit_places
edit_places_=function(obj,...) UseMethod('edit_places_')
edit_places_.cvdat=function(obj,EXPR=list(),KEEP=NULL,DROP=NULL) {
  EXPR=edit_fixneg(EXPR,'state',places(obj));
  data=edit_places1(obj$data,EXPR,KEEP,DROP);
  pop=edit_popp(obj$pop,EXPR,KEEP,DROP);
  clc(obj,list(data=data,pop=pop,edit.places=TRUE));
}
edit_places_.cvdoh=function(obj,EXPR=list(),KEEP=NULL,DROP=NULL) {
  EXPR=edit_fixneg(EXPR,'state',places(obj));
  ages=ages(obj);
  data=sapply(ages,function(age) edit_places1(obj$data[[age]],EXPR,KEEP,DROP),
              simplify=FALSE);
  pop=edit_popp(obj$pop,EXPR,KEEP,DROP);
  clc(obj,list(data=data,pop=pop,edit.places=TRUE));
}
## edit_ages
edit_ages_=function(obj,...) UseMethod('edit_ages_')
edit_ages_.cvdat=function(obj,EXPR=list(),KEEP=NULL,DROP=NULL) {
  stop("Editing ages only makes sense for 'doh' objects, not ",obj$datasrc," objects");
}
edit_ages_.cvdoh=function(obj,EXPR=list(),KEEP=NULL,DROP=NULL) {
  EXPR=edit_fixneg(EXPR,'all',ages(obj));
  data=edit_ages1(obj$data,EXPR,KEEP,DROP);
  pop=edit_popa(obj$pop,EXPR,KEEP,DROP);
  clc(obj,list(data=data,pop=pop,edit.ages=TRUE));
}
## edit_dates
edit_dates_=function(obj,...) UseMethod('edit_dates_')
edit_dates_.cvdat=function(obj,r) {
  data=obj$data[r,];
  clc(obj,list(data=data,edit.dates=TRUE));
}
edit_dates_.cvdoh=function(obj,r) {
  ages=ages(obj);
  data=sapply(ages,function(age) obj$data[[age]][r,],simplify=FALSE);
  clc(obj,list(data=data,edit.dates=TRUE));
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
