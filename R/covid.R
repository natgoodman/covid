## process Washington DOH data circa Apr 26, 2020

library(readxl)
library(RColorBrewer)
source('../NewPro/toutr/R/util.R')
source('../NewPro/toutr/R/plot_util.R')

doit=function(version='20-04-26',notKing=T) {
  file=filename('Covid',base='WADOH',tail=version,suffix='xlsx')
  cases=suppressMessages(read_excel(file,sheet='Cases'))
  deaths=suppressMessages(read_excel(file,sheet='Deaths'))
  cases=as.data.frame(cases,stringsAsFactors=F)
  deaths=as.data.frame(deaths,stringsAsFactors=F)
  cases$County=sub(' County','',cases$County)
  deaths$County=sub(' County','',deaths$County)

  cases=cases[,1:3]
  deaths=deaths[,1:3]

  colnames(cases)=cq(county,week,cases)
  colnames(deaths)=cq(county,week,deaths)
  ## remove bogus cases row with week=2020-01-16
  cases=subset(cases,subset=(week!='2020-01-16'))
  cases$week=as.Date(cases$week)
  deaths$week=as.Date(deaths$week)
  
  cases.bycounty=split(cases,cases$county)
  deaths.bycounty=split(deaths,deaths$county)
  
  weeks=data.frame(week=sort(unique(c(cases$week,deaths$week))),stringsAsFactors=F)
  counties=data.frame(county=sort(unique(c(cases$county,deaths$county))),stringsAsFactors=F)
  weeks<<-weeks$week
  counties<<-counties$county
  
  cases.bycounty=lapply(cases.bycounty,function(cases) {
    cases=merge(weeks,cases[,cq(week,cases)],all=T);
    cases$cases=with(cases,ifelse(is.na(cases),0,cases));
    cases})
  deaths.bycounty=lapply(deaths.bycounty,function(deaths) {
    deaths=merge(weeks,deaths[,cq(week,deaths)],all=T);
    deaths$deaths=with(deaths,ifelse(is.na(deaths),0,deaths));
    deaths})

  cases=as.data.frame(do.call(cbind,lapply(cases.bycounty,function(cases) cases$cases)))
  colnames(cases)=names(cases.bycounty)
  total=rowSums(cases)
  if (notKing) cases$notKing=total-cases$King
  cases$state=total
  cases=cbind(week=weeks,cases)
  file=filename('Covid',base='cases',tail=version,suffix='txt')
  write.table(cases,file=file,sep='\t',quote=F,row.names=F);
  cases<<-cases;
  
  deaths=as.data.frame(do.call(cbind,lapply(deaths.bycounty,function(deaths) deaths$deaths)))
  colnames(deaths)=names(deaths.bycounty)
  total=rowSums(deaths)
  if (notKing) deaths$notKing=total-deaths$King
  deaths$state=total
  deaths=cbind(week=weeks,deaths)
  file=filename('Covid',base='deaths',tail=version,suffix='txt')
  write.table(deaths,file=file,sep='\t',quote=F,row.names=F);
  deaths<<-deaths;
}
interpit=function(method='aspline',interp.len=1000) {
  cases.f=interp_fun(cases,method=method,interp.len=interp.len)
  deaths.f=interp_fun(deaths,method=method,interp.len=interp.len)
  weeks.interp=seq(min(weeks),max(weeks),len=interp.len)
  cases.interp=do.call(data.frame,lapply(cases.f,function(f) f(weeks.interp)))
  deaths.interp=do.call(data.frame,lapply(deaths.f,function(f) f(weeks.interp)))
  cases.interp=cbind(week=weeks.interp,cases.interp)
  deaths.interp=cbind(week=weeks.interp,deaths.interp)
  cases.f<<-cases.f
  deaths.f<<-deaths.f
  weeks.interp<<-weeks.interp
  cases.interp<<-cases.interp
  deaths.interp<<-deaths.interp
}
## compute lagged correlation
corlagit=function(names=NULL) {
  all.names=intersect(names(cases.f),names(deaths.f))
  names=if(is.null(names)) all.names else names[names%in%all.names]
  if (is.null(names)) stop('No valid names provided. Nothing to do');
  ## lag expressed as days
  lag=seq(min(weeks),max(weeks)+7,by=1)
  corlag=sapply(names,simplify=FALSE,function(name) {
    cf=cases.f[[name]]
    df=deaths.f[[name]]
    ## code adapted from technofob.com/2016/02/14/. Thx!
    corlag=ccf(cf(lag),df(lag),lag.max=length(lag)-1,plot=FALSE)
    data.frame(cor=corlag$acf[,,1],lag=corlag$lag[,,1])
  })
  ## all lags should be identical. check for sanity
  lag=corlag[[1]]$lag;
  bad=sapply(corlag,function(cl) !identical(cl$lag,lag))
  if (any(bad))
    stop(paste('Bad news: lag(s) returned by ccf not identical for names',
               paste(collapse=', ',names[bad])))
  ## reformat as data frame analogous to cases, deaths
  corlag=do.call(data.frame,lapply(corlag,function(cl) cl$cor))
  corlag=cbind(lag=lag,corlag);
  lag<<-lag
  corlag<<-corlag;
}
## res = data.frame(cor=ccc$acf[,,1],lag=ccc$lag[,,1]); res[which.max(res$cor),]$lag

plotit=function(what,names=NULL) {
  what=as.character(pryr::subs(what))
  data=get(what,parent.frame(n=1))
  x=data$week;
  names=if(is.null(names)) setdiff(colnames(data),'week') else names[names%in%colnames(data)]
  y=data[,names]
  col=col_brew(n=ncol(y),'Set1')
  matplot(x,y,type='l',lty='solid',col=col,xlab=NA,ylab=what,main=what,xaxt='n');
  grid(nx=NA,ny=NULL) # draw y grid lines. we'll draw x ourselves at first day of month
  mon.01=mon_day(x,1)
  mon.15=mon_day(x,15)
  abline(v=c(mon.01,mon.15),col="lightgray",lty="dotted")
  ## axis line below adapted from stackoverflow.com/questions/4843969. Thx!
  axis(1,mon.01,format(mon.01,"%b-%d"),cex.axis=0.8)
  add_legend(where='topleft',col=col,labels=colnames(y),cex=0.7)
  invisible()
}
## TODO: plot cases and deaths together
plot_corlag=
  function(data,names=setdiff(colnames(corlag),'lag'),
           title='Lagged correlation of cases to deaths',cex.title=cex_title(title)) {
    if (missing(data)) data=get('corlag',parent.frame(n=1))
    x=data$lag
    names=if(is.null(names)) setdiff(colnames(data),'lag') else names[names%in%colnames(data)]
    y=data[,names]
    col=col_brew(n=ncol(y),'Set1')
    matplot(x,y,type='l',lty='solid',col=col,xlab='lag (in days)',ylab='correlation',
            main=title,cex.main=cex.title);
    grid();
    add_legend(where='topleft',col=col,labels=colnames(y),cex=0.7)
    invisible()
  }

## adapted from www.r-bloggers.com/dates-in-r-and-the-first-day-of-the-month. Thx!
mon_day=function(date,day) as.Date(paste(sep='-',unique(format(date,format="%Y-%m")),day))

## adapted from toutr/R/stats_hiddn.R
## driver for interpolation methods
## interp.len used for methods whose core function can only return interpolated results
##   (presently just aspline)
interp_fun=
  function(data,names=setdiff(colnames(data),'week'),
           method=cq(aspline,ispline,sspline,spline,loess,linear,step),method.args=NULL,
           interp.len=1000) {
    x=data$week;
    names=names[names%in%colnames(data)]
    y=data[,names]
    method=match.arg(method);
    method.fun=get0(paste0(method,'_fun'),mode='function');
    if (is.null(method.fun)) stop(paste0('Bad news: function not found for method=',method,
                                         '. Should have been caught earlier!'));
    interp.x=seq(min(x),max(x),len=interp.len);
    if (is.null(method.args))
      method.args=
        switch(method,
               aspline=list(method='improved',interp.x=interp.x),
               sspline=list(spar=NULL),
               spline=list(method='monoH.FC'),
               loess=list(span=0.75),
               linear=list(method='linear',yleft=0,yright=1),
               step=list(method='constant',yleft=0,yright=1),
               list());
    method.args$x=x;
    f.exact=sapply(names,function(name)
      do.call(method.fun,c(list(y=data[[name]]),method.args)))
  }
## these functions wrap underlying interpolation methods with consistent API
## CAUTION: can't simply pass ... to approxfun, 'cuz aspline needs ..., too
##   can't simply split ... based on names(formals(fun)) 'cuz both functions have 'method'
##   could give aspline first dibs, remove its args from ..., then pass rest to approxfun
##     but solution here is simpler and works fine
## akima spline
aspline_fun=function(x,y,interp.x,...) {
  interp.y=suppressWarnings(akima::aspline(x,y,interp.x,...))$y;
  approxfun(interp.x,interp.y,yleft=0,yright=0);
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
linear_fun=step_fun=function(x,y,interp=F,x.interp,...) {
  approxfun(x,y,...);
}

## construct file or directory pathname from components
## wrapper for file.path with base, tail and suffix pasted on
##  base appended with '.'
##  tail components combined with '.'
##  suffix added unless already there
filename=function(...,base=NULL,tail=NULL,suffix=NULL) {
  if (!is.null(base)||!is.null(tail)) base=paste(collapse='.',c(base,tail));
  ## NG 18-10-15: remove NULL from ... before calling file.path
  ## do.call(f,as.list(unlist(list(...))))) from https://stackoverflow.com/questions/47360937
  ##  if (is.null(base)) file=file.path(...) else file=file.path(...,base);
  file=do.call(file.path,as.list(unlist(list(...))));
  if (!is.null(base)) file=file.path(...,base);
  if (!is.null(suffix)) {
    ## remove leading '.' if present
    suffix=sub('^\\.','',suffix,perl=T);
    suffix.pattern=paste(collapse='|',paste(sep='','\\.',suffix,'$'));
    file=ifelse(grepl(suffix.pattern,file),file,paste(sep='.',file,suffix[1]));
  }
  file;
}
## remove suffix from filename
desuffix=function(file,suffix=c('RData','txt')) {
  if (!is.null(suffix)) {
    ## remove leading '.' if present
    suffix=sub('^\\.','',suffix,perl=T);
    suffix.pattern=paste(collapse='|',paste(sep='','\\.',suffix,'$'));
    file=sub(suffix.pattern,'',file);
  }
  file;
}
## filebasename same as filename but w/o suffix
filebasename=function(...) filename(...,suffix=NULL)
## construct directory pathname. synonym for filebasename
## Sigh. unfortunate choice of name as it masks base::dirname
## dirname=filebasename;
