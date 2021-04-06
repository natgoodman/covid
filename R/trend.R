#################################################################################
##
## Author:  Nat Goodman
## Created: 21-02-10
##
## Copyright (C) 2020 Nat Goodman.
## 
## Calculate trend score, ie, assess whether trend in data real
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
trend=function(obj,widths=4,places='state',ages='all',end.dates=NA,do.plot=FALSE) {
  if (is.null(end.dates)) end.dates=NA;
  cases=expand.grid(width=widths,place=places,age=ages,end.date=end.dates,stringsAsFactors=FALSE);
  tr=do.call(rbind,withrows(cases,case,{
    tr1=trend1(obj,width,place,age,end.date,do.plot);
  }));
  data.frame(cases,sign=sign(tr[,'slope']),tr)
}
## assumes one instance of width, places, age, end.date
trend1=function(obj,width=4,place='state',age='all',end.date=NA,do.plot=TRUE) {
  if (is.null(end.date)) end.date=NA;
  if (!is.na(end.date)) obj=edit(obj,date<=end.date);
  data=data_cvdat(obj,places=place,ages=age,cnm='y');
  data=tail(data,n=width);
  mdl=lm(y~date,data=data);
  if (do.plot) {
    plon();
    data$lm=predict(mdl);
    plotm(data,legend='bottomleft');
    ploff();
  }
  ## suppress warning 'essentially perfect fit'
  suppressWarnings(setNames(summary(mdl)$coefficients[2,c(1,4)],cq(slope,pval)));
}
########## code below here experimental

trend_roll=function(obj,width=4,place='state',age='all') {
  w1=width-1;
  data=data_cvdat(obj,places=place,ages=age,cnm='y');
  dates=data$date;
  end.dates=if(w1>0) tail(dates,n=-w1) else dates;
  len=nrow(data);
  troll=lapply(end.dates,function(end.date) {
    trend_roll1(data,end.date=end.date,width=width);
    ## pred=pred[,cq(date,pred)]
    ## colnames(pred)[2]=as.character(end.date)
    ## pred
  })
  ## colnames(pred)[3]=as.character(end.date);
  pred=lapply(troll,function(troll)
    with(troll,{
      pred=pred[,cq(date,pred)];
      colnames(pred)[2]=as.character(end.date);
      pred;
    }));
  pval=sapply(troll,function(troll) troll$pval);
  pred=Reduce(function(...) merge(...,by='date',all=TRUE), pred);
  list(pval=pval,pred=pred)
}

trend_roll1=function(data,end.date,width=4) {
  data=data[data$date<=end.date,];
  data=tail(data,n=width);
  mdl=lm(y~date,data=data);
  slope=summary(mdl)$coefficients[2,1];
  pval=summary(mdl)$coefficients[2,4];
  pred=data.frame(data,pred=predict(mdl))
  list(slope=slope,sign=sign(slope),pval=pval,end.date=end.date,pred=pred);
}
