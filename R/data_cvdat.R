#################################################################################
##
## Author:  Nat Goodman
## Created: 20-07-13
##          from code in transform.R
##
## Copyright (C) 202 Nat Goodman.
## 
## Generate data frame cvdat objects
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Make data.frame from cvdat objects ----
## like plot_cvdat but generates data frome
data_cvdat=
  function(objs,places='state',ages=NULL,per.capita=FALSE,
           attrs=cq(unit,cumulative,what,datasrc,version,fit),
           attrs.colnames=attrs) {
    if (is_cvdat(objs)) objs=list(objs);
    places.all=Reduce(intersect,lapply(objs,function(obj)
      if (obj$datasrc!='doh') colnames(obj$data)[-1] else colnames(obj$data$all)[-1]));
    places=if(is.null(places)) places.all else places.all %&% places;
    if (length(places)==0) stop("No places. Nothing to plot");
    if (is.null(ages))
      ages=if(all('doh'==sapply(objs,function(obj) obj$datasrc))) names(objs[[1]]$data) else 'all';
    series=data_series(objs,places,ages,attrs);
    if (per.capita) series=series_percap(series);
    ct=ct_attrs(series,attrs)
    ## construct colnames. apply to series so merge won't complain about duplicate colnames
    nattr=series$xattr[,ct$mv.attrs%-%cq(series,obj),drop=FALSE];
    names=apply(nattr,1,function(row) paste(collapse=';',row));
    y=mapply(function(data,name) {colnames(data)[2]=name; data},series$series,names,SIMPLIFY=F);
    ## gather all dates for joining, then do it!
    dates=sort(unique(do.call(c,lapply(series$series,function(series) series$date))));
    data=Reduce(function(...) merge(...,by='date',all=TRUE), y, data.frame(date=dates));
    data;
  }
