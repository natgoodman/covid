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
    places.all=Reduce(intersect,lapply(objs,function(obj) places(obj)));
    ages.all=Reduce(intersect,lapply(objs,function(obj) ages(obj)))
    if (length(places.all)==0) stop("No valid places for these objects");
    if (length(ages.all)==0) stop("No valid ages for these objects");
    if (is.null(places)) places=places.all
    else {
      bad=places %-% places.all;
      if (length(bad)>0)
        stop("Invalid places: ",paste(collapse=', ',bad),
             ".\nValid places for these objects are: ",paste(collapse=', ',places.all));
    }
    if (is.null(ages)) ages=ages.all
    else {
      bad=ages %-% ages.all;
      if (length(bad)>0)
        stop("Invalid ages: ",paste(collapse=', ',bad),
             ".\nValid ages for these objects are: ",paste(collapse=', ',ages.all));
    }
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
