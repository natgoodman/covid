#################################################################################
##
## Author:  Nat Goodman
## Created: 20-07-09
##          with code adapted from clapi/distr_3 created 20-04-05
##
## Copyright (C) 2020 Nat Goodman.
## 
## S3 class for covid data objects
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- S3 classes for covid data objects ----
## constructor for cvdat class
cvdat=function(...) {
  obj=list(...);
  class(obj)='cvdat';
  invisible(obj);
}
## constructor for cvdoh class (subclass of cvdat)
cvdoh=function(...) {
  obj=cvdat(...);
  class(obj)=c('cvdoh',class(obj));
  invisible(obj);
}
## constructor for cvcdc class (subclass of cvdat)
cvcdc=function(...) {
  obj=cvdat(...);
  class(obj)=c('cvcdc',class(obj));
  invisible(obj);
}
## testers for cvdat and suclasses
is_cvdat=function(obj) 'cvdat' %in% class(obj);
is_cvdoh=function(obj) 'cvdoh' %in% class(obj);
is_cvcdc=function(obj) 'cvcdc' %in% class(obj);

## generic accessors
places=function(obj) UseMethod('places')
places.cvdat=function(obj) colnames(obj$data)[-1]
places.cvdoh=function(obj) colnames(obj$data[[1]])[-1]
places.cvcdc=places.cvdoh;
ages=function(obj) UseMethod('ages')
ages.cvdat=function(obj) 'all'
ages.cvdoh=function(obj) names(obj$data)
ages.cvcdc=ages.cvdoh;
dates=function(obj) UseMethod('dates')
dates.cvdat=function(obj) obj$data$date
dates.cvdoh=function(obj) obj$data[[1]]$date
dates.cvcdc=dates.cvdoh;
counts=function(obj,age=NULL) UseMethod('counts')
counts.cvdat=function(obj,age='all') {
  if (is.null(age)|age=='all') {
    counts=obj$data[,-1,drop=FALSE];
    rownames(counts)=obj$data$date;
    counts;
  }
  else stop("Can only select age groups from 'doh' objects, not ",obj$datasrc," objects")
}
counts.cvdoh=function(obj,age='all') {
  if (is.null(age)) age='all';
  ages=names(obj$data);
  if (age %notin% ages)
    stop("'age' ",age," not found in object.\nValid ages are ",paste(collapse=', ',ages));
  data=obj$data[[age]];
  counts=data[,-1,drop=FALSE];
  rownames(counts)=data$date;
  counts;
}
counts.cvcdc=counts.cvdoh;

## simple accessors
datasrc=function(obj) obj$datasrc
what=function(obj) obj$what
version=function(obj) obj$version
vdate=function(obj) as_date(obj$version)
unit=function(obj) obj$unit
fit_method=function(obj) {
  fit=obj$fit;
  if (fit==FALSE) 'raw' else fit;
}
extra_method=function(obj) {
  extra=obj$extra;
  if (extra==FALSE) 'raw' else extra;
}
pop=function(obj) obj$pop
mort=function(obj) obj$mort

## simple attribute testers
is_fit=function(obj) if (obj$fit==FALSE) FALSE else TRUE
is_extra=function(obj) obj$extra
is_daily=function(obj) obj$unit==1
is_weekly=function(obj) obj$unit==7
is_center=function(obj) obj$center
is_cum=is_cumulative=function(obj) obj$cumulative
is_inc=is_incremental=function(obj) !obj$cumulative







