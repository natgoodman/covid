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
## ---- S3 class for covid data objects ----
## constructor for cvdat class
cvdat=function(...) {
  obj=list(...);
  class(obj)='cvdat';
  invisible(obj);
}
## tester for cvdat class
is_cvdat=function(obj) 'cvdat' %in% class(obj);

