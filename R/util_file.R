#################################################################################
##
## Author:  Nat Goodman
## Created: created 20-05-03
##          from dat.R created 20-05-02
##          from frecl/R/dat.R created 20-01-17
##          from misg/R/datman.R created 19-01-01
##          from repwr/R/datman.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## File utilities
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Manipulate file names ----
## construct file or directory pathname from components
## wrapper for file.path with base, tail and suffix pasted on
##  base appended with '.'
##  tail components combined with '.'
##  suffix added unless already there
filename=function(...,base=NULL,tail=NULL,suffix=NULL) {
  if (!is.null(base)||!is.null(tail)) base=paste(collapse='.',c(base,tail));
  ## NG 18-10-15: remove NULL from ... before calling file.path
  ## do.call(f,as.list(unlist(list(...))))) from https://stackoverflow.com/questions/47360937/call-an-r-function-with-run-time-generated-ellipsis-arguments-dot-dot-dot-thr
  ##  if (is.null(base)) file=file.path(...) else file=file.path(...,base);
  file=do.call(file.path,as.list(unlist(list(...))));
  if (!is.null(base)) file=file.path(...,base);
  if (!is.null(suffix)) {
    ## remove leading '.' if present
    suffix=sub('^\\.','',suffix,perl=T);
    pattern=paste(collapse='|',paste(sep='','\\.',suffix,'$'));
    file=ifelse(grepl(pattern,file),file,paste(sep='.',file,suffix[1]));
  }
  file;
}
## NOTE: basename, dirname base R functions.
##   basename - filename part of path
##   dirname - path. everything but basename

## remove suffix from path or base
desuffix=function(path,suffix=NULL,keep.dir=TRUE) {
  if (!is.null(suffix)) {
    ## remove leading '.' if present
    suffix=sub('^\\.','',suffix,perl=T);
    pattern=paste(collapse='|',paste(sep='','\\.',suffix,'$'));
  } else
    ## remove any suffix (from last '.' to end of string)
    pattern='\\.[^.]*$';
  if (keep.dir) sub(pattern,'',path) else sub(pattern,'',basename(path));
}
## replace suffix from path or base
resuffix=function(path,old.suffix=cq(txt,RData),suffix='RData',keep.dir=TRUE) {
  sapply(path,function(path) {
    base=desuffix(path,old.suffix,keep.dir);
    filename(base,suffix=suffix);
  });
}
## remove tail & suffix from path or base. leaves part of basename before first .
baseonly=function(path,suffix=NULL,keep.dir=TRUE) {
  dir=dirname(path);
  file=basename(path);
  if (!is.null(suffix)) {
    ## remove leading '.' if present
    suffix=sub('^\\.','',suffix,perl=T);
    pattern=paste0('(\\..*)*(',paste(collapse='|',paste(sep='','\\.',suffix,'$')),')');
  } else
    ## remove everything from first '.' to end of string
    pattern='\\..*$';
  base=sub(pattern,'',file);
  if (keep.dir) filename(dir,base) else base;
}
