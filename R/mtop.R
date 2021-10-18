#################################################################################
##
## Author:  Nat Goodman
## Created: 21-10-04
##          from mort.R created 21-07-30
##          by copying meta.R created 20-06-23 
##          with code adapted from age.R created 20-05-29
##
## Copyright (C) 2021 Nat Goodman.
## 
## Import and manage leading causes of death data from CDC WONDER database
## WA state only. uses DOH age groups
## Input data generated "manually" using web query tool
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Import mortality data ----
## input files in input/meta/mtop
## output files meta/mtop .RData, .txt
download_mtop=function(...) {
  stop("mtop files downloaded 'manually' using CDC WONDER web query tool and stored in ",
       param(inmtopdir));
}
import_mtop=function(ntop=5) {
  dir=param(inmtopdir);
  if (param(verbose)) print(paste('>>> importing mtop files in directory',dir));
  ## read the WA files
  files=list.files(dir,pattern='^wa\\..*.txt$',full.names=TRUE);
  mtop=lapply(files,function(file) read_mtop_input(file,ntop));
  names(mtop)=sub('^wa\\.','',desuffix(basename(files)));
  ## tack on USA
  usa=read_mtop_input(filename(dir,'usa.all.txt'),ntop);
  mtop$USA=usa;
  save_mtop(mtop);
  invisible(mtop);
}
## colwant is vector of column names as they appear in file.
## colmap maps external column names from file to internal column names we use
## CAUTION: software harcodes column names
read_mtop_input=
  function(file,ntop=5,colwant=cq(Notes,'15 Leading Causes of Death',Deaths,'Crude Rate')) {
    mtop=read_(file);
    cause=param(mtopcause);             # map ICD causes to lay terms. NOTE: hand crafted!
    if (is.null(cause)) cause=load_mtopcause();
    ## colmap maps column names from file to internal names we use
    ## CAUTION: software hardcodes column names. be mindful if you change them!
    colmap=setNames(cq(notes,cause,deaths,per.capita),colwant);
    ## for sanity, make sure file has columns we need and all columns valid
    bad=colwant%-%colnames(mtop);
    if (length(bad)) stop(file," is missing column(s): ",paste(collapse=', ',bad));
    bad=colwant%-%names(colmap);
    if (length(bad)) stop("unknown column name(s) in 'colwant': ",paste(collapse=', ',bad));
    ## select and rename columns we want
    mtop=mtop[,colwant];
    colnames(mtop)=colmap[colwant];
     ## remove comment rows at bottom of file. first is line with notes='---'
    i.last1=which(mtop$notes=='---')[1];
    ## for sanity, make sure all deaths after i.last1 are NA
    bad=!is.na(mtop$deaths[i.last1:nrow(mtop)]);
    if (any(bad)) stop(file," has non-NA deaths after first comment line: ",nv(i=i.last1))
    ## trim comment rows and rows beyond ntop
    if (is.null(ntop)) ntop=Inf;
    mtop=head(mtop,min(i.last1-1,ntop));
    ## check content that remains and convert as needed
    mtop$per.capita[mtop$per.capita=='Unreliable']=NA;
    mtop[,cq(deaths,per.capita)]=capply(mtop[,cq(deaths,per.capita)],as.numeric);
    mtop$cause=gsub('^#| \\([^\\(\\)]*\\)$','',mtop$cause,perl=TRUE);
    bad=mtop$cause%-%cause$cause;
    if (length(bad)) stop("Bad news: these cause(s) in 'mtop' not in 'cause' table: ",
                          paste(collapse=', ',bad));
    mtop=merge(cause,mtop);
    mtop$cause=mtop$short;
    mtop=mtop[order(mtop$deaths,decreasing=TRUE),colmap%-%'notes'];
    invisible(mtop);
  }
## construct annual data from 'data_cvdat' result
## assumes cum with daily rows - what 'fit' produces
## TODO: should really be transform
ann=function(data,test.mono=TRUE) {
  dates=data$date;
  counts=data[,-1,drop=FALSE];
  if (unique(diff(dates))!=1) stop ("ann assumes data has daily rows");
  ## 'is.unsorted' tests monotonicity. from stackoverflow.com/questions/13093912. Thx!!
  if (test.mono&&any(capply(counts,is.unsorted))) stop ("ann assumes data is cumulative");
  ## do it.
  ## scale counts in first year (365 days)
  c0=head(counts,n=365);                # first year
  a=365/1:nrow(c0);                     # scale factor for first year
  y0=capply(c0,function(x) x*a);
  ## use annual intervals thereafter
  if (nrow(counts)>365) {
    i=366:nrow(counts);
    y1=counts[i,,drop=FALSE]-counts[i-365,,drop=FALSE];
    y=rbind(y0,y1);
  } else y=y0;
  invisible(cbind(date=dates,y));
}
## merge annualized COVID counts with mtop
## obj is usually doh.deaths.cum or jhu.deaths.cum
cv_mtop=function(obj,places=NULL,ages=NULL,mtop=param(mtop)) {
  if (is.null(mtop)) mtop=load_mtop();
  if (is.null(places)) {
    places=if(datasrc(obj)=='doh') 'state' else 'USA';
  } else { 
    if (datasrc(obj)=='doh') {
      bad=places%-%cq(state);
      if (length(bad))
        stop("Invalid place(s): ",paste(collapse=', ',bad),"; when  obj is 'doh', only valid place is 'state' (because don't have 'mtop' data for other places)");
    } else {
      bad=places%-%cq(state,USA);
      if (length(bad))
        stop("Invalid place(s): ",paste(collapse=', ',bad),";  when  obj is not 'doh', valid places are 'state', 'USA' (because don't have 'mtop' data for other places)");
    }}
  if (is.null(ages)) {
    ages=if(datasrc(obj)=='doh') ages(obj) else 'all';
  }
  names=if(datasrc(obj)=='doh') ages else places;
  mt=cv_mtop1(obj,places,ages,names,mtop);
  mt;
}
cv_mtop1=function(obj,places,ages,names,mtop) {
  ## counts
  deaths=data_cvdat(obj,places=places,ages=ages,cnm=names,per.capita=FALSE);
  per.capita=data_cvdat(obj,places=places,ages=ages,cnm=names,per.capita=TRUE);
  ## annualized counts
  deaths=ann(deaths,test.mono=FALSE);
  per.capita=ann(per.capita,test.mono=FALSE);
  ## current annualized counts
  deaths=tail(deaths,n=1)[1,-1,drop=FALSE];
  per.capita=tail(per.capita,n=1)[1,-1,drop=FALSE];
  ## merge COVID counts with mtop
  mt=lapply(names,function(name) {
    mt=if(name=='state') mtop[['all']] else mtop[[name]];
    mt=rbind(mt,data.frame(cause='**COVID**',deaths=deaths[1,name],per.capita=per.capita[1,name]));
        mt[,cq(deaths,per.capita)]=round(mt[,cq(deaths,per.capita)]);
    rownames(mt)=NULL;
    mt[order(mt$per.capita,decreasing=TRUE),];
      });
  names(mt)=names;
  mt;
}

  
