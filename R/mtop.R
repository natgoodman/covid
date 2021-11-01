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
  ## replace 'all' by 'state'
  names(mtop)[names(mtop)=='all']='state';
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
#####
## TODO: decide whether ann_cvdat, mort_ann belong here or in doc_mtop
## TODO: use these functions in doc_mtop
## combine data_cvdat and ann. ... passed to data_cvdat
ann_cvdat=function(obj,...) {
  data=data_cvdat(obj,...);
  ann(data,test.mono=FALSE); # test.mono=FALSE 'cuz data sometimes overshoots and backs up
}
## scale ann data by mort
mort_ann=function(ann,obj=NULL,mort=NULL) {
  if (is.null(mort)) {
    ## get mort from obj
    if (is.null(obj)) stop("'obj' and 'mort' cannot both be NULL");
    mort=obj$mort;
  }
  ## mort can be a vector or data frame with 1 row or col
  if (!is.null(dim(mort))) {
    mort=if (nrow(mort)==1) as.numeric(mort[1,colnames(ann)[-1]])
         else if (ncol(mort==1)) as.numeric(mort[colnames(ann)[-1],1])
         else stop("'mort' must be vector or object with 1 row or 1 col: dim(mort)=",
                   paste(collapse=', ',dim(mort)));
  }
  data.frame(date=ann$date,rapply(ann[,-1],function(counts) counts/mort));
}
## combine data_cvdat, ann, mort_ann. ... passed to data_cvdat
mortann_cvdat=function(obj,...) {
  ann=ann_cvdat(obj,...);
  mort=mort_ann(ann,obj);
}
#####

## merge annualized COVID counts with mtop
## objs is usually list of doh.deaths.cum, jhu.deaths.cum
## do.percap=T means calculate per.capita here
##           F means use per.capita from files even if NA
##           NA means calculate per.capita if NA, else use from files
cv_mtop=function(objs,do.percap=NA,mtop=param(mtop)) {
  if (is_cvdat(objs)) objs=list(objs);
  ## merge doh, jhu pops into column matrix. rownames are mt names
  pop=do.call(rbind,lapply(objs,function(obj) {
    pop=if (datasrc(obj)=='doh') obj$pop else t(obj$pop);
    colnames(pop)='pop';
    pop;
  }))
  if (is.null(mtop)) mtop=load_mtop();
  mt=do.call(c,lapply(objs,function(obj) cv_mtop1(obj,mtop,pop,do.percap)))
  mt;
}
cv_mtop1=function(obj,mtop,pop,do.percap=NA) {
  places=if(datasrc(obj)=='doh') 'state' else cq(USA,state);
  ages=if(datasrc(obj)=='doh') ages(obj) else 'all'
  names=if(datasrc(obj)=='doh') ages else places;
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
    mt=mtop[[name]];
    mt=rbind(mt,data.frame(cause='COVID',deaths=deaths[1,name],per.capita=per.capita[1,name]));
    mt[,cq(deaths,per.capita)]=round(mt[,cq(deaths,per.capita)]);
    percap=round(1e6*mt$deaths/pop[name,1]);
    if (is.na(do.percap)) mt$per.capita=ifelse(is.na(mt$per.capita),percap,mt$per.capita)
    else if (do.percap) mt$per.capita=percap;
    rownames(mt)=NULL;
    mt[order(mt$per.capita,decreasing=TRUE),];
  });
  names(mt)=names;
  mt;
}

  
