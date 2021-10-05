#################################################################################
##
## Author:  Nat Goodman
## Created: created 20-05-02
##          from frecl/R/dat.R created 20-01-17
##          from misg/R/datman.R created 19-01-01
##          from repwr/R/datman.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## General data management functions
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Save and Load ----
## save data for data source
## version used as base part of filename unless file is set
## file, if set, includes path and supercedes version
save_data=function(what,datasrc,version,file=NULL,suffix=NULL,data=NULL) {
  param(save.data,save.txt.data);
  if (is.null(data)) {
    what=as.character(pryr::subs(what));
    if (!exists(what,envir=parent.frame(n=1))) stop(paste("Object",what,"does not exist"));
    data=get(what,envir=parent.frame(n=1));
    if (is.null(data)) stop('Trying to save NULL object. Is "what" set correctly?');
  }
  base=basename_data(what,datasrc,version,file,suffix);
  save_(data,base=base,save=save.data,save.txt=save.txt.data);
}
## load data for data source
## if version specified, load that version only, else do all
## latest=TRUE means use latest version
## version='latest' also works
## file, if set, includes path and supercedes version. can be directory or single file
load_data=function(what,datasrc,version='latest',file=NULL,whatv=NULL,SIMPLIFY=TRUE) {
  what=if(is.null(whatv)) as.character(pryr::subs(what)) else whatv;
  if (!is.null(file)) {
    if (!file.exists(file)) stop(paste('File',file,'does not exist'))
    files=if(file.info(file)$isdir) list.files(file,full.names=TRUE) else file;
  } else {
    ## code copied to import. TODO: refactor!
    if (!is.null(version)&&version=='latest') version=latest_version(datasrc,what);
    pattern=paste0('^',what,'.',version);
    files=list.files(datadir(datasrc),pattern=pattern,full.names=TRUE);
  }
  if (length(files)==0)
    stop(paste('No files found for',nv(what,datasrc,version,file,SEP=', ')));
  files=sort(decreasing=TRUE,
             unique(resuffix(files,old.suffix=cq(RData,txt),suffix='RData',keep.dir=TRUE)));
  data=lapply(files,function(file) load_(file=file));
  ## if just one file and SIMPLIFY is TRUE. return its data
  if (SIMPLIFY&&length(data)==1) data=data[[1]]
  else names(data)=desuffix(files,suffix='RData',keep.dir=FALSE);
  invisible(data)
}
get_data=load_data;

basename_data=function(what,datasrc,version,file=NULL,suffix=NULL) {
  if (!is.null(file)) desuffix(file,suffix,keep.dir=TRUE)
  else {
    base=paste(sep='.',what,version);
    filename(datadir(datasrc),base);
  }
}
### list available versions for source in datadir
list_versions=function(datasrc,what=NULL,dir=datadir(datasrc),version=NULL) {
  if (!is.null(what)) what=paste0('^',what);
  if (is.function(dir)) dir=dir(datasrc);
  latest=!is.null(version)&&version=='latest';
  if (latest) version=NULL;
  pattern=paste(collapse='.',c(what,version));
  files=list.files(dir,pattern=pattern,full.names=FALSE);
  versions=sort(unique(regmatches(files,regexpr('\\d\\d-\\d\\d-\\d\\d',files))));
  if (latest) tail(versions,n=1) else sort(versions);
}
latest_version=function(datasrc,what=NULL,dir=datadir(datasrc))
  list_versions(datasrc,what,dir,'latest')

### directory names that are data source specific
indir=function(datasrc) filename(param(indir),datasrc)
datadir=function(datasrc) filename(param(datadir),datasrc)

## ---- Save and Load population metadata ----
## base includes path. suffix optional
## pop - population by place, age - for per capita calculations
save_pop=function(pop,base=param(pop.file)) {
  param(save.meta,save.txt.meta);
  save_(pop,base=base,save=save.meta,save.txt=FALSE);
  if (save.txt.meta) {
    ## want explicit age column in text file
    ## NG 21-04-12: use 'cbind' instead of 'data.frame' so R won't munge column names. sigh...
    pop=cbind(age=rownames(pop),pop);
    file=resuffix(base,old.suffix=cq(txt,RData),suffix='txt');
    write.table(pop,file=file,sep='\t',quote=F,row.names=F);
  }
}
load_pop=function(base=param(pop.file)) {
  pop=load_(base=base);
  param(pop=pop);
}
read_pop=function(base=param(pop.file)) {
  pop=read_(base=base,row.names='age');
  param(pop=pop);
}
## geo - geoids and place names - eg, for places_wa, places_nonwa functions
save_geo=function(geo,base=param(geo.file))
  save_(geo,base=base,save=param(save.meta),save.txt=param(save.txt.meta))
load_geo=function(base=param(geo.file)) {
  geo=load_(base=base);
  param(geo=geo);
}
read_geo=function(base=param(geo.file)) {
  geo=read_(base=base);
  param(geo=geo);
}
## stateid - map state names to IDs (eg, Washington to WA)
save_stateid=function(stateid,base=param(stateid.file))
  save_(stateid,base=base,save=param(save.meta),save.txt=param(save.txt.meta))
load_stateid=function(base=param(stateid.file)) {
  stateid=load_(base=base);
  param(stateid=stateid);
}
read_stateid=function(base=param(stateid.file)) {
  stateid=read_(base=base);
  param(stateid=stateid);
}
## ---- Save and Load mortality data ----
## base includes path. suffix optional
## deaths, pop - deaths, population by place, age - for per capita calculations
save_mort=function(data,base=param(mort.file)) save_pop(data,base=base);
save_mortpop=function(data,base=param(mortpop.file)) save_pop(data,base=base)
  
load_mort=function(base=param(mort.file)) {
  mort=load_(base=base);
  param(mort=mort);
}
read_mort=function(base=param(mort.file)) {
  mort=read_(base=base,row.names='age');
  param(mort=mort);
}
load_mortpop=function(base=param(mortpop.file)) {
  mortpop=load_(base=base);
  param(mortpop=mortpop);
}
read_mortpop=function(base=param(mortpop.file)) {
  mortpop=read_(base=base,row.names='age');
  param(mortpop=mortpop);
}
## ---- Save and Load leading cause of death (mtop) data ----
## mtop is list. names are ages
save_mtop=function(mtop,base=param(mtop.file)) {
  param(save.meta,save.txt.meta);
  save_(mtop,base=base,save=save.meta,save.txt=FALSE);
  if (save.txt.meta) {
    ## flatten list into table with explicit age, place columns
    mtop=do.call(rbind,lapply(names(mtop),function(name) {
      mt=mtop[[name]];
      mt=if(name=='USA') cbind(age='all',place='USA',mt) else cbind(age=name,place='state',mt);
    }));
    file=resuffix(base,old.suffix=cq(txt,RData),suffix='txt');
    write.table(mtop,file=file,sep='\t',quote=F,row.names=F);
  }
}
load_mtop=function(base=param(mtop.file)) {
  mtop=load_(base=base);
  param(mtop=mtop);
}
## CAUTION: this one returns data frame not list. dunno if that's a problem
read_mtop=function(base=param(mtop.file)) {
  mtop=read_(base=base,row.names='age');
  ## param(mtop=mtop);
}


