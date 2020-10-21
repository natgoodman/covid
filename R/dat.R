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
    stop(paste('No files found for',nv(what,datarsc,version,file,SEP=', ')));
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
  versions=unique(regmatches(files,regexpr('\\d\\d-\\d\\d-\\d\\d',files)));
  if (latest) tail(versions,n=1) else sort(versions);
}
latest_version=function(datasrc,what=NULL,dir=datadir(datasrc))
  list_versions(datasrc,what,dir,'latest')

### directory names that are data source specific
indir=function(datasrc) filename(param(indir),datasrc)
datadir=function(datasrc) filename(param(datadir),datasrc)

########## BELOW HERE NOT PORTED ##########

##### table - saved in tbldir
save_tbl=function(tbl,file,obj.ok=F) {
  param(save.tbl,save.txt.tbl);
  if (is.null(tbl)) stop('Trying to save NULL table. Is table name set correctly?');
  base=desuffix(file);
  file=filename(base=base,suffix='RData');
  if ((is.na(save.tbl)&!file.exists(file))|(!is.na(save.tbl)&save.tbl)) {
    save(tbl,file=file);
    if (save.txt.tbl) {
      file=filename(base=base,suffix='txt');
      if (length(dim(tbl))==2) write.table(tbl,file=file,sep='\t',quote=F,row.names=F)
      else if (is.list(tbl)) {
        sink(file);
        print(tbl);
        sink();
      }
      else if (is.vector(tbl)) {
        names=names(tbl);
        if (!is.null(names)) {
          tbl=data.frame(name=names,value=as.character(tbl));
          write.table(tbl,file=file,sep='\t',quote=F,row.names=F);
        } else writeLines(as.character(tbl),file);
      }
      else if (!obj.ok) stop('Trying to save generic object but obj.ok=F.');
    }}
  invisible(tbl);
}

## figure and table functions
filename_fig=function(figlabel,sect,figname,suffix='png')
  filename(param(figdir),paste(collapse='_',c('figure',figlabel,sect,figname)),suffix=suffix);
filename_tbl=function(tbllabel,sect,tblname,suffix='RData')
  filename(param(tbldir),paste(collapse='_',c('table',tbllabel,sect,tblname)),suffix=suffix);

