#################################################################################
##
## Author:  Nat Goodman
## Created: 20-01-17
##          from misg/datman.R created 19-01-01
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
## ---- Read Free Cell input files ----
## input formats vary a bit, so need separate code
read_frecl=function(what=1,file=NULL) {
  library(readr);
  if (is.null(file)) file=filename(param(indir),paste0('free_cell_',what,'.csv'))
  else what=gsub('^.*free_cell_|\\.csv','',file);
  frecl=read.csv(file,stringsAsFactors=F);
  colnames(frecl)=tolower(colnames(frecl));
  cols=cq(score,rank,total);
  for (col in cols) frecl[,col]=parse_number(frecl[,col]);
  if (what %in% cq(4,5)) {
    ## fill blank DateTime fields with value above
    for (i in 2:nrow(frecl))
      if (is_blank(frecl[i,'datetime'])) frecl[i,'datetime']=frecl[i-1,'datetime'];
    ## convert datetime to date
    frecl[,'date']=parse_date(frecl[,'datetime'],"%m/%d/%Y %H:%M %p");
    cols=c('date',cols);
  }
  frecl[,cols];
}


## ---- Save and Load ----
## save data in RData and optionally txt formats
save_=function(data,file,save,save.txt=F) {
  if ((is.na(save)&!file.exists(file))|(!is.na(save)&save)) {
    base=desuffix(file);
    save(data,file=filename(base=base,suffix='RData'));
    if (save.txt)
      write.table(data,file=filename(base=base,suffix='txt'),sep='\t',quote=F,row.names=F);
  }
}
## load data from RData file
load_=function(file,what) {
  base=desuffix(file);
  what=load(file=filename(base=base,suffix='RData')); # what is name of saved data
  get(what);                          # return it
}
##### top-level data saved in datadir
## base does not include path
save_data=function(what,file=NULL,data=NULL,base=NULL) {
  param(datadir,save.data,save.txt.data);
  what=as.character(pryr::subs(what));
  if (missing(data) && exists(what,envir=parent.frame(n=1)))
    data=get(what,envir=parent.frame(n=1));
  if (is.null(data)) stop('Trying to save NULL object. Is "what" set correctly?');
  if (is.null(file)) {
    if (is.null(base)) base=basename_data(what) else base=basename_data(base);
  } else base=desuffix(file);
  file=filename(base=base,suffix='RData');
  save_(data,file=file,save=save.data,save.txt=save.txt.data);
}
## load data from file
load_data=function(...,file=NULL,base=NULL,list=character()) {
  dots=match.call(expand.dots=FALSE)$...;
  if (length(dots) &&
      !all(vapply(dots,function(x) is.atomic(x)||is.symbol(x)||is.character(x),
                  NA,USE.NAMES=FALSE))) 
    stop("... must contain atomic data like names or character strings");
  parent.env=parent.frame(n=1);
  names=vapply(dots,as.character,"");
  if (length(names)==0L) names=character();
  names=c(list,names);
  if (length(names)==0) {
    ## easy case - load and return one file
    if (is.null(file)) stop('Cannot load data unless file or what is set');
    return(load_(file=file));
  }
  val=lapply(1:length(names),function(i) {
    what=names[i];
    if (length(file)>=i) file=file[i] else file=filename_data(what);
    val=load_(file=file);
    assign(what,val,envir=parent.env);
    val;
  });
  if (length(names)==1) return(val[[1]]) else names;
}
get_data=load_data;
##### sim file functions
### save, get, other manipulation
##
## save regular sim file
save_sim=function(sim,...,what,file=NULL) {
  ## what=as.character(pryr::subs(what));
  param(save.sim,save.txt.sim);
  if (is.null(file)) {
    tail=nvq_file(DOTS=match.call(expand.dots=FALSE)$...);
    file=filename_sim(what,tail=tail);
  }
  save_(sim,file,save=save.sim,save.txt=save.txt.sim);
}
## save inner loop files
save_sim_tmp=function(sim,...,file=NULL) {
  if (is.null(file)) {
    tail=nvq_file(DOTS=match.call(expand.dots=FALSE)$...);
    file=filename_sim_tmp(tail=tail);
  }
  save(sim,file=file);
}
## load (aka get) regular sim file
load_sim=get_sim=function(...,what,file=NULL) {
  if (is.null(file)) {
    tail=nvq_file(DOTS=match.call(expand.dots=FALSE)$...);
    file=filename_sim(what,tail=tail);
  }
  load_(file,'sim');
}
## consolidate inner loop sim files. file is name of output (permanent) sim file
cat_sim=function(...,what,file=NULL) {
  if (is.null(file)) {
    tail=nvq_file(DOTS=match.call(expand.dots=FALSE)$...);
    file=filename_sim(what,tail=tail);
  }
  pattern=sub('^sim\\.','^sim\\.i=[0-9]+\\.',basename(file));
  files=list.files(param(tmpdir),full.names=T,pattern=pattern);
  sim=do.call(rbind,lapply(files,function(file) load_(file,'sim')));
  save_sim(sim,file=file);
  file.remove(files);
  invisible(sim);
}
### filenames
filename_sim=function(what,...,tail=NULL) {
  if (is.null(tail)) tail=nvq_file(DOTS=match.call(expand.dots=FALSE)$...);
  ## DOTS=c(match.call(expand.dots=FALSE)$...,unlist(DOTS));
  dir=param(list=paste(sep='.','sim',what,'dir'));
  filename(dir,basename_sim(tail=tail));
}
filename_sim_tmp=function(...,tail=NULL) {
  if (is.null(tail)) tail=nvq_file(DOTS=match.call(expand.dots=FALSE)$...);
  filename(param(tmpdir),basename_sim(tail=tail));
}
basename_sim=function(...,tail=NULL) {
  base='sim';
  if (is.null(tail)) tail=nvq_file(DOTS=match.call(expand.dots=FALSE)$...);
  filename(base=base,tail=tail,suffix='RData');
}
## load (aka get) specific sim file type. wrappers for load_sim.
## TODO: replace by generic load_sim
load_sim_rand=get_sim_rand=function(file=NULL,n) load_sim(n,what='rand',file=file);
load_sim_fixd=get_sim_fixd=function(file=NULL,n,d) load_sim(n,d,what='fixd',file=file);
load_sim_hetd=get_sim_hetd=function(file=NULL,n,d,sd) load_sim(n,d,sd,what='hetd',file=file);

##### interp_hetd. interp is approx data, NOT function, 'cuz it's dangerour to save approxfun
save_interp_hetd=function(interp,n,d,sd,file=NULL) {
  param(save.interp,save.txt.interp);
  if (is.null(file))
    file=filename(param(interp.hetd.dir),base='interp',
                  tail=paste(sep=',',paste_nv(n),paste_nv(d,d_pretty(d)),sd_pretty(sd)));
  save_(interp,file,save=save.interp,save.txt=save.txt.interp);
}
load_interp_hetd=get_interp_hetd=function(file=NULL,n,d,sd) {
  if (is.null(file))
    file=filename(param(interp.hetd.dir),base='interp',
                  tail=paste(sep=',',paste_nv(n),paste_nv(d,d_pretty(d)),sd_pretty(sd)));
  interp=load_(file,'interp');
  approxfun(interp)
}
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

##### data - arbitrary objects saved in datadir
filename_data=function(what,suffix='RData')
  filename(basename_data(what),suffix=suffix);
basename_data=function(what) filename(param(datadir),base=what)

## figure and table functions
filename_fig=function(figlabel,sect,figname,suffix='png')
  filename(param(figdir),paste(collapse='_',c('figure',figlabel,sect,figname)),suffix=suffix);
filename_tbl=function(tbllabel,sect,tblname,suffix='RData')
  filename(param(tbldir),paste(collapse='_',c('table',tbllabel,sect,tblname)),suffix=suffix);

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
    suffix.pattern=paste(collapse='|',paste(sep='','\\.',suffix,'$'));
    file=ifelse(grepl(suffix.pattern,file),file,paste(sep='.',file,suffix[1]));
  }
  file;
}
## remove suffix from filename
desuffix=function(file,suffix=c('RData','txt')) {
  if (!is.null(suffix)) {
    ## remove leading '.' if present
    suffix=sub('^\\.','',suffix,perl=T);
    suffix.pattern=paste(collapse='|',paste(sep='','\\.',suffix,'$'));
    file=sub(suffix.pattern,'',file);
  }
  file;
}
## filebasename same as filename but w/o suffix
filebasename=function(...) filename(...,suffix=NULL)
## construct directory pathname. synonym for filebasename
dirname=filebasename;

