#################################################################################
##
## Author:  Nat Goodman
## Created: 20-05-01
##          from frecl/R/init.R created 20-01-15
##          from misig/R/init.R created 19-01-01
##          from repwr/R/init.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Initialization code for covid
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################

## ---- init ----
## initialization.
## process parameters and store in param environment.
## create output directories if necessary.
## no docs at present
## inputs and data are source specific, figures and tables global
## doc.all=cq(readme,covid);
init=function(
  ## datasrc=cq(doh,ihme,jhu,tracking,c19pro),
  datasrc=cq(doh,ihme,jhu,nyt,trk,yyg),
  ## doc parameters 
  ## doc=NULL,                             # controls data, figure, table subdirs
  ## docx=match.arg(doc,doc.all),
  ## docx=NULL,
  run.id=NULL,                              # to separate runs for tests
  ## data directories. 
  indir='input',                            # top level input dir
  datadir=filename('data',run.id),          # top level output data files
  figdir=filename('figure',run.id),         # figures
  tbldir=filename('table',run.id),          # tables
  tmpdir=filename(datadir,'tmp'),           # tmp dir if needed
  # outdir=c(datadir,figdir,tbldir,tmpdir),  # top level output dirs
  
  ## program control
  verbose=F,                     # print progress messages
  debug=FALSE,                   # call debug code
  must.exist=F,                  # must all sub-inits succeed?
  save=NA,                       # shorthand for other save params 
                                 #   NA means save unless file exists
                                 #   T, F mean always or never save
  save.data=save,                # save top level data
  save.txt=NA,                   # save results in txt format as well as RData
                                 #   NA means use default rule for type:
                                 #   F for all but top level data
  save.txt.data=is.na(save.txt)|save.txt, # save txt top level results. default T
  save.out=T,                    # save outputs - figures and tables - when called via dofig
  save.fig=save.out,             # save figures (when called via dofig)
  save.tbl=save.out,             # save tables (when called via dotbl)
  save.txt.tbl=T,                # save txt tables. default T
                                 #    
  ## clean=switch(docx,readme=T,F), # remove everything and start fresh
  clean=F,                       # remove everything and start fresh
  clean.data=clean,              # remove datadir
  clean.sim=F,                   # clean simulations. default F
  clean.top=F,                   # clean top level data. default F
  clean.type=NULL,               # specific data types to clean. see clean_type
  clean.out=clean,               # remove outputs - figures and tables
  clean.fig=clean.out,           # remove figdir
  clean.tbl=clean.out,           # remove tbldir
                                 # 
  ## import params for specific data sources
  doh.notKing=TRUE,              # compute state minus King
  jhu.notKing=TRUE,              # compute state minus King
  nyt.notKing=TRUE,              # compute state minus King
  ihme.maxdate='latest',         # max date is latest version
  yyg.maxdate='latest',          # max date is latest version
                                 #
  end=NULL                       # placeholder for last parameter
  ) {
  ## doc=docx;                      # to avoid confusion later
  ## source doc-specific files
  ## source_doc(doc);
  ## inputs and data are source specific, figures and tables global
  ## input dirs
  indirs=sapply(datasrc,function(src) filename(indir,src));
  ## output dirs
  datadirs=sapply(datasrc,function(src) filename(datadir,src));
  outdirs=c(datadirs,figdir,tbldir,tmpdir)
  ## assign parameters to param environment
  ## do it before calling any functions that rely on params
  init_param();
  ## clean and create directories as needed
  if (clean.data) unlink(datadir,recursive=TRUE)
  else {
    if (clean.top) {
      ## adapted from stackoverflow.com/questions/22069095. Thx!
      paths=list.files(datadir,full.names=TRUE);
      unlink(paths[!file.info(paths)$isdir]);
    }}
  if (clean.fig) unlink(figdir,recursive=T);
  if (clean.tbl) unlink(tbldir,recursive=T);
  ## clean specific types if desired.
  ## TODO: probably not right/useful here
  sapply(clean.type,clean_type);
  ## create input directories. nop if already exist
  sapply(indirs,function(dir) dir.create(dir,recursive=TRUE,showWarnings=FALSE));
  ## create output directories. nop if already exist
  sapply(outdirs,function(dir) dir.create(dir,recursive=TRUE,showWarnings=FALSE));
  ## init_doc();
  invisible();
}

## clean specific data type. deletes directory, and any top level files
## TODO: probably not right/useful here
clean_type=function(what,cleandir=T) {
  param(datadir);
  ## delete top level files if exist
  files=list.files(datadir,full.names=T,pattern=paste(sep='','^',what,'\\.(RData|txt)'));
  unlink(files);
  if (cleandir) {
    whatdir=paste(sep='',what,'dir');
    ## delete directory if exists
    if (exists(whatdir,envir=param.env)) unlink(get(whatdir,envir=param.env),recursive=T);
  }
}
cleanq=function(what,cleandir=T) {
  what=as.character(pryr::subs(what));
  clean_type(what,cleandir);
}

