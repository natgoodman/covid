#################################################################################
##
## Author:  Nat Goodman
## Created: 20-01-15
##          from misig/init.R created 19-01-01
##          from repwr/R/init.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Initialization code for frecl
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################

## ---- init ----
## initialization.
## process parameters and store in param environment.
## create output directories if necessary.
doc.all=cq(readme,hiddn);
init=function(
  ## doc parameters 
  doc='readme',                             # controls data, figure, table subdirs
  docx=match.arg(doc,doc.all),
  run.id=NULL,                              # to separate runs for tests
  ## data directories
  datadir=dirname('data',docx,run.id),      # data files
  figdir=dirname('figure',docx,run.id),     # figures. default eg, figure/hiddn
  tbldir=dirname('table',docx,run.id),      # tables. default eg, table/hiddn
  tmpdir=dirname(datadir,'tmp'),            # directory for inner loop sim files
  outdir=c(datadir,figdir,tbldir,tmpdir),   # output dirs doc needs
  indir='input',                            # input files not doc-specific
  
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
  clean=switch(docx,readme=T,F), # remove everything and start fresh
  clean.data=clean,              # remove datadir
  clean.sim=F,                   # clean simulations. default F
  clean.top=F,                   # clean top level data. default F
  clean.type=NULL,               # specific data types to clean. see clean_type
  clean.out=clean,               # remove outputs - figures and tables
  clean.fig=clean.out,           # remove figdir
  clean.tbl=clean.out,           # remove tbldir
                                 # 
  end=NULL                       # placeholder for last parameter
  ) {
  doc=docx;                      # to avoid confusion later
  ## source doc-specific files
  source_doc(doc);
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
  sapply(clean.type,clean_type);
  ## create output directories. nop if already exist
  sapply(outdir,function(dir) dir.create(dir,recursive=TRUE,showWarnings=FALSE));
  init_doc();
  invisible();
}
## initialize doc parameters - figure labels and such
## NG 19-01-11: abandon subdoc concept for 'supp' - not useful for effit
##              retain for xperiment just in case...
init_doc=function(
  ## output label modifiers
  outpfx=NULL,                  # prefix before figure or table number - NOT USED
  outsfx=letters,               # suffix in figure and table blocks
  sectpfx=F,                    # add section number to prefix eg, S1 - NOT USED
  sectnum=1,                    # section number. usually set in docs
  sect=NULL,
  ## figures
  figpfx=outpfx,
  figsfx=outsfx,
  fignum=1,
  figblk=NULL,                  # index into figsfx if in figure block
  ## for multiple plots per device (page) - CAUTION: works poorly!!
  figdev=FALSE,                 # TRUE when multiple plots per device active
  figdev.num=1,                 # device (page) number. to construct filenames
  figdev.pfx='dev',             # prefix for device filenames
  figdev.screen=NA,             # R dev number for plot to screen
  figdev.file=NA,               # R dev number for plot to file
  fig.file=NULL,                # figure filename - for plotting to screen and file
  ## tables
  tblpfx=outpfx,
  tblsfx=outsfx,
  tblnum=1,
  tblblk=NULL,                  # index into tblsfx if in table block
  ## xtra figures - not included in document
  xfigpfx='X',
  xfigsfx=outsfx,
  ## xfignum=1,                 # extras now use same numbers and blocks as regulars
  ## xfigblk=NULL,              # ditto
  ## plot control
  figscreen=if(param(doc)=='readme') T else !param(save.fig),
                                 # plot figures on screen
  fignew=figscreen,              # plot each figure in new window
  figextra=F,                    # plot extra figures
  ## doc generation function
  docfun=get(paste(collapse='',c('doc_',param(doc)))),
  docsect=NULL,                  # all document sections. set by docfun
  end=NULL                       # placeholder for last parameter
  ) {
  ## assign parameters to param environment
  ## do it before calling any functions that rely on params
  assign_param();
  invisible();
}

## clean specific data type. deletes directory, and any top level files
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

