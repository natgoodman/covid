#################################################################################
##
## Author:  Nat Goodman
## Created: 20-11-14
##          from misig/doc.R created 19-01-10
##          from repwr/doc_resig.R created 18-06-19
##
## Copyright (C) 2019 Nat Goodman.
## 
## General functions to generate figures and tables for documents
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## --- Generate Figures and Tables for Document ---
## sect is which sections to run - for use during development
##   uses prefix matching and all matches run
## other params passed to init or init_doc. typically
##   subdoc - NULL or 'supp'. default: NULL
##   fignum, tblnum - initial figure, table number. default: 1
##   figpfx, tblpfx - prefix prepended to figure, table number, eg, 'S' for supp
##   figsfx, tblsfx - suffices appended to figure, table number in 'blocks' eg, a,b,c,...
##   save.out - save figures, tables to files. default: T
##   figscreen - plot figures on screen. default: T for readme, F otherwsie
##   fignew - plot each figure in new window. default figscreen
##   docfun document-specific function. default calculated from doc,subdoc eg, doc_repwr
dodoc=
  function(sect=NULL,need.init=F,xperiment=F,...) {
    if (!xperiment) {
      ## normal doc
      if (need.init) wrapfun(init,...);
      wrapfun(init_doc,...);
    } else {
      ## experimenal sandbox
      if (need.init) wrapfun(init_xperiment,...);
      wrapfun(init_doc_xperiment,...);
    }
    param(docfun);
    docfun(sect=sect);
  }

## --- Document Functions ---
## utility functions to make figures and tables for documents
## run plot function, label result with name
## figfun can be function call, or statement block. eval'ed in parent env
dofig=function(figname,figfun,sect=param(sect),pjto=param(pjto)) {
  if (!is.logical(pjto)) pjto=if(pjto=='file') TRUE else FALSE;
  parent.env=parent.frame(n=1);
  file=filename_fig(figlabel(where='filename'),sect,figname);
  ## png parameters found by trial and error. look reasonable
  ## in any case, same as in plon
  ## TODO: learn the right way to do this!
  png(filename=file,height=8,width=8,units='in',res=200,pointsize=12);
  dev=dev.cur();
  figfun=pryr::subs(figfun);
  eval(figfun,parent.env)               # draw the figure!
  dev.off(dev);                         # close device
  if (pjto) system(paste('pjto',file)); # copy to Mac if desired (usally is)
  figinc();                             # increment figure info for next time
  figname;
}
## generate and save one table.
## tblfun can be a function or call or statement block
##   if function, args from ... passed to it
##   else eval'ed as is
## tblfun can be a variable, function call, or statement block. eval'ed in parent env
dotbl=function(tblname,tblfun=NULL,sect=param(sect),pjto=param(pjto),obj.ok=TRUE) {
  if (!is.logical(pjto)) pjto=if(pjto=='file') TRUE else FALSE;
  parent.env=parent.frame(n=1);
  base=filename_tbl(tbllabel(where='filename'),sect,tblname,suffix=NULL);
  tblfun=pryr::subs(tblfun);
  tbl=eval(tblfun,parent.env);          # generate table!
  save_(tbl,base=base,save=param(save.RData.tbl),save.txt=param(save.txt.tbl),
        pjto=pjto,obj.ok=obj.ok)
  tblinc();                             # increment table info for next time
  tblname;
}

## construct figure title
## use CAP arg names to reduce conflicts with partial arg matching
figtitle=function(TEXT=NULL,...,SEP=' ') {
  dots=unlist(list(...));
  fig=if(param(figlabel)) paste(sep='','Figure ',figlabel()) else NULL;
  TEXT=paste(collapse=SEP,TEXT);
  if (!is.null(dots)) {
    dots=paste(collapse=', ',sapply(names(dots),function(name) paste(sep='=',name,dots[name])));
    TEXT=paste(collapse='. ',c(TEXT,dots));
  }
  paste(collapse="\n",c(fig,TEXT));
}
## construct figure label, eg, S1-2c
figlabel=function(extra=FALSE,where=cq(content,filename)) {
  if (!param(figlabel)) return('');
  where=match.arg(where);
  param(figextra,sectpfx,sectnum,figpfx,fignum,figsfx,figblk);
  if (figextra) {
    param(xfigpfx,xfigsfx);
    pfx=xfigpfx;
    figsfx=xfigsfx;
  } else {
    pfx=figpfx;
  }
  sfx=if(!is.null(figblk)) lblsfx(figblk,figsfx) else NULL;
  num=fignum;
  if (where=='filename') {
    num=sprintf('%03i',num);
    if (!is.null(sectnum)) sectnum=sprintf('%02i',sectnum);
  }
  pfx=if(sectpfx) paste(collapse='',c(pfx,sectnum)) else pfx;
  numsfx=paste(collapse='',c(num,sfx));
  if (where=='filename') paste(collapse='',c(pfx,numsfx)) else paste(collapse='-',c(pfx,numsfx));
}
## construct table label, eg, S1-2c
tbllabel=function(where=cq(content,filename)) {
  if (!param(tbllabel)) return('');
  where=match.arg(where);
  param(tblpfx,sectpfx,sectnum,tblnum,tblsfx,tblblk);
  if (where=='filename') {
    tblnum=sprintf('%03i',tblnum);
    sectnum=sprintf('%02i',sectnum);
  }
  pfx=if(sectpfx) paste(collapse='',c(tblpfx,sectnum)) else tblpfx;
  sfx=if(!is.null(tblblk)) lblsfx(tblblk,tblsfx) else NULL;
  paste(collapse='-',c(pfx,paste(collapse='',c(tblnum,sfx))));
}
## construct label suffix. handles big blk nums - arise in doc_ovrhtsupp
lblsfx=function(i,sfx) {
  base=length(sfx);
  if (i<base) return(sfx[i]);           # usual cases
  if (base==1) return(strrep(sfx,i));   # silly degenerate case
  out=NULL; 
  while(i>0) {
    ## loop essentially does decimal to base conversion
    i=i-1;                              # 'cuz R uses 1-offset indexing
    digit=i%%base;
    i=i%/%base;
    out=c(digit,out);
  }
  paste(collapse='',sfx[out+1]);
}
## manage figure,table numbers, blocks
sect_start=function(sect,sect.all) {
  ## compute section number. from stackoverflow.com/questions/5577727
  sectnum=which(sect==sect.all)[1];
  ## reset fignum if we're doing section-specific numbering else set to sectnum
  if (param(sectpfx)) fignum=1 else fignum=sectnum;
  ## each section starts new blocks
  param(sect=sect,sectnum=sectnum,fignum=fignum,figblk=NULL,tblblk=NULL,xfigblk=NULL);
}
figinc=function(extra=FALSE)
  if (extra) {
    param(xfigblk,xfignum);
    if (!is.null(xfigblk)) param(xfigblk=xfigblk+1) else param(xfignum=xfignum+1);
  } else {
    param(figblk,fignum);
    if (!is.null(figblk)) param(figblk=figblk+1) else param(fignum=fignum+1);
  }
figblk_start=function(extra=FALSE) {
  if (extra) return(xfigblk_start());
  param(figblk,fignum);
  ## if already in block, end it
  if (!is.null(figblk)) param(fignum=fignum+1);
  param(figblk=1);
}
figblk_end=function(extra=FALSE) {
  if (extra) {xfigblk_end(); return(); }
  param(figblk,fignum);
  ## do nothing if not in block, else end it
  if (!is.null(figblk)) param(figblk=NULL,fignum=fignum+1);
}
xfigblk_start=function() {
  ## param(xfigblk,xfignum);
  ## ## if already in block, end it
  ## if (!is.null(xfigblk)) param(xfignum=xfignum+1);
  param(xfigblk=1);
}
xfigblk_end=function() {
  param(xfigblk,xfignum);
  ## do nothing if not in block, else end it
  if (!is.null(xfigblk)) param(xfigblk=NULL,xfignum=xfignum+1);
}
tblinc=function() {
    param(tblblk,tblnum);
    if (!is.null(tblblk)) param(tblblk=tblblk+1) else param(tblnum=tblnum+1);
}
tblblk_start=function() {
  param(tblblk,tblnum);
  ## if already in block, end it
  if (!is.null(tblblk)) param(tblnum=tblnum+1);
  param(tblblk=1);
}
tblblk_end=function() {
  param(tblblk,tblnum);
  ## do nothing if not in block, else end it
  if (!is.null(tblblk)) param(tblblk=NULL,tblnum=tblnum+1);
}
outblk_start=function() {
  figblk_start(extra=FALSE);
  xfigblk_start();
  tblblk_start();
}
outblk_end=function() {
  figblk_end(extra=FALSE);
  xfigblk_end();
  tblblk_end();
}
##### Manage multiple plots per device (page)
## CAUTION: quick hack for TRN post!
##  doesn't worry about user plotting too many figures for the device!
##  sets cex=0.86 except when nrow=ncol=1 - correct for nrow<=2, ncol=2
##  doesn't scale properly when plotting to both screen and file expect for default nrow, ncol 
figdev_start=function(figdev.name=NULL,sect=NULL,nrow=2,ncol=2,...) {
  param(figdev,save.fig,figscreen,fignew);
  if (figdev) figdev_end();             # if already in figdev, end it
  file=filename_fig(figdev_label(),sect,figdev.name);
  param(fig.file=file);
  plot.to.file=((is.na(save.fig)&!file.exists(file))|(!is.na(save.fig)&save.fig));
  plot.to.screen=figscreen;           # for stylistic consistency
  ## NG 18-08-10: new scheme for plotting to file
  ##   plot to screen and file: dev.new here, dev.copy later
  ##   plot to screen only: dev.new here, no dev.copy later
  ##   plot to file only: png here, dev.off later
  ## plot.to.file only doesn't work if figfun returns multiple figures
  ##   trash multi-figure capability. we don't use it now
  if (!(plot.to.file||plot.to.screen)) {
    msg=paste(sep=' ',nv(figscreen),'and',nv(save.fig));
    if (is.na(save.fig)) msg=paste(sep=' ',msg,'and figure file',file,'exists');
    msg=paste(sep=' ',msg,'which means there is no where to plot the figure');
    stop(msg);
  }
  ## if (!missing(mfrow)&&!missing(mfcol)) stop("can only specify one of 'mfrow' or 'mfcol'");
  ## if (!missing(mfcol)) mfrow=NULL;
  ##   bg='white' needed else image copied with transparent bg; renders as grey
  if (plot.to.screen) {
    dev.new(bg='white');
    param(figdev.screen=dev.cur());
  }
  if (plot.to.file&!plot.to.screen) {
    ## png parameters found by trial and error. look reasonable
    ## TODO: learn the right way to do this!
    height=if(nrow==1&&ncol!=1) 4 else 8;
    png(filename=file,height=height,width=8,units='in',res=200,pointsize=12);
    param(figdev.file=dev.cur());
  }
  ## set mfrow and adjust cex unless nrow=ncol=1
  if (nrow!=1||ncol!=1) par(mfrow=c(nrow,ncol),cex=0.86);
  param(figdev=TRUE);
}
figdev_end=function() {
  param(figdev,save.fig,figscreen,fignew,fig.file,figdev.file,figdev.screen);
  if (!figdev) return();                # do nothing if not if not in figdev
  plot.to.file=((is.na(save.fig)&!file.exists(file))|(!is.na(save.fig)&save.fig));
  plot.to.screen=figscreen;           # for stylistic consistency
  if (plot.to.file&plot.to.screen) {
    ## png parameters found by trial and error. look reasonable
    ## TODO: learn the right way to do this!
    dev.png=dev.copy(png,filename=fig.file,height=8,width=8,units='in',res=200,pointsize=12);
    dev.off(dev.png);
  }
  ## always close plot.to.file device if exists
  if (plot.to.file&!is.na(figdev.file))  dev.off(figdev.file);
  ## close plot.to.screen device unless user wants each figure in new window
  if (plot.to.screen&&!fignew) dev.off(figdev.screen)
  else {
    ## reset 'figs per page' to default
    par(mfcol=c(1,1));
    par(mfcol=c(1,1));
  }
  figdev_inc();
  param(fig.file=NULL,figdev.file=NA,figdev.screen=NA);
}
figdev_inc=function() param(figdev.num=param(figdev.num)+1);

## construct figdev label for filename
figdev_label=function() {
  param(sectpfx,sectnum,figdev.pfx,figdev.num);
  pfx=figdev.pfx;
  num=figdev.num;
  num=sprintf('%03i',num);
  if (!is.null(sectnum)) sectnum=sprintf('%02i',sectnum);
  if(sectpfx) pfx=paste(collapse='',c(pfx,sectnum));
  paste(collapse='',c(pfx,num));
}

## figure and table filenames
filename_fig=function(figlbl,sect=NULL,figname,suffix='png') {
  base=paste(collapse='_',c(sect,figname));
  if (param(figlabel)) base=paste(collapse='_',c('figure',figlbl,base));
  filename(param(figdir),base,suffix=suffix);
}
filename_tbl=function(tbltbl,sect=NULL,tblname,suffix='RData') {
  base=paste(collapse='_',c(sect,tblname));
  if (param(tbllabel)) base=paste(collapse='_',c('table',tbltbl,base));
  filename(param(tbldir),base,suffix=suffix);
}
