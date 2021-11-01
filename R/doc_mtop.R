#################################################################################
##
## Author:  Nat Goodman
## Created: 21-10-18
##          from doc_updat.R created 20-11-15
##          from misig/doc_confi.R created 19-07-16
##          from misig/confi.R created 19-07-04
##
## Copyright (C) 2019-2021 Nat Goodman.
## 
## Generate figures and tables for mtop (leading causes of death) document
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
library(knitr);
## --- Generate Figures and Tables for mtop Blog Post ---
## no sections
## TODO: mostly NOT YET PORTED
## TODO: version should be fixed to whatever version I used in document
doc_mtop=function(doc='mtop',need.objs=TRUE,need.init=TRUE,version='latest',
                  rawfun=cq(raw,raw_mtop),do.fig=TRUE,do.tbl=TRUE,pal.jhu='d3',...) {
  what='deaths';
  datasrc=cq(doh,jhu);
  if (is.null(version)||version=='latest') version=max(sapply(datasrc,latest_version));
  if (param(verbose)) print(paste('+++ doc_mtop',nv(version)));
  if (need.objs) make_mtop_objs(rawfun=rawfun,what=what,datasrc=datasrc,version=version);
  if (need.init) init_doc(doc=doc,version=version,...);
  places.jhu<<-cq(USA,state);
  labels.jhu<<-setNames(c('USA','Washington State'),places.jhu);
  titles.jhu<<-setNames(c('USA','WA'),places.jhu);
  col.jhu<<-col_brew(places.jhu,pal.jhu);
  ages.doh<<-ages(doh);
  labels.doh<<-age_label(ages.doh,fmt='legend');
  col.doh<<-col_ages(doh,skip.beg=3);      # col_ages in doc_updat.R
 
  if (do.tbl) {
    if (param(verbose)) print(paste('+++ making tables'));
    mt.jhu=cv_mtop(jhu);
    mt.doh=cv_mtop(doh);
    mtop=cv_mtop(list(jhu,doh));
    ## Table 1 mort percentage
    jhu.mort=mortann_cvdat(jhu,places=places.jhu,per.capita=FALSE);
    doh.mort=mortann_cvdat(doh,ages=ages.doh,per.capita=FALSE);
    pct=c(as.numeric(jhu.mort[nrow(jhu.mort),-1,drop=T]),
          as.numeric(doh.mort[nrow(doh.mort),-1,drop=T]));
    pct=paste0(100*round(pct,digits=2),'%');
    mort.pct=data.frame(c('USA all ages','WA all ages',
                          sapply(ages.doh,function(age) paste('WA',age_label(age,'legend')))),
                        pct);
    colnames(mort.pct)=c('Location/Age','Pct of Expected Deaths');
    dotbl('mort_pct',mort.pct,
          title=tbltitle('COVID deaths relative to expected non-COVID deaths'),
          align=cq(l,r));    
    
 ########################################
    ## TODO: mort percentage (table and/or barchart as decided)
    ## jhu.ann=ann_cvdat(jhu,places=places.jhu,per.capita=FALSE);
    ## mort=as.numeric(jhu$mort[,colnames(jhu.ann)[-1]]);
    ## jhu.mort<<-data.frame(date=jhu.ann$date,rapply(jhu.ann[,-1],function(counts) counts/mort));
    ## doh.ann=ann_cvdat(doh,ages=ages.doh,per.capita=FALSE);
    ## mort=as.numeric(doh$mort[colnames(doh.ann)[-1],]);
    ## doh.mort<<-data.frame(date=doh.ann$date,rapply(doh.ann[,-1],function(counts) counts/mort));
    ## mort percentage table
    ## mort.pct=c(as.numeric(jhu.mort[nrow(jhu.mort),-1,drop=T]),
    ##            as.numeric(doh.mort[nrow(doh.mort),-1,drop=T]));
    ## mort.pct=paste0(round(mort.pct,digits=2),'%');
    ## mort.df=data.frame(c('USA all ages','WA all ages',
    ##                      sapply(doh.ages,function(age) age_label(age,none))),
    ##                    mort.pct);
    ## barchart TBD
    ########################################
   ## Tables 2a-b mtop tables for USA, state
    tblblk_start();
    sapply(places.jhu,function(place) {
      mt=mtop[[place]];
      tblname=paste0('jhu_',lc(place));
      mtkbl(tblname,mt,title=tbltitle(paste(titles.jhu[[place]],'all ages')));
    });
    ## Tables 3a-f mtop tables for usual ages from doh 
    tblblk_start();
    sapply(ages.doh,function(age) {
      mt=mtop[[age]];
      tblname=paste0('doh_',age);
      mtkbl(tblname,mt,title=tbltitle(paste('WA',age_label(age,fmt='legend'))));
    });
  }
  if (do.fig) { 
    if (param(verbose)) print(paste('+++ making figures'));
    ## Figures 1a-b cum
    figblk_start();
    dofig('jhu_cum',
          plot_cvdat(
            jhu,places=places.jhu,ages='all',per.capita=TRUE,lwd=2,
            title=figtitle("Cumulative COVID deaths per million in USA and Washington"),
            ylab="cumulative deaths per million",
            legends=list(labels=labels.jhu)));
     dofig('doh_cum',
          plot_cvdat(
            doh,ages=ages.doh,per.capita=TRUE,lwd=2,col=col.doh,
            title=figtitle("Cumulative COVID deaths per million in Washington by age"),
            ylab="cumulative deaths per million",
            legends=list(labels=labels.doh)));
    ## Figures 2a-b ann
    figblk_start();
    jhu.data=data_cvdat(jhu,places=places.jhu,per.capita=TRUE);
    jhu.ann=ann(jhu.data,test.mono=FALSE); # test.mono=FALSE 'cuz jhu overshoots then backs up
    dofig('jhu_ann',
          plotm(
            jhu.ann,lwd=2,lty='solid',
            title=figtitle("Annualized COVID deaths per million in USA and Washington"),
            ylab="annualized deaths per million",          
            legend='topleft',legend.title='Place',legend.labels=labels.jhu));
    doh.data=data_cvdat(doh,ages=ages.doh,per.capita=TRUE);
    doh.ann=ann(doh.data,test.mono=FALSE); # test.mono=FALSE 'cuz fit produces small decreases
    dofig('doh_ann',
          plotm(
            doh.ann,lwd=2,lty='solid',col=col.doh,
            title=figtitle("Annualized COVID deaths per million in Washington by age"),
            ylab="annualized deaths per million",          
            legend='topright',legend.title='Age',legend.labels=labels.doh));
    ## Figures 3a-b ann relative to mort
    figblk_start();
    jhu.data=data_cvdat(jhu,places=places.jhu,per.capita=FALSE);
    jhu.ann=ann(jhu.data,test.mono=FALSE); # test.mono=FALSE 'cuz jhu overshoots then backs up
    mort=as.numeric(jhu$mort[,colnames(jhu.ann)[-1]]);
    jhu.mort=data.frame(date=jhu.ann$date,rapply(jhu.ann[,-1],function(counts) counts/mort));
    dofig('jhu_mort',
          plotm(
            jhu.mort,lwd=2,lty='solid',
            title=figtitle("Annualized COVID relative to non-COVID deaths in USA and Washington"),
            ylab="annualized relative deaths",          
            legend='topleft',legend.title='Place',legend.labels=labels.jhu));
    doh.data=data_cvdat(doh,ages=ages.doh,per.capita=FALSE);
    doh.ann=ann(doh.data,test.mono=FALSE); # test.mono=FALSE 'cuz fit produces small decreases
    mort=as.numeric(doh$mort[colnames(doh.ann)[-1],]);
    doh.mort=data.frame(date=doh.ann$date,rapply(doh.ann[,-1],function(counts) counts/mort));
    dofig('doh_mort',
          plotm(
            doh.mort,lwd=2,lty='solid',col=col.doh,
            title=figtitle("Annualized COVID relative to non-COVID deaths in Washington by age"),
            ylab="annualized relative deaths",          
            legend='top',legend.title='Age',legend.labels=labels.doh));
   
  }
}

## replicates much of 'dotbl'. refactor someday...
mtkbl=
  function(tblname,mt,title=NULL,sect=param(sect),
           save.kbl=param(save.kbl),save.RData=param(save.RData.tbl),save.txt=param(save.txt.tbl),
           pjto=param(pjto)) {
    if (!is.logical(pjto)) pjto=if(pjto=='file') TRUE else FALSE;
    base=filename_tbl(tbllabel(where='filename'),sect,tblname,suffix=NULL);
    ## save mt as table (RData, txt)
    save_(mt,base=base,save.RData=save.RData,save.txt=save.txt,pjto=pjto);
    ## create and save kable
    if (save.kbl) {
      ## format table as needed for doc
      mt$cause=with(mt,ifelse(cause=='COVID',paste0('**',cause,'**'),cause));
      mt=mt[,cq(cause,per.capita)];
      colnames(mt)=c('Cause','Deaths per Million');
      kt=kable(mt,caption=title,row.names=FALSE,escape=T);
      save_(kt,file=filename(base,suffix='kbl'),save=TRUE,save.txt=FALSE,pjto=pjto);
    }
    tblinc();                             # increment table info for next time
    tblname;
  }
  
## make objs doc_mtop needs. deaths.cum
make_mtop_objs=
  function(rawfun=cq(raw,raw_mtop),what='deaths',datasrc=cq(doh,jhu),version='latest') {
    rawfun=get(match.arg(rawfun));
    datasrc=match.arg(datasrc,several.ok=TRUE);
    sapply(datasrc,function(datasrc) {
      if (param(verbose)) print(paste('+++ making',datasrc,what));
      obj=rawfun(what,datasrc,version);
      assign(paste(sep='.',datasrc,'src'),obj,globalenv());  # save as 'src'
      ## with raw_mtop, doh already limited
      obj=switch(datasrc,               # transform as needed for datasrc
                 doh={
                   obj=edit(obj,KEEP='state'); # NOTE: with raw_mtop, doh already just 'state'
                   obj=extra(obj,incompatible.ok=TRUE);
                   obj=edit(obj,DROP='all');
                   cumulative(obj);
                 },
                 jhu=edit(obj,KEEP=cq(state,USA))
                 ## note jhu already cum
                 );
      assign(paste(sep='.',datasrc,'raw'),obj,globalenv());  # save as 'raw'
      obj=fit(obj);                     # default method=sspline, fit.unit=1
      ## assign(paste(sep='.',datasrc,what,'cum'),obj,globalenv());  # save as 'cum'
      ## assign(paste(sep='.',datasrc,what,'std'),obj,globalenv());  # and 'std'
      ## assign(paste(sep='.',datasrc,what),obj,globalenv());        # and 'final'
      assign(datasrc,obj,globalenv());                            # save as jhu, doh
      datasrc;
    });
    datasrc;
  }
## remove superflous objects - either because they were created by mistake or to start clean
## if id is set, only removes those objects, else all that fit the pattern
rm_mtop_objs=function(what='deaths',datasrc=cq(doh,jhu),
                       objid=cq(raw),rm.std=is.null(objid)) {
  datasrc=match.arg(datasrc,several.ok=TRUE);
  if (length(datasrc)==0) invisible(NULL); # nothing to remove
  names.all=ls(globalenv());
  pat=paste0('^(',paste(collapse='|',datasrc),')','\\.',
             ## '(',paste(collapse='|',what),')','\\.',
             '(',paste(collapse='|',objid),')');
  names=grep(pat,names.all,value=TRUE);
  pat=paste0('^(',paste(collapse='|',datasrc),')$');
  names=c(names,grep(pat,names.all,value=TRUE));
  rm(list=names,envir=globalenv());
  invisible(names);
}

## customized raw
## pop.young, mort.young manually extracted from input/meta/mort/state_0_11.txt, state_12_19.txt
raw_mtop=function(what='deaths',datasrc=cq(doh,jhu),version='latest',
                  pop.young=c('0_11'=1113968,'12_19'=726338),
                  mort.young=c('0_11'=493,'12_19'=236),
                  drop.ages=c('0_19')) {
  what=match.arg(what);
  datasrc=match.arg(datasrc);
  if (datasrc=='jhu') obj=raw(what=what,datasrc=datasrc,version=version)
  else {
    if (!is.null(version)&&version=='latest') version=latest_version(datasrc,what);
    data=load_data(whatv=what,datasrc=datasrc,version=version);
    drop.ages=c('0_19');
    keep.places='state';
    data=data[names(data)%notin%drop.ages];
    data=lapply(data,function(data) data[,c('date',keep.places)]);
    newobj=cvdoh;
    obj=newobj(data=data,datasrc=datasrc,what=what,version=version,
               id=FALSE,fit=FALSE,roll=FALSE,extra=FALSE,edit=FALSE);
    obj$pop=obj_pop(obj);
    obj$mort=obj_mort(obj);
    ## patch pop, mort with hardcoded values for 0_11, 12_19
    obj$pop[names(pop.young),]=pop.young;
    obj$mort[names(mort.young),]=mort.young;
    obj=clc(obj,list(unit=7,start.on='Sunday',center=FALSE,cumulative=FALSE));
  }
  obj;
}
