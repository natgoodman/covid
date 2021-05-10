#################################################################################
##
## Author:  Nat Goodman
## Created: 21-04-04 
##          from doc_updat.R created  20-11-15
##          from misig/doc_confi.R created 19-07-16
##          from misig/confi.R created 19-07-04
## Includes content adapted 
##           from workflow.R created 20-10-08
##           from misig/doc_readmesupp.R created 19-05-09
##
## Copyright (C) 2019-2021 Nat Goodman.
## 
## Generate figures and tables for updat (weekly data update) supplement
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## --- Generate Figures and Tables for updat suplement ---
doc_updatsupp=
  function(sect=NULL,need.objs=TRUE,need.places=TRUE,need.ages=TRUE,need.init=TRUE,
           what=cq(cases,admits,deaths,admdea),datasrc=cq(doh,jhu,nyt),ageids=1:4,
           doc='updatsupp',version='latest',figs.all=TRUE,...) {
    what=match.arg(what,several.ok=TRUE);
    datasrc=match.arg(datasrc,several.ok=TRUE);
    if (is.null(version)||version=='latest') version=max(sapply(datasrc,latest_version));
    if (need.objs) make_updatsupp_objs(what=what,datasrc=datasrc,version=version);
    if (need.places) make_updatsupp_places();
    if (need.ages) make_updatsupp_ages();
    ## if (need.init) init_doc(doc='updatsupp',version=version,sectpfx=TRUE,...);
    if (need.init) init_doc(doc=doc,version=version,figlabel=FALSE,...);
    sect.all=cq(base.wa1,base.wa2,base.nonwa1,base.nonwa2,base.fav,byage,
                cmp.wa1,cmp.wa2,cmp.nonwa1,cmp.nonwa2,
                cumcmp.wa1,cumcmp.wa2,cumcmp.nonwa1,cumcmp.nonwa2);
    if (is.null(sect)) sect=sect.all else sect=pmatch_choice(sect,sect.all,start=FALSE);
    if ('byage'%in%sect&&'doh'%notin%datasrc)
      stop("datasrc must contain 'doh' to run 'byage' section");
    if ('base.nonwa'%in%sect&&is_empty(cq(jhu,nyt)%&%datasrc))
      stop("datasrc must contain 'jhu' or 'nyt' to run 'base.nonwa' section");
    sapply(sect,function(sect) {
      if (sect=='base.wa1') sect_base(where='wa1',what=what,datasrc=datasrc);
      if (sect=='base.wa2') sect_base(where='wa2',what=what,datasrc=datasrc);
      if (sect=='base.nonwa1') sect_base(where='nonwa1',what=what,datasrc=datasrc%-%'doh');
      if (sect=='base.nonwa2') sect_base(where='nonwa2',what=what,datasrc=datasrc%-%'doh');
      if (sect=='base.fav') sect_base(where='fav',what=what,datasrc=datasrc%-%'doh');
      if (sect=='byage') sapply(ageids,function(aid) sect_byage(aid=aid,what=what,datasrc='doh'));
      if (sect=='cmp.wa1') sect_cmp(places=places.wa1,what=what,datasrc=datasrc);
      if (sect=='cmp.wa2') sect_cmp(places=places.wa2,what=what,datasrc=datasrc);
      if (sect=='cmp.nonwa1') sect_cmp(places=places.nonwa1,what=what,datasrc=datasrc%-%'doh');
      if (sect=='cmp.nonwa2') sect_cmp(places=places.nonwa2,what=what,datasrc=datasrc%-%'doh');
      if (sect=='cumcmp.wa1') sect_cumcmp(places=places.wa1,what=what,datasrc=datasrc);
      if (sect=='cumcmp.wa2') sect_cumcmp(places=places.wa2,what=what,datasrc=datasrc);
      if (sect=='cumcmp.nonwa1')
        sect_cumcmp(places=places.nonwa1,what=what,datasrc=datasrc%-%'doh');
      if (sect=='cumcmp.nonwa2')
        sect_cumcmp(places=places.nonwa2,what=what,datasrc=datasrc%-%'doh');
   });
    sect;
  }
