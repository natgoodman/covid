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
           where=param(where),what=param(what),datasrc=param(datasrc),
           ageid=1:4,objids=cq(inc,std),conf.prob=cq(conf,prob),
           doc='updatsupp',version='latest',pjto=cq(dir,file),...) {
    where=match.arg(where,several.ok=TRUE);
    what=match.arg(what,several.ok=TRUE);
    datasrc=match.arg(datasrc,several.ok=TRUE);
    objids=match.arg(objids,several.ok=TRUE);
    conf.prob=match.arg(conf.prob,several.ok=TRUE);
    if (missing(pjto)) pjto=match.arg(pjto);
    ## if (is.null(version)||version=='latest') version=max(sapply(datasrc,latest_version));
    if (need.objs) make_updatsupp_objs(what=what,datasrc=datasrc,version=version);
    if (need.places) make_updatsupp_places();
    if (need.ages) make_updatsupp_ages();
    ## if (need.init) init_doc(doc='updatsupp',version=version,sectpfx=TRUE,...);
    if (need.init) init_doc(doc=doc,version=version,figlabel=FALSE,pjto=pjto,...);
    sect.all=cq(byplace,byage,bysrc);

    ## sect.all=cq(inc.wa1,inc.wa2,inc.datasrc%in%cq(doh,nonwa1,inc.nonwa2,inc.fav,
    ##             cum.wa1,cum.wa2,cum.nonwa1,cum.nonwa2,cum.fav,
    ##             inc.byage.wa,cum.byage.wa,inc.byage.usa,cum.byage.usa,
    ##             inc.bysrc.wa1,inc.bysrc.wa2,inc.bysrc.nonwa1,inc.bysrc.nonwa2,inc.bysrc.fav,
    ##             cum.bysrc.wa1,cum.bysrc.wa2,cum.bysrc.nonwa1,cum.bysrc.nonwa2,cum.bysrc.fav))
    ## ;
    if (is.null(sect)) sect=sect.all else sect=pmatch_choice(sect,sect.all,start=FALSE);
    ## if ('byage'%in%sect&&'doh'%notin%datasrc)
    ##   stop("datasrc must contain 'doh' to run 'byage' section");
    ## if ('inc.nonwa'%in%sect&&is_empty(cq(jhu,nyt)%&%datasrc))
    ##   stop("datasrc must contain 'jhu' or 'nyt' to run 'inc.nonwa' section");
    sapply(sect,function(sect) {
      switch(
        sect,
        byage=sect_byage(ageid=ageid,where=where,what=what,datasrc=datasrc,id=objids,
                         conf.prob=conf.prob),
        byplace=sect_byplace(where=where,what=what,datasrc=datasrc,id=objids,conf.prob=conf.prob),
        bysrc=sect_bysrc(where=where,what=what,datasrc=datasrc,id=objids,conf.prob=conf.prob))
    });

      ## if (sect=='inc.wa1') sect_byplace(where='wa1',what=what,datasrc=datasrc);
      ## if (sect=='inc.wa2') sect_byplace(where='wa2',what=what,datasrc=datasrc);
      ## if (sect=='inc.nonwa1') sect_byplace(where='nonwa1',what=what,datasrc=datasrc);
      ## if (sect=='inc.nonwa2') sect_byplace(where='nonwa2',what=what,datasrc=datasrc);
      ## if (sect=='inc.fav') sect_byplace(where='fav',what=what,datasrc=datasrc);
      ## if (sect=='inc.usa') sect_byplace(where='usa',what=what,datasrc=datasrc);

      ## if (sect=='cum.wa1') sect_byplace(where='wa1',what=what,datasrc=datasrc,id='cum');
      ## if (sect=='cum.wa2') sect_byplace(where='wa2',what=what,datasrc=datasrc,id='cum');
      ## if (sect=='cum.nonwa1')
      ##   sect_byplace(where='nonwa1',what=what,datasrc=datasrc,id='cum');
      ## if (sect=='cum.nonwa2')
      ##   sect_byplace(where='nonwa2',what=what,datasrc=datasrc,id='cum');
      ## if (sect=='cum.fav') sect_byplace(where='fav',what=what,datasrc=datasrc,id='cum');
      ## if (sect=='cum.usa') sect_byplace(where='usa',what=what,datasrc=datasrc,id='cum');
      
      ## if (sect=='inc.byage.wa') sect_byage(aid=ageids,what=what,datasrc='doh');
      ## if (sect=='cum.byage.wa') sect_byage(aid=ageids,what=what,datasrc='doh',id='cum');
      ## if (sect=='inc.byage.usa') sect_byage(aid=ageids,what=what,datasrc='cdc');
      ## if (sect=='cum.byage.usa') sect_byage(aid=ageids,what=what,datasrc='cdc',id='cum');

      ## if (sect=='inc.bysrc.wa1') sect_bysrc(places=places.wa1,what=what,datasrc=datasrc);
      ## if (sect=='inc.bysrc.wa2') sect_bysrc(places=places.wa2,what=what,datasrc=datasrc);
      ## if (sect=='inc.bysrc.nonwa1')
      ##   sect_bysrc(places=places.nonwa1,what=what,datasrc=datasrc);
      ## if (sect=='inc.bysrc.nonwa2')
      ##   sect_bysrc(places=places.nonwa2,what=what,datasrc=datasrc);
      ## if (sect=='inc.bysrc.fav') sect_bysrc(places=places.fav,what=what,datasrc=datasrc);

      ## if (sect=='cum.bysrc.wa1') sect_bysrc(places=places.wa1,what=what,datasrc=datasrc,id='cum');
      ## if (sect=='cum.bysrc.wa2') sect_bysrc(places=places.wa2,what=what,datasrc=datasrc,id='cum');
      ## if (sect=='cum.bysrc.nonwa1')
      ##   sect_bysrc(places=places.nonwa1,what=what,datasrc=datasrc,id='cum');
      ## if (sect=='cum.bysrc.nonwa2')
      ##   sect_bysrc(places=places.nonwa2,what=what,datasrc=datasrc,id='cum');
      ##  if (sect=='cum.bysrc.fav') sect_bysrc(places=places.fav,what=what,datasrc=datasrc,id='cum');
    ## });
    if (pjto=='dir') system(paste('pjto -r',param(figdir))); # copy figdir to Mac if desired
    sect;
  }
