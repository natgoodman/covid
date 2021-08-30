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
           ageid=1:4,objid=cq(inc,std),conf.prob=cq(conf,prob),do.raw='auto',do.mort=FALSE,
           ages=NULL,places=NULL,col=NULL,col.pal='d3',
           byage.args=list(),byplace.args=list(),bysrc.args=list(),
           doc='updatsupp',version='latest',pjto=cq(dir,file),...) {
    where=match.arg(where,several.ok=TRUE);
    what=match.arg(what,several.ok=TRUE);
    datasrc=match.arg(datasrc,several.ok=TRUE);
    objid=match.arg(objid,several.ok=TRUE);
    conf.prob=match.arg(conf.prob,several.ok=TRUE);
    defaults=
      list(ageid=ageid,where=where,what=what,datasrc=datasrc,objid=objid,
           conf.prob=conf.prob,do.raw=do.raw,do.mort=do.mort,
           ages=ages,places=places,col=col,col.pal='d3');
    byage.args=fill_defaults(defaults,byage.args)[names(formals(sect_byage))];
    byplace.args=fill_defaults(defaults,byplace.args)[names(formals(sect_byplace))];
    bysrc.args=fill_defaults(defaults,bysrc.args)[names(formals(sect_bysrc))];
    pjto=match.arg(pjto);
    if (need.init) init_doc(doc=doc,version=version,figlabel=FALSE,pjto=pjto,...);
    if (need.objs) make_updatsupp_objs(what=what,datasrc=datasrc,version=version);
    if (need.places) make_updatsupp_places();
    if (need.ages) make_updatsupp_ages();
    sect.all=cq(byage,byplace,bysrc);
    if (is.null(sect)) sect=sect.all else sect=pmatch_choice(sect,sect.all,start=FALSE);
     sapply(sect,function(sect) {
      switch(
        sect,
        byage=do.call(sect_byage,byage.args),
        byplace=do.call(sect_byplace,byplace.args),
        bysrc=do.call(sect_bysrc,bysect.args))
    });
    if (pjto=='dir') system(paste('pjto -r',param(figdir))); # copy figdir to Mac if desired
    sect;
  }
