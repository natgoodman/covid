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
## At present, just a place to put code that extends updat
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## --- Generate Figures and Tables for updat suplement ---
## TODO: not yet ported
doc_updatsupp=function(sect=NULL,need.objs=TRUE,need.init=TRUE,version='latest',figs.all=FALSE,...) {
  datasrc=cq(doh,jhu,nyt);
  if (is.null(version)||version=='latest') version=max(sapply(datasrc,latest_version));
  if (need.objs) make_updatsupp_objs(datasrc=datasrc,version=version);
  if (need.init) init_doc(doc='updatsupp',version=version,...);
  sect.all=cq(jhu.cases,jhu.deaths,nyt.cases,nyt.deaths,doh.cases,doh.admits,doh.deaths);
  if (is.null(sect)) sect=sect.all else sect=pmatch_choice(sect,sect.all,start=FALSE);
  ## TEMPORARY
  places.wa=cq(state,King,Snohomish,Pierce);
  labels.wa=setNames(c('Washington state','Seattle (King County)',
                       'Snohomish (North of Seattle)','Pierce (South of Seattle)'),
                     places.wa);
  places.nonwa=cq('Ann Arbor',Boston,'San Diego',DC);
  labels.nonwa=setNames(c('Ann Arbor','Boston','San Diego','Washington DC'),places.nonwa);
 
  sapply(sect,function(sect) {
    sect_start(sect,sect.all);
    if (sect=='jhu.cases') {
      figblk_start();
      dofig('cases_wa',
            plot_cvdat(
              jhu.cases,places=places.wa,ages='all',per.capita=TRUE,lwd=2,
              title=figtitle("Weekly cases per million in Washington locations"),
              legends=list(labels=labels.wa)));
      dofig('cases_nonwa',
            plot_cvdat(
              jhu.cases,places=places.nonwa,ages='all',per.capita=TRUE,lwd=2,
              title=figtitle("Weekly cases per million in non-Washington locations"),
              legends=list(labels=labels.nonwa)));
      ## recent WA raw data very ragged in versions >= 21-01-24. include figures showing this
      dofig('cases_wa_ragged',
            plot_finraw(
              datasrc='jhu',what='cases',places=places.wa,ages='all',per.capita=TRUE,lwd=2,
              title=figtitle(
                "Weekly cases per million in Washington locations showing raw data"),
              legends=list(labels=labels.wa)));
      dofig('cases_nonwa_ragged',
            plot_finraw(
              datasrc='jhu',what='cases',places=places.nonwa,ages='all',per.capita=TRUE,lwd=2,
              title=figtitle(
                "Weekly cases per million in non-Washington locations showing raw data"),
              legends=list(labels=labels.nonwa)));
    }
     if (sect=='jhu.deaths') {
    figblk_start();
    dofig('deaths_wa',
        plot_cvdat(
          jhu.deaths,places=places.wa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly deaths per million in Washington locations"),
          legend='top',legends=list(labels=labels.wa)));
  dofig('deaths_nonwa',
        plot_cvdat(
          jhu.deaths,places=places.nonwa,
          ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly deaths per million in non-Washington locations"),
          legend='top',legends=list(labels=labels.nonwa)));
  ## recent WA raw data very ragged in version 21-01-24. include figures showing this
  dofig('deaths_wa_ragged',
        plot_finraw(
          datasrc='jhu',what='deaths',places=places.wa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle(
            "Weekly deaths per million in Washington locations showing raw data"),
          legends=list(labels=labels.wa)));
  dofig('deaths_nonwa_ragged',
        plot_finraw(
          datasrc='jhu',what='deaths',places=places.nonwa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle(
            "Weekly deaths per million in non-Washington locations showing raw data"),
          legends=list(labels=labels.nonwa)));

     }
  });
}

NOT_PORTED=function() {
  places.wa=cq(state,King,Snohomish,Pierce);
  labels.wa=setNames(c('Washington state','Seattle (King County)',
                       'Snohomish (North of Seattle)','Pierce (South of Seattle)'),
                     places.wa);
  places.nonwa=cq('Ann Arbor',Boston,'San Diego',DC);
  labels.nonwa=setNames(c('Ann Arbor','Boston','San Diego','Washington DC'),places.nonwa);
  ## Tables 1-2 trend analysis. Tables 3-4 raw data counts. not used in document.
  ## TODO: rewrite using dotbl when implemented!
  ## NOTE: save_tbl in this file not dat.R where you might expect it
  if (param(verbose)) print(paste('+++ making tables'));
  widths=if(version<'21-02-28') 4 else c(4,6,8);
  trend.cases=trend(jhu.cases.raw,places=c(places.wa,places.nonwa),widths=widths);
  trend.deaths=trend(jhu.deaths.raw,places=c(places.wa,places.nonwa),widths=widths);
  counts.cases=data_cvdat(jhu.cases.raw,places=c(places.wa,places.nonwa),per.capita=TRUE);
  counts.deaths=data_cvdat(jhu.deaths.raw,places=c(places.wa,places.nonwa),per.capita=TRUE);
  save_tbl(trend.cases,1,'trend_cases');
  save_tbl(trend.deaths,2,'trend_deaths');
  save_tbl(counts.cases,3,'counts_cases');
  save_tbl(counts.deaths,4,'counts_deaths');
  assign_global(trend.cases,trend.deaths,counts.cases,counts.deaths);
  if (param(verbose)) print(paste('+++ making figures'));
  ## Figures 1a-b cases
  figblk_start();
  dofig('cases_wa',
        plot_cvdat(
          jhu.cases,places=places.wa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly cases per million in Washington locations"),
          legends=list(labels=labels.wa)));
  dofig('cases_nonwa',
        plot_cvdat(
          jhu.cases,places=places.nonwa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly cases per million in non-Washington locations"),
          legends=list(labels=labels.nonwa)));
  ## recent WA raw data very ragged in versions >= 21-01-24. include figures showing this
  dofig('cases_wa_ragged',
        plot_finraw(
          datasrc='jhu',what='cases',places=places.wa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle(
            "Weekly cases per million in Washington locations showing raw data"),
          legends=list(labels=labels.wa)));
  dofig('cases_nonwa_ragged',
        plot_finraw(
          datasrc='jhu',what='cases',places=places.nonwa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle(
            "Weekly cases per million in non-Washington locations showing raw data"),
          legends=list(labels=labels.nonwa)));
  ## Figures 2a-b deaths
  figblk_start();
  dofig('deaths_wa',
        plot_cvdat(
          jhu.deaths,places=places.wa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly deaths per million in Washington locations"),
          legend='top',legends=list(labels=labels.wa)));
  dofig('deaths_nonwa',
        plot_cvdat(
          jhu.deaths,places=places.nonwa,
          ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly deaths per million in non-Washington locations"),
          legend='top',legends=list(labels=labels.nonwa)));
  ## recent WA raw data very ragged in version 21-01-24. include figures showing this
  dofig('deaths_wa_ragged',
        plot_finraw(
          datasrc='jhu',what='deaths',places=places.wa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle(
            "Weekly deaths per million in Washington locations showing raw data"),
          legends=list(labels=labels.wa)));
  dofig('deaths_nonwa_ragged',
        plot_finraw(
          datasrc='jhu',what='deaths',places=places.nonwa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle(
            "Weekly deaths per million in non-Washington locations showing raw data"),
          legends=list(labels=labels.nonwa)));
  if ('doh'%notin%datasrc) return();
  ## Figures 3 WA DOH cases by age for state
  figblk_end();
  ages.wa=sort(ages(doh.cases)%-%'all');
  col=col_brew(length(ages.wa),'d3');
  place='state';
  dofig(paste(sep='_','cases',place),
        plot_cvdat(
          doh.cases,places=place,ages=ages.wa,per.capita=TRUE,lwd=2,col=col,
          title=figtitle(paste("Weekly cases per million by age in",labels.wa[place]))));
  ## Figures 4 WA DOH deaths by age for state
  ages.wa=sort(ages(doh.deaths)%-%'all');
  ## select col to better match cases (Figure 3). use 1st and last 2
  col=col[c(1,length(col)-1,length(col))];
  dofig(paste(sep='_','deaths',place),
        plot_cvdat(
          doh.deaths,places=place,ages=ages.wa,per.capita=TRUE,lwd=2,col=col,
          title=figtitle(paste("Weekly deaths per million by age in",labels.wa[place])),
          legend='top'));
  ## Figures 5a-b compare DOH, JHU.
  ## Note: not used in versions after Dec 13. might come back...
  if (!figs.all) return();
  figblk_start();
  dofig('cases_dohjhu',plot_dohjhu('cases'));
  dofig('deaths_dohjhu',plot_dohjhu('deaths'));
  return();
}
## make objs doc_updatsupp needs
## from workflow.R do_objs
make_updatsupp_objs=
  function(what=cq(cases,admits,deaths),datasrc=cq(doh,jhu,nyt),version='latest') {
    cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
    ## only 'doh' has 'admits'. prune others
    cases=cases[(cases$what!='admits'|cases$datasrc%in%cq(doh)),]
    ## sorted county populations and names
    pop.wa=sort(pop_wa(),decreasing=TRUE);
    counties.wa=colnames(pop.wa);
    top5=head(counties.wa,n=5);
    bottom10=tail(counties.wa,n=10);
    withrows(cases,case,{
      if (param(verbose)) print(paste('+++ making',datasrc,what));
      case.env=environment();
      obj.src=raw(what,datasrc,version);
      ## do edit first so downstream objects all comparable
      obj.edit=edit(obj.src,SKP=Snohomish+King+Pierce,
                    SUM=list(Top5=top5,Bottom10=bottom10),
                    NEG=list(notKing='King',notSKP='SKP',notTop5='Top5',notBottom10='Bottom10'));
      if (datasrc=='doh') 
        obj.edit=if(version(obj.src)<='21-03-07')
                   ## original ages
                   edit(obj.edit,young='20_39'+'40_59',old='60_79'+'80_')
                 else ## new ages
                   edit(obj.edit,young='20_34'+'35_49'+'50_64',old='65_79'+'80_');
      ## 'raw' really means 'weekly, incremental'
      obj.raw=if(datasrc=='doh') obj.edit else weekly(incremental(obj.edit));
      ## 'cum'. convert jhu, nyt to weekly so dates will match doh
      obj.cum=if(datasrc=='doh') cumulative(obj.edit) else weekly(obj.edit);
      obj.roll=roll(obj.raw);
      ## for fit, use 1 day for cases, 10.5 days (1.5 weeks) for deaths
      fit.unit=if(what=='cases') 1 else 10.5;
      obj.fit=fit(obj.raw,fit.unit=fit.unit);
      if(datasrc=='doh')  {
        obj.extra=extra(obj.raw);
        obj.std=obj.fitx=fit(obj.extra,fit.unit=fit.unit);
      } else {
        obj.std=obj.fit;
      }
      ## iterate over ids. get object, add id, assign to global
      id=cq(src,edit,raw,cum,roll,fit,extra,fitx,std);
      names.local=paste0('obj.',id);
      names.global=paste(sep='.',datasrc,what,id);
      sapply(id,function(id) {
        name=paste0('obj.',id);
        if (exists(name,envir=case.env,inherits=FALSE)) {
          obj=get(name,envir=case.env,inherits=FALSE);
          obj$id=id;
          assign(paste(sep='.',datasrc,what,id),obj,envir=globalenv())
          if (id=='std') 
            ## also assign 'std' to variable w/o suffix, eg, doh.cases
            assign(paste(sep='.',datasrc,what),obj,envir=globalenv());
        }
      });
    });
    cases;  
  }
## remove superflous objects - either because they were created by mistake or to start clean
rm_updatsupp_objs=
  function(what=cq(cases,admits,deaths),datasrc=cq(doh,jhu,nyt),
           id=cq(src,edit,raw,cum,roll,fit,extra,fitx,std),rm.std=FALSE) {
    what=match.arg(what,several.ok=TRUE);
    datasrc=match.arg(datasrc,several.ok=TRUE);
    id=match.arg(id,several.ok=TRUE);
    cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
    ## only 'doh' has 'admits'. prune others
    cases=cases[(cases$what!='admits'|cases$datasrc%in%cq(doh)),]
    if (is.null(id)) rm.std=TRUE;
    withrows(cases,case,{
      if (!is.null(id)) {
        names=paste(sep='.',datasrc,what,id);
        names=names[sapply(names,function(name) exists(name,envir=globalenv()))];
        if (length(names)>0) {
          if (param(verbose))
            print(paste('--- rm\'ing',paste(collapse=',',names)));
          rm(list=names,envir=globalenv());
        }}
      if (rm.std) {
        name=paste(sep='.',datasrc,what);
        if (exists(name,envir=globalenv())) {
          if (param(verbose)) print(paste('--- rm\'ing',name,'(no suffix)'));
          rm(list=paste(sep='.',datasrc,what),envir=globalenv());
        }}
    });
    cases;
  }
## hack to plot pairs of objects together for same datasrc, typically processed and raw data 
## similar to plot_finraw in doc_updat.R
## datasrc, what, ids define obj pairs not explicitly given
## objpairs is list of object pairs: overrides choices from previous args
## TODO: add this to plot_cvdat!
plot_pairs=
  function(what=cq(cases,deaths),datasrc=cq(doh,jhu,nyt),
           id=cq(src,edit,raw,cum,roll,fit,extra,fitx),
           objpairs=list(),
           raw.plot=cq(lines,points),
           lwd=2,lwd.prc=lwd,lwd.raw=0.375*lwd.prc,
           lty.prc='solid',lty.raw='dotted',pch=20,...) {
    what=if(missing(what)) what[1] else match.arg(what,several.ok=TRUE);
    datasrc=match.arg(datasrc,several.ok=TRUE);
    id=if(missing(id)) cq(std,raw) else match.arg(id,several.ok=TRUE);
    ## TODO: make sure id has usable length
    raw.plot=match.arg(raw.plot,several.ok=TRUE);
    ## figure out object pairs
    cases=expand.grid(what=what,datasrc=datasrc,objpair=NA,stringsAsFactors=FALSE);
    ## only 'doh' has 'admits'. prune others
    cases=cases[(cases$what!='admits'|cases$datasrc%in%cq(doh)),]
    rownames(cases)=with(cases,paste(sep='.',what,datasrc));
    ## override cases from objpairs
    sapply(seq_along(objpairs),function(i) { 
      objpair=objpairs[[i]];
      if (is_cvdat(objpair)) objpair=list(objpair);
      if (length(objpair)==0) return(); # skip empty ones
      if (length(objpair)>2)
        stop("each objpair must contain one or two objects, not ",length(objpair));
      ## objpair is singleton or pair. if pair, must have same 'what' and 'datasrc'
      what=unique(sapply(objpair,function(obj) what(obj)));
      datasrc=unique(sapply(objpair,function(obj) datasrc(obj)));
      if (length(what)>1) 
        stop("each objpair must contain objects with same 'what', not ",
             paste(collapse=', ',what));
      if (length(datasrc)>1) 
        stop("each objpair must contain objects with same 'datasrc', not ",
             paste(collapse=', ',datasrc));
      ## override case for this 'what', 'datasrc'
      cases[paste(sep='.',what,datasrc),]<<-c(what,datasrc,i);
    });
    
    fin=get(paste(sep='.',datasrc,what));
    raw=get(paste(sep='.',datasrc,what,'raw'));
    data=data_cvdat(list(fin,raw),places=places,ages=ages,per.capita=per.capita);
    ymax=max(data[,-1],na.rm=TRUE);
    plot_cvdat(fin,places=places,ages=ages,per.capita=per.capita,ymax=ymax,
               lwd=lwd.fin,lty=lty.fin,...);
    if ('lines'%in%raw.plot)
      plot_cvdat(
        raw,places=places,ages=ages,per.capita=per.capita,add=TRUE,lwd=lwd.raw,lty=lty.raw);
    if ('points'%in%raw.plot)
      plot_cvdat(
        raw,places=places,ages=ages,per.capita=per.capita,add=TRUE,lwd=lwd.raw,lty=lty.raw,
        type='p',pch=rep(pch,length(places)*length(ages)))
  }
## ---- Standard plots ----
## mostly a placeholder. expect it to change
do_plots=function(datasrc=cq(doh,jhu,nyt,trk),objs=NULL,objs.noroll=NULL) {
  ok=system('pjtest')==0;
  if (!ok) stop('Reverse tunnel not running; stopping before doing plots');
  if (is.null(objs)) {
    objs=sapply(datasrc,function(datasrc) {
      name=paste(sep='.',datasrc,'cases');
      get(name,globalenv());
    });
  }
  if (is.null(objs.noroll)) {
    objs.noroll=sapply(datasrc,function(datasrc) {
      name=paste(sep='.',datasrc,'cases.noroll');
      get(name,globalenv());
    });
  }
  objs.notrk=objs[sapply(objs,function(obj) datasrc(obj)!='trk')]
  ## all sources 'state'
  plon('all.state');
  plot_cvdat(objs,places=cq(state),ages='all',per.capita=TRUE);
  ploff();
  ## all sources (except trk) 'King'
  plon('all.King');
  plot_cvdat(objs.notrk,places=cq(King),ages='all',per.capita=TRUE);
  ploff();
  ## all sources (except trk) 'San Juan'
  plon('all.SanJuan');
  plot_cvdat(objs.notrk,places=cq('San Juan'),ages='all',per.capita=FALSE);
  ploff();
  ## all sources (except trk) 'Okanogan'
  plon('all.Okanogan');
  plot_cvdat(objs.notrk,places=cq('Okanogan'),ages='all',per.capita=TRUE);
  ploff();
  if ('doh' %in% datasrc) {
    ## doh several places
    plon('doh.places');
    plot_cvdat(doh.cases,places=cq(state,King,SKP,Top5,Top10),ages='all',per.capita=TRUE);
    ploff();
    ## doh several ages
    plon('doh.ages');
    plot_cvdat(doh.cases,places=cq(state),ages=NULL,per.capita=TRUE);
    ploff();
  }
  if ('jhu' %in% datasrc) {
    ## jhu King, kids
    plon('jhu.kids');
    plot_cvdat(jhu.cases,places=cq(King,'Ann Arbor',DC),ages=NULL,per.capita=TRUE);
    ploff();
    ## jhu King, others
    plon('jhu.other');
    plot_cvdat(jhu.cases,places=places_other(),ages=NULL,per.capita=TRUE);
    ploff();
  }
  if (cq(jhu,nyt) %<=% datasrc) {
    ## jhu, nyt other
    plon('all.other');
    plot_cvdat(list(jhu.cases,nyt.cases),places=places_other(),ages=NULL,per.capita=TRUE);
    ploff();
  }
  ## all sources 'state', regular and noroll
  n=length(objs.noroll);
  plon('all.regnoroll');
  plot_cvdat(c(objs,objs.noroll),places=cq(state),ages='all',per.capita=TRUE,
             lty=c(rep('solid',n),rep('dotted',n)),col=rep(col_brew(n,'d3')));
  ploff()
  ## all sources 'state' noroll
  plon('all.noroll');
  plot_cvdat(objs.noroll,places=cq(state),ages='all',per.capita=TRUE);
  ploff()
}

