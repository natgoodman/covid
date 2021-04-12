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
doc_updatsupp=function(sect=NULL,need.objs=TRUE,need.init=TRUE,
                       what=cq(cases,admits,deaths),datasrc=cq(doh,jhu,nyt),version='latest',
                       figs.all=FALSE,...) {
  if (is.null(version)||version=='latest') version=max(sapply(datasrc,latest_version));
  if (need.objs) make_updatsupp_objs(what=what,datasrc=datasrc,version=version);
  ## if (need.init) init_doc(doc='updatsupp',version=version,sectpfx=TRUE,...);
  if (need.init) init_doc(doc='updatsupp',version=version,figlabel=FALSE,...);
  sect.all=cq(base.wa,age.wa,base.nonwa,cmp.wa,cmp.nonwa);
  if (is.null(sect)) sect=sect.all else sect=pmatch_choice(sect,sect.all,start=FALSE);
  ## TEMPORARY
  places.wa1=cq(state,King,Snohomish,Pierce);
  labels.wa1=setNames(c('Washington state','Seattle (King County)',
                       'Snohomish (North of Seattle)','Pierce (South of Seattle)'),
                      places.wa1);
  places.wa2=cq(SKP,North,South,West,EastNotYakima);
  labels.wa2=setNames(c('Seattle Metro (SKP)','North of SKP','South of SKP',
                        'West of SKP','East of SKP (except Yakima)'),
                      places.wa2);
  places.nonwa1=cq('Ann Arbor',Boston,'San Diego',DC);
  labels.nonwa1=setNames(c('Ann Arbor','Boston','San Diego','Washington DC'),places.nonwa1);
  places.nonwa2=cq('Big Island',Fairbury,'Cape Cod','Mackinac Island',Omaha,Austin);
  labels.nonwa2=setNames(places.nonwa2,places.nonwa2);

  sapply(sect,function(sect) {
    ## base wa
    if (sect=='base.wa') {
      cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
      ## only 'doh' has 'admits'. prune others
      cases=cases[(cases$what!='admits'|cases$datasrc%in%cq(doh)),]
      withrows(cases,case,{
        if (param(verbose)) print(paste('+++ plotting',datasrc,what,'Washington'));
        pfx=paste(sep='_',what,datasrc);
        obj=get(paste(sep='.',datasrc,what),envir=globalenv());
        title.pfx=paste("Weekly",what,"per million from",toupper(datasrc));
        figblk_start();
        dofig(paste(sep='_',pfx,'wa1'),
              plot_cvdat(
                obj,places=places.wa1,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in core Washington locations")),
                legends=list(labels=labels.wa1)));
        dofig(paste(sep='_',pfx,'wa2'),
              plot_cvdat(
                obj,places=places.wa2,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in supp Washington locations")),
                legends=list(labels=labels.wa2)));
        ## incude figures showing ragged raw data
        dofig(paste(sep='_',pfx,'wa1','ragged'),
              plot_finraw(
                datasrc=datasrc,what=what,places=places.wa1,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in core Washington locations showing raw data")),
                legends=list(labels=labels.wa1)));
        dofig(paste(sep='_',pfx,'wa2','ragged'),
              plot_finraw(
                datasrc=datasrc,what=what,places=places.wa2,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in supp Washington locations showing raw data")),
                legends=list(labels=labels.wa2)));
      });
    }
    ## base nonwa
    if (sect=='base.nonwa') {
      datasrc=datasrc%-%'doh';          # DOH is only Washington
      what=what%-%'admits';             # admits are only DOH
      cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
      withrows(cases,case,{
        if (param(verbose)) print(paste('+++ plotting',datasrc,what,'non-Washington'));
        pfx=paste(sep='_',what,datasrc);
        obj=get(paste(sep='.',datasrc,what),envir=globalenv());
        title.pfx=paste("Weekly",what,"per million from",toupper(datasrc));
        figblk_start();
        dofig(paste(sep='_',pfx,'nonwa1'),
              plot_cvdat(
                obj,places=places.nonwa1,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in core non-Washington locations")),
                legends=list(labels=labels.nonwa1)));
        dofig(paste(sep='_',pfx,'nonwa2'),
              plot_cvdat(
                obj,places=places.nonwa2,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in supp non-Washington locations")),
                legends=list(labels=labels.nonwa2)));
        ## incude figures showing ragged raw data
        dofig(paste(sep='_',pfx,'nonwa1','ragged'),
              plot_finraw(
                datasrc=datasrc,what=what,places=places.nonwa1,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in core non-Washington locations showing raw data")),
                legends=list(labels=labels.nonwa1)));
        dofig(paste(sep='_',pfx,'nonwa2','ragged'),
              plot_finraw(
                datasrc=datasrc,what=what,places=places.nonwa2,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in supp non-Washington locations showing raw data")),
                legends=list(labels=labels.nonwa2)));
      });
    }
    
  })
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
    ## BREAKPOINT('make_updatsupp_objs: before withrows')
    withrows(cases,case,{
      if (param(verbose)) print(paste('+++ making',datasrc,what));
      case.env=environment();
      obj.src=raw(what,datasrc,version);
      ## do edit first so downstream objects all comparable
      ## BREAKPOINT('make_updatsupp_objs: in withrows before edit')
      obj.edit=edit(obj.src,
                    SKP=Snohomish+King+Pierce,
                    North=Whatcom+Skagit,
                    South=Thurston+Lewis+Skamania,
                    West=Kitsap+Island+'San Juan',
                    East=Okanogan+Chelan+Kittitas+Yakima+Klickitat,
                    EastNotYakima=Okanogan+Chelan+Kittitas+Klickitat,
                    SUM=list(Top5=top5,Bottom10=bottom10),
                    NEG=list(notKing='King',notSKP='SKP',notTop5='Top5',notBottom10='Bottom10'));
      ## BREAKPOINT('make_updatsupp_objs: in withrows after edit')
      if (datasrc=='doh')
        obj.edit=if(version(obj.src)<='21-03-07')
                   ## original ages
                   edit(obj.edit,kids='0_19',young='20_39'+'40_59',old='60_79'+'80_')
                 else ## new ages
                   edit(obj.edit,kids='0_19',young='20_34'+'35_49'+'50_64',old='65_79'+'80_');
      ## 'raw' really means 'editted', 'weekly, incremental'
      obj.raw=if(datasrc=='doh') obj.edit else weekly(incremental(obj.edit));
      ## 'cum'. convert jhu, nyt to weekly so dates will match doh
      obj.cum=if(datasrc=='doh') cumulative(obj.edit) else weekly(obj.edit);
      obj.roll=roll(obj.raw);
      ## for fit, use 1 day for cases, 10.5 days (1.5 weeks) for deaths
      fit.unit=switch(what,cases=1,admits=7,deaths=10.5);
      obj.fit=fit(obj.raw,fit.unit=fit.unit);
      if(datasrc=='doh')  {
        obj.extra=extra(obj.raw);
        obj.rollx=roll(obj.extra);
        obj.std=obj.fitx=fit(obj.extra,fit.unit=fit.unit);
      } else {
        obj.std=obj.fit;
      }
      ## iterate over objects. get id, assign to global
      names.local=grep('^obj\\.',ls(case.env),value=TRUE);
      sapply(names.local,function(name) {
        id=sub('^obj\\.','',name);
        name=paste0('obj.',id);
        obj=get(name,envir=case.env,inherits=FALSE);
        obj$id=id;
        assign(paste(sep='.',datasrc,what,id),obj,envir=globalenv())
        if (id=='std') 
          ## also assign 'std' to variable w/o suffix, eg, doh.cases
          assign(paste(sep='.',datasrc,what),obj,envir=globalenv());
      });
    });
    cases;  
  }
## remove superflous objects - either because they were created by mistake or to start clean
## if id is set, only removes those objects, else all that fit the pattern
rm_updatsupp_objs=
  function(what=cq(cases,admits,deaths),datasrc=cq(doh,jhu,nyt),
           id=NULL,rm.std=is.null(id)) {
    what=match.arg(what,several.ok=TRUE);
    datasrc=match.arg(datasrc,several.ok=TRUE);
    if (length(what)==0||length(datasrc)==0) invisible(NULL); # nothing to remove
    names.all=ls(globalenv());
    pat=paste0('^(',paste(collapse='|',datasrc),')','\\.',
               '(',paste(collapse='|',what),')','\\.',
               '(',paste(collapse='|',id),')');
    names1=grep(pat,names.all,value=TRUE);
    BREAKPOINT('rm_updatsupp_objs: after id grep, before rm')
    rm(list=names1,envir=globalenv());
    if (rm.std) {
      pat=paste0('^(',paste(collapse='|',datasrc),')','\\.',
                  '(',paste(collapse='|',what),')$');
      names2=grep(pat,names.all,value=TRUE);
      BREAKPOINT('rm_updatsupp_objs: after rm.std grep, before rm')
      rm(list=names2,envir=globalenv());
      invisible(c(names1,names2));
    } else invisible(names1);
  }
## hack to plot pairs of objects together for same datasrc, typically processed and raw data 
## similar to plot_finraw in doc_updat.R
## datasrc, what, ids define obj pairs not explicitly given in objpairs
## objpairs is list of object pairs: overrides choices from previous args
## TODO: add this to plot_cvdat!
plot_pairs=
  function(what=cq(cases,deaths),datasrc=cq(doh,jhu,nyt),
           id=cq(src,edit,raw,cum,roll,fit,extra,fitx),
           objpairs=list(),
           places='state',ages='all',per.capita=TRUE,
           type2=cq(lines,points),
           lwd=2,lwd1=lwd,lwd2=0.375*lwd1,
           lty1='solid',lty2='dotted',pch=20,...) {
    if (is.null(what)||is.null(datasrc)||is.null(id)) {
      ## use objpairs as is. set cases to empty data frame
      cases=data.frame(what=character(),datasrc=character(),objpair=integer(),
                       stringsAsFactors=FALSE);
    } else {
      what=if(missing(what)) 'cases' else match.arg(what,several.ok=TRUE);
      datasrc=if(missing(datasrc)) 'jhu' else match.arg(datasrc,several.ok=TRUE);
      id=if(missing(id)) cq(std,raw) else unique(match.arg(id,several.ok=TRUE));
      if (length(id)!=2) stop ("'id' must have two elements, not ",length(id),
                               ". id=",paste(collapse=', ',id));
      type2=unique(match.arg(type2,several.ok=TRUE));
      if (length(type2)!=2) stop ("'type2' must have two elements, not ",length(type2),
                                  ". type2=",paste(collapse=', ',type2));
      ## figure out object pairs
      cases=expand.grid(what=what,datasrc=datasrc,objpair=NA,stringsAsFactors=FALSE);
      ## only 'doh' has 'admits'. prune others
      cases=cases[(cases$what!='admits'|cases$datasrc%in%cq(doh)),]
      rownames(cases)=with(cases,paste(sep='.',what,datasrc));
    }
    ## override cases from objpairs
    ## special case" objpairs is itself a pair of objects
    if (length(objpairs)==2&all(sapply(objpairs,function(obj) is_cvdat(obj))))
      objpairs=list(objpairs);
    sapply(seq_along(objpairs),function(i) { 
      objpair=objpairs[[i]];
      if (length(objpair)==0) return(); # skip empty ones
      if (length(objpair)!=2)
        stop("each objpair must contain 2 objects, not ",length(objpair));
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
    ## get obj pairs for cases still not handled
    withrows(cases,case,{
      if (is.na(objpair)) {
        i=length(objpairs)+1;
        obj1=get(paste(sep='.',datasrc,what,id[1]),envir=globalenv())
        obj2=get(paste(sep='.',datasrc,what,id[2]),envir=globalenv())
        objpairs[[i]]<<-list(obj1,obj2);
        cases[paste(sep='.',what,datasrc),]<<-c(what,datasrc,i)
        ## BREAKPOINT('plot_pairs: bottom of cases loop')
      }
      });
    ## BREAKPOINT('plot_pairs: after creating cases')
    ## flatten objpairs to get y range
    objs=do.call(c,objpairs);
    data=data_cvdat(objs,places=places,ages=ages,per.capita=per.capita);
    ymax=max(data[,-1],na.rm=TRUE);
    objs1=do.call(c,lapply(objpairs,function(pair) pair[1]));
    objs2=do.call(c,lapply(objpairs,function(pair) pair[2]));
    ## BREAKPOINT('plot_pairs: after extracting objs1, objs2')
    plot_cvdat(objs1,places=places,ages=ages,per.capita=per.capita,ymax=ymax,
               lwd=lwd1,lty=lty1,...);
    if ('lines'%in%type2)
      plot_cvdat(
        objs2,places=places,ages=ages,per.capita=per.capita,add=TRUE,lwd=lwd2,lty=lty2);
    if ('points'%in%type2)
      plot_cvdat(
        objs2,places=places,ages=ages,per.capita=per.capita,add=TRUE,lwd=lwd2,lty=lty2,
        type='p',pch=rep(pch,length(objs2)*length(places)*length(ages)));
    cases;
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
    ## jhu King, nonwas
    plon('jhu.nonwa');
    plot_cvdat(jhu.cases,places=places_nonwa(),ages=NULL,per.capita=TRUE);
    ploff();
  }
  if (cq(jhu,nyt) %<=% datasrc) {
    ## jhu, nyt nonwa
    plon('all.nonwa');
    plot_cvdat(list(jhu.cases,nyt.cases),places=places_nonwa(),ages=NULL,per.capita=TRUE);
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

