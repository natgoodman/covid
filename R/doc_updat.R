#################################################################################
##
## Author:  Nat Goodman
## Created: 20-11-15
##          from misig/doc_confi.R created 19-07-16
##          from misig/confi.R created 19-07-04
##
## Copyright (C) 2019-2021 Nat Goodman.
## 
## Generate figures and tables for updat (weekly data update) document
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## --- Generate Figures and Tables for updat Blog Post ---
## In past, I kept code for all previous versions. This got completely out of hand.
## Starting with this version, I only have for the current version.
## Previous versions are available in GitHub, of course.
## no sections.
doc_updat=function(need.objs=TRUE,need.init=TRUE,version='latest',figs.all=FALSE,...) {
  if (is.null(version)||version=='latest') version=max(sapply(cq(jhu,doh),latest_version));
  datasrc=cq(doh,jhu);
  if (need.objs) make_updat_objs(datasrc=datasrc,version=version);
  if (need.init) init_doc(doc='updat',version=version,...);
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

## hack to plot final (processed) and raw data together
## used for jhu in Figures 1,2 version 21-01-24
## TODO: add this to plot_cvdat!
plot_finraw=
  function(datasrc=cq(doh,jhu,nyt,trk),what=cq(cases,deaths),title,legends,
           raw.plot=cq(lines,points),
           places,ages='all',per.capita=TRUE,
           lwd=2,lwd.fin=lwd,lwd.raw=0.375*lwd.fin,lty.fin='solid',lty.raw='dotted',pch=20) {
    datasrc=match.arg(datasrc);
    what=match.arg(what);
    if (!is.null(raw.plot)) raw.plot=match.arg(raw.plot,several.ok=TRUE);
    fin=get(paste(sep='.',datasrc,what));
    raw=get(paste(sep='.',datasrc,what,'raw'));
    data=data_cvdat(list(fin,raw),places=places,ages=ages,per.capita=per.capita);
    ymax=max(data[,-1],na.rm=TRUE);
    plot_cvdat(fin,places=places,ages=ages,per.capita=per.capita,ymax=ymax,
               title=title,legends=legends,lwd=lwd.fin,lty=lty.fin);
    if ('lines'%in%raw.plot)
      plot_cvdat(
        raw,places=places,ages=ages,per.capita=per.capita,add=TRUE,lwd=lwd.raw,lty=lty.raw);
    if ('points'%in%raw.plot)
      plot_cvdat(
        raw,places=places,ages=ages,per.capita=per.capita,add=TRUE,lwd=lwd.raw,lty=lty.raw,
        type='p',pch=rep(pch,length(places)*length(ages)))
  }

## hack to plot doh, jhu processed and raw data together. used in Figure 5
## TODO: add this to plot_cvdat!
plot_dohjhu=function(what=cq(cases,deaths)) {
  what=match.arg(what);
  doh=get(paste(sep='.','doh',what));
  jhu=get(paste(sep='.','jhu',what));
  doh.raw=get(paste(sep='.','doh',what,'raw'));
  jhu.raw=get(paste(sep='.','jhu',what,'raw'));
  data=data_cvdat(list(doh,jhu,doh.raw,jhu.raw),places=cq(state),ages='all',per.capita=TRUE);
  ymax=max(data[,-1],na.rm=TRUE);
  title=paste('Weekly fitted and raw',what,'per million: DOH and JHU (Washington state)');
  plot_cvdat(
    list(doh,jhu),places=cq(state),ages='all',per.capita=TRUE,lwd=2,ymax=ymax,
    title=title,legends=list(title='Data Source',labels=c('DOH','JHU')));
  plot_cvdat(
    list(doh.raw,jhu.raw),places=cq(state),ages='all',per.capita=TRUE,lwd=2,
    title=title,type='p',add=TRUE,pch=c(20,20));
}

## make objs doc_updat needs
## NG 21-02-17: oops. still need transform pipeline for old versions
##   calling function (doc_updat) sets trnsforms if needed
## NG 21-02-09: abandoned the cute transform pipeline
##   too many exceptions and with only two sources, not worth the trouble
## NG 20-12-14: fixed longstanding bug. have to do 'extra' before 'editing' object to create
##   new places or ages, else objects will be incompatible.
##   bug in 'extra' caused error to be missed and results of edited places and ages to be 0!
## as of 21-03-28, okay to fo 'edit' before 'extra'
make_updat_objs=
  function(what=cq(cases,deaths),datasrc=cq(doh,jhu),version='latest') {
    datasrc=match.arg(datasrc,several.ok=TRUE);
    cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
    withrows(cases,case,{
      if (param(verbose)) print(paste('+++ making',datasrc,what));
      ## start with raw. really 'weekly, incremental'
      obj=raw(what,datasrc,version);    # start with raw
      obj=switch(datasrc,               # transform as needed for src
                 doh=edit(obj,KEEP=cq(state,King,Snohomish,Pierce)),
                 jhu=weekly(incremental(obj)));
      assign(paste(sep='.',datasrc,what,'raw'),obj,globalenv());  # save as 'raw'
      if (datasrc=='doh') {
        if (what=='deaths') obj= edit(obj,'0_64'='0_19'+'20_34'+'35_49'+'50_64',
                                      DROP=cq('0_19','20_34','35_49','50_64'));
        obj=extra(obj);
      }
      obj=fit_updat_obj(obj,what);
      assign(paste(sep='.',datasrc,what),obj,globalenv());        # save as 'final'
    });
    cases;
  }
fit_updat_obj=function(obj,what) {
  ## use 1 day for cases, 10.5 days (1.5 weeks) for deaths
  fit.unit=if(what=='cases') 1 else 10.5;
  fit(obj,fit.unit=fit.unit);
}
## save table as txt file
## TODO: rewrite using dotbl when implemented!
save_tbl=function(tbl,tblnum,tblname,sfx=NULL) {
  tblnum=sprintf('%03i',tblnum);
  base=paste(sep='_','table',paste(collapse='',c(tblnum,sfx)),tblname);
  file=filename(param(tbldir),base,suffix='txt')
  write.table(tbl,file=file,sep='\t',quote=FALSE,row.names=FALSE);
  if (param(pjto)) system(paste('pjto',file));           # copy to Mac if desired (usally is)
  file;
}

## show trend results in convenient format. for interactive use
show_trend=
  function(cases=parent(trend.cases),deaths=parent(trend.deaths),pval.cutoff=0.15,do.print=TRUE) {
    if (!is.na(pval.cutoff)) {
      cases=cases[cases$pval<=pval.cutoff,];
      deaths=deaths[deaths$pval<=pval.cutoff,];
    }
    cases$pval=round(cases$pval,digits=3);
    deaths$pval=round(deaths$pval,digits=3);
    cases.bywidth=split(cases,cases$width);
    deaths.bywidth=split(deaths,deaths$width);
    assign_global(cases.bywidth,deaths.bywidth);
    if (do.print) {
      print('cases');
      print(cases.bywidth);
      print('----------');
      print('deaths');
      print(deaths.bywidth);
    }
    invisible(list(cases=cases.bywidth,deaths=deaths.bywidth));
  }
## show counts results in convenient format. for interactive use
show_counts=
  function(cases=parent(counts.cases),deaths=parent(counts.deaths),
           places.wa=parent(places.wa),places.nonwa=parent(places.nonwa),
           peak.cases.wa=c('2020-06-21','2020-08-16'), peak.deaths.wa=c('2020-06-21','2020-09-06'),
           do.print=TRUE) {
    cases.wa=cases[,c('date',places.wa)];
    deaths.wa=deaths[,c('date',places.wa)];
    cases.nonwa=cases[,c('date',places.nonwa)];
    deaths.nonwa=deaths[,c('date',places.nonwa)];
    assign_global(cases.wa,deaths.wa,cases.nonwa,deaths.nonwa);
    if (do.print) {
      print('cases.wa summer peak');
      print(subset(cases.wa,subset=btwn_cc(date,peak.cases.wa[1],peak.cases.wa[2])));
      print('cases.wa now');
      print(tail(cases.wa));
      print('----------');
      print('deaths.wa summer peak');
      print(subset(deaths.wa,subset=btwn_cc(date,peak.deaths.wa[1],peak.deaths.wa[2])));
      print('deaths.wa now');
      print(tail(deaths.wa));
      print('----------');
      print('cases.nonwa now');
      print(tail(cases.nonwa));
      print('deaths.nonwa now');
      print(tail(deaths.nonwa));
    }
    invisible(list(cases.wa=cases.wa,deaths.wa=deaths.wa,
                   cases.nonwa=cases.nonwa,deaths.nonwa=deaths.nonwa));
  }


  
