#################################################################################
##
## Author:  Nat Goodman
## Created: 20-11-15
##          from misig/doc_confi.R created 19-07-16
##          from misig/confi.R created 19-07-04
##
## Copyright (C) 2019 Nat Goodman.
## 
## Generate figures and tables for updat (weekly data update) document
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## --- Generate Figures and Tables for updat Blog Post ---
## no sections.
doc_updat=function(need.objs=TRUE,need.init=TRUE,version='latest',transforms=NULL,...) {
  if (is.null(version)||version=='latest') version=max(sapply(cq(jhu,doh),latest_version));
  if (need.objs) {
    if (is.null(transforms))
      transforms=if (version<'20-12-20') list(jhu=roll,doh=c(roll,extra))
                 else if (version=='20-12-20') list(jhu=roll)
                 else if (version<'21-02-07') list(jhu=fit_updat_objs,doh=c(extra,fit_updat_objs));
    make_updat_objs(version=version,transforms=transforms);
  }
  if (need.init) init_doc(doc='updat',version=version,...);
  places.wa=cq(state,King,Snohomish,Pierce);
  labels.wa=setNames(c('Washington state','Seattle (King County)',
                       'Snohomish (North of Seattle)','Pierce (South of Seattle)'),
                     places.wa);
  places.nonwa=cq('Ann Arbor',Boston,'San Diego',DC);
  labels.nonwa=setNames(c('Ann Arbor','Boston','San Diego','Washington DC'),places.nonwa);
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
  if (version>='21-01-24') {
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
  if (version>='21-01-24') {
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
  ## version 20-12-20: DOH update not available. hopefully temporary...
  if (version=='20-12-20') return();
  ## starting 21-02-14. I only show statewise by-age results. other place similar
  if (version<'21-02-14') {
    ## Figures 3a-d cases by age
    figblk_start();
    ages=c('0_19','20_39','40_59','60_79','80_');
    ##  col=rev(col_brew(6,'viridis'))[-1];
    col=col_brew(5,'d3');
    sapply(cq(state,King,Snohomish,Pierce),function(place) 
      dofig(paste(sep='_','cases',place),
            plot_cvdat(
              doh.cases,places=place,ages=ages,per.capita=TRUE,lwd=2,col=col,
              title=figtitle(paste("Weekly cases per million by age in",labels.wa[place])))));
    ## Figures 4a-d deaths
    figblk_start();
    ages=c('0_59','60_79','80_');
    col=col[c(1,4,5)];
    sapply(cq(state,King,Snohomish,Pierce),function(place) 
      dofig(paste(sep='_','deaths',place),
            plot_cvdat(
              doh.deaths,places=place,ages=ages,per.capita=TRUE,lwd=2,col=col,
              title=figtitle(paste("Weekly deaths per million by age in",labels.wa[place])),
              legend='top')));
  } else {
    figblk_end();
    ##  col=rev(col_brew(6,'viridis'))[-1];
    col=col_brew(5,'d3');
    place='state';
    ## Figure 3 statewide cases by age
    ages=c('0_19','20_39','40_59','60_79','80_');
    dofig(paste(sep='_','cases',place),
          plot_cvdat(
            doh.cases,places=place,ages=ages,per.capita=TRUE,lwd=2,col=col,
            title=figtitle(paste("Weekly cases per million by age in",labels.wa[place]))));
    ## Figure 4 statewide deaths by age
    ages=c('0_59','60_79','80_');
    dofig(paste(sep='_','deaths',place),
          plot_cvdat(
            doh.deaths,places=place,ages=ages,per.capita=TRUE,lwd=2,col=col,
            title=figtitle(paste("Weekly deaths per million by age in",labels.wa[place]))));
  }
  ## Figures 5a-b compare DOH, JHU.
  ## Note: not used in versions after Dec 13. Will probably come back...
  figblk_start();
  if (version<='20-12-06') {
    dofig('cases_dohjhu',
          plot_cvdat(
            list(doh.cases.roll,doh.cases,jhu.cases),
            places=cq(state),ages='all',per.capita=TRUE,lwd=c(4,2,2),
            title=figtitle("Weekly cases per million: DOH and JHU (Washington state)"),
            legends=list(title='Data Source',labels=c('DOH raw','DOH extrapolated','JHU'))));
    dofig('deaths_dohjhu',
          plot_cvdat(
            list(doh.deaths.roll,doh.deaths,jhu.deaths),
            places=cq(state),ages='all',per.capita=TRUE,lwd=c(4,2,2),
            title=figtitle("Weekly deaths per million: DOH and JHU (Washington state)"),
            legends=list(title='Data Source',labels=c('DOH raw','DOH extrapolated','JHU'))));
  } else if (version<='20-12-27') {
    dofig('cases_dohjhu',
          plot_cvdat(
            list(doh.cases,jhu.cases),
            places=cq(state),ages='all',per.capita=TRUE,lwd=2,
            title=figtitle("Weekly cases per million: DOH and JHU (Washington state)"),
            legends=list(title='Data Source',labels=c('DOH','JHU'))));
    dofig('deaths_dohjhu',
          plot_cvdat(
            list(doh.deaths,jhu.deaths),
            places=cq(state),ages='all',per.capita=TRUE,lwd=2,
            title=figtitle("Weekly deaths per million: DOH and JHU (Washington state)"),
            legends=list(title='Data Source',labels=c('DOH','JHU'))));
  } else {
    dofig('cases_dohjhu',plot_dohjhu('cases'));
    dofig('deaths_dohjhu',plot_dohjhu('deaths'));
  }
  ## Tables 1-2 trend analysis. Tables 3-4 raw data counts. not used in document.
  if (version>='21-02-07') {
    if (param(verbose)) print(paste('+++ making tables'));
    trend.cases=trend(jhu.cases.raw,places=c(places.wa,places.nonwa));
    trend.deaths=trend(jhu.deaths.raw,places=c(places.wa,places.nonwa));
    counts.cases=data_cvdat(jhu.cases.raw,places=c(places.wa,places.nonwa),per.capita=TRUE);
    counts.deaths=data_cvdat(jhu.deaths.raw,places=c(places.wa,places.nonwa),per.capita=TRUE);
    ## TODO: rewrite using dotbl when implemented!
    ## dotbl('trend_cases',emit_trend,data=trend.cases,title='Trend analysis for cases');
    ## dotbl('trend_deaths',emit_trend,data=trend.deaths,title='Trend analysis for deaths');
    param(tbldir,pjto);
    ## Tables 1-2 trend analysis
    file=file.path(tbldir,'table_001_trend_cases.txt');
    write.table(trend.cases,file=file,sep='\t',quote=F,row.names=F);
    if (pjto) system(paste('pjto',file));           # copy to Mac if desired (usally is)
    file=file.path(tbldir,'table_002_trend_deaths.txt');
    write.table(trend.deaths,file=file,sep='\t',quote=F,row.names=F);
    if (pjto) system(paste('pjto',file));           # copy to Mac if desired (usally is)
    ## Tables 3-4 raw data counts
    file=file.path(tbldir,'table_003_counts_cases.txt');
    write.table(counts.cases,file=file,sep='\t',quote=F,row.names=F);
    if (pjto) system(paste('pjto',file));           # copy to Mac if desired (usally is)
    file=file.path(tbldir,'table_003_counts_deaths.txt');
    write.table(counts.deaths,file=file,sep='\t',quote=F,row.names=F);
    if (pjto) system(paste('pjto',file));           # copy to Mac if desired (usally is)
  }
  invisible();
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
make_updat_objs=
  function(what=cq(cases,deaths),datasrc=cq(doh,jhu),version='latest',transforms=NULL) {
    cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
    if (is.null(transforms)) {
      withrows(cases,case,{
        if (param(verbose)) print(paste('+++ making',datasrc,what));
        ## start with raw. really 'weekly, incremental'
        obj=raw(what,datasrc,version);    # start with raw
        obj=switch(datasrc,               # transform as needed for src
                   doh=doh_updat_objs(obj,what),
                   jhu=jhu_updat_objs(obj,what));
        assign(paste(sep='.',datasrc,what),obj,globalenv())        # save as 'final'
      });
    } else {
      ## R needs each transform entry to be a list for sapply below to work. sigh...
      transforms=lapply(transforms,function(t) if (length(t)==1) list(t) else t);
      withrows(cases,case,{
        if (param(verbose)) print(paste('+++ making',datasrc,what));
        ## start with raw. really 'weekly, incremental'
        obj=raw(what,datasrc,version);    # start with raw
        obj=switch(datasrc,               # transform as needed for src
                   doh=edit(obj,KEEP=cq(state,King,Snohomish,Pierce)),
                   jhu=weekly(incremental(obj)),
                   nyt=weekly(incremental(obj)),
                   trk=weekly(obj));
        assign(paste(sep='.',datasrc,what,'raw'),obj,globalenv());  # save as 'raw'
        ## now work down transforms
        if (datasrc=='doh'&&version%in%c('21-01-24','21-01-31')) {
          ## doh 21-01-24, 21-01-31 have problems... not exactly the same natch...
          ## do transforms explicitly
          ## in 21-01-24, data before 2020-01-26 is too high, eg, 2020-01-12 has 8779 cases!
          ## in 21-01-31, data before 2020-02-02 is too high, eg, 2020-01-12 has 10331 cases!
          min.date=if(version=='21-01-24') '2020-01-26' else '2020-02-02'
          obj=edit(obj,date>=min.date);
          assign(paste(sep='.',datasrc,what,'raw'),obj,globalenv());  # save as 'raw'
          ## must do 'extra' before editing out last weeks, else code crashes
          obj=extra(obj); 
          ## both versions missing data after 21-01-10!
          ##   21-01-24, should have week of 21-01-17 but ends at 21-01-10
          ##   21-01-31, should have week of 21-01-24 but ends at 21-01-10
          obj=edit(obj,date<='2021-01-10');
          ## edit out last weeks before doing fit, else 0s at end pull fit down
          obj=fit_updat_objs(obj);
        } else 
          sapply(transforms[[datasrc]],function(f) obj<<-f(obj));
        ## do final edit for doh
        if (datasrc=='doh') obj=edit(obj,'0_59'='0_19'+'20_39'+'40_59');
        assign(paste(sep='.',datasrc,what),obj,globalenv());        # save as 'final'
      });
    }
    cases;
  }
## hard-coded 'updat_objs' functions only used for 21-02-07 and later
## controlled in doc_updat above by setting transforms or leaving it NULL
doh_updat_objs=function(obj,what) {
  version=version(obj);
  if (version<'21-02-07')
    stop("Bad news: 'doh_updat_objs' only handles versions 21-02-07 and later, not ",
         version,". Should have been caught sooner");
  obj=edit(obj,KEEP=cq(state,King,Snohomish,Pierce));
  assign(paste(sep='.','doh',what,'raw'),obj,globalenv()); 
  obj=extra(obj);
  assign(paste(sep='.','doh',what,'extra'),obj,globalenv());
  obj=fit_updat_objs(obj,what);
  if (what=='deaths') obj=edit(obj,'0_59'='0_19'+'20_39'+'40_59');
  assign(paste(sep='.','doh',what),obj,globalenv());        # save as 'final'
  obj;
}
jhu_updat_objs=function(obj,what) {
  version=version(obj);
  if (version<'21-02-07')
    stop("Bad news: 'jhu_updat_objs' only handles versions 21-02-07 and later, not ",
         version,". Should have been caught sooner");
  obj=weekly(incremental(obj));
  assign(paste(sep='.','jhu',what,'raw'),obj,globalenv());
  obj=fit_updat_objs(obj,what);
  assign(paste(sep='.','jhu',what),obj,globalenv());        # save as 'final'
  obj;
}

fit_updat_objs=function(obj,what) {
  ## use 1 day for cases, 10.5 days (1.5 weeks) for deaths
  fit.unit=if(what=='cases') 1 else 10.5;
  fit(obj,fit.unit=fit.unit);
}
