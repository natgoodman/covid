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
## no sections. only 4 figures
doc_updat=function(need.objs=TRUE,need.init=TRUE,version='latest',do.roll=TRUE,do.extra=NA,...) {
  if (need.objs) make_updat_objs(version=version,do.roll=do.roll,do.extra=do.extra);
  if (need.init) init_doc(doc='updat',version=version,...);
  labels.wa=setNames(c('Washington state','Seattle (King County)',
                       'Snohomish (North of Seattle)','Pierce (South of Seattle)'),
              cq(state,King,Snohomish,Pierce));
  if (param(verbose)) print(paste('+++ making figures'));
  ## Figures 1a-b cases
  figblk_start();
  dofig('cases_wa',
        plot_cvdat(
          jhu.cases,places=cq(state,King,Snohomish,Pierce),
          ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly cases per million in Washington locations"),
          legends=list(labels=labels.wa)));
  dofig('cases_other',
        plot_cvdat(
          jhu.cases,places=cq('Ann Arbor',Boston,'San Diego',DC),
          ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly cases per million in non-Washington locations")));
  ## Figures 2a-b deaths
  figblk_start();
  dofig('deaths_wa',
        plot_cvdat(
          jhu.deaths,places=cq(state,King,Snohomish,Pierce),
          ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly deaths per million in Washington locations"),
          legend='top',
          legends=list(labels=labels.wa)));
  dofig('deaths_other',
        plot_cvdat(
          jhu.deaths,places=cq('Ann Arbor',Boston,'San Diego',DC),
          ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly deaths per million in non-Washington locations")));
  ## Figures 3a-d cases by age
  figblk_start();
  ages=ages_all()[-1];
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

   ## Figures 5a-b compare DOH, JHU
  figblk_start();
  version=version(doh.cases);
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
  } else {
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
  }
    invisible();
}

## make objs doc_updat needs. 'global' controls whether set globally or just in parent
## NG 20-12-14: fixed longstanding bug. have to do 'extra' before 'editing' object to create
##   new places or ages, else objects will be incompatible.
##   bug in 'extra' caused error to be missed and results of edited places and ages to be 0!
make_updat_objs=
  function(what=cq(cases,deaths),datasrc=cq(doh,jhu),version='latest',do.roll=TRUE,do.extra=NA) {
    cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
    ## only 'doh' has 'admits'. prune others
    cases=cases[(cases$what!='admits'|cases$datasrc%in%cq(doh,trk)),]
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
      ## in version 20-12-06, I tried not using roll in 'final' objects
      ## but results were way noisy so I decided to keep it. hence the commented out section
      ## in version 20-12-13, I decided to omit 'extra'
      ##   extrapolation became unacceptably erratic for some reason
      ##   will probably change again next week...
      ## if (version(obj)<'20-12-06') {
      version=version(obj);
      roll.width=if(is.numeric(do.roll)) do.roll else NULL;
      if ((is.logical(do.roll)&&do.roll)||is.numeric(do.roll)) {
        obj=roll(obj,roll.width);                                   #  rolling mean
        assign(paste(sep='.',datasrc,what,'roll'),obj,globalenv()); # save as 'roll'
      }
      if (datasrc=='doh') {
        if (is.na(do.extra)) if (version<='20-12-06') do.extra=TRUE else do.extra=FALSE;
        if (do.extra) obj=extra(obj);
        obj=edit(obj,'0_59'='0_19'+'20_39'+'40_59');
      }
      ## in version 20-12-06, I tried not using roll in 'final' objects
      ## but results were way noisy so I decided to keep it. hence this commented out section
      ## if (datasrc=='doh') obj=extra(obj);
      ##   obj=edit(obj,'0_59'='0_19'+'20_39'+'40_59');
      assign(paste(sep='.',datasrc,what),obj,globalenv());        # save as 'final'
    });
    cases;
  }

