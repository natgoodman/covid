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
  if (is.null(transforms))
    transforms=if (version<'20-12-20') list(jhu=roll,doh=c(roll,extra))
               else if (version=='20-12-20') list(jhu=roll)
               else list(jhu=fit_updat_objs,doh=c(extra,fit_updat_objs));                      
  if (need.objs) {
    make_updat_objs(version=version,transforms=transforms);
  }
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
  ## version 20-12-20: DOH update not available. hopefully temporary...
  if (version=='20-12-20') return();
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

  ## Figures 5a-b compare DOH, JHU.
  ## Note: not used in versions after Dec 13. Will probably come back...
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
  }
  else {
    dofig('cases_dohjhu',fig_dohjhu('cases'));
    dofig('deaths_dohjhu',fig_dohjhu('deaths'));
  }
  
    invisible();
}
## hack to plot processed and raw data together
## TODO: add this  plot_cvdat!
fig_dohjhu=function(what=cq(cases,deaths)) {
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

## make objs doc_updat needs. 'global' controls whether set globally or just in parent
## NG 20-12-14: fixed longstanding bug. have to do 'extra' before 'editing' object to create
##   new places or ages, else objects will be incompatible.
##   bug in 'extra' caused error to be missed and results of edited places and ages to be 0!
make_updat_objs=
  function(what=cq(cases,deaths),datasrc=cq(doh,jhu),version='latest',
           transforms=list(jhu=fit_updat_objs,doh=c(extra,fit_updat_objs))) {
    datasrc=datasrc%&%names(transforms);
    ## R needs each transform entry to be a list for sapply below to work. sigh...
    transforms=lapply(transforms,function(t) if (length(t)==1) list(t) else t);
    cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
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
      sapply(transforms[[datasrc]],function(f) obj<<-f(obj))
      ## do final edit for doh
      if (datasrc=='doh') obj=edit(obj,'0_59'='0_19'+'20_39'+'40_59');
      assign(paste(sep='.',datasrc,what),obj,globalenv());        # save as 'final'
    });
    cases;
  }
fit_updat_objs=function(obj) {
  ## use 1 day for cases, 10.5 days (1.5 weeks) for deaths
  fit.unit=if(what(obj)=='cases') 1 else 10.5;
  fit(obj,fit.unit=fit.unit);
}
