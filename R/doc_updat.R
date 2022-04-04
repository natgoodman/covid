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
library(pryr);
## --- Generate Figures and Tables for updat Blog Post ---
## In past, I kept code for all previous versions. This got completely out of hand.
## Starting with this version, I only have for the current version.
## Previous versions are available in GitHub, of course.
## as of Jan 10 2022, DOH doesn't provide download file...
## no sections.
doc_updat=function(doc='updat',need.objs=TRUE,need.init=TRUE,need.vars=TRUE,version='latest',
                   do.extra=FALSE,figs.all=TRUE,do.fig=TRUE,do.tbl=TRUE,do.USA=TRUE,
                   xmin.ragged=12,...) {
  ## what=cq(cases,admits,deaths);
  ## datasrc=cq(doh,jhu);
  what=cq(cases,deaths);
  datasrc=cq(jhu);
  if (is.null(version)||version=='latest') version=max(sapply(datasrc,latest_version));
  if (param(verbose)) print(paste('+++ doc_updat',nv(version)));
  if (need.init) init_doc(doc=doc,version=version,...);
  if (need.objs) make_updat_objs(what=what,datasrc=datasrc,version=version,do.extra=do.extra);
  ## if (is.null(xmin.ragged)&&version>='21-05-30') xmin.ragged='2021-02-15';
  if (is.numeric(xmin.ragged)) xmin.ragged=as_date(version)-(xmin.ragged*7)
  else if (is.null(xmin.ragged)) {
    if (btwn_co(version,'21-05-30','21-07-11')) xmin.ragged='2021-02-14'
    else if (btwn_co(version,'21-07-11','21-08-22')) xmin.ragged='2021-05-02'
    else if (btwn_co(version,'21-08-22','21-09-12')) xmin.ragged='2021-06-06'
    else xmin.ragged='2021-06-13';
    ## graphs look nicer when xmin aligned with version. 
    xmin.ragged=sunday_week(as_date(xmin.ragged));
  }
  places.wa<<-cq(state,King,Snohomish,Pierce);
  labels.wa=setNames(c('Washington state','Seattle (King County)',
                       'Snohomish (North of Seattle)','Pierce (South of Seattle)'),
                     places.wa);
  places.nonwa<<-cq('Ann Arbor',Boston,'San Diego',DC);
  labels.nonwa=setNames(c('Ann Arbor','Boston','San Diego','Washington DC'),places.nonwa);
  if (do.USA) places.nonwa<<-c(places.nonwa,'USA');
  if (need.vars) {
    ## make variable used for tables and interactive analysis
    if (param(verbose)) print(paste('+++ making vars'));
    widths=if(version<'21-02-28') 4 else if(version<'21-12-26') c(4,6,8) else c(4,6,8,10,12);
    widths.dly=7*(1:6);
    trend.cases=trend.cases.std=trend(jhu.cases,places=c(places.wa,places.nonwa),widths=widths);
    trend.deaths=trend.deaths.std=trend(jhu.deaths,places=c(places.wa,places.nonwa),widths=widths);
    trend.cases.raw=trend(jhu.cases.raw,places=c(places.wa,places.nonwa),widths=widths);
    trend.deaths.raw=trend(jhu.deaths.raw,places=c(places.wa,places.nonwa),widths=widths);
    trend.cases.dly=trend(jhu.cases.dly,places=c(places.wa,places.nonwa),widths=widths.dly);
    trend.deaths.dly=trend(jhu.deaths.dly,places=c(places.wa,places.nonwa),widths=widths.dly);
    assign_global(
      trend.cases,trend.deaths,trend.cases.std,trend.deaths.std,
      trend.cases.raw,trend.deaths.raw,trend.cases.dly,trend.deaths.dly);
    counts.cases=counts.cases.std=
      data_cvdat(jhu.cases,places=c(places.wa,places.nonwa),per.capita=TRUE);
    counts.deaths=counts.deaths.std=
      data_cvdat(jhu.deaths,places=c(places.wa,places.nonwa),per.capita=TRUE);
    counts.cases.raw=data_cvdat(jhu.cases.raw,places=c(places.wa,places.nonwa),per.capita=TRUE);
    counts.deaths.raw=data_cvdat(jhu.deaths.raw,places=c(places.wa,places.nonwa),per.capita=TRUE);
    counts.cases.dly=data_cvdat(jhu.cases.dly,places=c(places.wa,places.nonwa),per.capita=TRUE);
    counts.deaths.dly=data_cvdat(jhu.deaths.dly,places=c(places.wa,places.nonwa),per.capita=TRUE);
    assign_global(
      counts.cases,counts.deaths,counts.cases.std,counts.deaths.std,
      counts.cases.raw,counts.deaths.raw,
      counts.cases.dly,counts.deaths.dly); 
    ## cmp.doh.cases=cmp_doh(what='cases');
    ## cmp.doh.admits=cmp_doh(what='admits');
    ## cmp.doh.deaths=cmp_doh(what='deaths');
    ## rat.doh.cases=cmp_doh_ratio(cmp.doh.cases);
    ## rat.doh.admits=cmp_doh_ratio(cmp.doh.admits);
    ## rat.doh.deaths=cmp_doh_ratio(cmp.doh.deaths);
    ## assign_global(
    ##   cmp.doh.cases,cmp.doh.admits,cmp.doh.deaths,
    ##   rat.doh.cases,rat.doh.admits,rat.doh.deaths);
  }
  if (do.tbl) {
    ## Tables 1-2 trend analysis.
    if (param(verbose)) print(paste('+++ making tables'));
    tblblk_start();
    dotbl('trend_cases',trend.cases);
    dotbl('trend_cases_raw',trend.cases.raw);
    dotbl('trend_cases_dly',trend.cases.dly);
    tblblk_start();
    dotbl('trend_deaths',trend.deaths);
    dotbl('trend_deaths_raw',trend.deaths.raw);
    dotbl('trend_deaths_dly',trend.deaths.dly);
    ## Tables 3-4 data counts.
    tblblk_start();
    dotbl('counts_cases',counts.cases);
    dotbl('counts_cases_raw',counts.cases.raw);
    dotbl('counts_cases_dly',counts.cases.dly);
    tblblk_start();
    dotbl('counts_deaths',counts.deaths);
    dotbl('counts_deaths_raw',counts.deaths.raw);
    dotbl('counts_deaths_dly',counts.deaths.dly);
    ## ## Tables 5-7 DOH comparisons
    ## tblblk_start();
    ## dotbl('cmp_doh_cases',cmp.doh.cases,row.names='place');
    ## dotbl('cmp_doh_admits',cmp.doh.admits,row.names='place');
    ## dotbl('cmp_doh_deaths',cmp.doh.deaths,row.names='place');
    ## tblblk_start();
    ## dotbl('rat_doh_cases',rat.doh.cases,row.names='place');
    ## dotbl('rat_doh_admits',rat.doh.admits,row.names='place');
    ## dotbl('rat_doh_deaths',rat.doh.deaths,row.names='place');
  }
  if (!do.fig) return(version);
  if (param(verbose)) print(paste('+++ making figures'));
  ## REAL HACK: remove 'USA' from places.nonwa for figures 1, 2
  places.nonwa=places.nonwa%-%'USA';
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
  ## recent raw data very ragged in some versions. include figures showing this
  ## first compute ymax across all data of interest
  ## NG 21-12-28. DO NOT use same y-scale for ragged figures
  ##   nonwa counts so high now that this obscures shape of wa graph
  ## data=data_cvdat(list(jhu.cases,jhu.cases.raw),
  ##                 places=c(places.wa,places.nonwa),per.capita=TRUE);
  ## data=data[data$date>=xmin.ragged,];
  ## ymax=max(data[,-1],na.rm=TRUE);
  ymax='auto';
  dofig('cases_wa_ragged',
        plot_finraw(
          datasrc='jhu',what='cases',places=places.wa,ages='all',per.capita=TRUE,lwd=2,
          xmin=xmin.ragged,ymax=ymax,
          title=figtitle(
            "Weekly cases per million in Washington locations showing recent raw data"),
          where.legend='topleft',legends=list(labels=labels.wa)));
  dofig('cases_nonwa_ragged',
        plot_finraw(
          datasrc='jhu',what='cases',places=places.nonwa,ages='all',per.capita=TRUE,lwd=2,
          xmin=xmin.ragged,ymax=ymax,
          title=figtitle(
            "Weekly cases per million in non-Washington locations showing recent raw data"),
          where.legend='topleft',legends=list(labels=labels.nonwa)));
  ## Figures 2a-b deaths
  figblk_start();
  dofig('deaths_wa',
        plot_cvdat(
          jhu.deaths,places=places.wa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly deaths per million in Washington locations"),
           where.legend='topleft',legends=list(labels=labels.wa)));
  dofig('deaths_nonwa',
        plot_cvdat(
          jhu.deaths,places=places.nonwa,
          ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly deaths per million in non-Washington locations"),
          where.legend='topright',legends=list(labels=labels.nonwa)));
  ## recent raw data very ragged in some versions. include figures showing this
  ## first compute ymax across all data of interest
  data=
    data_cvdat(list(jhu.deaths,jhu.deaths.raw),places=c(places.wa,places.nonwa),per.capita=TRUE);
  data=data[data$date>=xmin.ragged,];
  ymax=max(data[,-1],na.rm=TRUE);
  dofig('deaths_wa_ragged',
        plot_finraw(
          datasrc='jhu',what='deaths',places=places.wa,ages='all',per.capita=TRUE,lwd=2,
          xmin=xmin.ragged,ymax=ymax,
          title=figtitle(
            "Weekly deaths per million in Washington locations showing recent raw data"),
          where.legend='topleft',legends=list(labels=labels.wa)));
  dofig('deaths_nonwa_ragged',
        plot_finraw(
          datasrc='jhu',what='deaths',places=places.nonwa,ages='all',per.capita=TRUE,lwd=2,
          xmin=xmin.ragged,ymax=ymax,
          title=figtitle(
            "Weekly deaths per million in non-Washington locations showing recent raw data"),
           where.legend='topleft',legends=list(labels=labels.nonwa)));
  if (do.USA) {
    ## next block include USA
    figblk_start();
    places.usa<<-cq(state,USA);
    labels.usa=setNames(c('Washington state','USA'),places.usa);
    dofig('cases_usa',
        plot_cvdat(
          jhu.cases,places=places.usa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly cases per million in USA and Washington"),
          legends=list(labels=labels.usa)));
    dofig('deaths_usa',
        plot_cvdat(
          jhu.deaths,places=places.usa,ages='all',per.capita=TRUE,lwd=2,
          title=figtitle("Weekly deaths per million in USA and Washington"),
          legends=list(labels=labels.usa)));
    ## ragged graphs. not used in document
    dofig('cases_usa_ragged',
        plot_finraw(
          datasrc='jhu',what='cases',places=places.usa,ages='all',per.capita=TRUE,lwd=2,
          xmin=xmin.ragged,
          title=figtitle(
            "Weekly cases per million in USA and Washington showing recent raw data"),
          where.legend='topleft',legends=list(labels=labels.usa)));
    dofig('deaths_usa_ragged',
        plot_finraw(
          datasrc='jhu',what='deaths',places=places.usa,ages='all',per.capita=TRUE,lwd=2,
          xmin=xmin.ragged,
          title=figtitle(
            "Weekly deaths per million in USA and Washington showing recent raw data"),
          where.legend='topleft',legends=list(labels=labels.usa)));
  }
  if ('doh'%notin%datasrc) return();
  ## next two blocks: WA DOH cases, admits&deaths by age
  ages=ages_doh();
  col=col_ages(ages=ages);
  if (figs.all) {
    ## Figures 3a-d, 4a-d WA DOH cases, admits&deaths by age for all locations
    ## not always in document, but do 'em so I can check to decide whether to include
    fignum.sav=param(fignum);           # to restore after doing a-d figs
    ## Figures 3a-d cases by age
    figblk_start();
    data=data_cvdat(doh.cases,places=places.wa,ages=ages,per.capita=TRUE);
    ymax=max(data[,-1],na.rm=TRUE);
    sapply(places.wa,function(place) 
      dofig(paste(sep='_','cases',place),
            plot_cvdat(
              doh.cases,places=place,ages=ages,col=col,
              per.capita=TRUE,lwd=2,ymax=ymax,
              title=figtitle(paste("Weekly cases per million by age in",labels.wa[place])))));
    ## Figures 4a-d admits&deaths, deaths by age
    figblk_start();
    data=data_cvdat(list(doh.admits,doh.deaths),places=places.wa,ages=ages,per.capita=TRUE);
    ymax=max(data[,-1],na.rm=TRUE);
    sapply(places.wa,function(place) 
      dofig(paste(sep='_','admits_deaths',place),
            plot_admdea(
              places=place,ages=ages,col=col,per.capita=TRUE,ymax=ymax,
              title=figtitle(paste("Weekly admits and deaths per million by age in",
                                   labels.wa[place])))));
    param(fignum=fignum.sav);           # restore fignum after a-d figs
  }
  ## Figures 3 WA DOH cases by age for state
  figblk_end();
  place='state';
  dofig(paste(sep='_','cases',place),
        plot_cvdat(
          doh.cases,places=place,ages=ages,col=col,per.capita=TRUE,lwd=2,
          title=figtitle(paste("Weekly cases per million by age in",labels.wa[place]))));
  ## Figures 4 WA DOH admits&deaths by age for state
  ## NOTE: admits broken in version 21-06-20. hopefully temporary...
  dofig(paste(sep='_','admits_deaths',place),
        plot_admdea(
          places=place,ages=ages,col=col,per.capita=TRUE,
          title=figtitle(paste("Weekly admits and deaths per million by age in",
                               labels.wa[place]))));
  ## Figures 5a-b compare DOH, JHU.
  ## Note: not used in versions after Dec 13, 2020 until Jun 2, 2021. might go away again...
  ##if (figs.all) {
  figblk_start();
  dofig('cases_dohjhu',
        plot_dohjhu(
          'cases',
          title=
            figtitle('Weekly fitted and raw cases per million: DOH and JHU (Washington state)')));
  dofig('deaths_dohjhu',
        plot_dohjhu(
          'deaths',
          title=
            figtitle('Weekly fitted and raw deaths per million: DOH and JHU (Washington state)')));
  ## end of figures
  version;
}

##### NG 21-11-19: functions moved to docfun_updat.R #####
