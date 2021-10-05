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
doc_updat=function(doc='updat',need.objs=TRUE,need.init=TRUE,version='latest',
                   figs.all=TRUE,do.fig=TRUE,do.tbl=TRUE,xmin.ragged=12,...) {
  what=cq(cases,admits,deaths);
  datasrc=cq(doh,jhu);
  if (is.null(version)||version=='latest') version=max(sapply(datasrc,latest_version));
  if (param(verbose)) print(paste('+++ doc_update',nv(version)));
  if (need.objs) make_updat_objs(what=what,datasrc=datasrc,version=version);
  if (need.init) init_doc(doc=doc,version=version,...);
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
  if (do.tbl) {
    ## Tables 1-2 trend analysis.
    if (param(verbose)) print(paste('+++ making tables'));
    widths=if(version<'21-02-28') 4 else c(4,6,8);
    widths.dly=7*(1:6);
    trend.cases=trend.cases.std=trend(jhu.cases,places=c(places.wa,places.nonwa),widths=widths);
    trend.deaths=trend.deaths.std=trend(jhu.deaths,places=c(places.wa,places.nonwa),widths=widths);
    trend.cases.raw=trend(jhu.cases.raw,places=c(places.wa,places.nonwa),widths=widths);
    trend.deaths.raw=trend(jhu.deaths.raw,places=c(places.wa,places.nonwa),widths=widths);
    trend.cases.dly=trend(jhu.cases.dly,places=c(places.wa,places.nonwa),widths=widths.dly);
    trend.deaths.dly=trend(jhu.deaths.dly,places=c(places.wa,places.nonwa),widths=widths.dly);
    tblblk_start();
    dotbl('trend_cases',trend.cases);
    dotbl('trend_cases_raw',trend.cases.raw);
    dotbl('trend_cases_dly',trend.cases.dly);
    tblblk_start();
    dotbl('trend_deaths',trend.deaths);
    dotbl('trend_deaths_raw',trend.deaths.raw);
    dotbl('trend_deaths_dly',trend.deaths.dly);
    assign_global(
      trend.cases,trend.deaths,trend.cases.std,trend.deaths.std,
      trend.cases.raw,trend.deaths.raw,trend.cases.dly,trend.deaths.dly);
   ## Tables 3-4 data counts.
    counts.cases=counts.cases.std=
      data_cvdat(jhu.cases,places=c(places.wa,places.nonwa),per.capita=TRUE);
    counts.deaths=counts.deaths.std=
      data_cvdat(jhu.deaths,places=c(places.wa,places.nonwa),per.capita=TRUE);
    counts.cases.raw=data_cvdat(jhu.cases.raw,places=c(places.wa,places.nonwa),per.capita=TRUE);
    counts.deaths.raw=data_cvdat(jhu.deaths.raw,places=c(places.wa,places.nonwa),per.capita=TRUE);
    counts.cases.dly=data_cvdat(jhu.cases.dly,places=c(places.wa,places.nonwa),per.capita=TRUE);
    counts.deaths.dly=data_cvdat(jhu.deaths.dly,places=c(places.wa,places.nonwa),per.capita=TRUE);
    tblblk_start();
    dotbl('counts_cases',counts.cases);
    dotbl('counts_cases_raw',counts.cases.raw);
    dotbl('counts_cases_dly',counts.cases.dly);
    tblblk_start();
    dotbl('counts_deaths',counts.deaths);
    dotbl('counts_deaths_raw',counts.deaths.raw);
    dotbl('counts_deaths_dly',counts.deaths.dly);
    assign_global(
      counts.cases,counts.deaths,counts.cases.std,counts.deaths.std,
      counts.cases.raw,counts.deaths.raw,
      counts.cases.dly,counts.deaths.dly);
    ## Tables 5-7 DOH comparisons
    cmp.doh.cases=cmp_doh(what='cases');
    cmp.doh.admits=cmp_doh(what='admits');
    cmp.doh.deaths=cmp_doh(what='deaths');
    rat.doh.cases=cmp_doh_ratio(cmp.doh.cases);
    rat.doh.admits=cmp_doh_ratio(cmp.doh.admits);
    rat.doh.deaths=cmp_doh_ratio(cmp.doh.deaths);
    tblblk_start();
    dotbl('cmp_doh_cases',cmp.doh.cases)
    dotbl('cmp_doh_admits',cmp.doh.admits)
    dotbl('cmp_doh_deaths',cmp.doh.deaths)
    tblblk_start();
    dotbl('rat_doh_cases',rat.doh.cases)
    dotbl('rat_doh_admits',rat.doh.admits)
    dotbl('rat_doh_deaths',rat.doh.deaths)
    assign_global(
      cmp.doh.cases,cmp.doh.admits,cmp.doh.deaths,
      rat.doh.cases,rat.doh.admits,rat.doh.deaths);
  }
  if (!do.fig) return(version);
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
  ## recent raw data very ragged in some versions. include figures showing this
  ## first compute ymax across all data of interest
  data=data_cvdat(list(jhu.cases,jhu.cases.raw),places=c(places.wa,places.nonwa),per.capita=TRUE);
  data=data[data$date>=xmin.ragged,];
  ymax=max(data[,-1],na.rm=TRUE);
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
           where.legend='topright',legends=list(labels=labels.wa)));
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
          where.legend='top',legends=list(labels=labels.wa)));
  dofig('deaths_nonwa_ragged',
        plot_finraw(
          datasrc='jhu',what='deaths',places=places.nonwa,ages='all',per.capita=TRUE,lwd=2,
          xmin=xmin.ragged,ymax=ymax,
          title=figtitle(
            "Weekly deaths per million in non-Washington locations showing recent raw data"),
           where.legend='top',legends=list(labels=labels.nonwa)));
  if ('doh'%notin%datasrc) return();
  ## Figures 3,4  WA DOH cases, admits&deaths by age
  ages=sort(ages(doh.cases)%-%'all');
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
## make colors for doh ages. used in Figures 3,4 version 21-04-25 and later
col_ages=
  function(obj=doh.cases.raw,ages=NULL,
           col1.pal='rainbow',skip.beg=2,skip.end=0,col2=cq(grey60,black),col2.n=length(col2)) {
    if (is.null(ages)) ages=sort(ages(obj)%-%'all');
    col2=setNames(col2,tail(ages,n=col2.n));
    col1=col_brew(head(ages,n=-col2.n),col1.pal,skip.beg=skip.beg,skip.end=skip.end);
    col=c(col1,col2);
    ## col=rep(col,each=2);
    col;
  }
## hack to plot final (processed) and raw data together
## used for jhu in Figures 1,2 version 21-01-24
## TODO: add this to plot_cvdat!
plot_finraw=
  function(datasrc=param(datasrc),what=cq(cases,admits,deaths),
           places,ages='all',per.capita=TRUE,per.mort=FALSE,
           title,legends=list(labels=places),where.legend='topright',raw.plot=cq(lines,points),
           xmin=NULL,xmax=NULL,ymin=NULL,ymax='auto',
           xgrid=cq(biweekly,weekly,quadweekly,semimonthly,monthly),xformat='%b-%d',cex.axis=0.75,
           col=NULL,
           lwd=2,lwd.fin=lwd,lwd.raw=0.375*lwd.fin,lty.fin='solid',lty.raw='dotted',pch=20) {
    datasrc=match.arg(datasrc);
    what=match.arg(what);
    if (!is.numeric(xgrid)) xgrid=match.arg(xgrid);
    if (!is.null(raw.plot)) raw.plot=match.arg(raw.plot,several.ok=TRUE);
    fin=get(paste(sep='.',datasrc,what));
    raw=get(paste(sep='.',datasrc,what,'raw'));
    if (ymax=='auto') {
      if (!is.null(xmin)) {
        fin=edit(fin,date>=xmin);
        raw=edit(raw,date>=xmin);
      }
      if (!is.null(xmax)) {
        fin=edit(fin,date<=xmax);
        raw=edit(raw,date<=xmax);
      }
      ymax=NULL;
    }
    if (is.null(ymax)) {
      data=data_cvdat(list(fin,raw),places=places,ages=ages,
                      per.capita=per.capita,per.mort=per.mort);
      ymax=max(data[,-1],na.rm=TRUE);
    }
    plot_cvdat(fin,places=places,ages=ages,per.capita=per.capita,per.mort=per.mort,
               xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,
               xgrid=xgrid,xformat=xformat,cex.axis=cex.axis,
               title=title,legends=legends,where.legend=where.legend,
               col=col,lwd=lwd.fin,lty=lty.fin);
    if ('lines'%in%raw.plot)
      plot_cvdat(
        raw,places=places,ages=ages,per.capita=per.capita,per.mort=per.mort,add=TRUE,
        col=col,lwd=lwd.raw,lty=lty.raw);
    if ('points'%in%raw.plot)
      plot_cvdat(
        raw,places=places,ages=ages,per.capita=per.capita,per.mort=per.mort,add=TRUE,
        col=col,lwd=lwd.raw,lty=lty.raw,type='p',pch=rep(pch,length(places)*length(ages)))
  }
## hack to plot admits and deaths data together
## used for Figure 4 version 21-04-25 and later
## TODO: add this to plot_cvdat!
plot_admdea=
  function(objs=list(doh.admits,doh.deaths),
           places='state',ages=NULL,per.capita=TRUE,title=NULL,ylab=NULL,ymax=NULL,
           where.legend='topleft',
           lwd.admits=2,lwd.deaths=3,lwd=c(lwd.admits,lwd.deaths),
           lty.admits='dotted',lty.deaths='solid',lty=c(lty.admits,lty.deaths),
           col=NULL,
           col1.pal='rainbow',skip.beg=2,skip.end=0,col2=cq(grey60,black),col2.n=length(col2)) {
    if (is.null(ages)) ages=ages(doh.cases.raw)%-%'all';
    if (is.null(title)) 
      title=paste(collapse=' ',
                  c('Weekly admits and deaths',
                    if(per.capita) 'per million' else NULL,
                    'for',places));
    if (is.null(ylab))
      ylab=paste(collapse=' ',
                 c('admits and deaths',if(per.capita) 'per million' else NULL))
    if (is.null(col))
      col=col_ages(ages=ages,col1.pal=col1.pal,skip.beg=skip.beg,skip.end=skip.end,
                   col2=col2,col2.n=col2.n);
    plot_cvdat(objs,places=places,ages=ages,per.capita=per.capita,
               title=title,ylab=ylab,ymax=ymax,where.legend=where.legend,
               lty=lty,lwd=lwd,col=rep(col,each=2),
               legend=list(list(labels=cq(admits,deaths),lty=lty,lwd=lwd,col='black'),
                           list(labels=age_label(ages,fmt='legend'),lty='solid',lwd=2,col=col)));
  }
## hack to plot doh, jhu processed and raw data together. used in Figure 5
## TODO: add this to plot_cvdat!
plot_dohjhu=function(what=cq(cases,deaths),title=NULL) {
  what=match.arg(what);
  doh=get(paste(sep='.','doh',what));
  jhu=get(paste(sep='.','jhu',what));
  doh.raw=get(paste(sep='.','doh',what,'raw'));
  jhu.raw=get(paste(sep='.','jhu',what,'raw'));
  data=data_cvdat(list(doh,jhu,doh.raw,jhu.raw),places=cq(state),ages='all',per.capita=TRUE);
  ymax=max(data[,-1],na.rm=TRUE);
  if (is.null(title))
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
  function(what=cq(cases,admits,deaths),datasrc=cq(doh,jhu),version='latest') {
    what=match.arg(what,several.ok=TRUE);
    datasrc=match.arg(datasrc,several.ok=TRUE);
    cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
    cases=cases[(cases$what!='admits'|cases$datasrc%in%cq(doh)),]; # only 'doh' has 'admits'
    withrows(cases,case,{
      if (param(verbose)) print(paste('+++ making',datasrc,what));
      ## admits broken in version 21-06-20. use previous version
      obj=if(version=='21-06-20'&what=='admits') raw(what,datasrc,version='21-06-13')
          else raw(what,datasrc,version);
      assign(paste(sep='.',datasrc,what,'src'),obj,globalenv());  # save as 'src'
      obj=switch(datasrc,               # transform as needed for src
                 doh=edit(obj,KEEP=cq(state,King,Snohomish,Pierce)),
                 jhu={
                   obj=incremental(obj);
                   assign(paste(sep='.',datasrc,what,'dly'),obj,globalenv());  # save as 'dly'
                   weekly(obj);
                 });
      assign(paste(sep='.',datasrc,what,'raw'),obj,globalenv());  # save as 'raw'
      if (datasrc=='doh') {
        ## if (what=='deaths') obj=edit(obj,'0_64'='0_19'+'20_34'+'35_49'+'50_64',
        ##                               DROP=cq('0_19','20_34','35_49','50_64'));
        obj=extra(obj);
      }
      ## NG 21-07-15: using different fit.units for cases and others was bad idea
      ##   jagged deaths plots actually caused by rounding of per.capita results
      ## obj=fit_updat_obj(obj,what);
      obj=fit(obj);                     # default method=sspline, fit.unit=1
      assign(paste(sep='.',datasrc,what),obj,globalenv());        # save as 'final'
      assign(paste(sep='.',datasrc,what,'std'),obj,globalenv());  # and 'std' 
      assign(paste(sep='.',datasrc,what,'fit'),obj,globalenv());  # and 'fit'
      obj=cumulative(obj);
      assign(paste(sep='.',datasrc,what,'cum'),obj,globalenv());  # save as 'cum'
    });
    cases;
  }
## NG 21-07-15: using different fit.units for cases and others was bad idea
##   jagged deaths plots actually caused by rounding of per.capita results
## fit_updat_obj=function(obj,what) {
##   ## use 1 day for cases, 10.5 days (1.5 weeks) for deaths
##   fit.unit=if(what=='cases') 1 else 10.5;
##   fit(obj,fit.unit=fit.unit);
## }
## remove superflous objects - either because they were created by mistake or to start clean
## if id is set, only removes those objects, else all that fit the pattern
rm_updat_objs=function(what=cq(cases,admits,deaths),datasrc=param(datasrc),
                       id=NULL,rm.std=is.null(id)) {
  what=match.arg(what,several.ok=TRUE);
  datasrc=match.arg(datasrc,several.ok=TRUE);
  if (length(what)==0||length(datasrc)==0) invisible(NULL); # nothing to remove
  names.all=ls(globalenv());
  pat=paste0('^(',paste(collapse='|',datasrc),')','\\.',
             '(',paste(collapse='|',what),')','\\.',
             '(',paste(collapse='|',id),')');
  names1=grep(pat,names.all,value=TRUE);
  rm(list=names1,envir=globalenv());
  if (rm.std) {
    pat=paste0('^(',paste(collapse='|',datasrc),')','\\.',
               '(',paste(collapse='|',what),')$');
    names2=grep(pat,names.all,value=TRUE);
    rm(list=names2,envir=globalenv());
    invisible(c(names1,names2));
  } else invisible(names1);
}

## save table as txt file
## TODO: rewrite using dotbl when implemented!
## save_tbl=function(tbl,tblnum,tblname,sfx=NULL) {
##   tblnum=sprintf('%03i',tblnum);
##   base=paste(sep='_','table',paste(collapse='',c(tblnum,sfx)),tblname);
##   file=filename(param(tbldir),base,suffix='txt')
##   write.table(tbl,file=file,sep='\t',quote=FALSE,row.names=FALSE);
##   if (param(pjto)) system(paste('pjto',file));           # copy to Mac if desired (usally is)
##   file;
## }

## show trend results in convenient format. for interactive use
show_trend=show_trends=
  function(objid=cq(raw,std,dly),where=cq(wa,nonwa),what=cq(cases,deaths),
           cases=NULL,deaths=NULL,
           pval.cutoff=NA,do.print=TRUE) {
    objid=match.arg(objid);
    where=match.arg(where,several.ok=TRUE);
    what=match.arg(what,several.ok=TRUE);
    if (is.null(cases)) cases=get(paste0('trend.cases.',objid),envir=globalenv());
    if (is.null(deaths)) deaths=get(paste0('trend.deaths.',objid),envir=globalenv());
    if (!is.na(pval.cutoff)) {
      cases=cases[cases$pval<=pval.cutoff,];
      deaths=deaths[deaths$pval<=pval.cutoff,];
    }
    cases$pval=round(cases$pval,digits=3);
    deaths$pval=round(deaths$pval,digits=3);
    cases$slope=round(cases$slope,digits=2);
    deaths$slope=round(deaths$slope,digits=2);
    cases.byplace=split(cases,cases$place);
    deaths.byplace=split(deaths,deaths$place);
    assign_global(cases.byplace,deaths.byplace);
    if (do.print) {
      if ('wa'%in%where&&'cases'%in%what) {
        print('cases.wa');
        print(cases.byplace[places.wa]);
        print('----------');
      }
      if ('nonwa'%in%where&&'cases'%in%what) {
        print('cases.nonwa');
        print(cases.byplace[places.nonwa]);
        print('----------');
      }
      if ('wa'%in%where&&'deaths'%in%what) {
        print('deaths.wa');
        print(deaths.byplace[places.wa]);
        print('----------');
      }
      if ('nonwa'%in%where&&'deaths'%in%what) {
        print('deaths.nonwa');
        print(deaths.byplace[places.nonwa]);
      }
    }
    invisible(list(cases=cases.byplace,deaths=deaths.byplace));
  }
## show counts results in convenient format. for interactive use
show_counts=
  function(objid=cq(raw,std,dly),where=cq(wa,nonwa),what=cq(cases,deaths),
           places.wa=parent(places.wa),places.nonwa=parent(places.nonwa),
           cases=NULL,deaths=NULL,
           tail.n=c(3,10),round.digits=0,
           ## ## wa spring 2020 peak dates
           ## do.spring=FALSE,
           ## peak.cases.spring=c('2020-03-15','2020-05-01'),
           ## peak.deaths.spring=c('2020-03-15','2020-06-01'),
           ## ## wa summer 2020 peak dates
           ## do.summer=FALSE,
           ## peak.cases.summer=c('2020-06-21','2020-08-16'),
           ## peak.deaths.summer=c('2020-06-21','2020-09-06'),
           ## cut dates and labels for WA peaks
           do.peaks=TRUE,
           cuts=c('2020-01-26','2020-06-01','2020-09-15','2021-03-01','2021-07-01'),
           labels=cq(spring20,summer20,winter20,spring21,summer21),
           do.print=TRUE) {
    objid=match.arg(objid);
    where=match.arg(where,several.ok=TRUE);
    what=match.arg(what,several.ok=TRUE);
    if (is.null(cases)) cases=get(paste0('counts.cases.',objid),envir=globalenv());
    if (is.null(deaths)) deaths=get(paste0('counts.deaths.',objid),envir=globalenv());
    cases[,-1]=round(cases[,-1],digits=round.digits);
    deaths[,-1]=round(deaths[,-1],digits=round.digits);
    cases.wa=cases[,c('date',places.wa)];
    deaths.wa=deaths[,c('date',places.wa)];
    cases.nonwa=cases[,c('date',places.nonwa)];
    deaths.nonwa=deaths[,c('date',places.nonwa)];
    assign_global(cases.wa,deaths.wa,cases.nonwa,deaths.nonwa);
     if (do.print) {
      if ('wa'%in%where&&'cases'%in%what) {
        if (do.peaks)
          show_peaks(cases.wa,cuts,labels=paste('cases.wa',labels));
        ## if (do.spring)
        ##   show_peak(cases.wa,dates=peak.cases.spring,'cases.wa spring 2020');
        ## if (do.summer)
        ##   show_peak(cases.wa,dates=peak.cases.summer,'cases.wa summer 2020');
        show_now(cases.wa,objid,tail.n,'cases.wa');
        print('----------');
      }
      if ('wa'%in%where&&'deaths'%in%what) {
        if (do.peaks)
          show_peaks(deaths.wa,cuts,labels=paste('deaths.wa',labels));
        ## if (do.spring)
        ##   show_peak(deaths.wa,dates=peak.deaths.spring,'deaths.wa spring 2020');
        ## if (do.summer)
        ##   show_peak(deaths.wa,dates=peak.deaths.summer,'deaths.wa summer 2020');
        show_now(deaths.wa,objid,tail.n,'deaths.wa');
        print('----------');
      }
      if ('nonwa'%in%where&&'cases'%in%what) show_now(cases.nonwa,objid,tail.n,'cases.nonwa');
      if ('nonwa'%in%where&&'deaths'%in%what) show_now(deaths.nonwa,objid,tail.n,'deaths.nonwa');
    }
    invisible(list(cases.wa=cases.wa,deaths.wa=deaths.wa,
                   cases.nonwa=cases.nonwa,deaths.nonwa=deaths.nonwa));
  }
## show_peak obsolete. replaced by show_peaks
## show_peak=function(counts,dates,label=NULL,do.print=TRUE) {
##   if (do.print) print(paste(collapse=' ',c(label,'peak')));
##   peak=subset(counts,subset=btwn_cc(date,dates[1],dates[2]));
##   i=capply(peak[,-1],which.max);
##   peak=peak[i,];
##   smax=capply(peak[,-1],max);
##   peak=rbind(peak,data.frame(date=NA,smax))
##   if (do.print) {
##     print(peak);
##     invisible(peak);
##   } else peak;
## }
show_now=function(counts,objid,tail.n,label='now') {
  if (!grepl('now$',label)) label=paste(label,'now');
  print(label);
  ## print(paste(collapse=' ',c(label,'now')));
  if (objid!='dly') now=tail(subset(counts,subset=weekdays(date)=='Sunday'),n=tail.n[1])
  else now=tail(subset(counts,subset=date<vdate(jhu.cases)),n=tail.n[2]);
  print(now);
  invisible(now);
}
show_peaks=function(counts,cuts,labels) {
  cuts=c(as_date(cuts),Inf);
  cats=cut(counts$date,cuts,right=F,labels=FALSE);
  peaks=split(counts,cats);
  peaks=lapply(seq_along(peaks),function(i) {
    peak=peaks[[i]];
    label=labels[i];
    show_peak1(peak,label);
  });
  names(peaks)=labels;
  invisible(peaks);
}
show_peak1=function(peak,label) {
  print(paste(collapse=' ',c(label,'peak')));
  i=unique(as.vector(capply(peak[,-1],which.max)));
  peak=peak[i,,drop=FALSE];
  smax=capply(peak[,-1,drop=FALSE],max);
  peak=rbind(peak,data.frame(date=NA,smax,check.names=FALSE))
  print(peak);
  invisible(peak);
}
########################################
## show and compare DOH counts in convenient format. for interactive use
## based on show_counts used for JHU
## multiple ages for one place, or multiple places for one age
## default: usual ages for 'state'
show_doh=
  function(objid=cq(std,raw,dly),what=cq(cases,admits,deaths),
           places='state',ages=NULL,obj=NULL,data=NULL,per.capita=TRUE,per.mort=FALSE,
           tail.n=c(6,10),round.digits=0,
           do.peaks=TRUE,do.now=TRUE,do.cmp=TRUE,
           cuts=c('2020-01-26','2020-06-01','2020-09-15','2021-03-01','2021-07-01'),
           labels=cq(spring20,summer20,winter20,spring21,summer21)) {
    objid=match.arg(objid);
    what=match.arg(what,several.ok=FALSE);
    if (is.null(obj)) obj=get(paste(sep='.','doh',what,objid));
    if (is.null(ages)) ages=ages(obj);
    if (length(places)>1&&length(ages)>1)
      stop("Only one of 'places' or 'ages' can have mulitple values");
    if (length(places)==0&&length(ages)==0)
      stop("Both 'places' and 'ages' are empty; nothing to show!");
    if (is.null(data))
      data=data_cvdat(obj,places=places,ages=ages,per.capita=per.capita,per.mort=per.mort);
    data[,-1]=round(data[,-1],digits=round.digits);
    if (length(places)==1) {
      peak.labels=paste(what,places,labels);
      now.label=paste(what,places,'now');
      colnames(data)=c('date',ages);
    } else {
      peak.labels=paste(what,ages,labels);
      now.label=paste(what,ages,'now');
      colnames(data)=c('date',places);
    }
    if (do.peaks) peaks=show_peaks(data,cuts,labels=peak.labels) else peaks=list();
    if (do.now) now=show_now(data,objid,tail.n,label=now.label);
    print('----------');
    invisible(cl(peaks,now=now));
  }
## TODO: these are CRUDE!! do it better
## show data for one date for all places, ages. automates analysis I did for DOH vsn 21-09-29
## NG 21-10-05: now also used for tables
cmp_doh=
  function(objid=cq(std,raw,dly),what=cq(cases,admits,deaths),
           places=NULL,ages=NULL,obj=NULL,per.capita=TRUE,per.mort=FALSE,
           tail.n=1,round.digits=0) {
    objid=match.arg(objid);
    what=match.arg(what,several.ok=FALSE);
    if (is.null(obj)) obj=get(paste(sep='.','doh',what,objid));
    if (is.null(places)) places=places(obj);
    if (is.null(ages)) ages=ages(obj);
    rows=sapply(places,function(place) {
      data=data_cvdat(obj,places=place,ages=ages,per.capita=per.capita,per.mort=per.mort);
      row=tail(data,n=tail.n)[1,,drop=FALSE];
    },simplify=FALSE);
    data=do.call(rbind,rows);
    data[,-1]=round(data[,-1],digits=round.digits);
    data;
  }
## compute ratios from cmp_doh data
## data is from cmp_doh
cmp_doh_ratio=function(data,base.place='state',round.digits=2) {
  counts=data[,-1,drop=FALSE];
  base=repr(counts[base.place,,drop=FALSE],nrow(counts));
  ratio=round(base/counts,digits=round.digits);
  rownames(ratio)=rownames(data);
  ratio;
}
## xper_cmp_doh=function(places=places.wa,ages=NULL,do.print=FALSE) {
##   if (!do.print) sink('/dev/null');
##   if (is.null(ages)) ages=ages(doh.cases)%-%'all';
##   cmp=sapply(ages,function(age) {
##     data=show_doh(what='cases',places=places,ages=age,do.peaks=F);
##    cmp_doh_places(data);
##   });
##   if (!do.print) sink();
##   t(cmp);
## }
