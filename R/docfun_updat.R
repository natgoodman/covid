#################################################################################
##
## Author:  Nat Goodman
## Created: 21-11-19
##          from doc_updat.R created 20-11-15
##          from misig/doc_confi.R created 19-07-16
##          from misig/confi.R created 19-07-04
##
## Copyright (C) 2019-2021 Nat Goodman.
## 
## Specialized functions used in doc_updat.R
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## --- doc_updat plot functions ---
## make colors for doh ages. used in other docs, too
col_ages=
  function(obj=doh.cases.raw,ages=NULL,
           col1.pal='rainbow',skip.beg=2,skip.end=0,col2=cq(grey60,black),col2.n=length(col2)) {
    if (is.null(ages)) ages=ages_doh;
    col2=setNames(col2,tail(ages,n=col2.n));
    col1=col_brew(head(ages,n=-col2.n),col1.pal,skip.beg=skip.beg,skip.end=skip.end);
    col=c(col1,col2);
    ## col=rep(col,each=2);
    col;
  }
ages_doh=function(obj=doh.cases.raw) sort(ages(obj)%-%'all');
  
## plot final (processed) and raw data together
## TODO: add this to plot_cvdat!
plot_finraw=
  function(datasrc=param(datasrc),what=cq(cases,admits,deaths),
           places,ages='all',per.capita=TRUE,per.mort=FALSE,
           title,legends=list(labels=places),where.legend='topright',raw.plot=cq(lines,points),
           xmin=NULL,xmax=NULL,ymin=NULL,ymax='auto',
           xgrid=cq(biweekly,weekly,quadweekly,semimonthly,monthly),xformat='%b-%d',cex.axis=0.75,
           col=NULL,
           lwd=2,lwd.fin=lwd,lwd.raw=0.375*lwd.fin,lty.fin='solid',lty.raw='dotted',pch=20) {
    datasrc=match.arg(datasrc,param(datasrc));
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
## plot admits and deaths data together
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
## plot doh, jhu processed and raw data together. used in Figure 5
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

## --- Manage doc_updat objects ---
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
  function(what=cq(cases,admits,deaths),datasrc=cq(doh,jhu),version='latest',do.extra=FALSE) {
    what=match.arg(what,several.ok=TRUE);
    datasrc=match.arg(datasrc,param(datasrc),several.ok=TRUE);
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
                 ## jhu={
                 ##   obj=incremental(obj);
                 ##   assign(paste(sep='.',datasrc,what,'dly'),obj,globalenv());  # save as 'dly'
                 ##   weekly(obj);
                 ## });
                 jhu=incremental(weekly(obj))
                 );
      assign(paste(sep='.',datasrc,what,'raw'),obj,globalenv());  # save as 'raw'
      if (datasrc=='doh'&&do.extra) {
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
                       objid=NULL,rm.std=is.null(objid)) {
  what=match.arg(what,several.ok=TRUE);
  datasrc=match.arg(datasrc,param(datasrc),several.ok=TRUE);
  if (length(what)==0||length(datasrc)==0) invisible(NULL); # nothing to remove
  names.all=ls(globalenv());
  pat=paste0('^(',paste(collapse='|',datasrc),')','\\.',
             '(',paste(collapse='|',what),')','\\.',
             '(',paste(collapse='|',objid),')');
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

## --- doc_updat interactive functions ---
## wrapper for show_counts to emit count ranges used in doc
show_range=show_ranges=
  function(objid=cq(raw,std,dly),where=cq(wa,nonwa),what=cq(cases,deaths),
           places.wa=parent(places.wa),places.nonwa=parent(places.nonwa)) {
    objid=match.arg(objid);
    what=match.arg(what);
    round.50=(what=='cases');
    counts=show_counts(
      objid=objid,where=where,what=what,places.wa=places.wa,places.nonwa=places.nonwa,
      round.50=round.50,do.peaks=FALSE,do.range=TRUE,do.print=FALSE);
    ranges=do.call(rbind,lapply(counts,function(counts) tail(counts$now,n=1)[,cq(min,max)]));
    ranges$text=paste(sep='-',ranges$min,ranges$max);
    ranges;
  }
## wrapper for show_counts to show now vs. peak comparisons used in doc
show_peak=show_peaks=
  function(objid=cq(std,raw,dly),where=cq(wa,nonwa),what=cq(cases,deaths),
           places.wa=parent(places.wa),places.nonwa=parent(places.nonwa),
           SIMPLIFY=TRUE) {
    objid=match.arg(objid);
    what=match.arg(what);
    round.50=(what=='cases');
    counts=show_counts(
      objid=objid,where=where,what=what,places.wa=places.wa,places.nonwa=places.nonwa,
      round.50=round.50,do.peaks=TRUE,do.range=TRUE,do.print=FALSE);
    counts=lapply(counts,function(counts) {names(counts)=sub('^.* ','',names(counts)); counts});
    peaks=lapply(counts,function(peaks) 
      do.call(rbind,lapply(peaks,function(peak) tail(peak,n=1)[,-1])));
    masks=lapply(peaks,function(peaks) head(capply(peaks,function(x) sign(x['now']-x)),n=-1));
    texts=lapply(masks,function(mask)
      as.data.frame(capply(mask,function(x) ifelse(x==-1,'B',ifelse(x==0,'0','-')))));
    out=mapply(function(peaks,texts) list(counts=peaks,texts=texts),peaks,texts,SIMPLIFY=FALSE);
    if (SIMPLIFY&&length(out)==1) out=out[[1]];
    out;
  }
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
           tail.n=c(3,10),
           round.digits=0,round.50=FALSE,round.to=if(round.50) 50 else 10^(-round.digits),
           do.peaks=TRUE,do.range=TRUE,do.print=TRUE,
           cuts.wa=c('2020-01-26','2020-06-01','2020-09-15','2021-03-01','2021-07-01',
                     '2021-10-01','2022-02-15'),
           labels.wa=cq(spring20,summer20,'winter20-21',spring21,summer21,'winter21-22'),
           cuts.nonwa=c('2020-01-26','2020-09-15','2021-03-01','2021-07-01',
                        '2021-10-01','2022-02-15'),
           labels.nonwa=cq('spring-summer20','winter20-21',spring21,summer21,'winter21-22')) {
    objid=match.arg(objid);
    where=match.arg(where,several.ok=TRUE);
    what=match.arg(what,several.ok=TRUE);
    if (is.null(cases)) cases=get(paste0('counts.cases.',objid),envir=globalenv());
    if (is.null(deaths)) deaths=get(paste0('counts.deaths.',objid),envir=globalenv());
    ## cases[,-1]=round(cases[,-1],digits=round.digits);
    ## deaths[,-1]=round(deaths[,-1],digits=round.digits);
    cases[,-1]=round_to(cases[,-1],round.to);
    deaths[,-1]=round_to(deaths[,-1],round.to);
    cases.wa=cases[,c('date',places.wa)];
    deaths.wa=deaths[,c('date',places.wa)];
    cases.nonwa=cases[,c('date',places.nonwa)];
    deaths.nonwa=deaths[,c('date',places.nonwa)];
    if (do.range) {
      cases.wa=do_range(cases.wa);
      deaths.wa=do_range(deaths.wa);
      cases.nonwa=do_range(cases.nonwa);
      deaths.nonwa=do_range(deaths.nonwa);
    }
    assign_global(cases.wa,deaths.wa,cases.nonwa,deaths.nonwa);
    out=list();
    if ('wa'%in%where&&'cases'%in%what) {
      if (do.peaks)
        peaks=do_peaks(cases.wa,cuts.wa,labels=paste('cases.wa',labels.wa),do.print=do.print)
      else peaks=list();
      now=show_now(cases.wa,objid,tail.n,'cases.wa',do.print=do.print);
      out=cl(out,cases.wa=cl(peaks,now=now));
      if (do.print) print('----------');
    }
    if ('wa'%in%where&&'deaths'%in%what) {
      if (do.peaks)
        peaks=do_peaks(deaths.wa,cuts.wa,labels=paste('deaths.wa',labels.wa),do.print=do.print)
      else peaks=list();
      now=show_now(deaths.wa,objid,tail.n,'deaths.wa',do.print=do.print);
      out=cl(out,deaths.wa=cl(peaks,now=now));
      if (do.print) print('----------');
    }
    if ('nonwa'%in%where&&'cases'%in%what) {
      if (do.peaks)
        peaks=do_peaks(cases.nonwa,cuts.nonwa,labels=paste('cases.nonwa',labels.nonwa),
                         do.print=do.print)
      else peaks=list();
      now=show_now(cases.nonwa,objid,tail.n,'cases.nonwa',do.print=do.print);
      out=cl(out,cases.nonwa=cl(peaks,now=now));
      if (do.print) print('----------');
    }
    if ('nonwa'%in%where&&'deaths'%in%what) {
      if (do.peaks)
        peaks=do_peaks(deaths.nonwa,cuts.nonwa,labels=paste('deaths.nonwa',labels.nonwa),
                         do.print=do.print)
      else peaks=list();
      now=show_now(deaths.nonwa,objid,tail.n,'deaths.nonwa',do.print=do.print);
      out=cl(out,deaths.nonwa=cl(peaks,now=now));
      if (do.print) print('----------');
    }
    invisible(out);
  }
## add row ranges to data
do_range=function(data,do.ratio=FALSE,round.digits=2) {
  data$min=as.numeric(rapply(data[,-1,drop=FALSE],min));
  data$max=as.numeric(rapply(data[,-1,drop=FALSE],max));
  if (do.ratio) data$ratio=round(data$max/data$min,digits=round.digits)
  data;
}
show_now=function(counts,objid,tail.n,label='now',do.print=TRUE) {
  if (!grepl('now$',label)) label=paste(label,'now');
  if (do.print) print(label);
  ## print(paste(collapse=' ',c(label,'now')));
  if (objid!='dly') now=tail(subset(counts,subset=weekdays(date)=='Sunday'),n=tail.n[1])
  else now=tail(subset(counts,subset=date<vdate(jhu.cases)),n=tail.n[2]);
  if (do.print) print(now);
  invisible(now);
}
do_peaks=function(counts,cuts,labels,do.print=TRUE) {
  ## NG 22-04-18; fix longstanding 'off-by-1' bug in cuts
  ## cuts=c(as_date(cuts),Inf);
  cuts=as_date(cuts);
  cats=cut(counts$date,cuts,right=FALSE,labels=FALSE);
  peaks=split(counts,cats);
  peaks=lapply(seq_along(peaks),function(i) {
    peak=peaks[[i]];
    label=labels[i];
    do_peak1(peak,label,do.print=do.print);
  });
  names(peaks)=labels;
  invisible(peaks);
}
do_peak1=function(peak,label,do.print=TRUE) {
  if (do.print) print(paste(collapse=' ',c(label,'peak')));
  i=unique(as.vector(capply(peak[,-1],which.max)));
  peak=peak[i,,drop=FALSE];
  smax=capply(peak[,-1,drop=FALSE],max);
  peak=rbind(peak,data.frame(date=NA,smax,check.names=FALSE));
  if (do.print) print(peak);
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
           tail.n=c(6,10),
           round.digits=0,round.50=FALSE,round.to=if(round.50) 50 else 10^(-round.digits),
           do.peaks=TRUE,do.now=TRUE,do.range=TRUE,do.ratio=TRUE,do.print=TRUE,
           cuts=c('2020-01-26','2020-06-01','2020-09-15','2021-03-01','2021-07-01',
                     '2021-10-01'),
           labels=cq(spring20,summer20,winter20,spring21,summer21,fall21)) {
    objid=match.arg(objid);
    what=match.arg(what,several.ok=FALSE);
    if (is.null(obj)) obj=get(paste(sep='.','doh',what,objid));
    if (is.null(ages)) ages=ages_doh();
    if (length(places)>1&&length(ages)>1)
      stop("Only one of 'places' or 'ages' can have mulitple values");
    if (length(places)==0&&length(ages)==0)
      stop("Both 'places' and 'ages' are empty; nothing to show!");
    if (is.null(data))
      data=data_cvdat(obj,places=places,ages=ages,per.capita=per.capita,per.mort=per.mort);
    ##    data[,-1]=round(data[,-1],digits=round.digits);
    data[,-1]=round_to(data[,-1],round.to);
    if (length(places)==1) {
      peak.labels=paste(what,places,labels);
      now.label=paste(what,places,'now');
      colnames(data)=c('date',ages);
    } else {
      peak.labels=paste(what,ages,labels);
      now.label=paste(what,ages,'now');
      colnames(data)=c('date',places);
    }
    ## NG 21-11-29: do do_range after fixing colnames above. else range column names munged
    if (do.range) data=do_range(data,do.ratio=do.ratio);
    if (do.peaks)
      peaks=do_peaks(data,cuts,labels=peak.labels,do.print=do.print) else peaks=list();
    if (do.now)
      now=show_now(data,objid,tail.n,label=now.label,do.print=do.print) else now=list();
    if (do.print) print('----------');
    invisible(cl(peaks,now=now));
  }
## TODO: these are CRUDE!! do it better
## show data for one date for all places, ages. automates analysis I did for DOH vsn 21-09-29
## NG 21-10-05: now also used for tables
cmp_doh=
  function(objid=cq(std,raw,dly),what=cq(cases,admits,deaths),
           places=NULL,ages=NULL,obj=NULL,base.place='state',per.capita=TRUE,per.mort=FALSE,
           tail.n=1,
           round.digits=0,round.50=FALSE,round.to=if(round.50) 50 else 10^(-round.digits),
           do.range=TRUE,do.ratio=TRUE,do.cmp=FALSE,ratio.digits=2) {
    objid=match.arg(objid);
    what=match.arg(what,several.ok=FALSE);
    if (is.null(obj)) obj=get(paste(sep='.','doh',what,objid));
    if (is.null(places)) places=places(obj);
    if (is.null(ages)) ages=ages_doh(obj);
    rows=sapply(places,function(place) {
      data=data_cvdat(obj,places=place,ages=ages,per.capita=per.capita,per.mort=per.mort);
      row=tail(data,n=tail.n)[1,,drop=FALSE];
    },simplify=FALSE);
    data=do.call(rbind,rows);
    ## data[,-1]=round(data[,-1],digits=round.digits);
    data[,-1]=round_to(data[,-1],round.to);
    if (do.range) data=do_range(data,do.ratio=do.ratio,round.digits=ratio.digits);
    if (do.cmp) cmp_doh_ratio(data,base.place=base.place,round.digits=ratio.digits)
    else data;
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
##### compare USA and WA cumulative totals
cmp_usa_wa=function(what=cq(cases,deaths),round.to=NULL,round.digits=1) {
  what=match.arg(what,several.ok=FALSE);
  if (is.null(round.to)) round.to=if(what=='cases') 1000 else 100;
  obj=get(paste(sep='.','jhu',what,'src'),envir=globalenv());
  data=data_cvdat(obj,places=cq(USA,state),per.capita=TRUE);
  counts=setNames(as.numeric(tail(data[,-1],n=1)),cq(USA,state));
  counts=round_to(counts,round.to);
  ratio=round(counts[1]/counts[2],digits=round.digits);
  list(counts=counts,ratio=ratio,
       text=paste0(ratio,'x ',paste(collapse=' vs. ',format(counts,big.mark=','))));
}
