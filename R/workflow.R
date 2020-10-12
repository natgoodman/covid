#################################################################################
##
## Author:  Nat Goodman
## Created: 20-10-08
##          with code adapted from clapi/distr_3 created 20-04-05
##
## Copyright (C) 2020 Nat Goodman.
## 
## Functions implementing simple useful workflows, aka pipelines
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Download and import big 3 data sources ----
do_dlim3=function(datasrc=cq(doh,jhu,nyt),version=NULL,monday.only=TRUE,cmp.prev=monday.only,
                   url=param(download3.url)) {
  datasrc=match.arg(datasrc,several.ok=TRUE);
  if (is.null(version)) version=dl3_version(monday.only=monday.only);
  if (param(verbose)) print(paste('+++ running download3: version',version));
  ok=system('pjtest')==0;
  if (!ok) stop('Reverse tunnel not running; stopping before download');
  download3(datasrc,version,url=url);
  if (cmp.prev) datasrc=cmp_prev(datasrc,version);
  BREAKPOINT('do_dlim3 after download and cmp: ',nvq(version));
  import3(datasrc,version);
  pjto3(datasrc,version);
  version;
}
download3=function(datasrc,version,url) {
  filename=dl3_filenames(datasrc,version);
  sapply(datasrc,function(datasrc) {
    if (param(verbose)) print(paste('+++ downloading',datasrc));
    BREAKPOINT('download3: ',nvq(datasrc));
    switch(datasrc,
           doh=download.file(url$doh,filename$doh),
           jhu={
             download.file(url$jhu.cases,filename$jhu[1]);
             download.file(url$jhu.deaths,filename$jhu[2])
           },
           nyt=download.file(url$nyt,filename$nyt));
  });
}
import3=function(datasrc,version) {
  sapply(datasrc,function(datasrc) {
    if (param(verbose)) print(paste('+++ importing',datasrc));
    import(datasrc,version);
  });
}
cmp_prev=function(datasrc,version) {
  prev=to_version(as_date(version)-7) ;
  filename=dl3_filenames(datasrc,version);
  prevname=dl3_filenames(datasrc,prev);
  ok=!sapply(datasrc,function(datasrc) {
    switch(datasrc,
           doh=cmp_files(filename$doh,prevname$doh),
           jhu=cmp_files(filename$jhu[1],prevname$jhu[1])&
             cmp_files(filename$jhu[2],prevname$jhu[2]),
           nyt=cmp_files(filename$nyt,prevname$nyt));
  });
  sapply(datasrc[!ok],function(datasrc) 
    print(paste0('+++ skiping ',datasrc,': current version ',version,' same as previous ',prev)));
  datasrc=datasrc[ok];
}
cmp_files=function(file1,file2) system(paste('cmp',file1,file2),ignore.stdout=T)==0;

pjto3=function(datasrc,version) {
  sapply(datasrc,function(datasrc) {
    dir=indir(datasrc);
    cmd=paste('pjto',
              switch(datasrc,
                     doh=filename(dir,version,suffix='xlsx'),
                     jhu=paste(collapse=' ',
                               filename(dir,base='cases',tail=version,suffix='csv'),
                               filename(dir,base='deaths',tail=version,suffix='csv')),
                     nyt=filename(dir,version,suffix='csv')));
    if (param(verbose)) print(paste('+++',cmd));
    system(cmd);
    dir=datadir(datasrc);
    bases=switch(datasrc,doh=cq(cases,admits,deaths),cq(cases,deaths));
    cases=expand.grid(base=bases,suffix=cq(txt,RData),stringsAsFactors=FALSE);
    files=unlist(withrows(cases,case,filename(dir,base=base,tail=version,suffix=suffix)));
    cmd=paste('pjto',paste(collapse=' ',files));
    if (param(verbose)) print(paste('+++',cmd));
    system(cmd);
  });
}
dl3_version=function(today=Sys.Date(),delta=0,monday.only=TRUE) {
  today=today+delta;
  if ((weekdays(today)!='Monday')&&monday.only) 
    stop("Have to run 'download3' workflow on Mondays so 'jhu' and 'nyt' versions will match 'doh' version");
  to_version(today-1);
}
dl3_filenames=function(datasrc,version) {
  sapply(datasrc,function(datasrc) {
    dir=indir(datasrc);
    switch(datasrc,
           doh=filename(dir,version,suffix='xlsx'),
           jhu=c(filename(dir,base='cases',tail=version,suffix='csv'),
                 filename(dir,base='deaths',tail=version,suffix='csv')),
           nyt=filename(dir,version,suffix='csv'));
    },simplify=FALSE);
}

## ---- Make standard objects ----
## make objects I tend to use circa Oct 2010
do_objs=function(what=cq(cases,admits,deaths),datasrc=cq(doh,jhu,nyt),version='latest') {
  cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
  ## only 'doh' has 'admits'. prune others
  cases=cases[(cases$what!='admits'|cases$datasrc=='doh'),]
  withrows(cases,case,{
    ## all cases need raw
    obj=raw(what,datasrc,version);
    assign(paste(sep='.',datasrc,what,'raw'),obj,globalenv());
    ## 'standard' final objects differ by datasrc
    obj=if(datasrc=='doh') extra(obj) else weekly(incremental(obj))
    assign(paste(sep='.',datasrc,what),obj,globalenv());
  });
  cases;  
}

## ---- Test transforms ----
## CAUTION: not up-to-date !!
do_test=function(what=cq(cases,deaths),datasrc=cq(doh,jhu,nyt),version='latest') {
  objs=list();
  cases=expand.grid(what=what,datasrc=datasrc,version=version,stringsAsFactors=FALSE);
  objs=withrows(cases,case,{
    ## casename=paste(sep='.',what,datasrc);
    if (param(verbose)) print(paste('>>> transforming',nvq(what,datasrc,version)));
    within(list(), {
      raw=raw(what,datasrc,version);
      cum=cumulative(raw);
      cum2=cum2(raw);
      inc=incremental(raw);
      weekly=weekly(raw);
      daily=daily(raw);
      wklycum=weekly(cum);
      dlycum=daily(cum);
      wklyinc=weekly(inc);
      dlyinc=daily(inc);
      fitwklycum=fit(weekly(cum));
      fitdlycum=fit(daily(cum));
      fitwklyinc=fit(weekly(inc));
      fitdlyinc=fit(daily(inc));
    });
  })
  names(objs)=unlist(withrows(cases,case,
                              paste(collapse='.',c(what,datasrc,if(version!='latest') version))));
  invisible(objs);
}
