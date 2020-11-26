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
## ---- Download and import big 3 data sources plus trk ----
do_dlim=function(datasrc=cq(doh,jhu,nyt,trk),version=NULL,monday.only=TRUE,cmp.prev=monday.only,
                   url=param(download.url)) {
  datasrc=match.arg(datasrc,several.ok=TRUE);
  if (is.null(version)) version=dl_version(monday.only=monday.only);
  if (param(verbose)) print(paste('+++ running download: version',version));
  ok=system('pjtest')==0;
  if (!ok) stop('Reverse tunnel not running; stopping before download');
  download(datasrc,version,url=url);
  if (cmp.prev) datasrc=cmp_prev(datasrc,version);
  sapply(datasrc,function(datasrc) {
    if (param(verbose)) print(paste('+++ importing',datasrc));
    import(datasrc,version);
  });
  pjto(datasrc,version);
  version;
}
download=function(datasrc,version,url) {
  filename=dl_filenames(datasrc,version);
  sapply(datasrc,function(datasrc) {
    if (param(verbose)) print(paste('+++ downloading',datasrc));
    switch(datasrc,
           doh=download.file(url$doh,filename$doh),
           jhu={
             download.file(url$jhu.cases,filename$jhu[1]);
             download.file(url$jhu.deaths,filename$jhu[2])
           },
           nyt=download.file(url$nyt,filename$nyt),
           trk=download.file(url$trk,filename$trk));
  });
}
cmp_prev=function(datasrc,version) {
  prev=as_version(as_date(version)-7) ;
  filename=dl_filenames(datasrc,version);
  prevname=dl_filenames(datasrc,prev);
  ok=!sapply(datasrc,function(datasrc) {
    switch(datasrc,
           doh=cmp_files(filename$doh,prevname$doh),
           jhu=cmp_files(filename$jhu[1],prevname$jhu[1])&
             cmp_files(filename$jhu[2],prevname$jhu[2]),
           nyt=cmp_files(filename$nyt,prevname$nyt),
           trk=cmp_files(filename$trk,prevname$trk));
  });
  sapply(datasrc[!ok],function(datasrc) 
    print(paste0('--- skiping ',datasrc,': current version ',version,' same as previous ',prev)));
  datasrc=datasrc[ok];
}
cmp_files=function(file1,file2) {
  if (file.exists(file1)&&file.exists(file2))
    system(paste('cmp',file1,file2),ignore.stdout=T)==0
  else FALSE;
}

pjto=function(datasrc,version) {
  sapply(datasrc,function(datasrc) {
    dir=indir(datasrc);
    cmd=paste('pjto',
              switch(datasrc,
                     doh=filename(dir,version,suffix='xlsx'),
                     jhu=paste(collapse=' ',
                               filename(dir,base='cases',tail=version,suffix='csv'),
                               filename(dir,base='deaths',tail=version,suffix='csv')),
                     nyt=filename(dir,version,suffix='csv'),
                     trk=filename(dir,version,suffix='csv')));
    if (param(verbose)) print(paste('+++',cmd));
    system(cmd);
    dir=datadir(datasrc);
    bases=if(datasrc%in%cq(doh,trk)) cq(cases,admits,deaths) else cq(cases,deaths);
    cases=expand.grid(base=bases,suffix=cq(txt,RData),stringsAsFactors=FALSE);
    files=unlist(withrows(cases,case,filename(dir,base=base,tail=version,suffix=suffix)));
    cmd=paste('pjto',paste(collapse=' ',files));
    if (param(verbose)) print(paste('+++',cmd));
    system(cmd);
  });
}
dl_version=function(today=Sys.Date(),delta=0,monday.only=TRUE) {
  today=today+delta;
  if ((weekdays(today)!='Monday')&&monday.only) 
    stop("Have to run 'download' workflow on Mondays so non-'doh' and 'doh' versions will match");
  as_version(today-1);
}
dl_filenames=function(datasrc,version) {
  sapply(datasrc,function(datasrc) {
    dir=indir(datasrc);
    switch(datasrc,
           doh=filename(dir,version,suffix='xlsx'),
           jhu=c(filename(dir,base='cases',tail=version,suffix='csv'),
                 filename(dir,base='deaths',tail=version,suffix='csv')),
           nyt=filename(dir,version,suffix='csv'),
           trk=filename(dir,version,suffix='csv'));
    },simplify=FALSE);
}

## ---- Make standard objects ----
## make objects I tend to use circa Oct 2010
do_objs=function(what=cq(cases,admits,deaths),datasrc=cq(doh,jhu,nyt,trk),version='latest') {
  cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
  ## only 'doh' has 'admits'. prune others
  cases=cases[(cases$what!='admits'|cases$datasrc%in%cq(doh,trk)),]
  ## cum is names of sorted cumulative cases. handcrafted 20-10-14 from jhu.cases.raw
  cum=c('King','Yakima','Pierce','Spokane','Snohomish','Benton','Franklin','Clark','Grant','Chelan','Whitman','Whatcom','Kitsap','Thurston','Douglas','Skagit','Okanogan','Walla Walla','Adams','Cowlitz','Lewis','Kittitas','Grays Harbor','Mason','Island','Clallam','Stevens','Klickitat','Asotin','Pacific','Pend Oreille','Jefferson','Skamania','Lincoln','Ferry','San Juan','Columbia','Garfield','Wahkiakum');
  withrows(cases,case,{
    if (param(verbose)) print(paste('+++ making',datasrc,what));
    ## all cases need raw
    obj=raw(what,datasrc,version);
    assign(paste(sep='.',datasrc,what,'raw'),obj,globalenv());
    ## 'standard' objects differ by datasrc
    obj=switch(datasrc,
               doh={obj=extra(obj); edit(obj,'0_59'='0_19'+'20_39'+'40_59')},
               jhu=weekly(incremental(obj)),
               nyt=weekly(incremental(obj)),
               trk=weekly(obj));
    ## edit all but trk to include, eg, SKP, before assigning to global
    if (datasrc!='trk')
      obj=edit(obj,SUM=list(SKP=cq(Snohomish,King,Pierce),Top5=head(cum,n=5),Top10=head(cum,n=10)),
               NEG=list(notKing='King',notSKP='SKP',notTop5='Top5',notTop10='Top10'));
    assign(paste(sep='.',datasrc,what,'noroll'),obj,globalenv());
    ## rolling mean
    obj=roll(obj);
    assign(paste(sep='.',datasrc,what),obj,globalenv());
  });
  cases;  
}

## ---- Standard plots ----
## mostly a placeholder. expect it to change
do_plots=function(objs=NULL,objs.noroll=NULL) {
  ok=system('pjtest')==0;
  if (!ok) stop('Reverse tunnel not running; stopping before doing plots');
  if (is.null(objs)) objs=list(doh.cases,jhu.cases,nyt.cases,trk.cases);
  if (is.null(objs.noroll))
    objs.noroll=list(doh.cases.noroll,jhu.cases.noroll,nyt.cases.noroll,trk.cases.noroll);
 ## all sources 'state'
  plon('all.state');
  plot_cvdat(objs,places=cq(state),ages='all',per.capita=TRUE);
  ploff();
  ## all sources (except trk) 'King'
  objs.notrk=objs[sapply(objs,function(obj) datasrc(obj)!='trk')]
  plon('all.King');
  plot_cvdat(objs.notrk,places=cq(King),ages='all',per.capita=TRUE);
  ploff();
  ## doh several places
  plon('doh.places');
  plot_cvdat(doh.cases,places=cq(state,King,SKP,Top5,Top10),ages='all',per.capita=TRUE);
  ploff();
  ## doh several ages
  plon('doh.ages');
  plot_cvdat(doh.cases,places=cq(state),ages=NULL,per.capita=TRUE);
  ploff();
  ## jhu King, kids
  plon('jhu.kids');
  plot_cvdat(jhu.cases,places=cq(King,'Ann Arbor',DC),ages=NULL,per.capita=TRUE);
  ploff();
  ## jhu King, others
  plon('jhu.other');
  plot_cvdat(jhu.cases,places=cq(King,'Ann Arbor',DC,Boston,'San Diego',Austin),
             ages=NULL,per.capita=TRUE);
  ploff();
  ## jhu, nyt other
  plon('all.other');
  plot_cvdat(list(jhu.cases,nyt.cases),places=places_other(),ages=NULL,per.capita=TRUE);
  ploff();
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

## ---- Download and import metadata ----
## Need to do this once before running pretty much anything else
do_meta=function() {
  import_geo();
  import_stateid();
  download_pop();
  import_pop();
}

## ---- Deploy updat doc  ----
## Run after rendering doc in RStudio
##   can't render here 'cuz main sunfish R version not compatible...
do_updat=function(save=param(save)) {
  if (is.na(save)) overwrite=FALSE
  else {
    if (!save) stop("Cannot deploy: 'save' is FALSE");
    overwrite=save;
  }
  param(verbose,pjto);
  if (pjto) {
    ok=system('pjtest')==0;
    if (!ok) warning('Cannot copy to Mac: reverse tunnel not running. Deploying on Linux');
  }
  vsn=latest_version('doh',NULL,'data/doh');
  ## create version directory (for maintaining old version)
  vsndir=file.path('doc.nnn','updat',vsn);
  if (verbose) print(paste('+++ creating directory',vsndir))
  dir.create(vsndir,recursive=TRUE,showWarnings=FALSE);
  ## create date file used by updat_vsn.Rmd, blog/README
  date=format(as.Date(vsn,format='%y-%m-%d')+1,'%B %-d, %Y');
  file='updat.date';
  if (overwrite||(!file.exists(file))) {
    if (verbose) print(paste('+++ creating',file));
    cat(date,file=file);
  } else warning("Cannot update ",file,". File exists and save is NA");
  ## copy files to 'stable' and version directory
  sfx=cq(Rmd,html,pdf);
  sapply(sfx,function(sfx) {
    file=paste(sep='.','updat',sfx);
    cp_if_allowed(file,paste(sep='.','updat','stable',sfx),overwrite=overwrite);
    cp_if_allowed(file,file.path(vsndir,file),overwrite=overwrite);
  });
  ## create link file used by updat_vsn.Rmd
  vsn=sort(list.files('doc.nnn/updat','\\d\\d-\\d\\d-\\d\\d'));
  date=format(as.Date(vsn,format='%y-%m-%d')+1,'%B %-d, %Y');
  file=file.path('doc.nnn/updat',vsn,'updat.html')
  link=paste0('[',date,'](',file,')')
  file='updat.link';
  if (overwrite||(!file.exists(file))) {
    if (verbose) print(paste('+++ creating',file));
    writeLines(link,con='updat.link')
  } else warning("Cannot update ",file,". File exists and save is NA");
  if (verbose) 
    print('>>> do_updat done. remember to knit updat_vsn.Rmd and commit all docs');
}
cp_if_allowed=function(infile,file,overwrite) {
  if (overwrite||(!file.exists(file))) {
    param(verbose,pjto);
    if (verbose) print(paste('+++ creating',file));
    file.copy(infile,file,overwrite=TRUE);
    if (pjto) system(paste('pjto',file)); # copy to Mac if desired (usally is)
  }
  else warning("Cannot update ",file,". File exists and save is NA");
}


## ---- Test transforms ----
## CAUTION: not up-to-date !!
do_test=function(what=cq(cases,deaths),datasrc=cq(doh,jhu,nyt),version='latest') {
  objs=list();
  cases=expand.grid(what=what,datasrc=datasrc,version=version,stringsAsFactors=FALSE);
  objs=withrows(cases,case,{
    ## casename=paste(sep='.',what,datasrc);
    if (param(verbose)) print(paste('>>> transforming',nv(what,datasrc,version)));
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
