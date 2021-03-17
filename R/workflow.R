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
## with new DOH release schedule have to do it on Tuesdays
## make sure obj transforms truncate non-DOH data to previous week
do_dlim=function(datasrc=cq(doh,jhu,nyt,trk),version=NULL,
                 force.sunday=TRUE,monday.only=!force.sunday,cmp.prev=force.sunday,
                 url=param(download.url),
                 do.download=TRUE,do.import=TRUE,do.pjto=TRUE) {
  datasrc=match.arg(datasrc,several.ok=TRUE);
  if (is.null(version)) version=dl_version(force.sunday=force.sunday,monday.only=monday.only);
  ## trk ended 21-03-07. see covidtracking.com
  if (version>'21-03-07') datasrc=datasrc%-%'trk'; 
  if (do.pjto) {
    ok=system('pjtest')==0;
    if (!ok) stop('Reverse tunnel not running; stopping before download');
  }
  if (do.download) do_download(datasrc,version,url);
  if (do.import) {
    if (cmp.prev) datasrc=cmp_prev(datasrc,version);
    do_import(datasrc,version);
  }
  if (do.pjto) pjto(datasrc,version);
  version;
}
do_download=function(datasrc=cq(doh,jhu,nyt,trk),version=NULL,
                     force.sunday=TRUE,monday.only=!force.sunday,cmp.prev=force.sunday,
                     url=param(download.url)) {
  if (is.null(version)) version=dl_version(force.sunday=force.sunday,monday.only=monday.only);
  ## trk ended 21-03-07. see covidtracking.com
  if (version>'21-03-07') datasrc=datasrc%-%'trk'; 
  if (param(verbose)) print(paste('+++ running download: version',version));
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
do_import=function(datasrc=cq(doh,jhu,nyt,trk),version=NULL) {
  if (is.null(version)||version=='latest') version=latest_version(datasrc[1],dir=indir);
  ## trk ended 21-03-07. see covidtracking.com
  if (version>'21-03-07') datasrc=datasrc%-%'trk'; 
  if (param(verbose)) print(paste('+++ running import: version',version));
  sapply(datasrc,function(datasrc) {
    if (param(verbose)) print(paste('+++ importing',datasrc));
    import(datasrc,version);
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
pjto=function(datasrc=cq(doh,jhu,nyt,trk),version=NULL) {
  if (is.null(version)||version=='latest') version=latest_version(datasrc[1],dir=indir);
  ## trk ended 21-03-07. see covidtracking.com
  if (version>'21-03-07') datasrc=datasrc%-%'trk'; 
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
dl_version=function(today=Sys.Date(),delta=0,force.sunday=TRUE,monday.only=!force.sunday) {
  today=today+delta;
  if ((weekdays(today)!='Monday')&&monday.only) 
    stop("Have to run 'download' workflow on Mondays so non-'doh' and 'doh' versions will match");
  if (force.sunday) as_version(sunday_week(today)) else as_version(today-1);
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
## make objects I tend to use circa Oct 2020
## NG 20-12-14: fixed longstanding bug. have to do 'extra' before 'edit' else objects
##   will be incompatible. bug in 'extra' caused error to be missed and results of edited
##   places and ages to be 0!
## NG 21-03-14: I don't use this any more
##   doh 'extra' broken because they changed age groups
##   trk finished as of 21-03-07
do_objs=function(what=cq(cases,admits,deaths),datasrc=cq(doh,jhu,nyt,trk),version='latest') {
  cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
  ## only 'doh' has 'admits'. prune others
  cases=cases[(cases$what!='admits'|cases$datasrc%in%cq(doh,trk)),]
  ## cum is names of sorted cumulative cases. handcrafted 20-10-14 from jhu.cases.raw
  cum=c('King','Yakima','Pierce','Spokane','Snohomish','Benton','Franklin','Clark','Grant','Chelan','Whitman','Whatcom','Kitsap','Thurston','Douglas','Skagit','Okanogan','Walla Walla','Adams','Cowlitz','Lewis','Kittitas','Grays Harbor','Mason','Island','Clallam','Stevens','Klickitat','Asotin','Pacific','Pend Oreille','Jefferson','Skamania','Lincoln','Ferry','San Juan','Columbia','Garfield','Wahkiakum');
  id=cq(raw,roll,extra,rx,xr);
  withrows(cases,case,{
    if (param(verbose)) print(paste('+++ making',datasrc,what));
    obj=raw(what,datasrc,version);
    ## 'raw' really means 'weekly, incremental'
    obj.raw=switch(datasrc,
                   doh=obj,
                   jhu=weekly(incremental(obj)),
                   nyt=weekly(incremental(obj)),
                   trk=weekly(obj));
    ## NG 20-12-14: for doh, do 'extra' before 'edit'. not an issue for jhu, nyt
    ##   means we have to process doh raw and extra in separate streams
    obj.roll=roll(obj.raw);
    if (datasrc=='doh') {
      obj.extra=extra(obj.raw);
      obj.rx=extra(obj.roll);
      obj.xr=roll(obj.extra);
    }
    ## iterate over object. edit (except trk), add id, assign to global
    names.local=paste0('obj.',id);
    names.global=paste(sep='.',datasrc,what,id);
    objs=mget(names.local,ifnotfound=rep(list(NULL),length(id)));
    sapply(seq_along(objs),function(i) {
      obj=objs[[i]];
      if (!is.null(obj)) {
        obj$id=id[i];
        if (datasrc=='doh') obj=edit(obj,'0_59'='0_19'+'20_39'+'40_59');
        if (datasrc!='trk')
          obj=edit(obj,SKP=Snohomish+King+Pierce,
                   SUM=list(Top5=head(cum,n=5),Top10=head(cum,n=10)),
                   NEG=list(notKing='King',notSKP='SKP',notTop5='Top5',notTop10='Top10'));
        assign(names.global[i],obj,globalenv());
      }});
    ## also assign 'standard' and 'noroll' object for source
    name.std=paste(sep='.',datasrc,what,if(datasrc=='doh') 'xr' else 'roll');  
    assign(paste(sep='.',datasrc,what),get(name.std,globalenv()),globalenv());
    name.noroll=paste(sep='.',datasrc,what,if(datasrc=='doh') 'extra' else 'raw');  
    assign(paste(sep='.',datasrc,what,'noroll'),get(name.noroll,globalenv()),globalenv());
  });
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
    ## jhu King, others
    plon('jhu.other');
    plot_cvdat(jhu.cases,places=places_other(),ages=NULL,per.capita=TRUE);
    ploff();
  }
  if (cq(jhu,nyt) %<=% datasrc) {
    ## jhu, nyt other
    plon('all.other');
    plot_cvdat(list(jhu.cases,nyt.cases),places=places_other(),ages=NULL,per.capita=TRUE);
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
do_updat=function(save=param(save),version='latest') {
  if (is.null(version)||version=='latest') version=max(sapply(cq(jhu,doh),latest_version));
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
  ## create version directory (for maintaining old version)
  vsndir=file.path('doc.nnn','updat',version);
  if (verbose) print(paste('+++ creating directory',vsndir))
  dir.create(vsndir,recursive=TRUE,showWarnings=FALSE);
  ## create date file used by updat_vsn.Rmd, blog/README
  ## '+3' in line below sets date to Wed
  date=format(as.Date(version,format='%y-%m-%d')+3,'%B %-d, %Y');
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
  ## create link file used by updatvsn.Rmd
  ## NG 20-12-07: sort links from latest to oldest. makes much more sense!
  versions=sort(list.files('doc.nnn/updat','\\d\\d-\\d\\d-\\d\\d'),decreasing=TRUE);
  date=sapply(versions,function(version) {
    ## used to update doc on Mondays. now Wednesdays
    delta=if(version<'20-12-27') 1 else 3; 
    format(as.Date(version,format='%y-%m-%d')+delta,'%B %-d, %Y');
  });
  file=file.path('doc.nnn/updat',versions,'updat.html')
  link=paste0('[',date,'](',file,')')
  file='updat.link';
  if (overwrite||(!file.exists(file))) {
    if (verbose) print(paste('+++ creating',file));
    writeLines(link,con='updat.link')
  } else warning("Cannot update ",file,". File exists and save is NA");
  if (verbose) 
    print('>>> do_updat done. remember to knit updatvsn.Rmd, blog/README.Rmd and commit all docs');
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
