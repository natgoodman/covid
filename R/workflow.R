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
## ---- Download and import big 3 data sources (now 2) ----
## as of Jan 10 2022, DOH doesn't provide download file...
## prior to cutoff, DOH did releases late Mondays (except for holidays)
##   operationally easiest to do download on Tuesdays
## make sure obj transforms truncate non-DOH data to previous week
do_dlim=function(datasrc=cq(jhu,nyt),version=NULL,
                 force.sunday=TRUE,monday.only=!force.sunday,cmp.prev=force.sunday,
                 url=param(download.url),
                 do.download=TRUE,do.import=TRUE,do.pjto=TRUE) {
  datasrc=match.arg(datasrc,param(datasrc),several.ok=TRUE);
  ## for now, have to do CDC by itself - big and slow!
  if (is.null(version)) version=dl_version(force.sunday=force.sunday,monday.only=monday.only);
  if (('cdc'%in%datasrc)&&(length(datasrc)>1))
    stop("Have to process CDC by itself - big and slow! ");
  if (('doh'%in%datasrc)&&(version>='22-01-10'))
    stop("As of Jan 10 2022, DOH doesn't provide download file");
  ## trk ended 21-03-07. see covidtracking.com
  ## if (version>'21-03-07') datasrc=datasrc%-%'trk'; 
  if (do.pjto) {
    ok=system('pjtest')==0;
    if (!ok) stop('Reverse tunnel not running; stopping before download');
  }
  if (do.download) ok=do_download(datasrc,version,url);
  if (any(!ok)) {
    print(paste0('--- skiping ',datasrc[!ok],': download failed'));
    datasrc=datasrc[ok];
  }
  if (cmp.prev) datasrc=cmp_prev(datasrc,version);
  if (length(datasrc)) {
    if (do.import) do_import(datasrc,version);
    if (do.pjto) pjto(datasrc,version);
  }
  version;
}
do_download=function(datasrc=param(datasrc),version=NULL,
                     force.sunday=TRUE,monday.only=!force.sunday,cmp.prev=force.sunday,
                     url=param(download.url)) {
  datasrc=match.arg(datasrc,several.ok=TRUE);
  ## for now, have to do CDC by itself - big and slow!
  if (length(datasrc)>1) datasrc=datasrc%-%'cdc'; 
  if (is.null(version)) version=dl_version(force.sunday=force.sunday,monday.only=monday.only);
  ## trk ended 21-03-07. see covidtracking.com
  ## if (version>'21-03-07') datasrc=datasrc%-%'trk'; 
  if (param(verbose)) print(paste('+++ running download: version',version));
  filename=dl_filenames(datasrc,version);
  ok=sapply(datasrc,function(datasrc) {
    if (param(verbose)) print(paste('+++ downloading',datasrc));
    if (datasrc=='jhu') {
      status=c(
        download.file(url$jhu.cases,filename$jhu[1]),
        download.file(url$jhu.deaths,filename$jhu[2]));
    } else status=download.file(url[[datasrc]],filename[[datasrc]]);
    all(status==0);
  });
  ok;
}
do_import=function(datasrc=param(datasrc),version=NULL) {
  datasrc=match.arg(datasrc,several.ok=TRUE);
  ## for now, have to do CDC by itself - big and slow!
  if (length(datasrc)>1) datasrc=datasrc%-%'cdc'; 
  if (is.null(version)||version=='latest') version=latest_version(datasrc[1],dir=indir);
  ## trk ended 21-03-07. see covidtracking.com
  ## if (version>'21-03-07') datasrc=datasrc%-%'trk'; 
  if (param(verbose)) print(paste('+++ running import: version',version));
  sapply(datasrc,function(datasrc) {
    if (param(verbose)) print(paste('+++ importing',datasrc));
    import(datasrc,version);
  });
}
cmp_prev=function(datasrc,version) {
  ok=sapply(datasrc,function(datasrc) {
    versions=list_versions(datasrc,dir=indir(datasrc));
    now=which(version==versions);
    if (!length(now)) {
      print(paste0("--- skiping ",datasrc,": doesn't have desired version ",version));
      return(FALSE);
    }
    if (now==1) return(TRUE)        # no previous version
    prev=versions[now-1];
    filename=dl_filenames(datasrc,version);
    prevname=dl_filenames(datasrc,prev);
    same=(if (datasrc=='jhu') 
            cmp_files(filename$jhu[1],prevname$jhu[1])&cmp_files(filename$jhu[2],prevname$jhu[2])
          else cmp_files(filename[[datasrc]],prevname[[datasrc]]));
    if (same)
      print(paste0('--- skiping ',datasrc,': current version ',version,' same as previous ',prev))
    !same;
  });
  datasrc[ok];
}
cmp_files=function(file1,file2) {
  if (file.exists(file1)&&file.exists(file2))
    system(paste('cmp',file1,file2),ignore.stdout=T)==0
  else FALSE;
}
pjto=function(datasrc=param(datasrc),version=NULL) {
  datasrc=match.arg(datasrc,several.ok=TRUE);
  ## for now, have to do CDC by itself - big and slow!
  if (length(datasrc)>1) datasrc=datasrc%-%'cdc'; 
  if (is.null(version)||version=='latest') version=latest_version(datasrc[1],dir=indir);
  ## trk ended 21-03-07. see covidtracking.com
  ## if (version>'21-03-07') datasrc=datasrc%-%'trk';
  sapply(datasrc,function(datasrc) {
    dir=indir(datasrc);
    cmd=paste('pjto',
              switch(datasrc,
                     doh=filename(dir,version,suffix='xlsx'),
                     jhu=paste(collapse=' ',
                               filename(dir,base='cases',tail=version,suffix='csv'),
                               filename(dir,base='deaths',tail=version,suffix='csv')),
                     nyt=filename(dir,version,suffix='csv'),
                     trk=filename(dir,version,suffix='csv'),
                     cdc={
                       orig=filename(dir,version,suffix='csv');
                       gz=paste(sep='.',orig,'gz');
                       ## zip before copying to Mac; saves 90%
                       ## NG 21-12-20: gzip w/o '-c' to delete original. BIG and not needed
                       ## cmd=paste('/bin/gzip -c',orig,'>',gz);
                       cmd=paste('/bin/gzip',orig);
                       if (param(verbose)) print(paste('+++',cmd));
                       system(cmd);
                       gz;
                     }));
    if (param(verbose)) print(paste('+++',cmd));
    system(cmd);
    dir=datadir(datasrc);
    bases=c('cases','deaths',if(datasrc%in%cq(doh,trk)) 'admits' else cq(admits,icus));
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
           ## nyt=filename(dir,version,suffix='csv'),
           ## cdc=filename(dir,version,suffix='csv'),
           ## trk=filename(dir,version,suffix='csv'));
           filename(dir,version,suffix='csv'));
    },simplify=FALSE);
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
## ---- Deploy mtop doc  ----
## Run after rendering doc in RStudio
##   can't render here 'cuz main sunfish R version not compatible...
do_mtop=function(save=param(save),version=NULL) {
  if (is.null(version)) stop('do_mtop needs explicit version');
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
  ## make sure pdf exists is up-to-date
  msg='must save mtop.pdf in Safari on Mac and pjto it here before running do_mtop';
  file='mtop.pdf';
  if (!file.exists('mtop.pdf')) stop(paste('mtop.pdf does not exist.',msg));
  time.html=file.info('mtop.html')$ctime;
  time.pdf=file.info('mtop.pdf')$ctime;
  if (time.pdf<time.html) stop(paste('mtop.pdf not up-to-date.',msg));
  ## create version directory (for maintaining old version)
  vsndir=file.path('doc.nnn','mtop',version);
  if (verbose) print(paste('+++ creating directory',vsndir));
  dir.create(vsndir,recursive=TRUE,showWarnings=FALSE);
  ## copy files to 'stable' and version directory
  sfx=cq(Rmd,html,pdf);
  sapply(sfx,function(sfx) {
    file=paste(sep='.','mtop',sfx);
    cp_if_allowed(file,paste(sep='.','mtop','stable',sfx),overwrite=overwrite);
    cp_if_allowed(file,file.path(vsndir,file),overwrite=overwrite);
  });
 if (verbose) print('>>> do_mtop done. remember to commit all docs');
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
