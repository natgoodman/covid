#################################################################################
##
## Author:  Nat Goodman
## Created: 20-05-01
##          from frecl/R/init.R created 20-01-15
##          from misig/R/init.R created 19-01-01
##          from repwr/R/init.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Initialization code for covid
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################

## ---- init ----
## initialization.
## process parameters and store in param environment.
## create output directories if necessary.
## no docs at present
## inputs and data are source specific, figures and tables global
## doc.all=cq(readme,covid);
init=function(
  ## datasrc=cq(doh,ihme,jhu,tracking,c19pro),
  datasrc=cq(doh,ihme,jhu,nyt,trk,yyg),
  ## doc parameters 
  ## doc=NULL,                             # controls data, figure, table subdirs
  ## docx=match.arg(doc,doc.all),
  ## docx=NULL,
  run.id=NULL,                              # to separate runs for tests
  ## data directories. 
  indir='input',                            # top level input dir
  datadir=filename('data',run.id),          # top level output data files
  metadir=filename('meta',run.id),          # metadata files
  figdir=filename('figure',run.id),         # figures
  tbldir=filename('table',run.id),          # tables
  tmpdir=filename(datadir,'tmp'),           # tmp dir if needed
  inmetadir=filename(indir,'meta'),
  placedir=filename(inmetadir,'place'),
  ## metadata input URLs and files
  ## acs5yr=filename(inmetadir,'acs2018_5yr_B01001_05000US53041.csv'),
  acsmeta=filename(inmetadir,'metadata.json'),
  geo.infile=filename(inmetadir,'geoid.txt'),         # geoids for all US counties (Census Bureau)
  stateid.infile=filename(inmetadir,'stateid.txt'),   # map state names to IDs(World Pop Review)
  ## computed metadata files
  pop.file=filename(metadir,'pop'),         # pop by place, age
  geo.file=filename(metadir,'geo'),         # geoids and place names
  stateid.file=filename(metadir,'stateid'), # map state names to IDs
  ## descriptors for non-WA places of interest
  ## entries are place, state, county. converted to data frame in code below
  places.other=
    list(c('Ann Arbor','MI','Washtenaw'),
         c('DC','DC','District of Columbia'),
         c('Boston','MA','Suffolk'),
         c('San Diego','CA','San Diego'),
         c('Omaha','NE','Douglas'),
         c('Austin','TX','Travis')),
  ages.all=c("all","0_19","20_39","40_59","60_79","80_"),
  ## outdir=c(datadir,figdir,tbldir,tmpdir),  # top level output dirs
  ## cached data
  pop=NULL,                      # pop by place, age. set by load_pop, read_pop
  geo=NULL,                      # geoids and place names. set by load_geo, read_geo
  stateid=NULL,                  # map state names to IDs. set by load_stateid, read_stateid
  pal.info=NULL,                 # color palette info. set in pal/init_pal

  ## import params for specific data sources
  ihme.maxdate='latest',         # max date is latest version
  yyg.maxdate='latest',          # max date is latest version
                                 #
  ## transform params
  extra.wmax=6,                  # max weeks for computing models
  extra.nvmin=extra.wmax,        # min versions covering data used in models
  extra.exmin=0.25,              # min extra value. smaller values expand counts too much
  age.label=list(),              # labels for custom ages. each entry is vector of labels for fmts
  incompatible.ok=FALSE,         # require edited objects to be compatible

  ## workflow params
  ## URLs for automatic downloads
  download.url=list(
    doh='https://www.doh.wa.gov/Portals/1/Documents/1600/coronavirus/data-tables/PUBLIC_CDC_Event_Date_SARS.xlsx',    
    jhu.cases='https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv',
    jhu.deaths='https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv',
    nyt='https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv',
    trk='https://covidtracking.com/data/download/washington-history.csv'),

  ## program control
  verbose=F,                     # print progress messages
  debug=FALSE,                   # call debug code
  must.exist=F,                  # must all sub-inits succeed?
  save=NA,                       # shorthand for other save params 
                                 #   NA means save unless file exists
                                 #   T, F mean always or never save
  save.data=save,                # save top level data
  save.meta=save,                # save metadata
  save.txt=NA,                   # save results in txt format as well as RData
                                 #   NA means use default rule for type:
                                 #   F for all but top level data
  save.txt.data=is.na(save.txt)|save.txt, # save txt top level results. default T
  save.txt.meta=is.na(save.txt)|save.txt, # save txt metadata. default T
  save.out=T,                    # save outputs - figures and tables - when called via dofig
  save.fig=save.out,             # save figures (when called via dofig)
  save.tbl=save.out,             # save tables (when called via dotbl)
  save.txt.tbl=T,                # save txt tables. default T
                                 #    
  ## clean=switch(docx,readme=T,F), # remove everything and start fresh
  clean=F,                       # remove everything and start fresh
  clean.data=clean,              # remove datadir
  clean.sim=F,                   # clean simulations. default F
  clean.top=F,                   # clean top level data. default F
  clean.type=NULL,               # specific data types to clean. see clean_type
  clean.out=clean,               # remove outputs - figures and tables
  clean.fig=clean.out,           # remove figdir
  clean.tbl=clean.out,           # remove tbldir
                                 # 
  end=NULL                       # placeholder for last parameter
  ) {
  ## doc=docx;                      # to avoid confusion later
  ## source doc-specific files
  ## source_doc(doc);
  ## inputs and data are source specific, figures and tables global
  ## input dirs
  indirs=sapply(datasrc,function(src) filename(indir,src));
  ## output dirs
  datadirs=sapply(datasrc,function(src) filename(datadir,src));
  outdirs=c(datadirs,metadir,figdir,tbldir,tmpdir)
  ## assign parameters to param environment
  ## do it before calling any functions that rely on params
  init_param();
  ## clean and create directories as needed
  if (clean.data) unlink(datadir,recursive=TRUE)
  else {
    if (clean.top) {
      ## adapted from stackoverflow.com/questions/22069095. Thx!
      paths=list.files(datadir,full.names=TRUE);
      unlink(paths[!file.info(paths)$isdir]);
    }}
  if (clean.fig) unlink(figdir,recursive=T);
  if (clean.tbl) unlink(tbldir,recursive=T);
  ## clean specific types if desired.
  ## TODO: probably not right/useful here
  sapply(clean.type,clean_type);
  ## create input directories. nop if already exist
  sapply(indirs,function(dir) dir.create(dir,recursive=TRUE,showWarnings=FALSE));
  ## create output directories. nop if already exist
  sapply(outdirs,function(dir) dir.create(dir,recursive=TRUE,showWarnings=FALSE));
  ## init_doc();
  ## convert places.other list into data frame
  places.other=do.call(
    rbind,lapply(places.other,function(row) 
      data.frame(place=row[1],state=row[2],county=row[3],stringsAsFactors=FALSE)));
  param(places.other=places.other);
  invisible();
}

## clean specific data type. deletes directory, and any top level files
## TODO: probably not right/useful here
clean_type=function(what,cleandir=T) {
  param(datadir);
  ## delete top level files if exist
  files=list.files(datadir,full.names=T,pattern=paste(sep='','^',what,'\\.(RData|txt)'));
  unlink(files);
  if (cleandir) {
    whatdir=paste(sep='',what,'dir');
    ## delete directory if exists
    if (exists(whatdir,envir=param.env)) unlink(get(whatdir,envir=param.env),recursive=T);
  }
}
cleanq=function(what,cleandir=T) {
  what=as.character(pryr::subs(what));
  clean_type(what,cleandir);
}

