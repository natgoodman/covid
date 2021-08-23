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
## doc.all=cq(readme,updat);
doc.all=cq(updat);                      # README presently has no code
init=function(
  ## NG 21-06-14: datasrc now active sources, datasrc.old inactive, datasrc.all all
  datasrc=cq(doh,jhu,nyt,cdc),
  datasrc.old=cq(ihme,trk,yyg),
  datasrc.all=c(datasrc,datasrc.old),
  ## doc parameters 
  doc='updat',                             # controls data, figure, table subdirs
  docx=match.arg(doc,doc.all),
  ## docx=NULL,
  run.id=NULL,                              # to separate runs for tests
  ## data directories. 
  indir='input',                            # top level input dir
  datadir=filename('data',run.id),          # top level output data files
  logdir=filename('log',run.id),            # top level log directory
  metadir=filename('meta',run.id),          # metadata files
  ## mortdir=filename(metadir,'mort',run.id),          # mortdata files
  ## NG 20-11-14: figdir, tbldir moved to init_doc
  ## figdir=filename('figure',run.id),         # figures
  ## tbldir=filename('table',run.id),          # tables
  tmpdir=filename(datadir,'tmp'),           # tmp dir if needed
  inmetadir=filename(indir,'meta'),
  placedir=filename(inmetadir,'place'),
  inmortdir=filename(inmetadir,'mort'),
  ## metadata input URLs and files
  ## acs5yr=filename(inmetadir,'acs2018_5yr_B01001_05000US53041.csv'),
  acsmeta=filename(inmetadir,'metadata.json'),
  geo.infile=filename(inmetadir,'geoid.txt'),         # geoids for all US counties (Census Bureau)
  stateid.infile=filename(inmetadir,'stateid.txt'),   # map state names to IDs(World Pop Review)
  ## computed metadata files
  pop.file=filename(metadir,'pop'),         # pop by place, age
  geo.file=filename(metadir,'geo'),         # geoids and place names
  stateid.file=filename(metadir,'stateid'), # map state names to IDs
  mort.file=filename(metadir,'mort'),       # mortality (see mort.R for details)
  mortpop.file=filename(metadir,'mortpop'), # mortality pop (see mort.R for details)
 ## descriptors for non-WA places of interest
  ## entries are place, state, county. converted to data frame in code below
  places.nonwa=
    list(cq('Ann Arbor',MI,Washtenaw),
         cq(DC,DC,'District of Columbia'),
         cq(Boston,MA,Suffolk),
         cq('San Diego',CA,'San Diego'),
         cq(Omaha,NE,Douglas),
         cq(Austin,TX,Travis),
         cq(Fairbury,IL,Livingston),
         cq('Mackinac Island',MI,Mackinac),
         cq('Big Island',HI,Hawaii),
         cq('Cape Cod',MA,Barnstable)),
  ## outdir=c(datadir,figdir,tbldir,tmpdir),  # top level output dirs
  ## cached data
  pop=NULL,                      # base pop by place, age. set by load_pop, read_pop
  geo=NULL,                      # geoids and place names. set by load_geo, read_geo
  stateid=NULL,                  # map state names to IDs. set by load_stateid, read_stateid
  mort=NULL,                     # mortality by place, age. set by load_mort, read_mort
  mortpop=NULL,                  # pop from mort files. set by load_mortpop, read_mortpop
  pal.info=NULL,                 # color palette info. set in pal/init_pal

  ## import params for specific data sources
  ihme.maxdate='latest',         # max date is latest version
  yyg.maxdate='latest',          # max date is latest version
  cdc.prepro=                    # shell pipeline for preprocessing cdc download
    "| /usr/bin/tail -n +2 | /bin/sed -r 's/\"[^\"]+\"//g' | /bin/awk -f bin/cdc.awk >",
  cdc.sql=list(
    cdc=
      "CREATE TABLE cdc (
         date DATE,age tinyint,sex char(1),cases char(1),admits char(1),icus char(1),deaths char(1)
       )",
    load.part1="LOAD DATA LOCAL INFILE",
    load.part2="INTO TABLE cdc FIELDS TERMINATED BY ','", 
    ageidx="CREATE INDEX age on cdc(age)",
    dateidx="CREATE INDEX date on cdc(date)",
    data=
      "CREATE TABLE data AS
         SELECT date,age,
         SUM(cases='1') AS cases, SUM(cases='0') AS casesF, SUM(cases='') AS casesU,
         SUM(admits='1') AS admits, SUM(admits='0') AS admitsF, SUM(admits='') AS admitsU,
         SUM(icus='1') AS icus, SUM(icus='0') AS icusF, SUM(icus='') AS icusU,
         SUM(deaths='1') AS deaths, SUM(deaths='0') AS deathsF, SUM(deaths='') AS deathsU
         FROM cdc GROUP BY date,age",
    ## cases="SELECT date,age,cases,casesF,casesU FROM data",
    ## admits="SELECT date,age,admits,admitsF,admitsU FROM data",
    ## icus="SELECT date,age,icus,icusF,icusU FROM data",
    ## deaths="SELECT date,age,deaths,deathsF,deathsU FROM data",
    fetch=
      "SELECT date,age,
         cases,casesF,casesU,admits,admitsF,admitsU,icus,icusF,icusU,deaths,deathsF,deathsU
       FROM data"),

  ## transform params
  extra.fmla='y~date:w+w',       # default formula for lm models
  extra.errtype=cq('*','+',multiplicative,additive),  # error type for models
  extra.wmax=6,                  # max weeks for computing models
  extra.mulmax=4,                # max multiplicative error. larger values expand counts too much
                                 # ages, places for models defaults fast and close enough
  extra.ages='all',              #   NULL means ages(obj)
  extra.places='state',          #   NULL means places(obj)
  extra.minobjs=2.5,             # how many objs do we need to compute model. multiple of wmax
  extra.maxobjs=4,               # max number of objs used to compute model. multiple of wmax
  incompatible.ok=FALSE,         # require edited objects to be compatible

  age.label=list(),              # labels for custom ages. each entry is vector of labels for fmts

  ## workflow params
  ## URLs for automatic downloads
  download.url=list(
    ## DOH URL changed as of version 20-12-20
    ## doh='https://www.doh.wa.gov/Portals/1/Documents/1600/coronavirus/data-tables/PUBLIC_CDC_Event_Date_SARS.xlsx',
    doh='https://www.doh.wa.gov/Portals/1/Documents/1600/coronavirus/data-tables/WA_COVID19_Cases_Hospitalizations_Deaths.xlsx',
    jhu.cases='https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv',
    jhu.deaths='https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv',
    nyt='https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv',
    trk='https://covidtracking.com/data/download/washington-history.csv',
    cdc='https://data.cdc.gov/api/views/vbim-akqf/rows.csv'),

  ## program control
  verbose=FALSE,                 # print progress messages
  debug=FALSE,                   # call debug code
  must.exist=FALSE,              # must all sub-inits succeed?
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
  save.out=TRUE,                 # save outputs - figures and tables - when called via dofig
  ## NG 20-11-14: save.fig, save.tbl moved to init_doc
  ## save.fig=save.out,             # save figures (when callsed via dofig)
  ## save.tbl=save.out,             # save tables (when called via dotbl)
  ## save.txt.tbl=T,                # save txt tables. default T
                                 #    
  ## clean=switch(docx,readme=T,F), # remove everything and start fresh
  clean=FALSE,                   # remove everything and start fresh
  clean.data=clean,              # remove datadir
  clean.sim=FALSE,               # clean simulations. default F
  clean.top=FALSE,               # clean top level data. default F
  clean.type=NULL,               # specific data types to clean. see clean_type
  clean.out=clean,               # remove outputs - figures and tables
  clean.fig=clean.out,           # remove figdir
  clean.tbl=clean.out,           # remove tbldir
                                 # 
  end=NULL                       # placeholder for last parameter
  ) {
  doc=docx;                      # to avoid confusion later
  ## source doc-specific files
  ## source_doc(doc);
  ## inputs and data are source specific, figures and tables global
  ## input dirs
  indirs=sapply(datasrc,function(src) filename(indir,src));
  ## output dirs
  datadirs=sapply(datasrc,function(src) filename(datadir,src));
  ## NG 20-11-14: figdir, tbldir moved to init_doc
  ## outdirs=c(datadirs,metadir,figdir,tbldir,tmpdir)
  outdirs=c(datadirs,metadir,logdir,tmpdir)
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
  ## convert places.nonwa list into data frame
  places.nonwa=do.call(
    rbind,lapply(places.nonwa,function(row) 
      data.frame(place=row[1],state=row[2],county=row[3],stringsAsFactors=FALSE)));
  param(places.nonwa=places.nonwa);
 invisible();
}
## initialize doc parameters
## NG 19-01-11: abandon subdoc concept for 'supp' - not useful for effit
##              retain for xperiment just in case...
init_doc=function(
  doc=param(doc),                  
  subdoc=NULL,
  version='latest',
  versionx=if(version=='latest') latest_version('doh') else version,
  ## output directories. filename function ignores subdoc if NULL
  vsndir=if(doc%in% cq(updat,updatsupp)) versionx else NULL,
  figdir=filename('figure',param(doc),subdoc,param(run.id),vsndir), # directory for figures
  tbldir=filename('table',param(doc),subdoc,param(run.id),vsndir),  # directory for tables
  ## 'where', 'what' used in updatsupp
  where=if(doc=='updatsupp') cq(wa1,wa2,wa3,nonwa1,nonwa2,fav,usa) else NULL,
  what=if(doc=='updatsupp') cq(cases,admits,icus,deaths,admdea) else NULL,
  ## output modifiers
  outpfx=NULL,                  # prefix before figure or table number - NOT USED
  outsfx=letters,               # suffix in figure and table blocks
  sectpfx=FALSE,                # add section number to prefix eg, S1
  outlabel=(doc!='updatsupp'),  # use label, eg, Figure nnn. updatsupp sets to FALSE
  sectnum=1,                    # section number. usually set in docs
  sect=NULL,
  ## figures
  figpfx=outpfx,
  figsfx=outsfx,
  figlabel=outlabel,
  fignum=1,
  figblk=NULL,                  # index into figsfx if in figure block
  ## tables
  tblpfx=outpfx,
  tblsfx=outsfx,
  tbllabel=outlabel,
  tblnum=1,
  tblblk=NULL,                  # index into tblsfx if in table block
  ##
  pjto=TRUE,                    # copy to Mac using pjto command (reverse tunnel)
  ## xtra figures - not included in document
  xfigpfx='X',
  xfigsfx=outsfx,
  ## xfignum=1,                 # extras now use same numbers and blocks as regulars
  ## xfigblk=NULL,              # ditto
  ## clean, save
  save.out=TRUE,
  save.fig=save.out,            # save figures (when called via dofig)
  save.tbl=save.out,            # save tables (when called via dotbl)
  save.RData.tbl=FALSE,         # save RData tables. default F
  save.txt.tbl=TRUE,            # save txt tables. default T
  clean.out=FALSE,
  clean.fig=clean.out,          # remove figdir
  clean.tbl=clean.out,          # remove tbldir
  ## plot control
  figscreen=FALSE,               # plot figures on screen - disabled due to X11 issues
  fignew=figscreen,              # plot each figure in new window
  figextra=FALSE,                # plot extra figures
  ## NG 21-04-25: docfun no longer used and messes up doc='xper'
  ## doc generation function
  ## docfun=get(paste(collapse='',c('doc_',param(doc),subdoc))),
  docsect=NULL,                  # all document sections. set by docfun
  end=NULL                       # placeholder for last parameter
  ) {
  version=versionx;              # to avoid confusion later
  ## assign parameters to param environment
  ## do it before calling any functions that rely on params
  assign_param();
  ## clean and create output directories if needed
  outdir=c(figdir,tbldir);
  if (clean.fig) unlink(figdir,recursive=T);
  if (clean.tbl) unlink(tbldir,recursive=T);
  sapply(outdir,function(dir) dir.create(dir,recursive=TRUE,showWarnings=FALSE));
  invisible();
}

## clean specific data type. deletes directory, and any top level files
## TODO: probably not right/useful here
clean_type=function(what,cleandir=T) {
  param(datadir);
  ## delete top level files if exist
  files=list.files(datadir,full.names=TRUE,pattern=paste(sep='','^',what,'\\.(RData|txt)'));
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

