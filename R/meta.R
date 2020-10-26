#################################################################################
##
## Author:  Nat Goodman
## Created: 20-06-23 
##          with code adapted from age.R created 20-05-29
##
## Copyright (C) 2020 Nat Goodman.
## 
## Import and manage metadata. Presently population by county and age group
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
library("rjson");
## ---- Import population metadata ----
## creates meta/popbyage .Rdata, .txt
## CAUTION: download sometimes fails mid-stream. no idea why... 
download_popbyage=function(places=NULL,acs5yr=param(acs5yr)) {
  geos=read.csv(acs5yr,stringsAsFactors=F);
  ## skip first row (USA) and fix 2nd row (state)
  if (geos$name[1]!='United States')
    stop("First name in acs file should be 'United States', not ",geos$name[1]);
  if (geos$name[2]!='Washington')
    stop("Second name in acs file should be 'Washington', not ",geos$name[2]);
  geos=geos[-1,1:2];
  colnames(geos)=cq(geoid,place);
  geos[1,'place']='state';
  ## clean up county names. 
  geos$place=sub(' County, WA','',geos$place);
  if (!is.null(places)) geos=subset(geos,subset=place%in%places);
  ## escape spaces in places like Walla Walla. do it after filtering places
  geos$place=gsub(' ','\\ ',geos$place,fixed=TRUE);
  ## construct wget commands, then do it!
  wgets=with(geos,{
    url=paste0('https://api.censusreporter.org/1.0/data/show/latest?table_ids=B01001&geo_ids=',
               geoid);
    qurl=paste0('"',url,'"');
    ## construct output filename with 'paste' because 'filename' elides name vector
    outfile=paste0(param(acsplacedir),'/',geos$place,'.json')
    wget=paste('wget',qurl,'-O',outfile);
  })
  ## do it!
  sapply(wgets,function(wget) {
    if (param(verbose)) print(wget);
    system(wget);
  })
}
## geoids are from CensusReporter website. place used for file name
download_annarbor=function(geoid='05000US26161',place='Washtenaw_MI') download_place(geoid,place)
download_omaha=function(geoid='05000US31055',place='Douglas_NE') download_place(geoid,place)
## download one county from one state
download_place=function(geoid,place) {
  url=paste0('https://api.censusreporter.org/1.0/data/show/latest?table_ids=B01001&geo_ids=',
             geoid);
  qurl=paste0('"',url,'"');
  ## construct output filename with 'paste' because 'filename' elides name vector
  outfile=paste0(param(acsplacedir),'/',place,'.json')
  wget=paste('wget',qurl,'-O',outfile);
  if (param(verbose)) print(wget);
  system(wget);
} 
import_popbyage=
  function(places=NULL,base=param(popbyage),
           ages.all=c("all","0_19","20_39","40_59","60_79","80_")) {
    dir=param(acsplacedir);
    metafile=param(acsmeta);
    places.all=sub('.json','',list.files(dir,pattern='.json',full.names=FALSE));
    if (is.null(places)) places=places.all
    else {
      bad=places%notin%places.all;
      if (any(bad)) stop('Invalid places(s): ',paste(collapse=', ',places[bad]));
    }
    metanames=meta_names(metafile);
    pop=do.call(cbind,lapply(places,function(place) import_pop1(dir,place,metanames,ages.all)));
    colnames(pop)=places;
    save_popbyage(pop,base=base);
  }
import_pop1=
  function(dir,place,metanames=NULL,
           ages.all=c("all","0_19","20_39","40_59","60_79","80_")) {
    if (is.null(metanames)) metanames=meta_names(param(acsmeta));
    file=filename(dir,place,suffix='.json');
    if (param(verbose)) print(paste('>>> importing',file));
    json=fromJSON(file=file);
    data=unlist(json$data[[1]]$B01001$estimate);
    ## import variables from metanames
    list2env(metanames,environment());
    names(data)=names;
    data.all=data[i.all]
    data.male=data[i.male]
    data.female=data[i.female]
    data.both=data.male[-1]+data.female[-1]
    ## for sanity, make sure totals match individual values
    if (data.all!=(data.male['male']+data.female['female']))
      stop("Bad news: data.all != male + female values");
    if (data.male[1]!=sum(data.male[-1]))
      stop("Bad news: male total != sum of male values");
    if (data.female[1]!=sum(data.female[-1]))
      stop("Bad news: female total != sum of female values");
    if (data.all!=sum(data.both))
      stop("Bad news: data.all != sum of combined male + female values");
    ## combine columns into age groups of interest
    names=names(data.both);
    ages=strsplit(names,'_');
    ages=apply(do.call(rbind,ages),2,as.integer);
    cats=cut(ages[,1],c(0,20,40,60,80,100),right=F,labels=ages.all[-1]);
    groups=split(names,cats);
    pop=c(data.all,sapply(groups,function(j) sum(data.both[j])));
    pop=data.frame(pop);
    pop;
  }
## process censusreport metadata file
meta_names=function(metafile=param(acsmeta)) {
  json=fromJSON(file = metafile);
  names=sapply(json$tables$B01001$columns,function(col) col$name);
  names=tolower(names);
  names[1]='all';
  names=gsub(':| years','',names);
  names=gsub('under 5','0_4',names);
  names=gsub(' and over','',names);
  names=gsub('( to )|( and )','_',names);
  i.male1=which('male'==names)
  i.female1=which('female'==names)
  list(names=names,i.all=which('all'==names),
       i.male=i.male1:(i.female1-1),i.female=i.female1:(length(names)));
}

## ---- Save and Load population metadata ----
## base includes path. suffix optional
save_popbyage=function(pop,base=param(popbyage),suffix=cq(txt,RData)) {
  param(save.meta,save.txt.meta);
  save_(pop,base=base,save=save.meta,save.txt=FALSE,suffix=suffix);
  if (save.txt.meta) {
    ## want explicit age column in text file
    pop=data.frame(age=rownames(pop),pop);
    file=resuffix(base,old.suffix=suffix,suffix='txt');
    write.table(pop,file=file,sep='\t',quote=F,row.names=F);
  }
}
load_popbyage=function(base=param(popbyage)) {
  pop=load_(base=base);
  param(pop=pop);
}
read_popbyage=function(base=param(popbyage)) {
  pop=read_(base=base,row.names='age');
  param(pop=pop);
}
## read_pop=function(places=NULL,ages=NULL,file=filename(param(metadir),'population.txt')) {
##   pop=read.delim(file,row.names='age',stringsAsFactors=FALSE);
##   pop=filter_pop(pop,places,ages);      # nop if places, ages NULL
##   pop;
## }
## ---- Filter and format population metadata ----
filter_pop=function(pop,places=NULL,ages=NULL) {
  if (!is.null(places)) pop=pop[,places,drop=FALSE];
  if (!is.null(ages)) pop=pop[ages,,drop=FALSE];
  pop;
}
places_all=function(pop=param(pop)) {
  if (is.null(pop)) pop=load_popbyage();
  colnames(pop);
}
places_wa=function(pop=param(pop)) {
  places.all=places_all(pop);
  places.away=grep('_',places.all,value=T);
  places.all %-% places.away;
}
ages_all=function(pop=param(pop)) {
  if (is.null(pop)) pop=load_popbyage();
  rownames(pop);
}
## compare pops from multiple, possibly edited, objects
## returns whether pops all equal
## CAUTION: make sure places, ages valid for all objects!
cmp_pops=function(objs,places=NULL,ages=NULL,pop=param(pop),
                  incompatible.ok=param(incompatible.ok)) {
  if (length(objs)<=1) return(TRUE);    # trivial case - just one object
  pops=lapply(objs,function(obj) obj$pop);
  orig=sapply(pops,is.null);
  if (all(orig)) return(TRUE);          # all objects 'original', so perforce equal
  ## some objects edited. have to do merge & compare these vs original pop and each other
  pops=pops[!orig];
  if (any(orig)) pops=c(list(load_popbyage()),pops); # include original pop in list to be merged
  ## tack 'age' onto each pop, filter, and order by age (for testing equality later)
  pops=lapply(pops,function(pop) {
    pop=filter_pop(pop,places,ages);
    pop=cbind(age=rownames(pop),pop);
    pop[order(pop$age),];
  });
  ## pick one pop to compare others against
  pop0=pops[[1]];
  pops=tail(pops,n=-1);
  ok=all(sapply(pops,function(pop) identical(pop0,pop)));
  if (!incompatible.ok&&!ok) 
    stop("Objects are incompatible (edited in conflicting ways). ",
         "Sorry I can't be more specific");
  ok;
}

## convert data frame of absolute counts into per capita counts
## express as counts per million. TODO: paramerize scale factor
## works for multiple age groups for one place or one age group for multiple places
per_capita=function(data,pop,places='state',ages='all') {
  if (length(places)>1&&length(ages)>1)
    stop("Only one of 'places' or 'ages' can have multiple values");
  pop=if(is.null(pop)) read_pop(places,ages) else filter(pop,places,ages);
  if (length(ages)>1) pop=data.frame(t(pop[,places,drop=F]),check.names=FALSE);    
  pop=repr(pop,nrow(data));
  data.frame(date=data$date,round(1e6*data[,-1]/pop),check.names=FALSE);
}

