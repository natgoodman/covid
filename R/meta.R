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
## input files in input/meta and subdirs
## output files meta/pop .RData, .txt
download_pop=function(geo=param(geo),placedir=param(placedir),usa=TRUE) {
  if (param(verbose)) print(paste('>>> downloading pop to',placedir));
  if (is.null(geo)) geo=load_geo();
  withrows(geo,row,download_place(geoid,place,placedir));
  if (usa) download_place(geoid='01000US',place='USA',placedir=placedir)
  nrow(geo);
}
## download one county from one state
download_place=function(geoid,place,placedir=param(placedir)) {
  url=paste0('https://api.censusreporter.org/1.0/data/show/latest?table_ids=B01001&geo_ids=',
             geoid);
  qurl=paste0('"',url,'"');
  qplace=paste0('"',place,'"');
  ## construct output filename with 'paste' because 'filename' elides name vector
  outfile=paste0(placedir,'/',qplace,'.json')
  wget=paste('wget',qurl,'-O',outfile);
  if (param(verbose)) print(wget);
  system(wget);
} 
import_pop=
  function(places=NULL,base=param(pop.file),placedir=param(placedir),metafile=param(acsmeta)) {
   places.all=sub('.json','',list.files(placedir,pattern='.json',full.names=FALSE));
    if (is.null(places)) places=places.all
    else {
      bad=places%notin%places.all;
      if (any(bad)) stop('Invalid places(s): ',paste(collapse=', ',places[bad]));
    }
   metanames=meta_names(metafile);
   pop=do.call(cbind,lapply(places,function(place) import_pop1(placedir,place,metanames)));
   colnames(pop)=places;
   save_pop(pop,base=base);
  }
import_pop1=function(placedir,place,metanames=NULL) {
  if (is.null(metanames)) metanames=meta_names(param(acsmeta));
  file=filename(placedir,place,suffix='.json');
  if (param(verbose)) print(paste('>>> importing pop',file));
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
  pop=data.frame(pop=c(data.all,data.both));
  pop;
}
age_starts=function(ages) as.integer(sapply(strsplit(ages,'_'),function(ages) ages[1]));
## create pop for object
obj_pop=function(obj) {
  pop=param(pop);
  if (is.null(pop)) pop=load_pop();
  places.pop=colnames(pop);
  places.obj=places(obj)%-%c('Unassigned','Out of WA','Unknown')
  bad=places.obj%-%places.pop;
  if (length(bad)) stop("Bad news: object contains unknown place(s): ",paste(collapse=', ',bad));
  pop=pop[,places.obj,drop=FALSE];
  ages.pop=rownames(pop)%-%'all';
  ages.obj=ages(obj)%-%'all';
  if (length(ages.obj)) {
    starts.pop=age_starts(ages.pop);
    starts.obj=age_starts(ages.obj);
    cats=cut(starts.pop,c(starts.obj,100),right=F,labels=ages.obj);
    groups=split(ages.pop,cats);
  } else groups=NULL;
  ## pop.obj=rbind(pop['all',], t(sapply(groups,function(i) colSums(pop[i,,drop=FALSE]))));
  ## NG 21-10-31: this way works even when only place. gotta love R...
  pop.obj=rbind(all=pop['all',,drop=FALSE],
                do.call(rbind,lapply(groups,function(i) colSums(pop[i,,drop=FALSE]))));
  ## colnames(pop.obj)=places.obj;
  pop.obj;
}
## create pop for CDC Case Surveillance Data
cdc_pop=function() {
  pop=param(pop);
  if (is.null(pop)) pop=load_pop();
  places.pop=colnames(pop);
  if ('USA'%notin%places.pop) stop('Cannot create CDC pop: USA not in main pop file');
  pop=pop[,'USA',drop=FALSE];
  ages.pop=rownames(pop)%-%'all';
  ## hardcode CDC ages to 10 year ranges
  ages.cdc=c(paste(sep='_',seq(0,70,by=10),seq(9,79,by=10)),'80_');
  starts.pop=age_starts(ages.pop);
  starts.cdc=age_starts(ages.cdc);
  cats=cut(starts.pop,c(starts.cdc,100),right=F,labels=ages.cdc);
  groups=split(ages.pop,cats);
  pop.cdc=c(all=pop['all','USA'],sapply(groups,function(i) sum(pop[i,])));
  data.frame(USA=pop.cdc);
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
## ---- Import geo & stateid metadata files
## geo - get entries for WA and other, reformat, store in meta directory
import_geo=function(infile=param(geo.infile),base=param(geo.file)) {
  if (param(verbose)) print(paste('>>> importing geo',infile));
  geo=read.delim(infile,stringsAsFactors=FALSE);
  geo=geo[,cq(USPS,GEOID,NAME)];            # select columms we want
  colnames(geo)=cq(state,geoid,county);     # change colnames per our convention
  geo$geoid=paste0('05000US',sprintf("%05i",geo$geoid));
                                            # CensusReporter county geoids are '05000US'+5 digits 
  geo$county=sub(' County','',geo$county);  # clean up county names
  ## get WA entries
  geo.wa=subset(geo,subset=state=='WA');
  colnames(geo.wa)[3]='place';
  geo.wa=rbind(geo.wa,cq(WA,'04000US53',state)); # add entry for WA state
  ## get 'nonwa' entries
  places.nonwa=param(places.nonwa);
  geo.nonwa=merge(geo,places.nonwa);             # join on state, county
  geo.nonwa=geo.nonwa[,cq(state,geoid,place)];   # rename and reorder columns to match WA
  geo=rbind(geo.wa,geo.nonwa)
  save_geo(geo,base=base);
}
## stateid - just convert colnames to lower case, store in meta directory
import_stateid=function(infile=param(stateid.infile),base=param(stateid.file)) {
  if (param(verbose)) print(paste('>>> importing stateid',infile));
  stateid=read.delim(infile,stringsAsFactors=FALSE);
  colnames(stateid)=tolower(colnames(stateid));
  save_stateid(stateid,base=base);
}
## compare pops from multiple, possibly edited, objects
## returns whether pops all equal
## CAUTION: make sure places, ages valid for all objects!
cmp_pops=function(objs,places=NULL,ages=NULL,pop=param(pop)) {
  if (length(objs)<=1) return(TRUE);    # trivial case - just one object
  pops=lapply(objs,function(obj) pop(obj)); # CAUTION: use pop(obj) form to avoid scope problem
  ## filter, and order by age (for testing equality later)
  pops=lapply(pops,function(pop) {
    pop=filter_pop(pop,places,ages);
    ## BREAKPOINT('cmp_pops: lapply pops, after filter')
    ## pop=cbind(age=rownames(pop),pop);
    ## pop[order(pop$age),,drop=FALSE];
    ## NG 21-10-31: dunno why I didn't do it this way in the first place...
    pop=pop[rownames(pop),,drop=F];
  });
  ## pick one pop to compare others against
  pop0=pops[[1]];
  pops=tail(pops,n=-1);
  ok=all(sapply(pops,function(pop) identical(pop0,pop)));
  if (!ok) 
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

## ---- Filter, compare, format population metadata ----
filter_pop=function(pop,places=NULL,ages=NULL) {
  places=places%&%colnames(pop);
  ages=ages%&%rownames(pop);
  if (!is.null(places)) pop=pop[,places,drop=FALSE];
  if (!is.null(ages)) pop=pop[ages,,drop=FALSE];
  pop;
}

## ---- Access metadata ----
places_all=function(geo=param(geo)) {
  if (is.null(geo)) geo=load_geo();
  geo$place;
}
places_wa=function(geo=param(geo)) {
  if (is.null(geo)) geo=load_geo();
  geo$place[geo$state=='WA'];
}
places_nonwa=function(geo=param(geo)) {
  if (is.null(geo)) geo=load_geo();
  geo$place[geo$state!='WA'];
}
geoids_all=function(geo=param(geo)) {
  if (is.null(geo)) geo=load_geo();
  geo$geoid;
}
geoids_wa=function(geo=param(geo)) {
  if (is.null(geo)) geo=load_geo();
  geo$geoid[geo$state=='WA'];
}
geoids_nonwa=function(geo=param(geo)) {
  if (is.null(geo)) geo=load_geo();
  geo$geoid[geo$state!='WA'];
}
geoid_state=geoids_state=function(geo=param(geo)) {
  if (is.null(geo)) geo=load_geo();
  geo$geoid[geo$place=='state']
}

## convenience function to get WA counties
counties_wa=function(geo=param(geo)) {
  if (is.null(geo)) geo=load_geo();
  geo[geo$state=='WA'&geo$place!='state','place'];
}
## convenience function to get WA county populations for 'all'
pop_wa=function(pop=param(pop),geo=param(geo),ages=NULL) {
  counties.wa=counties_wa(geo);
  pop['all',counties.wa];
}
## ages_all used for error message and label
## no easy way to get the correct value. fudge it by hardcoding all ages in uses circa Mar 2021
ages_all=function() c('all','0_19','20_39','40_59','60_79','20_34','35_49','50_64','65_79','80_')

state_name2id=function(name,stateid=param(stateid)) {
  if (is.null(stateid)) stateid=load_stateid();
  sapply(name,function(name) stateid$id[stateid$name==name]);
}
state_id2name=function(id,stateid=param(stateid)) {
  if (is.null(stateid)) stateid=load_stateid();
  sapply(id,function(id) stateid$name[stateid$id==id]);
}

