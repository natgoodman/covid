#################################################################################
##
## Author:  Nat Goodman
## Created: 21-07-30
##          by copying meta.R created 20-06-23 
##          with code adapted from age.R created 20-05-29
##
## Copyright (C) 2021 Nat Goodman.
## 
## Import and manage mortality data from CDC WONDER database
## WONDER online query tool limits results to 75K rows. This limits level of detail possible
##   in broad queries
## Workaround: get detail for WA counties and USA as a whole, but not all USA counties
## WONDER API too limited even for this purpose
## Workaround: generate data "manually" using web query tool
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
library("rjson");
## ---- Import mortality data ----
## input files in input/meta/mortality
## output files meta/mort .RData, .txt
download_mort=function(...) {
  stop("Mortality files downloaded 'manually' using CDC WONDER web query tool and stored in ",
       param(inmortdir));
}
import_mort=
  function(places=NULL,base=param(pop.file),placedir=param(placedir),metafile=param(acsmeta)) {
   places.all=sub('.json','',list.files(placedir,pattern='.json',full.names=FALSE));
    if (is.null(places)) places=places.all
    else {
      bad=places%-%places.all;
      if (length(bad)) stop('Invalid places(s): ',paste(collapse=', ',bad));
    }
   metanames=meta_names(metafile);
   pop=do.call(cbind,lapply(places,function(place) import_pop1(placedir,place,metanames)));
   colnames(pop)=places;
   save_pop(pop,base=base);
  }
import_mort_usa_age=function(file=param(mort.infiles)['usa_age']) {
  mort=read_mort_input(file,cq(Notes,Year,'Five-Year Age Groups Code',Deaths,Population));
  mort=import_mort_place(mort,'USA',file);
  total=tail(mort,n=1);
  mort=head(mort,n=-1)[,cq(age,deaths)]
  ## add total deaths row as 'all', place=USA column
  mort=rbind(data.frame(age='all',deaths=total$deaths,stringsAsFactors=FALSE),mort);
  mort=cbind(place='USA',mort);  
  ## TODO: find better way to hang onto pop
  assign('usa.age.pop',total$pop,envir=.GlobalEnv);
  mort; 
}
import_wa_county_age=function(file=param(mort.infiles)['wa_county_age']) {
  mort=read_mort_input(
    file,cq(Notes,Year,County,'County Code','Five-Year Age Groups Code',Deaths,Population),
    place.want=places_wa()%-%'state'
    );
  total=tail(mort,n=1);
  mort=head(mort,n=-1)[,cq(notes,place,age,deaths)];
  byplace=split(mort,mort$place);
  BREAKPOINT('before do.call')
  mort=do.call(rbind,lapply(byplace,function(mort) import_mort_place(mort,file,'wa')));
}
import_mort_place=function(mort,place,file) {
    total=tail(mort,n=1);
    mort=head(mort,n=-1)[,cq(place,age,deaths)];
    mort$age=mort_age(mort$age);
    age.want=c('0','1_4',
               sapply(seq(5,95,by=5),function(start) paste0(start,'_',start+4)),
               '100');
    bad=age.want%-%unique(mort$age);
    if (length(bad)) stop(file," is missing ages(s): ",paste(collapse=',',bad));
    bad=unique(mort$age)%-%c(age.want,NA);
    if (length(bad)) stop(file," has unexpected ages(s): ",paste(collapse=',',bad));

    ## add total deaths row as 'all'
    mort=rbind(data.frame(age='all',deaths=total$deaths,stringsAsFactors=FALSE),mort);
    ## TODO: find better way to hang onto pop
    assign(paste(sep='.',desuffix(basename(file)),place,'.age.pop'),total$pop,envir=.GlobalEnv);
    mort;
}

## colwant is vector of column names as they appear in file.
## colmap maps external column names from file to internal column names we use
## CAUTION: software harcodes internal names
read_mort_input=
  function(file,colwant,place.want=NULL,place.extra.ok=TRUE,geoid.want=NULL,geoid.extra.ok=TRUE) {
    mort=read_(file);
    ## colmap maps column names from file to internal names we use
    ## CAUTION: software harcodes internal names. be mindful if you change them!
    colmap=setNames(
      cq(notes,year,place,geoid,age,deaths,pop),
      cq(Notes,Year,County,'County Code','Five-Year Age Groups Code',Deaths,Population)
    );
    ## for sanity, make sure file has columns we need and all columns valid
    bad=colwant%-%colnames(mort);
    if (length(bad)) stop(file," is missing column(s): ",paste(collapse=', ',bad));
    bad=colwant%-%names(colmap);
    if (length(bad)) stop("unknown column name(s) in 'colwant': ",paste(collapse=', ',bad));
    ## select and rename columns we want
    mort=mort[,colwant];
    colnames(mort)=colmap[colwant];
    ## always need notes,year,age,deaths,pop
    bad=cq(notes,year,age,deaths,pop)%-%colnames(mort);
    if (length(bad)) stop("after mapping 'colwant', missing essential column(s): ",
                          paste(collapse=', ',bad));
    ## remove comment rows at bottom of file. all have year=NA
    i.last1=which(is.na(mort$year))[1];
    ## for sanity, make sure all years after i.last1 are NA
    bad=!is.na(mort$year[i.last1:nrow(mort)]);
    if (any(bad)) stop(file," has non-NA years after first NA. first NA row is ",i.last1)
    ## finally trim comment rows!
    i.last=i.last1-1;
    mort=mort[1:i.last,];
    ## now check content that remains and convert as needed
    ##   years are 2019
    bad=mort$year%-%2019;
    if (length(bad)) stop(file," has years other than 2019: ",paste(collapse=', ',bad));
    ## if ('notes'%in%colnames(mort)) {
    bad=mort$notes%-%c('Total','');
    if (length(bad)) stop(file," has unexpected notes(s): ",paste(collapse=',',bad))
    ## }
    if ('place'%in%colnames(mort)) {
      mort$place=sub(' County, [[:upper:]][[:upper:]]','',mort$place); # remove trailing verbiage
      if (!is.null(place.want)) {
        place.mort=unique(mort$place)%-%'';
        bad=place.want%-%place.mort;
        if (length(bad)) stop(file," is missing place(s): ",paste(collapse=', ',bad));
        if (!place.extra.ok) {
          bad=place.mort%-%place.want
          if (length(bad)) stop(file," has unexpected place(s): ",paste(collapse=',',bad));
        }}}
    if ('geoid'%in%colnames(mort)) {
      mort$geoid=ifelse(is.na(mort$geoid),NA,paste0('05000US',sprintf("%05i",mort$geoid)));
      if (!is.null(geoid.want)) {
        geoid.mort=unique(mort$geoid)%-%NA;
        bad=geoid.want%-%geoid.mort;
        if (length(bad)) stop(file," is missing geoid(s): ",paste(collapse=', ',bad));
        if (!geoid.extra.ok) {
          bad=geoid.mort%-%geoid.want
          if (length(bad)) stop(file," has unexpected geoid(s): ",paste(collapse=',',bad));
        }}}
    if (all(cq(place,geoid)%in%colnames(mort))) {
      ## make sure place,geoid pairs match what we have in geo file
      if (is.null(param(geo))) geo=load_geo();
      pg.geo=geo[geo$place%in%place.mort&geo$geoid%in%geoid.mort,cq(place,geoid)]
      pg.geo=pg.geo[order(place.mort),];
      pg.mort=unique(mort[,cq(place,geoid)])[order(place.mort),]
      bad=sapply(1:length(place.mort),function(i) any(pg.mort[i,]!=pg.geo[i,]));
      if (any(bad))
        stop("Some 'place,geoid' pairs in ",file," don't match geo file: ",
             cbind(pg.mort[bad,],pg.geo[bad,]));
    }
    ## if ('deaths'%in%colnames(mort))
    mort$deaths[mort$deaths=='Suppressed']=NA;
    ## if ('pop'%in%colnames(mort))
    mort$pop[mort$pop=='Not Applicable']=NA;
    mort;
  }


mort_age=function(age) {
  age=ifelse(age==1,0,age);             # convert 1 to 0
  age=sub('-','_',age,fixed=TRUE);      # convert '-' to '_', eg '1-4' to '1_4'
  age=sub('+','',age,fixed=TRUE);       # convert 100+ to 100
  age=ifelse(age%in%c('NS',''),NA,age); # convert NS, "" to NA
  age;
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
  pop=pop[,places.obj];
  ages.pop=rownames(pop)%-%'all';
  ages.obj=ages(obj)%-%'all';
  if (length(ages.obj)) {
    starts.pop=age_starts(ages.pop);
    starts.obj=age_starts(ages.obj);
    cats=cut(starts.pop,c(starts.obj,100),right=F,labels=ages.obj);
    groups=split(ages.pop,cats);
  } else groups=NULL;
  pop.obj=rbind(pop['all',], t(sapply(groups,function(i) colSums(pop[i,]))));
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
cmp_pops=function(objs,places=NULL,ages=NULL,pop=param(pop),
                  incompatible.ok=param(incompatible.ok)) {
  if (length(objs)<=1) return(TRUE);    # trivial case - just one object
  pops=lapply(objs,function(obj) pop(obj)); # CAUTION: use pop(obj) form to avoid scope problem
  ## filter, and order by age (for testing equality later)
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

