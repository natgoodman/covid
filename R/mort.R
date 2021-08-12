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
## See also xper_mort.R which contains 'extra' code needed to check and analyze
## mortality and pop data
## 
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Import mortality data ----
## input files in input/meta/mortality
## output files meta/mort .RData, .txt
download_mort=function(...) {
  stop("Mortality files downloaded 'manually' using CDC WONDER web query tool and stored in ",
       param(inmortdir));
}
import_mort=function() {
  ## import each piece. note spelling of function names 'imort' not 'import' (no 'p')
  usa=imort_usa();
  state=imort_state();
  wa=imort_wa();
  nonwa=imort_nonwa();
  mort=cbind(usa,state,wa,nonwa);
  mort=mort[rownames(mort)!='NS',]
  save_mort(mort);
  invisible(mort);
}
## import USA data by age and 'all'
imort_usa=function(base='usa',file=NULL) {
  if (param(verbose)) print(paste('>>> importing usa mortality'));
  mort=read_mort_input(base,cq(Notes,Year,'Five-Year Age Groups Code',Deaths),file);
  ## usa has total lines
  total=tail(mort,n=1);
  mort=head(mort,n=-1);
  mort=import_mort_age(mort,file,total,'USA');
  invisible(mort);
}
## import WA state data by age and 'all'
imort_state=function(bases=cq(state_age,state_all),files=NULL) {
  if (param(verbose)) print(paste('>>> importing WA state mortality'));
  if (is.null(files))
    files=sapply(bases,function(base) filename(param(inmortdir),base,suffix='txt'));
  ## state_age has ages but not 'all'
  file=files[1];
  mort=read_mort_input(
    file=file,colwant=cq(Notes,Year,State,'Five-Year Age Groups Code',Deaths));
  ## make sure place really is Washington
  bad=mort$place%-%'Washington';
  if (length(bad)) stop(file," has unexpected place(s): ",paste(collapse=',',bad));
  file=files[2];
  total=read_mort_input(file=file,colwant=cq(Notes,Year,State,Deaths));
  ## make sure place really is Washington
  bad=total$place%-%c('Washington','');
  if (length(bad)) stop(file," has unexpected place(s): ",paste(collapse=',',bad));
  total=tail(total,n=1);
  mort=import_mort_age(mort,file,total,'state');
  invisible(mort);
}
## import WA places except 'state' by age and 'all'
imort_wa=function(base='wa',file=NULL) {
  if (param(verbose)) print(paste('>>> importing WA county mortality'));
  mort=read_mort_input(
    base,cq(Notes,Year,County,'County Code','Five-Year Age Groups Code',Deaths),file);
  ## wa has total line for entire state which we don't need
  ## total=tail(mort,n=1);
  mort=head(mort,n=-1);
  mort$place=sub(' County, WA','',mort$place); # clean county names
  byplace=split(mort,mort$place);
  mort=do.call(cbind,lapply(byplace,function(mort) import_mort_age(mort,file)));
  invisible(mort);
}
## import non-WA places. no ages, just 'all'
imort_nonwa=function(base='nonwa',file=NULL) {
  if (param(verbose)) print(paste('>>> importing non-WA county mortality'));
  mort=read_mort_input(base,cq(Notes,Year,County,'County Code',Deaths),file);
  ## nonwa has total line for entire USA which we don't need
  ## total=tail(mort,n=1);
  mort=head(mort,n=-1);
  mort$geoid=paste0('05000US',sprintf("%05i",mort$geoid));
  if (is.null(param(geo))) geo-load_geo();
  geoids.want=geoids_nonwa();
  geoids.mort=unique(mort$geoid);
  bad=geoids.want%-%geoids.mort;
  if (length(bad)) stop(file," is missing geoid(s): ",paste(collapse=', ',bad));
  mort=mort[mort$geoid%in%geoids.want,]
  mort$place=sapply(mort$geoid,function(geoid) geo[geo$geoid==geoid,'place']);
  ## initialize output as blank data frame then overlay mort
  deaths=
    as.data.frame(
      matrix(nrow=length(ages_mort()),ncol=nrow(mort), dimnames=list(ages_mort(),mort$place)));
  deaths['all',]=mort$deaths;
  invisible(deaths);
}
## colwant is vector of column names as they appear in file.
## colmap maps external column names from file to internal column names we use
## CAUTION: software harcodes internal names
read_mort_input=
  function(base=NULL,colwant,file=NULL) {
    if (is.null(file)) file=filename(param(inmortdir),base,suffix='txt');
    if (param(verbose)) print(paste('+++ reading mortality file',file));
    mort=read_(file);
    ## colmap maps column names from file to internal names we use
    ## CAUTION: software harcodes internal names. be mindful if you change them!
    colmap=setNames(
      cq(notes,year,place,geoid,place,geoid,age,deaths),
      cq(Notes,Year,State,'State Code',County,'County Code','Five-Year Age Groups Code',Deaths));
    ## for sanity, make sure file has columns we need and all columns valid
    bad=colwant%-%colnames(mort);
    if (length(bad)) stop(file," is missing column(s): ",paste(collapse=', ',bad));
    bad=colwant%-%names(colmap);
    if (length(bad)) stop("unknown column name(s) in 'colwant': ",paste(collapse=', ',bad));
    ## select and rename columns we want
    mort=mort[,colwant];
    colnames(mort)=colmap[colwant];
    ## always need notes,year,deaths
    bad=cq(notes,year,deaths)%-%colnames(mort);
    if (length(bad)) stop("after mapping 'colwant', missing essential column(s): ",
                          paste(collapse=', ',bad));
    ## remove comment rows at bottom of file. all have year=NA
    i.last1=which(is.na(mort$year))[1];
    ## for sanity, make sure all years after i.last1 are NA
    bad=!is.na(mort$year[i.last1:nrow(mort)]);
    if (any(bad)) stop(file," has non-NA years after first NA. first NA row is ",i.last1)
    ## finally trim comment rows
    i.last=i.last1-1;
    mort=mort[1:i.last,];
    ## now check content that remains and convert as needed
    bad=mort$year%-%2019;
    if (length(bad)) stop(file," has years other than 2019: ",paste(collapse=', ',bad));
    bad=mort$notes%-%c('Total','');
    if (length(bad)) stop(file," has unexpected notes(s): ",paste(collapse=',',bad))
    mort$deaths[mort$deaths=='Suppressed']=NA;
    mort$deaths=as.integer(mort$deaths);
    invisible(mort);
  }
## convert mort ages and add row for 'all' if possible
import_mort_age=function(mort,file,total=NULL,place=NULL) {
  ## if no 'total' provided, use total line at end if exists
  if (is.null(total)&&('notes'%in%colnames(mort))&&('Total'==tail(mort$notes,n=1))) {
    total=tail(mort,n=1);
    mort=head(mort,n=-1)
  }
  ages.mort=cvt_mort_ages(mort$age);
  ages.want=ages_mort()%-%'all';
  if (is.null(place)) place=mort$place[1];
  bad=ages.want%-%ages.mort;
  if (length(bad))
    stop(file," is missing ages(s) for ",nv(place),": ",paste(collapse=',',bad));
  bad=ages.mort%-%ages.want;
  if (length(bad))
    stop(file," has unexpected ages(s) for ",nv(place),": ",paste(collapse=',',bad));
  ## make sure age in expected order
  bad=ages.want!=ages.mort;
  if (any(bad)) stop(file," has ages out of order for ",nv(place));
  mort=mort[,cq(deaths),drop=FALSE];
  if (!is.null(total)) {
    ## add total row as 'all'
    mort=rbind(total[cq(deaths)],mort);
    rownames(mort)=c('all',ages.want);
  }
  colnames(mort)=place;
  mort;
}
end_mort=function(mort,places) {
  deaths=mort[,grep('deaths$',colnames(mort)),drop=FALSE]
  pop=mort[,grep('pop$',colnames(mort)),drop=FALSE]
  colnames(deaths)=colnames(pop)=places;
  invisible(list(mort=deaths,pop=pop));
}
cvt_mort_ages=function(age) {
  age[age==1]=0;                    # convert 1 to 0 for consistency with my 'age' nomenclature
  age=sub('-','_',age,fixed=TRUE);  # convert '-' to '_', eg '1-4' to '1_4'
  age=sub('+','',age,fixed=TRUE);   # convert 100+ to 100
  age[is.na(age)]=NA;               # convert "" to NA
                                    # leave NS as is
  age;
}
## ---- mort 'constants' ----
ages_mort=function()
  c('all','0','1_4',
    sapply(seq(5,95,by=5),function(start) paste0(start,'_',start+4)),
    '100','NS');
places_mort=c('USA',places_all());

## ---- compare mortpop vs main pop ----
## derived from meta/obj_pop
chk_mortpop=function() {
  pop=load_pop();
  mpop=load_mortpop();
  places.pop=colnames(pop)%-% cq(Douglas_NE,Washtenaw_MI); # dunno why pop has these extra places
  places.mpop=colnames(mpop);
  bad=places.pop%--%places.mpop;
  if (length(bad)) stop("pop and mortpop differ even after removing known redundant places. bad=",
                        paste(collapse=', ',bad));
  pop=pop[,places.mpop];
  ## check 'NS' row - should be all NA
  chk.ns=mpop['NS',];
  bad=!is.na(chk.ns)
  if (any(bad)) stop("mortpop 'NS' row contains non-NA value(s) for place(s): ",
                     paste(collapse=', ',places.mpop[bad]));
  mpop=mpop[rownames(mpop)!='NS',];
  ## replace NA by 0 everywhere to simplify calculations
  mpop[is.na(mpop)]=0
  ## check 'all' row
  pall=pop['all',];
  mpall=mpop['all',];
  chk.all=data.frame(t(mpall),t(pall));
  colnames(chk.all)=cq(mpop,pop);  
  chk.all$diff=with(chk.all,pop-mpop);
  chk.all$ratio=with(chk.all,pop/mpop);
  chk.all=chk.all[order(abs(chk.all$ratio)),];
  BREAKPOINT('chk_mortpop: check all');
  ## now work on ages
  places.ages=c('USA',places_wa());     # places with age groups
  ages.pop=rownames(pop)%-%'all';
  ages.mpop=rownames(mpop)%-%'all';
  ages.doh=c('0_19',paste(sep='_',seq(20,65,by=15),seq(34,79,by=15)),'80_');
  ages.cdc=c(paste(sep='_',seq(0,70,by=10),seq(9,79,by=10)),'80_');

  starts.pop=age_starts(ages.pop);
  starts.mpop=age_starts(ages.mpop);
  starts.doh=age_starts(ages.doh);
  starts.cdc=age_starts(ages.cdc);

  ## check CDC ages
  cats=cut(starts.pop,c(starts.cdc,200),right=F,labels=ages.cdc);
  groups=split(ages.pop,cats);
  pop.cdc=do.call(rbind,lapply(groups,function(i) colSums(pop[i,places.ages])));
  cats=cut(starts.mpop,c(starts.cdc,200),right=F,labels=ages.cdc);
  groups=split(ages.mpop,cats);
  mpop.cdc=do.call(rbind,lapply(groups,function(i) colSums(mpop[i,places.ages])));
  diff.cdc=pop.cdc-mpop.cdc;
  ratio.cdc=pop.cdc/mpop.cdc;
  ## expect 80_ to be way off because mpop is missing data for 85_89 on up
  diff.80=diff.cdc['80_',,drop=FALSE];
  ratio.80=ratio.cdc['80_',,drop=FALSE];
  diff.non80=diff.cdc[rownames(diff.cdc)!='80_',];
  ratio.non80=ratio.cdc[rownames(ratio.cdc)!='80_',];
  BREAKPOINT('chk_mortpop: check CDC');
  
  ## check DOH ages
  cats=cut(starts.pop,c(starts.doh,200),right=F,labels=ages.doh);
  groups=split(ages.pop,cats);
  pop.doh=do.call(rbind,lapply(groups,function(i) colSums(pop[i,places.ages])));
  cats=cut(starts.mpop,c(starts.doh,200),right=F,labels=ages.doh);
  groups=split(ages.mpop,cats);
  mpop.doh=do.call(rbind,lapply(groups,function(i) colSums(mpop[i,places.ages])));
  diff.doh=pop.doh-mpop.doh;
  ratio.doh=pop.doh/mpop.doh;
  ## expect 80_ to be way off because mpop is missing data for 85_89 on up
  diff.80=diff.doh['80_',,drop=FALSE];
  ratio.80=ratio.doh['80_',,drop=FALSE];
  diff.non80=diff.doh[rownames(diff.doh)!='80_',];
  ratio.non80=ratio.doh[rownames(ratio.doh)!='80_',];
  BREAKPOINT('chk_mortpop: check DOH');
}
## ---- quick look at mortality rates across ages and places ----
## derived from meta/obj_pop
chk_mort=function() {
  pop=load_pop();
  mort=load_mort();
  places.pop=colnames(pop)%-% cq(Douglas_NE,Washtenaw_MI); # dunno why pop has these extra places
  places.mort=colnames(mort);
  bad=places.pop%--%places.mort;
  if (length(bad)) stop("pop and mort differ even after removing known redundant places. bad=",
                        paste(collapse=', ',bad));
  pop=pop[,places.mort];
  ## check 'NS' row - should be all NA or small
  chk.ns=mort['NS',];
  BREAKPOINT('chk_mortpop: check NS - should be all NA or small');
  mort=mort[rownames(mort)!='NS',];
  ## replace NA by 0 everywhere to simplify calculations
  mort[is.na(mort)]=0
  ## check 'all' row
  pall=pop['all',];
  mall=mort['all',];
  chk.all=as.data.frame(t(rbind(mall,pall,round(1e6*mall/pall))))
  colnames(chk.all)=cq(mort,pop,ratio);  
  BREAKPOINT('chk_mort: check all');
  ## now work on ages
  places.ages=c('USA',places_wa());     # places with age groups
  ages.pop=rownames(pop)%-%'all';
  ages.mort=rownames(mort)%-%'all';
  ages.doh=c('0_19',paste(sep='_',seq(20,65,by=15),seq(34,79,by=15)),'80_');
  ages.cdc=c(paste(sep='_',seq(0,70,by=10),seq(9,79,by=10)),'80_');

  starts.pop=age_starts(ages.pop);
  starts.mort=age_starts(ages.mort);
  starts.doh=age_starts(ages.doh);
  starts.cdc=age_starts(ages.cdc);

  ## check CDC ages
  cats=cut(starts.pop,c(starts.cdc,200),right=F,labels=ages.cdc);
  groups=split(ages.pop,cats);
  pop.cdc=do.call(rbind,lapply(groups,function(i) colSums(pop[i,places.ages])));
  cats=cut(starts.mort,c(starts.cdc,200),right=F,labels=ages.cdc);
  groups=split(ages.mort,cats);
  mort.cdc=do.call(rbind,lapply(groups,function(i) colSums(mort[i,places.ages])));
  chk.cdc=round(1e6*mort.cdc/pop.cdc);
  rat.cdc=chk.cdc/chk.cdc[,c('USA')];
  BREAKPOINT('chk_mort: check CDC');

  ## check DOH ages
  cats=cut(starts.pop,c(starts.doh,200),right=F,labels=ages.doh);
  groups=split(ages.pop,cats);
  pop.doh=do.call(rbind,lapply(groups,function(i) colSums(pop[i,places.ages])));
  cats=cut(starts.mort,c(starts.doh,200),right=F,labels=ages.doh);
  groups=split(ages.mort,cats);
  mort.doh=do.call(rbind,lapply(groups,function(i) colSums(mort[i,places.ages])));
  chk.doh=round(1e6*mort.doh/pop.doh);
  rat.doh=chk.doh/chk.doh[,c('USA')];
  BREAKPOINT('chk_mort: check DOH');
}
