#################################################################################
##
## Author:  Nat Goodman
## Created: created 20-05-02
##
## Copyright (C) 2020 Nat Goodman.
## 
## Import data from input directories. Produce ready-to-load files in data directories
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
library(readxl);
## wrapper for import functions
## if version specified, import that version only. if NULL do all
## file, if set, includes path and supercedes version. can be directory or single file
import=function(datasrc,version='latest',file=NULL,do.usa=TRUE) {
  datasrc=match.arg(datasrc,param(datasrc));
  import.fun=get0(paste0('import_',datasrc),mode='function');
  if (!is.null(file)) {
    if (!file.exists(file)) stop(paste('File',file,'does not exist'))
    files=if(file.info(file)$isdir) list.files(file,full.names=TRUE) else file;
  } else {
    ## code adapted from load_data. TODO: refactor!
    if (!is.null(version)&&version=='latest')
      version=sort(list_versions(datasrc,dir=indir),decreasing=TRUE)[1];
    files=list.files(indir(datasrc),pattern=version,full.names=TRUE);
    if (length(files)==0)
      stop(paste('No files found for',nv(datasrc,version,file,SEP=', ')));
    files=sort(files);
  }
  sapply(files,function(file) {
    args=list(file=file);
    if (datasrc%in%cq(jhu,nyt)) args=c(args,do.usa=do.usa);
    do.call(import.fun,args);
  });
  files;
}
import_all=function(datasrc=param(datasrc),version='latest',do.usa=TRUE) {
  if (is.null(version)||version=='latest') version=latest_version(datasrc[1],dir=indir);
  ## trk ended 21-03-07. see covidtracking.com
  if (version>'21-03-07') datasrc=datasrc%-%'trk';
  ## for now, have to do CDC by itself - big and slow!
  if (length(datasrc)>1) datasrc=datasrc%-%'cdc'; 
  sapply(datasrc,function(datasrc) {
    if (param(verbose)) print(paste('+++ importing',datasrc));
    import(datasrc,version,do.usa=do.usa);
  });
}

## import_doh in import_doh.R
## import_cdc in import_doh.R

## ---- Import JHU input file ----
import_jhu=function(file,do.usa=TRUE) {
  if (param(verbose)) print(paste('>>> importing',file));
  ## filenames are, eg, cases.20-05-03.csv. split into what,version.
  base=basename(file);
  parts=strsplit(base,'\\.')[[1]];
  what=parts[1];
  version=parts[2];
  data=read.csv(file,stringsAsFactors=FALSE);
  if (do.usa) usa=usa_jhu(data);
  data_wa=subset(data,subset=(Province_State=='Washington'));
  data_other=data_jhu_other(data);
  colwant=grep('Admin2|^X\\d',colnames(data),value=TRUE);
  data=rbind(data_wa[,colwant],data_other[,colwant]);
  ## first column is place. rest are dates
  counties=data[,1];
  dates=as.Date(tail(colnames(data),n=-1),format='X%m.%d.%y')
  ## invert data columns (leave out place, so everything's numeric)
  data=as.data.frame(t(data[,-1]));
  colnames(data)=counties;
  rownames(data)=NULL;
  data$state=rowSums(data[,places_wa()%-%'state']);
  data=cbind(date=dates,data);
  ## for sanity, make sure dates are a day apart
  dates=data$date;
  date.diff=diff(dates);
  bad=which(date.diff!=1);
  if (length(bad)>0) stop(paste("These dates in jhu",what,"version",version,
                                "are not sequential:",paste(collapse=', ',dates[bad])));
  ## for sanity, make sure we have all places
  places=colnames(data)[-1];
  bad=places_all() %-% places;
  if (length(bad)>0)
    stop(paste('jhu version',version,'missing places:',paste(collapse=', ',bad)));
  ## quick hack to tack on Ann Arbor and Omaha
  ## data=cbind(data,import_jhu_other(file));
  if (do.usa) data=cbind(data,USA=usa);
  save_data(what,'jhu',version,data=data);
}
## import non-Washington places
data_jhu_other=function(data,places.nonwa=param(places.nonwa)) {
  places.nonwa$state=state_id2name(places.nonwa$state)
  data_other=merge(data,places.nonwa,by.x=cq(Province_State,Admin2),by.y=cq(state,county));
  data_other$Admin2=data_other$place;
  data_other;
}
## compute counts for entire USA
usa_jhu=function(data) {
  ## columns we want are dates. don't care about place names here
  colwant=grep('^X\\d',colnames(data),value=TRUE);
  data=data[,colwant];
  usa=colSums(data);
}

## ---- Import NY Times input file ----
import_nyt=function(file,do.usa=TRUE) {
  if (param(verbose)) print(paste('>>> importing',file));
  version=baseonly(file,keep.dir=FALSE);
  data=read.csv(file,stringsAsFactors=FALSE);
  if (do.usa) usa=usa_nyt(data);
  ## format is dead simple: date,county,state,fips,cases,deaths
  data_wa=subset(data,subset=(state=='Washington'));
  data_other=data_nyt_other(data);
  data=rbind(data_wa,data_other);
  ## want everything except 'fips'
  colwant=cq(date,county,cases,deaths);
  ## for sanity, make sure columns are what we expect
  if (!is_subset(colwant,colnames(data)))
    stop(paste("nyt version",version,"format changed. Missing these columns:",
               paste(collapse=', ',colwant%-%colnames(data))));
  data=data[,colwant];
  data$date=as.Date(data$date,format='%Y-%m-%d');
  ## dates data frame used in merge below
  dates=data.frame(date=sort(unique(data$date)),stringsAsFactors=F);
  sapply(cq(cases,deaths),function(what) {
    data=data[,c('date','county',what)];
    bycounty=split(data,data$county);
    bycounty=lapply(bycounty,function(data) {
      data=merge(dates,data[c('date',what)],all=T);
      data[,what]=ifelse(is.na(data[,what]),0,data[,what]);
      data});
    data=as.data.frame(do.call(cbind,lapply(bycounty,function(data) data[,what])));
    ## line below not necessary. lapply above sets names
    ## colnames(data)=names(bycounty);
    data$state=rowSums(data[,places_wa()%-%'state']);
    ## use cbind, not data.frame - latter replaces spaces with dots in names like 'Walla Walla'
    data=cbind(date=as.Date(dates$date),data);
    ## expect Garfield to be missing in versions <= 20-07-05
    places=colnames(data)[-1];
    if (version<='20-07-05') 
      if ('Garfield' %in% places)
        stop(paste('nyt version',version,'is not missing Garfield as expected'))
      else {
        data$Garfield=0;
        places=c(places,'Garfield');
      }
    ## for sanity, make sure we have all places
    bad=places_all() %-% places;
    if (length(bad)>0)
      stop(paste('nyt version',version,'missing places:',paste(collapse=', ',bad)));
    ## for sanity, make sure dates are a day apart
    dates=data$date;
    date.diff=diff(dates);
    bad=which(date.diff!=1);
    if (length(bad)>0) stop(paste("These dates in nyt",what,"version",version,
                                  "are not sequential:",paste(collapse=', ',dates[bad])));
    if (do.usa) {
      ## for sanity, make sure data dates and usa dates equal
      bad=which(data$date!=usa$date);
      if (length(bad)>0) stop(paste(length(bad),"data and usa dates don't match"))
      data=cbind(data,USA=usa[[what]]);
    }
    save_data(what,'nyt',version,data=data);
  });
}
## import non-Washington places
data_nyt_other=function(data,places.nonwa=param(places.nonwa)) {
  places.nonwa$state=state_id2name(places.nonwa$state)
  data_other=merge(data,places.nonwa);
  data_other$county=data_other$place;
  data_other[,colnames(data)];
}
## compute counts for entire USA
usa_nyt=function(data) {
  ## group by date and sum counts
  bydate=split(data,data$date);
  cases=sapply(bydate,function(data) sum(data$cases,na.rm=TRUE));
  deaths=sapply(bydate,function(data) sum(data$deaths,na.rm=TRUE));
  usa=data.frame(date=as_date(names(bydate)),cases,deaths,row.names=NULL);
}

## ---- Import C19Pro (aka yyg) input file ----
## no counties
## actual and predicted deaths. predicted infections NOT cases
## includes predicted values into future. cutoff at maxdate (default latest version)
import_yyg=function(file,maxdate=param(yyg.maxdate)) {
  if (maxdate=='latest') {
    files=list.files(indir('yyg'),full.names=FALSE);
    maxdate=max(sapply(files,function(file) baseonly(file,keep.dir=FALSE)));
    param(yyg.maxdate=maxdate);
  }
  maxdate=as_date(maxdate);
  if (param(verbose)) print(paste('>>> importing',file));
  version=baseonly(file,keep.dir=FALSE);
  data=read.csv(file,stringsAsFactors=FALSE);
  data=subset(data,subset=(region=='WA'));
  ## as of 20-05-13, 3 different formats
  fmts=list(fmt1=cq(date,actual_deaths,predicted_deaths),
            fmt2=cq(date,actual_deaths,predicted_deaths_mean),
            fmt3=cq(date,actual_deaths,predicted_deaths_mean,predicted_new_infected_mean));
  fmt=if(version=="20-04-01") 1 else if(version<'20-04-08') 2 else 3;
  colwant=fmts[[paste0('fmt',fmt)]];
  ## for sanity, make sure columns match expected format
  if (!is_subset(colwant,colnames(data)))
    stop(paste("yyg version",version,"should be format",fmt,"but missing these columns:",
               paste(collapse=', ',colwant%-%colnames(data))));
  data=data[,colwant];
  colnames(data)=c(cq(date,actual_deaths,predicted_deaths),if(fmt==3) 'cases' else NULL);
  data$date=as_date(data$date);
  data=subset(data,date<=maxdate);
  sapply(cq(cases,deaths),function(what) {
    if (what=='cases') {
      ## cases only exist for fmt 3
      if (fmt<3) return();
      data=data[,cq(date,cases)];
      colnames(data)=cq(date,state);
    } else {
      data=data[,cq(date,actual_deaths,predicted_deaths)]; # omit cases column if present
      data$state=ifelse(is.na(data$actual_deaths),data$predicted_deaths,data$actual_deaths);
      data=data[,cq(date,state)];
    }
    data$state=ifelse(is.na(data$state),0,data$state);
    ## for sanity, make sure dates are a day apart
    date.diff=diff(dates);
    bad=which(date.diff!=1);
    if (length(bad)>0) stop(paste("These dates in yyg",what,"version",version,
                                  "are not sequential:",paste(collapse=', ',dates[bad])));
    save_data(what,'yyg',version,data=data);
  });
}
## ---- Import Covid Tracking (aka trk) input file ----
## no counties
import_trk=function(file) {
  if (param(verbose)) print(paste('>>> importing',file));
  version=baseonly(file,keep.dir=FALSE);
  data=read.csv(file,stringsAsFactors=FALSE);
  data=subset(data,subset=(state=='WA'));
  colwant=cq(date,positiveIncrease,hospitalizedIncrease,deathIncrease);
  ## for sanity, make sure columns are what we expect
  if (!is_subset(colwant,colnames(data)))
    stop(paste("trk version",version,"format changed. Missing these columns:",
               paste(collapse=', ',colwant%-%colnames(data))));
  ## trk dates all numeric - R reads as integer. file in descending order
  data$date=as_date(as.character(data$date));
  data=data[order(data$date),colwant];
  colnames(data)=cq(date,cases,admits,deaths);
  sapply(cq(cases,admits,deaths),function(what) {
    data=data[,c('date',what)];
    colnames(data)=cq(date,state);
    data$state=ifelse(is.na(data$state),0,data$state);
    ## for sanity, make sure dates are a day apart
    dates=data$date;
    date.diff=diff(dates);
    bad=which(date.diff!=1);
    if (length(bad)>0) stop(paste("These dates in trk",what,"version",version,
                                  "are not sequential:",paste(collapse=', ',dates[bad])));
    save_data(what,'trk',version,data=data);
  });
}
## ---- Import IMHE input file ----
## no counties
## actual and predicted deaths. confirmed infections. estimated infections NOT same as cases
## includes predicted values into future. cutoff at maxdate (default latest version)
import_ihme=function(file,maxdate=param(ihme.maxdate)) {
  if (maxdate=='latest') {
    files=list.files(indir('ihme'),full.names=FALSE);
    maxdate=max(sapply(files,function(file) baseonly(file,keep.dir=FALSE)));
    param(ihme.maxdate=maxdate);
  }
  maxdate=as_date(maxdate);
  if (param(verbose)) print(paste('>>> importing',file));
  version=baseonly(file,keep.dir=FALSE);
  data=read.csv(file,stringsAsFactors=FALSE);
  data=subset(data,subset=(location_name=='Washington'));
  colwant=c('date','deaths_mean',if(version>='20-05-01') 'confirmed_infections');
  ## for sanity, make sure columns are what we expect
  if (!is_subset(colwant,colnames(data)))
    stop(paste("ihme version",version,"format changed. Missing these columns:",
               paste(collapse=', ',colwant%-%colnames(data))));
  data$date=as_date(data$date);
  data=subset(data,date<=maxdate);
  data=data[,colwant];
  colnames(data)=c('date','deaths',if(version>='20-05-01') 'cases');
  what=c('deaths',if(version>='20-05-01') 'cases');
  sapply(what,function(what) {
    if (what=='cases') data=data[,cq(date,cases)] else data=data[,cq(date,deaths)];
    colnames(data)=cq(date,state);
    data$state=ifelse(is.na(data$state),0,data$state);
    ## ## delete early and late 0 rows
    ## minrow=min(which(data$state!=0));
    ## if (minrow>1) data=tail(data,-(minrow-1));
    ## maxrow=max(which(data$state!=0));
    ## if (maxrow>1) data=head(data,maxrow);
    ## for sanity, make sure dates are a day apart
    date.diff=diff(dates);
    bad=which(date.diff!=1);
    if (length(bad)>0) stop(paste("These dates in ihme",what,"version",version,
                                  "are not sequential:",paste(collapse=', ',dates[bad])));
    save_data(what,'ihme',version,data=data);
  });
}

