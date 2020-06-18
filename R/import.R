#################################################################################
##
## Author:  Nat Goodman
## Created: created 20-05-02
##
## Copyright (C) 2020 Nat Goodman.
## 
## Import data from input directories
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
library(readxl);
## wrapper for import functions
## if version specified, import that version only, else do all
## file, if set, includes path and supercedes version. can be directory or single file
import=function(datasrc,version='latest',file=NULL) {
  datasrc=match.arg(datasrc,param(datasrc));
  import.fun=get0(paste0('import_',datasrc),mode='function');
  if (!is.null(file)) {
    if (!file.exists(file)) stop(paste('File',file,'does not exist'))
    files=if(file.info(file)$isdir) list.files(file,full.names=TRUE) else file;
  } else {
    ## code adapted from load_data. TODO: refactor!
    latest=!is.null(version)&&version=='latest';
    if (latest) version=NULL;
    files=list.files(indir(datasrc),pattern=version,full.names=TRUE);
    if (length(files)==0)
      stop(paste('No files found for',nvq(datasrc,version,file,SEP=', ')));
    files=sort(files,decreasing=TRUE);
    if (latest) files=files[1];
  }
  sapply(files,function(file) do.call(import.fun,as.list(c(file=file))));
  files;
}
import_all=function(datasrc=param(datasrc),version='latest')
  sapply(datasrc,function(datasrc) {
    if (param(verbose)) print(paste('+++ importing',datasrc));
    import(datasrc,version);
  });

## ---- Import  WA DOH input file ----
import_doh=function(file,notKing=param(doh.notKing)) {
  if (param(verbose)) print(paste('>>> importing',file));
  version=baseonly(file,keep.dir=FALSE);
  sapply(cq(Cases,Deaths),function(What) {
    what=tolower(What);                 # to avoid confusion later
    data=suppressMessages(read_excel(file,sheet=What));
    data=as.data.frame(data,stringsAsFactors=F);
    data$County=sub(' County','',data$County);
    data=data[,1:3]
    colnames(data)=c('county','date',what);
    data$date=as_date(data$date);
    dates=data.frame(date=sort(unique(data$date)),stringsAsFactors=F);
    bycounty=split(data,data$county)
    bycounty=lapply(bycounty,function(data) {
      data=merge(dates,data[,2:3],all=T);
      data[,what]=ifelse(is.na(data[,what]),0,data[,what]);
      data})
    data=as.data.frame(do.call(cbind,lapply(bycounty,function(data) data[,what])));
    colnames(data)=names(bycounty);
    total=rowSums(data);
    if (notKing) data$notKing=total-data$King;
    data$state=total;
    data=data.frame(date=as_date(dates$date),data);
    ## NG 20-05-09: edit problem rows in cases data.
    ## see doh_check() at bottom of file
    ## fix non-Sundays
    bad=(weekdays(data$date)!='Sunday');
    if (what=='cases') {
      ## if (version %in% c('20-04-25','20-04-26','20-05-10','20-05-17','20-05-31')) {
      if ((version %in% c('20-04-26','20-05-10','20-05-17'))||(version>='20-05-31')) {
        ## expect bad 1st date 2020-01-16
        if ((sum(bad)!=1)&&data$date[1]!='2020-01-16')
          stop(paste("doh",what,"version",version,"does not have expected bad date: 2020-01-16"));
        ## change date to previous sunday: 2020-01-12
        data[1,]$date=as_date('2020-01-12');
        ## ## combine rows. replaces with single row dates '202-01-19'
        ## data.1=data.frame(date=as.Date('2020-01-19'),data[1,-1]+data[2,-1]);
        ## data=rbind(data.1,data[-(1:2),]);
      } else if (version=='20-05-03') {
        ## expect bad 1st & 2nd dates 2020-01-16, 2020-01-20
        if ((sum(bad)!=2)&&!all(data$date[1:2]==c('2020-01-16','2020-01-20')))
          stop(paste("doh",what,"version",version,
                     "does not have expected bad dates: 2020-01-16, 2020-01-20"));
        ## combine rows. replaces with single row dates '202-01-19'
        data.1=data.frame(date=as.Date('2020-01-19'),data[1,-1]+data[2,-1]);
        data=rbind(data.1,data[-(1:2),]);
      }
    }
    ## fix missing weeks
    bad=diff(data$date)!=7;
    if (what=='cases') {
      if (version %in% c('20-05-10','20-05-17','20-05-31')) {
        ## expect missing weeks after 2020-01-19
        if ((sum(bad)!=1)&&!bad[1]&&data$date[1]!='2020-01-19')
          stop(paste("doh",what,"version",version,
                     "does not have expected missing week after: 2020-01-19"));
        data=doh_insert(data,1,2);
      } else if (version %in% c('20-05-24','20-06-07','20-06-14')) {
        ## expect missing weeks after 2020-01-12
        if ((sum(bad)!=1)&&!bad[1]&&data$date[1]!='2020-01-12')
          stop(paste("doh",what,"version",version,
                     "does not have expected missing weeks after: 2020-01-12"));
        data=doh_insert(data,1,2);
      }}
    if (what=='deaths') {
      if (version=='20-04-26') {
        ## expect missing weeks after 2020-01-19
        if ((sum(bad)!=1)&&!bad[1]&&data$date[1]!='2020-01-19')
          stop(paste("doh",what,"version",version,
                     "does not have expected missing week after: 2020-01-19"));
        data=doh_insert(data,1,2);
      }
      if (version=='20-05-31') {
        ## expect missing weeks after 2020-01-26
        if ((sum(bad)!=1)&&!bad[1]&&data$date[1]!='2020-01-26')
          stop(paste("doh",what,"version",version,
                     "does not have expected missing week after: 2020-01-26"));
        data=doh_insert(data,1,2);
      }
    }
    ## for sanity, make sure we corrected all errors
    ## code adapted from doh_check. TODO: refactor!
    ## for sanity, make sure dates are a week apart
    dates=data$date;
    bad=(weekdays(dates)!='Sunday')
    if (any(bad))
      stop(paste("Bad news: doh",what,"version",version,'has non-Sundays even after correction:',
                  paste(collapse=', ',dates[bad])));
    sundays=dates[!bad];
    bad=diff(sundays)!=7;
    if (any(bad))
      stop(paste("Bad news: doh",what,"version",version,'has missing weeks even after correction:',
                 paste(collapse=', ',head(sundays,n=-1)[bad])));
    save_data(what,'doh',version,data=data);
  });
}
## ---- Import JHU input file ----
import_jhu=function(file,notKing=param(jhu.notKing)) {
  if (param(verbose)) print(paste('>>> importing',file));
  ## filenames are, eg, cases.20-05-03.csv. split into what,version.
  base=basename(file);
  parts=strsplit(base,'\\.')[[1]];
  what=parts[1];
  version=parts[2];
  data=read.csv(file,stringsAsFactors=FALSE);
  data=subset(data,subset=(Province_State=='Washington'));
  colwant=grep('Admin2|^X\\d',colnames(data));
  data=data[,colwant];
  ## first column is counties. rest are dates
  counties=data[,1];
  dates=as.Date(tail(colnames(data),n=-1),format='X%m.%d.%y')
  ## invert data columns (leave out counties, so everything's numeric)
  data=as.data.frame(t(data[,-1]));
  colnames(data)=counties;
  rownames(data)=NULL;
  total=rowSums(data)
  if (notKing) data$notKing=total-data$King;
  data$state=total;
  data=cbind(date=dates,data);
  ## for sanity, make sure dates are a day apart
  dates=data$date;
  date.diff=diff(dates);
  bad=which(date.diff!=1);
  if (length(bad)>0) stop(paste("These dates in jhu",what,"version",version,
                                "are not sequential:",paste(collapse=', ',dates[bad])));
  save_data(what,'jhu',version,data=data);
}
## ---- Import NY Times input file ----
import_nyt=function(file,notKing=param(nyt.notKing)) {
  if (param(verbose)) print(paste('>>> importing',file));
  version=baseonly(file,keep.dir=FALSE);
  data=read.csv(file,stringsAsFactors=FALSE);
  data=subset(data,subset=(state=='Washington'));
  ## formt is dead simple: date,county,state,fips,cases,deaths
  ## want everything except 'fips'
  colwant=cq(date,county,cases,deaths);
  ## for sanity, make sure columns are what we expect
  if (!is_subset(colwant,colnames(data)))
    stop(paste("nyt version",version,"format changed. Missing these columns:",
               paste(collapse=', ',setdiff(colwant,colnames(data)))));
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
    colnames(data)=names(bycounty);
    total=rowSums(data);
    if (notKing) data$notKing=total-data$King;
    data$state=total;
    data=data.frame(date=as.Date(dates$date),data);
    ## for sanity, make sure dates are a day apart
    dates=data$date;
    date.diff=diff(dates);
    bad=which(date.diff!=1);
    if (length(bad)>0) stop(paste("These dates in nyt",what,"version",version,
                                  "are not sequential:",paste(collapse=', ',dates[bad])));
    save_data(what,'nyt',version,data=data);
  });
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
               paste(collapse=', ',setdiff(colwant,colnames(data)))));
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
    minrow=min(which(data$state!=0));
    ## delete early 0 rows
    if (minrow>1) data=tail(data,-(minrow-1));
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
  colwant=cq(date,deathIncrease,positiveIncrease);
  ## for sanity, make sure columns are what we expect
  if (!is_subset(colwant,colnames(data)))
    stop(paste("trk version",version,"format changed. Missing these columns:",
               paste(collapse=', ',setdiff(colwant,colnames(data)))));
  ## trk dates all numeric - R reads as integer. file in descending order
  data$date=as_date(as.character(data$date));
  data=data[order(data$date),colwant];
  colnames(data)=cq(date,deaths,cases);
  sapply(cq(cases,deaths),function(what) {
    if (what=='cases') data=data[,cq(date,cases)] else data=data[,cq(date,deaths)];
    colnames(data)=cq(date,state);
    data$state=ifelse(is.na(data$state),0,data$state);
    minrow=min(which(data$state!=0));
    ## delete early 0 rows
    if (minrow>1) data=tail(data,-(minrow-1));
     ## for sanity, make sure dates are a day apart
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
               paste(collapse=', ',setdiff(colwant,colnames(data)))));
  data$date=as_date(data$date);
  data=subset(data,date<=maxdate);
  data=data[,colwant];
  colnames(data)=c('date','deaths',if(version>='20-05-01') 'cases');
  what=c('deaths',if(version>='20-05-01') 'cases');
  sapply(what,function(what) {
    if (what=='cases') data=data[,cq(date,cases)] else data=data[,cq(date,deaths)];
    colnames(data)=cq(date,state);
    data$state=ifelse(is.na(data$state),0,data$state);
    ## delete early and late 0 rows
    minrow=min(which(data$state!=0));
    if (minrow>1) data=tail(data,-(minrow-1));
    maxrow=max(which(data$state!=0));
    if (maxrow>1) data=head(data,maxrow);
    ## for sanity, make sure dates are a day apart
    date.diff=diff(dates);
    bad=which(date.diff!=1);
    if (length(bad)>0) stop(paste("These dates in ihme",what,"version",version,
                                  "are not sequential:",paste(collapse=', ',dates[bad])));
    save_data(what,'ihme',version,data=data);
  });
}
## ---- Check format of WA DOH input file ----
doh_check=function(version=NULL) {
  files=list.files(indir('doh'),pattern=version,full.names=TRUE);
  sapply(files,function(file) {
    ## print(paste('>>> checking',file))
    sapply(cq(Cases,Deaths),function(What) {
      what=tolower(What);                 # to avoid confusion later
      data=suppressMessages(read_excel(file,sheet=What));
      data=as.data.frame(data,stringsAsFactors=F);
      data$County=sub(' County','',data$County);
      data=data[,1:3]
      colnames(data)=c('county','date',what);
      data$date=as_date(data$date);
      dates=sort(unique(data$date));
      ## print(paste(what,paste(collapse=', ',dates)));
      bad=(weekdays(dates)!='Sunday')
      if (any(bad)) print(paste(file,What,'non-Sundays:',paste(collapse=', ',dates[bad])))
      sundays=dates[!bad];
      bad=diff(sundays)!=7;
      if (any(bad)) print(paste(file,What,'missing weeks after:',
                                paste(collapse=', ',head(sundays,n=-1)[bad])));
    })})
  invisible()
}
## add missing weeks in DOH input file. data all 0's
doh_insert=function(data,before,after) {
  missing=seq(data$date[before]+7,data$date[after]-7,by=7);
  nplace=ncol(data)-1;
  new=do.call(rbind,lapply(missing,function(date) data.frame(date,repc(0,nplace))))
  colnames(new)=colnames(data);
  data=rbind(data[1:before,],new,data[after:nrow(data),]);
}
