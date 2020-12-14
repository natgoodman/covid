#################################################################################
##
## Author:  Nat Goodman
## Created: created 20-06-18
##          with code from age.R created 20-05-29 and import.R created 20-05-02
##          from import.R created 20-05-02
##          with code adapted from plot.R and transform.R created 20-05-06
##
## Copyright (C) 2020 Nat Goodman.
## 
## Import DOH age data from input directories
## Quick hack to illustrate fallacy in Case Incidence Age Shift paper by Malmgren et al 
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
library(readxl);
## ---- Import  WA DOH input file ----
## called by top-level import function
## handles age groups
import_doh=function(file) {
  if (param(verbose)) print(paste('>>> importing',file));
  version=baseonly(file,keep.dir=FALSE);
  Sheets=c(cq(Cases,Deaths),if(version>='20-05-24') 'Hospitalizations');
  sapply(Sheets,function(Sheet) {
    what=if(Sheet!='Hospitalizations') tolower(Sheet) else 'admits'
    data=suppressMessages(read_excel(file,sheet=Sheet));
    data=as.data.frame(data,stringsAsFactors=F);
    data$County=sub(' County','',data$County);
    data=data[,1:8];
    names.age=colnames(data)[4:8];
    names.age=sub('\\+','_',sub('-','_',sub('Age ','',names.age)));
    colnames(data)=c(cq(county,date,all),names.age);
    ## Hospitalizations has some rows with Unknown date. delete 'em
    if (what=='admits') data=subset(data,subset=(date!='Unknown'));
    data$date=as_date(data$date);
    dates=data.frame(date=sort(unique(data$date)),stringsAsFactors=F);
    bycounty=split(data,data$county)
    bycounty=lapply(bycounty,function(data) {
      data=merge(dates,data[,-1],all=T);
      data=apply(data[,-1],2,function(col) ifelse(is.na(col),0,col));
      data})
    names.dat=c('all',names.age)
    byage=sapply(names.dat,simplify=FALSE,function(nm) {
      data=as.data.frame(do.call(cbind,lapply(bycounty,function(data) data[,nm])));
      total=rowSums(data);
      data$state=total;
      ## use cbind, not data.frame - latter replaces spaces with dots in names like 'Walla Walla'
      data=cbind(date=as_date(dates$date),data);
      fix_doh=switch(what,cases=fix_doh_cases,admits=fix_doh_admits,deaths=fix_doh_deaths);
      data=fix_doh(data,what,version);
      ## add in counties missing from early versions
      places=colnames(data)[-1];
      missing=places_wa() %-% places;
      if (length(missing)>0) data[,missing]=0;
      ## for sanity, make sure we corrected all errors
      check_doh(data,what,version);
      data;
    });
    ## save_data(paste(sep='_','age',what),'doh',version,data=byage);
    save_data(what,'doh',version,data=byage);
  });
}
## fix bad dates in cases
fix_doh_cases=function(data,what,version) {
  ## fix non-Sundays
  bad=(weekdays(data$date)!='Sunday');
  if ((version %in% c('20-04-26','20-05-10','20-05-17'))||(version>='20-05-31')) {
    ## expect bad 1st date 2020-01-16
    if ((sum(bad)!=1)&&data$date[1]!='2020-01-16')
      stop(paste("doh",what,"version",version,
                 "does not have expected bad date: 2020-01-16"));
    ## change date to previous sunday: 2020-01-12
    data[1,]$date=as_date('2020-01-12');
  } else if (version=='20-05-03') {
    ## expect bad 1st & 2nd dates 2020-01-16, 2020-01-20
    if ((sum(bad)!=2)&&!all(data$date[1:2]==c('2020-01-16','2020-01-20')))
      stop(paste("doh",what,"version",version,
                 "does not have expected bad dates: 2020-01-16, 2020-01-20"));
    ## combine rows. replaces with single row dates '202-01-19'
    ## use cbind, not data.frame - latter replaces spaces with dots in names like 'Walla Walla'
    data.1=cbind(date=as.Date('2020-01-19'),data[1,-1]+data[2,-1]);
    data=rbind(data.1,data[-(1:2),]);
  }
  ## fix missing weeks
  bad=diff(data$date)!=7;
  if (version %in% c('20-05-10','20-05-17','20-05-31')) {
    ## expect missing weeks after 2020-01-19
    if ((sum(bad)!=1)&&!bad[1]&&data$date[1]!='2020-01-19')
      stop(paste("doh",what,"version",version,
                 "does not have expected missing week after: 2020-01-19"));
    data=doh_insert(data,1,2);
  ## } else if (version %in% c('20-05-24','20-06-07','20-06-14','20-06-21')) {
  } else if ((version=='20-05-24')||(version>='20-06-07')) {
    ## expect missing weeks after 2020-01-12
    if ((sum(bad)!=1)&&!bad[1]&&data$date[1]!='2020-01-12')
      stop(paste("doh",what,"version",version,
                 "does not have expected missing weeks after: 2020-01-12"));
    data=doh_insert(data,1,2);
  }
  ## fix extra week (for some counties) in 20-10-11
  if (version=='20-10-11') data=fix_extra_week(data,what,version);
  ## done
  data;
}
## fix bad dates in admits
fix_doh_admits=function(data,what,version) {
  ## fix non-Sundays
  bad=(weekdays(data$date)!='Sunday');
  if (version>='20-05-31') {
    ## many non-Sundays. sigh...
    if (sum(bad)!=5)
      stop(paste("doh",what,"version",version,"does not have expected 5 non-Sundays"));
    ## adjust each non-Sunday back to previous Sunday
    data$date=data$date-(dayofweek(weekdays(data$date))-1);
  }
  ## fix missing weeks. shouldn't be any
  bad=diff(data$date)!=7;
  if (any(bad))
    stop(paste("doh",what,"version",version,"has",sum(bad),"unexpected missing weeks"));
  ## fix extra week (for some counties) in 20-10-11 and 20-11-22
  if (version %in% c('20-10-11','20-11-22')) data=fix_extra_week(data,what,version);
  ## done
  data;
}
fix_doh_deaths=function(data,what,version) {
  ## fix non-Sundays. shouldn't be any
  bad=(weekdays(data$date)!='Sunday');
  if (any(bad))
    stop(paste("doh",what,"version",version,"has",sum(bad),"unexpected non-Sundays:",
               paste(collapse=', ',data$date[bad])));
  ## fix missing weeks
  bad=diff(data$date)!=7;
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
  if (version %in% c('20-06-28','20-07-05','20-07-12','20-12-13')) {
    ## expect 2 missing weeks after 2020-01-26
    if ((sum(bad)!=1)&&!bad[1]&&!all(data$date[1:2]==c("2020-01-26","2020-02-16")))
      stop(paste("doh",what,"version",version,
                 "does not have expected 2 missing weeks after: 2020-01-26"));
    data=doh_insert(data,1,2);
  }
  data;
}
## for sanity, make sure we corrected all errors
check_doh=function(data,what,version) {
  ## make sure we have all counties
  places=colnames(data)[-1];
  bad=places_wa() %-% places;
  if (length(bad)>0)
    stop(paste('doh version',version,'missing counties:',paste(collapse=', ',bad)));
  ## check non-Sundays
  dates=data$date;
  bad=(weekdays(dates)!='Sunday');
  if (any(bad))
    stop(paste("Bad news: doh",what,"version",version,'has non-Sundays even after correction:',
               paste(collapse=', ',dates[bad])));
  ## check missing weeks
  bad=diff(data$date)!=7;
  if (any(bad))
    stop(paste("Bad news: doh",what,"version",version,'has missing weeks even after correction:',
               paste(collapse=', ',head(dates,n=-1)[bad])));
  ## check extra week
  vdate=as_date(version);
  bad=(data$date==vdate);
  if (any(bad)) stop(paste("doh",what,"version",version,'has extra weeks even after correction:',
                           paste(collapse=', ',head(dates,n=-1)[bad])));
  data;
}
## add missing weeks in DOH input file. data all 0's
doh_insert=function(data,before,after) {
  missing=seq(data$date[before]+7,data$date[after]-7,by=7);
  nplace=ncol(data)-1;
  new=do.call(rbind,lapply(missing,function(date) data.frame(date,repc(0,nplace))))
  colnames(new)=colnames(data);
  data=rbind(data[1:before,],new,data[after:nrow(data),]);
}
fix_extra_week=function(data,what,version) {
  vdate=as_date(version);
  bad=(data$date==vdate);
  if (sum(bad)!=1) stop(paste("doh",what,"version",version,
                              "does not have expected extra week:",vdate));
  data[!bad,];
}
