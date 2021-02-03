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
  vdate=as_date(version);
  Sheets=c(cq(Cases,Deaths),if(version>='20-05-24') 'Hospitalizations');
  sapply(Sheets,function(Sheet) {
    what=if(Sheet!='Hospitalizations') tolower(Sheet) else 'admits'
    data=suppressMessages(read_excel(file,sheet=Sheet));
    data=as.data.frame(data,stringsAsFactors=F);
    data$County=sub(' County','',data$County);
    if (version<'20-12-20') {
      col.all=3;                        # hardcode it to easily cover all 3 sheets
      col.noage=which(colnames(data)=='Positive UnkAge');
    } else {
      col.all=
        if(what=='cases') which(colnames(data)=='TotalCases') else which(colnames(data)==Sheet);
      col.noage=which(colnames(data)=='UnknownAge');
    }
    cols.age=grep('^Age',colnames(data));
    cols.data=c(1,2,col.all,cols.age,col.noage);
    ## for sanity, make sure 'all' matches individual groups
    bad=which(data[,col.all]!=rowSums(data[,c(cols.age,col.noage)]))
    if (length(bad)>0)
      stop(paste('doh version',version,"'all' does not equal sum of 'ages' in these rows:",
                 paste(collapse=', ',bad)));
    ## convert age colnames to my internal age labels
    names.age=colnames(data)[cols.age];
    names.age=sub('\\+','_',sub('-','_',sub('Age ','',names.age)));
    data=data[,cols.data];
    colnames(data)=c(cq(county,date,all),names.age,'noage');
    ## Hospitalizations has some rows with Unknown date. delete 'em
    if (what=='admits') data=subset(data,subset=(date!='Unknown'));
    data$date=as_date(data$date);
    ddates=data$date;                                     # dates in data
    wdates=seq(sunday_week(min(data$date)),vdate-7,by=7); # weekly dates spanning data
    adates=unique(c(ddates,wdates));                      # all dates of interest
    dates=data.frame(date=adates,stringsAsFactors=F);
    bycounty=split(data,data$county)
    bycounty=lapply(bycounty,function(data) {
      data=merge(dates,data[,-1],all=T);
      counts=apply(data[,-1],2,function(col) ifelse(is.na(col),0,col));
      data=data.frame(date=data$date,counts,check.names=FALSE);
      data=fix_nonsundays(data);
      data=fix_extraweeks(data,vdate);
      data});
    ## TODO: add 'noage' group to data. probably many ramifications... too hard to do now
    names.dat=c('all',names.age)
    byage=sapply(names.dat,simplify=FALSE,function(nm) {
      data=as.data.frame(do.call(cbind,lapply(bycounty,function(data) data[,nm])));
      total=rowSums(data);
      data$state=total;
      ## use cbind, not data.frame - latter replaces spaces with dots in names like 'Walla Walla'
      data=cbind(date=wdates,data);
      ## fix_doh=switch(what,cases=fix_doh_cases,admits=fix_doh_admits,deaths=fix_doh_deaths);
      ## data=fix_doh(data,what,version);
      ## add in counties and 'Unassigned' missing from early versions
      places=colnames(data)[-1];
      missing=c(places_wa(),'Unassigned') %-% places;
      if (length(missing)>0) data[,missing]=0;
      ## for sanity, make sure we corrected all errors
      check_doh(data,what,version);
      data;
    });
    ## save_data(paste(sep='_','age',what),'doh',version,data=byage);
    save_data(what,'doh',version,data=byage);
  });
}
## fix non-Sundays
fix_nonsundays=function(data) {
  bad=(weekdays(data$date)!='Sunday');
  if (any(bad)) {
    ## change dates to previous Sundays.
    data$date[bad]=sunday_week(data$date[bad]);
    ## since data contains all Sundays, this will cause duplicate dates. merge rows
    bydate=split(data,data$date);
    counts=do.call(rbind,lapply(bydate,function(data) counts=colSums(data[,-1])));
    data=data.frame(date=as_date(names(bydate)),counts,check.names=FALSE)
  }
  data;
}
## fix extra weeks at end
fix_extraweeks=function(data,vdate) {
  bad=(data$date>=vdate);
  data[!bad,];
}

## ## fix bad dates in cases
## fix_doh_cases=function(data,what,version) {
##   ## fix non-Sundays
##   bad=(weekdays(data$date)!='Sunday');
##   if (version>='20-12-20') {
##     ## expect 6 bad dates (sigh...): 2020-01-17,2020-02-01,2020-02-04,2020-02-11,2020-02-21
##     bad.expect=c('2020-01-17','2020-02-01','2020-02-04','2020-02-11','2020-02-21');
##     if ((sum(bad)!=6)&&data$date[bad]%==%bad.expect) {
##       missing=data$date[bad]%==%bad.expect;
##       stop(paste("doh",what,"version",version,"does not have expected bad date(s):",
##                  paste(collapse=', ',missing)));
##     }
##     ## change dates to previous Sundays. fortunately, none of these duplicate correct dates
##     data$date[bad]=sunday_week(data$date[bad])
##   }
##   else if ((version %in% c('20-04-26','20-05-10','20-05-17'))||(version>='20-05-31')) {
##     ## expect bad 1st date 2020-01-16
##     if ((sum(bad)!=1)&&data$date[1]!='2020-01-16')
##       stop(paste("doh",what,"version",version,
##                  "does not have expected bad date: 2020-01-16"));
##     ## change date to previous sunday: 2020-01-12
##     data[1,]$date=as_date('2020-01-12');
##   } else if (version=='20-05-03') {
##     ## expect bad 1st & 2nd dates 2020-01-16, 2020-01-20
##     if ((sum(bad)!=2)&&!all(data$date[1:2]==c('2020-01-16','2020-01-20')))
##       stop(paste("doh",what,"version",version,
##                  "does not have expected bad dates: 2020-01-16, 2020-01-20"));
##     ## combine rows. replaces with single row dates '202-01-19'
##     ## use cbind, not data.frame - latter replaces spaces with dots in names like 'Walla Walla'
##     data.1=cbind(date=as.Date('2020-01-19'),data[1,-1]+data[2,-1]);
##     data=rbind(data.1,data[-(1:2),]);
##   }
##   ## fix missing weeks
##   bad=diff(data$date)!=7;
##   if (version %in% c('20-05-10','20-05-17','20-05-31')) {
##     ## expect missing weeks after 2020-01-19
##     if ((sum(bad)!=1)&&!bad[1]&&data$date[1]!='2020-01-19')
##       stop(paste("doh",what,"version",version,
##                  "does not have expected missing week after: 2020-01-19"));
##     data=doh_insert(data,1,2);
##   ## } else if (version %in% c('20-05-24','20-06-07','20-06-14','20-06-21')) {
##   } else if ((version=='20-05-24')||(version>='20-06-07'&&version<'20-12-20')) {
##     ## expect missing weeks after 2020-01-12
##     if ((sum(bad)!=1)&&!bad[1]&&data$date[1]!='2020-01-12')
##       stop(paste("doh",what,"version",version,
##                  "does not have expected missing weeks after: 2020-01-12"));
##     data=doh_insert(data,1,2);
##   }
##   ## fix missing weeks at end
##   if (version>='21-01-24') {
##     dates=data$date
##     ## expect missing weeks at end
##     vdate=as_date(version);
##     d.last=tail(dates,n=1);
##     if ((vdate-d.last)==7)
##       stop(paste("doh",what,"version",version,
##                  "does not have expected missing week(s) at end (week",paste0(vdate-7,")")));
##     data=doh_append(data,vdate);
##   }
##   ## fix extra week (for some counties) in 20-10-11 and between 20-12-20, 21-01-17
##   if (version %in% c('20-10-11','20-12-20')) data=fix_extra_week(data,what,version);
##   if ((version=='20-10-11')||(btwn_cc(version,'20-12-20','21-01-17')))
##     data=fix_extra_week(data,what,version);
##   ## done
##   data;
## }
## ## fix bad dates in admits
## fix_doh_admits=function(data,what,version) {
##   ## fix non-Sundays
##   bad=(weekdays(data$date)!='Sunday');
##   if (version>='20-05-31') {
##     ## many non-Sundays. sigh...
##     if (sum(bad)!=5)
##       stop(paste("doh",what,"version",version,"does not have expected 5 non-Sundays"));
##     ## adjust each non-Sunday back to previous Sunday
##     data$date=data$date-(dayofweek(weekdays(data$date))-1);
##   }
##   ## fix missing weeks in middle. shouldn't be any
##   bad=diff(data$date)!=7;
##   if (any(bad))
##     stop(paste("doh",what,"version",version,"has",sum(bad),"unexpected missing weeks"));
##   ## fix missing weeks at end
##   if (version>='21-01-24') {
##     dates=data$date
##     ## expect missing weeks at end
##     vdate=as_date(version);
##     d.last=tail(dates,n=1);
##     if ((vdate-d.last)==7)
##       stop(paste("doh",what,"version",version,
##                  "does not have expected missing week(s) at end (week",paste0(vdate-7,")")));
##     data=doh_append(data,vdate);
##   }
##   ## fix extra week (for some counties) in 20-09-27, 20-10-11, 20-11-22,
##   ## and from 20-12-20 to 21-01-10 inclusive
##   if (((version %in% c('20-09-27','20-10-11','20-11-22'))||between(version,'20-12-20','21-01-17')))
##     data=fix_extra_week(data,what,version);
##   ## done
##   data;
## }
## fix_doh_deaths=function(data,what,version) {
##   ## fix non-Sundays
##   bad=(weekdays(data$date)!='Sunday');
##   if (version=='21-01-24') {
##     ## expect 1 bad date: 2020-01-18
##     bad.expect=as_date(c('2020-01-18'));
##     if (data$date[bad]%!=%bad.expect) {
##       missing=bad.expect%notin%data$date[bad];
##       stop(paste("doh",what,"version",version,"does not have expected bad date(s):",
##                  paste(collapse=', ',missing)));
##     }
##     ## change dates to previous Sundays. fortunately, none of these duplicate correct dates
##     data$date[bad]=sunday_week(data$date[bad])
##   } else  if (version=='21-01-31') {
##     ## expect 1 bad date: 2020-01-16
##     bad.expect=as_date(c('2020-01-16'));
##     if (data$date[bad]%!=%bad.expect) {
##       missing=bad.expect%notin%data$date[bad];
##       stop(paste("doh",what,"version",version,"does not have expected bad date(s):",
##                  paste(collapse=', ',missing)));
##     }
##     ## change dates to previous Sundays. fortunately, none of these duplicate correct dates
##     data$date[bad]=sunday_week(data$date[bad])
##   } else {
##     if (any(bad))
##       stop(paste("doh",what,"version",version,"has",sum(bad),"unexpected non-Sundays:",
##                  paste(collapse=', ',data$date[bad])));
##   }
##   ## fix missing weeks
##   bad=diff(data$date)!=7;
##   if (version=='20-04-26') {
##     ## expect missing weeks after 2020-01-19
##     if ((sum(bad)!=1)&&!bad[1]&&data$date[1]!='2020-01-19')
##             stop(paste("doh",what,"version",version,
##                        "does not have expected missing week after: 2020-01-19"));
##           data=doh_insert(data,1,2);
##   }
##   else if (version=='20-05-31') {
##     ## expect missing weeks after 2020-01-26
##     if ((sum(bad)!=1)&&!bad[1]&&data$date[1]!='2020-01-26')
##       stop(paste("doh",what,"version",version,
##                  "does not have expected missing week after: 2020-01-26"));
##     data=doh_insert(data,1,2);
##   }
##   else if (version %in% c('20-06-28','20-07-05','20-07-12','20-12-13')) {
##     ## expect 2 missing weeks after 2020-01-26
##     if ((sum(bad)!=1)&&!bad[1]&&!all(data$date[1:2]==c("2020-01-26","2020-02-16")))
##       stop(paste("doh",what,"version",version,
##                  "does not have expected 2 missing weeks after: 2020-01-26"));
##     data=doh_insert(data,1,2);
##   }
##   else if (version=='21-01-24') {
##     ## expect 4 missing weeks after 2020-01-12
##     if ((sum(bad)!=4)&&!bad[1]&&!all(data$date[1:2]==c("2020-01-12","2020-02-23")))
##      stop(paste("doh",what,"version",version,
##                 "does not have expected 4 missing weeks after: 2020-01-12"));
##     data=doh_insert(data,1,4);
##   }
##   else if (version=='21-01-31') {
##     ## expect 4 missing weeks after 2020-01-19
##     if ((sum(bad)!=1)&&!bad[2]&&!all(data$date[1:3]==c("2020-01-12","2020-01-19","2020-02-23")))
##      stop(paste("doh",what,"version",version,
##                 "does not have expected 4 missing weeks after: 2020-01-19"));
##     data=doh_insert(data,2,4);
##   }
##   ## fix missing weeks at end
##   if (version>='21-01-24') {
##     dates=data$date
##     ## expect missing weeks at end
##     vdate=as_date(version);
##     d.last=tail(dates,n=1);
##     if ((vdate-d.last)==7)
##       stop(paste("doh",what,"version",version,
##                  "does not have expected missing week(s) at end (week",paste0(vdate-7,")")));
##     data=doh_append(data,vdate);
##   }
##   data;
## }
## for sanity, make sure we corrected all errors
check_doh=function(data,what,version) {
  ## make sure we have all counties and Unassigned
  places=colnames(data)[-1];
  bad=places_wa() %-% places;
  if (length(bad)>0)
    stop(paste('doh version',version,'missing counties:',paste(collapse=', ',bad)));
  if ('Unassigned' %notin% places) stop(paste('doh version',version,"missing 'Unassigned'"));
  ## make sure we have no extra places
  bad=places %-% c(places_wa(),'Unassigned');
  if (length(bad)>0)
    stop(paste('doh version',version,'has unexpected places:',paste(collapse=', ',bad)));
  ## check non-Sundays
  dates=data$date;
  bad=(weekdays(dates)!='Sunday');
  if (any(bad))
     stop(paste("Bad news: doh",what,"version",version,'has non-Sundays even after correction:',
                paste(collapse=', ',dates[bad])));
  ## check missing weeks in middle
  bad=diff(dates)!=7;
  if (any(bad))
    stop(paste("Bad news: doh",what,"version",version,'has missing weeks even after correction:',
               paste(collapse=', ',dates[bad])));
  ## check missing weeks at end
  vdate=as_date(version);
  d.last=tail(dates,n=1);
  if ((vdate-d.last)>7) {
    missing=seq(d.last+7,vdate-7,by=7);
    stop(paste("Bad news: doh",what,"version",version,
               'has missing final weeks even after correction:',
               paste(collapse=', ',missing)));
  }
  ## check extra week
  vdate=as_date(version);
  bad=(dates>=vdate);
  if (any(bad))
    stop(paste("Bad news: doh",what,"version",version,'has extra weeks even after correction:',
               paste(collapse=', ',dates[bad])));
  data;
}
## ## add missing weeks in DOH input file. data all 0's
## doh_insert=function(data,before,after) {
##   missing=seq(data$date[before]+7,data$date[after]-7,by=7);
##   nplace=ncol(data)-1;
##   new=do.call(rbind,lapply(missing,function(date) data.frame(date,repc(0,nplace))))
##   colnames(new)=colnames(data);
##   data=rbind(data[1:before,],new,data[after:nrow(data),]);
## }
## ## add missing weeks at end of DOH input file. data all 0's
## doh_append=function(data,vdate) {
##   d.last=tail(data$date,n=1);
##   missing=seq(d.last+7,vdate-7,by=7);
##   nplace=ncol(data)-1;
##   new=do.call(rbind,lapply(missing,function(date) data.frame(date,repc(0,nplace))))
##   colnames(new)=colnames(data);
##   data=rbind(data,new);
## }
## fix_extra_week=function(data,what,version) {
##   vdate=as_date(version);
##   bad=(data$date==vdate);
##   if (sum(bad)!=1) stop(paste("doh",what,"version",version,
##                               "does not have expected extra week:",vdate));
##   data[!bad,];
## }
