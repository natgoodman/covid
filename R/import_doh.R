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
  logdir=filename(param(logdir),'doh.import');
  dir.create(logdir,recursive=TRUE,showWarnings=FALSE);
  Sheets=c(cq(Cases,Deaths),if(version>='20-05-24') 'Hospitalizations');
  sapply(Sheets,function(Sheet) {
    what=if(Sheet!='Hospitalizations') tolower(Sheet) else 'admits'
    data=suppressMessages(read_excel(file,sheet=Sheet));
    data=as.data.frame(data,stringsAsFactors=F);
    data$County=sub(' County','',data$County);
    if (version<'20-12-20') {
      col.all=3;                        # hardcode it to easily cover all 3 sheets
      col.noage=which(colnames(data)=='Positive UnkAge');
    } else if (version<'21-05-30') {
      col.all=
        if(what=='cases') which(colnames(data)=='TotalCases') else which(colnames(data)==Sheet);
      col.noage=which(colnames(data)=='UnknownAge');
    } else {
      ## grumph... they changed format again
      ## cases: ProbableCases,ConfirmedCases,7-Day Count
      ## others: 7-Day <what> Counts
      ## just hardcode col numbers
      col.all=if(what=='cases') 5 else 3;
      col.noage=which(colnames(data)=='UnknownAge');
    }
    cols.age=grep('^Age',colnames(data));
    cols.data=c(1,2,col.all,cols.age,col.noage);
    ## for sanity, make sure 'all' matches individual groups
    ## in version 21-03-07, age data messed up. row sums don't match. sigh...
    if (version!='21-03-07') {
      bad=which(data[,col.all]!=rowSums(data[,c(cols.age,col.noage)]))
      if (length(bad)>0)
        stop(paste('doh version',version,"'all' does not equal sum of 'ages' in these rows:",
                   paste(collapse=', ',bad)));
    }
    ## convert age colnames to my internal age labels
    names.age=colnames(data)[cols.age];
    names.age=sub('\\+','_',sub('-','_',sub('Age ','',names.age)));
    data=data[,cols.data];
    colnames(data)=c(cq(county,date,all),names.age,'noage');
    ## Hospitalizations has some rows with Unknown date. delete 'em
    if (what=='admits') data=subset(data,subset=(date!='Unknown'));
    data$date=as_date(data$date);
    ddates=sort(unique(data$date));                       # dates in data
    wdates=seq(sunday_week(min(data$date)),vdate-7,by=7); # weekly dates spanning data
    adates=sort(unique(c(ddates,wdates)));                # all dates of interest
    log_doherrs(logdir,what,version,vdate,ddates,wdates);
    dates=data.frame(date=adates,stringsAsFactors=FALSE);
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
## log errors
log_doherrs=function(logdir,what,version,vdate,ddates,wdates) {
  file=filename(logdir,base=what,tail=version,suffix='txt');
  nonsundays=ddates[weekdays(ddates)!='Sunday'];
  extraweeks=ddates[ddates>=vdate];
  dsundays=sunday_week(ddates);
  missing=as_date(wdates%-%dsundays);
  tbl=data.frame(label=c('non-Sundays','extra weeks at end','missing weeks'),
                 num=sapply(list(nonsundays,extraweeks,missing),length),
                 weeks=sapply(list(nonsundays,extraweeks,missing),
                              function(weeks) paste(collapse=', ',weeks)));
  write.table(tbl,file=file,sep='\t',quote=F,row.names=F);
  tbl;
}

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
