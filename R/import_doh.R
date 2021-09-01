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
## Import DOH data from input directory. Produce ready-to-load files in data directory
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
  ## Hospitalizations broken in version 21-06-20. hopefully temporary...
  ## Sheets=c(cq(Cases,Deaths),if(version>='20-05-24') 'Hospitalizations');
  Sheets=c(cq(Cases,Deaths),if(version>='20-05-24'&version!='21-06-20') 'Hospitalizations');
  sapply(Sheets,function(Sheet) {
    what=if(Sheet!='Hospitalizations') tolower(Sheet) else 'admits'
    data=suppressMessages(read_excel(file,sheet=Sheet));
    data=as.data.frame(data,stringsAsFactors=F);
    data$County=sub(' County','',data$County);
    cols.age=grep('^Age',colnames(data));
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
      col.noage=grep('Unk\\w*Age',colnames(data)); # v 21-08-29 changed 'UnknownAge' to 'UnkAge'
      if (length(col.noage)!=1)
        stop(paste('doh',what,'version',version,"has unrecognized 'Unknown Age' column name(s)"));
      if (what=='cases'&&version>='21-08-29') 
        ## v 21-08-29 case counts now seem to include 'probables' and
        ##   old 'all' column (7-Day Count) no longer matches totals
        ##   to simplify code below, forcibly set 7-Day Count to confirmed+probable
        data[,col.all]=data[,3]+data[,4];
    }
    cols.data=c(1,2,col.all,cols.age,col.noage);
    ## convert age colnames to my internal age labels
    names.age=colnames(data)[cols.age];
    names.age=sub('\\+','_',sub('-','_',sub('Age ','',names.age)));
    data=data[,cols.data];
    colnames(data)=c(cq(county,date,all),names.age,'noage');
    ## admits has some rows with Unknown date. delete 'em
    if (what=='admits') data=subset(data,subset=(date!='Unknown'));
    if (version>='21-08-29') {
      ## changed ages as of version 21-08-29. sigh! new ages (note overlap!!):
      ##   4_10, 11_13, 14_19, 0_11, 12_19, 20_34, 35_49, 50_64, 65_79, 80_
      ## better resolution for young ages but strange ranges not compatible with
      ## pop or mort. also overlapping ranges may confuse existing apps
      want.age=c('4_10','11_13','14_19','0_11','12_19','20_34','35_49','50_64','65_79','80_');
      if (names.age%!=%want.age) {
        bad=names.age%-%want.age;
        if (bad) stop(paste('doh',what,'version',version,"has unexpected age column(s):",
                            paste(collapse(', ',bad))));
        bad=want.age%-%names.age;
        stop(paste('doh',what,'version',version,"missing expected age column(s):",
                            paste(collapse(', ',bad))));
      }
      ## add 0_19 for backwards campatibility, remove ones that won't work,
      ## and arrange columns in sensible order
      data.src=data;                    # hang onto original data for error checks
      data$'0_19'=data$'0_11'+data$'12_19';
      names.age=c('0_19','20_34','35_49','50_64','65_79','80_');
      data=data[,c(cq(county,date,all),names.age,'noage')];
    }
    ## for sanity, make sure 'all' matches individual groups
    ## in version 21-03-07, age data messed up. row sums don't match. sigh...
    if (version!='21-03-07') {
      ages.sum=rowSums(data[,c(names.age,'noage')]);
      bad=which(data[,'all']!=ages.sum);
      if (length(bad)>0) {
        ## in version 21-08-29, some admits & deaths rows double count the overlapping ages
        ## handle as special case
        if (what!='cases'&&version=='21-08-29') {
          data.bad=data.src[bad,];
          ages.bad=rowSums(data.bad[,c('20_34','35_49','50_64','65_79','80_',
                                       '4_10','11_13','14_19','0_11','12_19','noage')]);
          bad=which(data.bad[,'all']!=ages.bad);
          if (length(bad)>0) {
            stop(paste('doh',what,'version',version,"'all' does not equal sum of 'ages' in these rows even as special case:",paste(collapse=', ',bad)));
          }
          ## forcibly fix errors by setting all to correct value
          data[,'all']=ages.sum;
        }
        else
          stop(paste('doh',what,'version',version, "'all' does not equal sum of 'ages' in these rows:",paste(collapse=', ',bad)));
      }
    }
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
    ## BREAKPOINT('import_doh: after bycounty',nv(Sheet))
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
