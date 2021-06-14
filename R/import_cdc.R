#################################################################################
##
## Author:  Nat Goodman
## Created: created 21-06-04
##
## Copyright (C) 2021 Nat Goodman.
## 
## Import CDC data from input directory. Produce ready-to-load files in data directory
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
library(RMySQL);
## ---- Import  CDC input file ----
## called by top-level import function
## key design issue: file too big to load straight into R
## solution:
##   shrink file using sed & awk, load into mysql, aggregate in mysql to get manageable dataset
##   then do something akin to the usual import process
## 2nd issue: lots of missing data
##   retain missing data for analasis later
import_cdc=
  function(file,do.fetchonly=FALSE,
           do.shrink=!do.fetchonly,do.load=!do.fetchonly,do.data=!do.fetchonly,do.fetch=TRUE) {
    if (param(verbose)) print(paste('>>> importing',file));
    version=baseonly(file,keep.dir=FALSE);
    ## connect to mysql and get sql statements from param. all stages need this
    dbh=dbConnect(MySQL(),'covid');
    param(cdc.sql);
    loadfile=filename(indir('cdc'),base=version,tail='load',suffix='csv');
    if (do.shrink) {
      ## shrink file using sed & awk 
      cmd=paste('cat',file,param(cdc.prepro),loadfile);
      if (param(verbose)) emit('+++ shrinking file');
      system(cmd);
    }
    if (do.load) {
      ## create mysql table to hold data, then load it
      dbRemoveTable(dbh,'cdc');             # drop table if exists
      sql=cdc.sql$cdc;
      if (param(verbose)) emit('+++ creating cdc table');
      dbSendQuery(dbh,sql);
      qloadfile=paste0("'",loadfile,"'");
      sql=paste(cdc.sql$load.part1,qloadfile,cdc.sql$load.part2);
      if (param(verbose)) emit('+++ loading data into cdc table (SLOW! ~20 mins)');
      dbSendQuery(dbh,sql);
      ## create indexes
      sql=cdc.sql$ageidx;
      if (param(verbose)) emit('+++ creating age index: (SLOW! ~5 mins)');
      dbSendQuery(dbh,sql);
      sql=cdc.sql$dateidx;
      if (param(verbose)) emit('+++ creating date index: (SLOW! ~5 mins):');
      dbSendQuery(dbh,sql);
    }
    if (do.data) {
      ## run query to create data table
      dbRemoveTable(dbh,'data');             # drop table if exists
      sql=cdc.sql$data;
      if (param(verbose)) emit('+++ running query to create data table: (SLOW! ~2 mins)');
      dbSendQuery(dbh,sql);
      if (param(verbose)) emit('--- data table done');
    }
    if (do.fetch) {
      sql=cdc.sql$fetch;
      if (param(verbose)) emit('+++ fetching data table');
      data=suppressWarnings(dbGetQuery(dbh,sql));
      ## convert dates to R Dates
      data$date=as_date(data$date);
      adates=data.frame(date=seqx(data$date,by=1)); # all dates of interest
      ## convert ages to my internal age labels
      age=10*data$age;
      data$age=ifelse(age=='80','80_',paste(sep='_',age,age+9));
      ## create data for each 'what'
      sapply(cq(cases,admits,icus,deaths),function(what) {
        data=data[,c('date','age',what,paste0(what,cq(F,U)))];
        colnames(data)=cq(date,age,USA,'USA F','USA U');
        ## split into data frames per age
        byage=split(data,data$age);
        ## add group for 'all' age
        bydate=split(data,data$date);
        byage$all=do.call(rbind,lapply(bydate,function(data) {
          counts=colSums(data[,3:5]);
          data.frame(date=data$date[1],age='all',t(counts),row.names=NULL,check.names=FALSE);
        }));
        byage=lapply(byage,function(data) {
          ## drop 'age' column
          data=data[,-2,drop=FALSE];
          ## add missing dates to each group. only really needed for 0_9
          data=merge(data,adates,all.y=TRUE)        
          data[is.na(data)]=0;                # from stackoverflow.com/questions/8161836. thx!
          data;
        });
        save_data(what,'cdc',version,data=byage);
      });
    }
    dbDisconnect(dbh);
  }
