#################################################################################
##
## Author:  Nat Goodman
## Created: created 20-05-29
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
## wrapper for import functions
## if version specified, import that version only, else do all
## file, if set, includes path and supercedes version. can be directory or single file
import_age=function(datasrc='doh',version='latest',file=NULL) {
  if (datasrc!='doh') stop("Only have age data for doh, not",datasrc);
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
  sapply(files,function(file) import_age1(file=file));
  files;
}

## ---- Import age data from WA DOH input file ----
import_age1=function(file,notKing=param(doh.notKing)) {
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
            stop(paste("doh",what,"version",version,
                       "does not have expected bad date: 2020-01-16"));
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
        }}

      if (what=='admits') {
        if (version>='20-05-31') {
          ## many non-Sundays. sigh...
          bad=(weekdays(data$date)!='Sunday');
          if (sum(bad)!=5)
            stop(paste("doh",what,"version",version,"does not have expected 5 non-Sundays"));
          ## adjust each non-Sunday back to previous Sunday
          data$date=data$date-(dayofweek(weekdays(data$date))-1);
        }}
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
        }}
      ## for sanity, make sure we corrected all errors
      ## code adapted from doh_check. TODO: refactor!
      ## for sanity, make sure dates are a week apart
      dates=data$date;
      bad=(weekdays(dates)!='Sunday');
      if (any(bad))
        stop(paste("Bad news: doh",what,"version",version,'has non-Sundays even after correction:',
                   paste(collapse=', ',dates[bad])));
      sundays=dates[!bad];
      bad=diff(sundays)!=7;
      if (any(bad))
        stop(paste("Bad news: doh",what,"version",version,
                   'has missing weeks even after correction:',
                   paste(collapse=', ',head(sundays,n=-1)[bad])));
      data;
    });
    save_data(paste(sep='_','age',what),'doh',version,data=byage);
  });
}

## ---- Read imported WA DOH age data ----
## read raw imported age data and return as standard age 'pseudo-object'
raw_age=function(what=cq(cases,deaths,admits),datasrc='doh',version='latest') {
  what=match.arg(what);
  if (datasrc!='doh') stop("Only have age data for doh, not",datasrc);
  data=load_data(whatv=paste(sep='_','age',what),datasrc=datasrc,version=version);
  raw=list(data=data,datasrc=datasrc,what=what,version=version,fit='raw',
           unit=7,start.on='Sunday',center=FALSE,cumulative=FALSE);
}
## ---- Format slices of age 'pseudo-object' as data.frame ----
## multiple ages for one place, or multiple places for one age
##   in multi-place mode, can hande special 'other' place
## do counts or percentages

data_age=function(obj,ages=names(obj$data),places='state',per.capita=FALSE) {
  if (length(ages)>1&&length(places)>1)
    stop("Only one of 'age' or 'places' can have multiple values");
  byage=obj$data[ages];
  if (length(ages)==1) {
    ## multiple places for one age
    data=byage[[ages]];
    if ('other' %in% places) {
      places=places[places!='other'];
      other=setdiff(colnames(data),c(places,cq(date,state,notKing)));
      data.other=rowSums(data[,other])
      data=data.frame(date=data$date,data[,places],other=data.other);
    } else data=data.frame(date=data$date,data[,places]);
  } else {
    ## multiple ages for one place
    data=data.frame(date=byage[[1]]['date'],
                    do.call(cbind,lapply(byage,function(data) data[,places]))); 
  }
  if (per.capita) {
    ## pop.byage=if(places=='state') read_popbyage()
    ##           else if(places=='King') read_kingbyage()
    ##           else stop(paste("Can only compute per capita for 'state' or 'King', not",places))
    pop.byage=read_popbyage(places);
    pop.byage=pop.byage[,colnames(data)[-1]];
    pop.byage=repr(pop.byage,nrow(data));
    ## express as counts per million. TODO: paramerize scale factor
    data=data.frame(date=data$date,round(1e6*data[,-1]/pop.byage));
  }
  data;
}
pct_age=function(obj,ages=names(obj$data),places='state',scale.pct=TRUE,per.capita=FALSE) {
  data=data_age(obj,ages,places,per.capita=per.capita);
  dates=data$date;
  if (length(ages)==1) {
    ## multiple places for one age
    data.all=data_age(obj,ages='all',places);
    pct=data[,-1]/data.all[,-1];
  } else {
    ## multiple ages for one place
    counts=data[,-1];
    if ('all' %in% ages) counts=counts[,'all'!=ages];
    data.all=rowSums(counts);
    data.all=repc(data.all,length(ages));
    pct=data[,-1]/data.all;
  }
  if (scale.pct) pct=pct*100;
  data.frame(date=dates,pct);
}
## read pop.byage table and reformat to match age data frames
## TODO: parameterize the constants
## TODO: obviously a hack. refactor!!
read_popbyage=
  function(places='state',
           file=filename(param(indir),paste(sep='_',lcfirst(places),'byagegrp.txt'))) {
    if (places %notin% cq(state,King,Yakima)) 
      stop(paste("Can only compute per capita results for 'state', 'King', or 'Yakima', not",
                 places));
    raw=read.delim(file)
    pop.byage=data.frame(sum(raw$population),t(raw$population));
    colnames(pop.byage)=c('all',paste0('X',raw$age));
    pop.byage;
  }
## read_popbyage=function(file=filename(param(indir),'pop_byagegrp.txt')) {
##   raw=read.delim(file)
##   pop.byage=data.frame(sum(raw$population),t(raw$population));
##   colnames(pop.byage)=c('all',paste0('X',raw$age));
##   pop.byage;
## }
## read_kingbyage=function(file=filename(param(indir),'king_byagegrp.txt')) {
##   raw=read.delim(file)
##   pop.byage=data.frame(sum(raw$population),t(raw$population));
##   colnames(pop.byage)=c('all',paste0('X',raw$age));
##   pop.byage;
## }


## ---- Plot WA DOH age data ----
## wrapper for matplot. adapted from plotm_obj
## plot one or more age columns for one place,
## or one age colums for multiple places
## col is names of one or more RColorBrewer palettes or vector of colors
## TODO: plot of pcts is UGLY - perhaps abandon. if keep, fix ylab
##       add ability to plot 'other'
## rename 'ages' to 'age' or 'place' to 'places' for consistency
plotage_obj=
  function(obj,ages=names(obj$data),places='state',what='cases',type='l',add=FALSE,
           plot.pct=FALSE,per.capita=FALSE,
           xlim=NULL,xmin=NULL,xmax=NULL,ylim=NULL,
           lty='solid',lwd=1,pch=20,col='Dark2',ylab=NULL,title=NULL,cex.title=NA,
           legend='topleft',title.legend=NULL,labels.legend=NULL,cex.legend=0.8) {
    data=if(plot.pct) pct_age(obj,ages,places,scale.pct=TRUE)
         else data_age(obj,ages,places,per.capita=per.capita);
    if (is.null(ylab)) 
      ylab=paste('weekly',if(plot.pct) 'pct of all tests'
                          else if(!per.capita) 'counts' else 'counts per million','of',what);
    if (is.null(title))
      title=paste('Weekly',
                  if(plot.pct) 'Pct of all Tests'
                  else if(!per.capita) 'Counts' else 'Counts per Million','of',ucfirst(what),
                  'for',
                  if(length(ages)==1) paste('Age',age) else places);
    if (is.na(cex.title)) cex.title=cex_title(title);
    byage=obj$data[ages];
    x=data[,1];
    y=data[,-1];
    if (all(col %in% rownames(brewer.pal.info))) col=col_brew(col,n=ncol(y));
    if (!add) {
      ## new plot. do everything
      if (is.null(xlim)) {
        if (is.null(xmin)) xmin=min(x);
        if (is.null(xmax)) xmax=max(x);
        xlim=c(xmin,xmax);
      }
      matplot(x,y,type=type,xaxt='n',lty=lty,lwd=lwd,col=col,xlab=NA,ylab=ylab,xlim=xlim,ylim=ylim,
              main=title,cex.main=cex.title);
      if (type!='n') {
        grid(nx=NA,ny=NULL); # draw y grid lines. we'll draw x ourselves at first day of month
        mon.01=mon_day(x,1);
        mon.15=mon_day(x,15);
        abline(v=c(mon.01,mon.15),col="lightgray",lty="dotted");
        ## axis line below adapted from stackoverflow.com/questions/4843969. Thx!
        axis(1,mon.01,format(mon.01,"%b-%d"),cex.axis=0.8);
        ## draw legend if desired
        if (is.null(legend)) legend=FALSE
        else if (!is.logical(legend)) {
          where.legend=legend;
          legend=TRUE;
        }
        if (legend) {
          if (is.null(labels.legend)) labels.legend=if(length(places)==1) ages else places;
          add_legend(labels=labels.legend,where=where.legend,title=title.legend,
                     cex=cex.legend,col=col,lty=lty,lwd=lwd);
        }}
    } else {
      ## add to existing plot
      if (type=='l') matlines(x,y,lty=lty,lwd=lwd,col=col)
      else matpoints(x,y,lty=lty,lwd=lwd,col=col,pch=pch);
    }
    invisible()
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
