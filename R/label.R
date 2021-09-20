#################################################################################
##
## Author:  Nat Goodman
## Created  20-06-24
##          from transform.R created 20-05-06
##          from import.R created 20-05-02
##
## Copyright (C) 2020 Nat Goodman.
## 
## Transform imported data for analysis
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Construct text strings from object attributes ----
## attributes of interest
##   data source
##   what - cases or deaths
##   fit (aka interpolated or smoothed)
##   roll (smoothed using rolling mean)
##   extra - late time points extrapolated
##   edit - object data edited
##   time unit - weekly or daily
##   cumulative vs incremental
time_label=Vectorize(function(unit=cq(1,7,daily,weekly)) {
  if (unit==FALSE) return(NA);
  unit=as.character(unit);
  unit=match.arg(unit);
  switch(unit,'1'='daily','7'='weekly',unit);
})
time_unit=Vectorize(function(label=cq(1,7,daily,weekly)) {
  label=as.character(label);
  label=match.arg(label);
  switch(label,daily=1,weekly=7,as.numeric(label));
})
cuminc_label=Vectorize(function(cum,fmt=cq(title,legend,ylab)) {
  fmt=match.arg(fmt);
  if (cum) 'cumulative' else if (fmt=='legend') 'incremental' else NA},
  vectorize.args='cum');

datasrc_label=Vectorize(function(datasrc)
  ## switch(datasrc,doh='DOH',ihme='IHME',jhu='JHU',nyt='NYTimes',trk='CovidTrack',yyg='C19Pro',
  ##        datasrc),
  toupper(datasrc),
  USE.NAMES=FALSE)
id_label=Vectorize(function(id,fmt=cq(title,legend,ylab)) {
  fmt=match.arg(fmt);
  switch(fmt,
         title=if(id==FALSE) NA else paste0("'",id,"'"),
         legend=if(id==FALSE) 'raw' else id,
         ylab=NA)},
  vectorize.args='id');
fit_label=Vectorize(function(fit,fmt=cq(title,legend,ylab)) {
  fmt=match.arg(fmt);
  switch(fmt,
         title=if(fit==FALSE) NA else fit,
         legend=if(fit==FALSE) 'raw' else fit,
         ylab=NA)},
  vectorize.args='fit');
roll_label=Vectorize(function(roll,fmt=cq(title,legend,ylab)) {
  fmt=match.arg(fmt);
  switch(fmt,
         title=if(roll==FALSE) NA else roll,
         legend=if(roll==FALSE) 'none' else roll,
         ylab=NA)},
  vectorize.args='roll');
extra_label=Vectorize(function(extra,fmt=cq(title,legend,ylab)) {
  fmt=match.arg(fmt);
  switch(fmt,
         title=if(extra==FALSE) NA else paste0('(late time points extrapolated by ',extra,')'),
         legend=if(extra==FALSE) 'original' else extra,
         ylab=NA)},
  vectorize.args='extra');
edit_label=Vectorize(function(edit,fmt=cq(title,legend,ylab)) {
  fmt=match.arg(fmt);
  switch(fmt,
         title=NA,
         legend=if(edit) 'edited' else 'unedited',
         ylab=NA)},
  vectorize.args='edit');
## age_label=Vectorize(function(age,fmt=cq(title,legend,ylab)) {
##   fmt=match.arg(fmt);
##   if (age %in% ages_all() || ) {
##     ## standard age
##     label=switch(age,all='all ages','80_'='80+',sub('_','-',age));
##     if (age!='all') {
##       suffix=switch(fmt,
##                     title='year olds',
##                     legend='years old',
##                     'yrs');
##       ## suffix=if(fmt=='title') 'year olds'
##       ##        else if (fmt=='legend') 
##       ##          paste0('year olds (',
##       ##                 switch(age,
##       ##                        '0_19'='children and teens',
##       ##                        '20_39'='young adults',
##       ##                        '40_59'='middle aged adults',
##       ##                        '60_79'='young seniors',
##       ##                        '80_'='old seniors'),
##       ##                 ')')
##       ##        else 'yrs';
##       label=paste(label,suffix);
##     }
##     label;
##   } else {
##     ## custom age
##     age.label=param(age.label)[[age]];
##     label=if(is.null(age.label)) age
##           else if (length(age.label)==1) age.label else age.label[fmt];
##     label;
##   }
##   label},
##   vectorize.args='age');
age_label=function(age,fmt=cq(title,legend,ylab,short)) {
  fmt=match.arg(fmt);
  ages.all=ages_all();
  age.label=param(age.label);
  sapply(age,function(age) 
    if (age %in% ages.all || age_range(age)) {
      ## standard age
      label=switch(age,all='all ages','80_'='80+',sub('_','-',age));
      if (age!='all') {
        suffix=switch(fmt,
                      title='year olds',
                      legend='years old',
                      'yrs');
        ## suffix=if(fmt=='title') 'year olds'
        ##        else if (fmt=='legend') 
        ##          paste0('year olds (',
        ##                 switch(age,
        ##                        '0_19'='children and teens',
        ##                        '20_39'='young adults',
        ##                        '40_59'='middle aged adults',
        ##                        '60_79'='young seniors',
        ##                        '80_'='old seniors'),
        ##                 ')')
        ##        else 'yrs';
        label=paste(label,suffix);
      }
      label;
    } else {
      ## custom age
      age.label=age.label[[age]];
      label=if(is.null(age.label)) age
            else if (length(age.label)==1) age.label else age.label[fmt];
      if (is.na(label)) label=age;
      setNames(label,NULL);
    });
}
age_range=function(age) regexpr('^\\d+_\\d+$',age,perl=T)[1]==1
  
name_label=function(name,val,fmt=cq(title,legend,ylab),SEP='&') {
  if (is.null(val)) val=NA;
  switch(name,
         datasrc=datasrc_label(val),
         unit=time_label(val),
         cumulative=cuminc_label(val,fmt),
         id=id_label(val,fmt=fmt),
         fit=fit_label(val,fmt=fmt),
         roll=roll_label(val,fmt=fmt),
         extra=extra_label(val,fmt=fmt),
         edit=edit_label(val,fmt=fmt),
         age=age_label(val,fmt=fmt),
         val);
}
paste_label=function(labels,SEP='&') {
  if (all(is.na(labels))) NULL else paste(collapse=SEP,unique(labels[!is.na(labels)]));
}
paste_title=function(attr,labels,SEP='&') {
  if (all(is.na(labels))) NULL
  else {
    labels=unique(labels);
    ## if (attr=='fit') labels=labels[labels!='raw'];
    term=paste(collapse=SEP,
          switch(attr,
                 unit=ucfirst(labels),
                 cumulative=ucfirst(labels),
                 what=ucfirst(labels),
                 roll=paste0(labels,')'),
                 edit=NULL,
                 labels));
    prefix=switch(attr,
                  datasrc='from',
                  fit='fitted to',
                  roll='(rolling mean',
                  place='for',
                  age='for',
                  NULL);
    title=if(length(labels)!=0) paste(collapse=' ',c(prefix,term)) else NULL;
  }
}
paste_legend=function(row,SEP=', ') {
  attrs=names(row);
  terms=unlist(sapply(attrs,function(attr) {
    label=name_label(attr,row[attr],fmt='legend');
    if (is.na(label)) NULL else label;
  }));
  paste(collapse=SEP,terms);
}
ltitle_label=function(attr) {
  switch(attr,
         unit='Interval',
         cumulative='Cum/Inc',
         datasrc='Source',
         roll='Rolling Mean',
         id='ID',
         ## what, version, fit, extra, edit, place, age
         ucfirst(attr));
}
                 
## adapted from www.r-bloggers.com/dates-in-r-and-the-first-day-of-the-month. Thx!
## NG 21-09-19: code moved to covid/R/label.R and extended to get last day of month
## mon_day=function(date,day) {
##   date=as_date(date);                   # in case comes to us numeric
##   ## as.Date(paste(sep='-',unique(format(date,format="%Y-%m")),day))
##   as_date(paste(sep='-',unique(format(date,format="%Y-%m")),day))
## }

## produce data frame of obj typically single-valued params
obj_attr=function(obj,attrs=cq(datasrc,what,unit,cumulative,version,fit,roll,extra,edit),
                  label=TRUE) {
  vals=with(obj,mget(attrs,ifnotfound=list(NULL)));
  if (label) vals=sapply(attrs,function(attr) name_label(attr,vals[[attr]]),simplify=FALSE);
  xattr=as.data.frame(vals,stringsAsFactors=FALSE);
  rownames(xattr)=NULL;
  xattr;
}
objs_attr=function(objs,attrs=cq(datasrc,what,unit,cumulative,version,fit,roll,extra,edit),
                   label=TRUE) 
  do.call(rbind,lapply(objs,function(obj) obj_attr(obj,attrs,label)));


