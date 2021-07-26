#################################################################################
##
## Author:  Nat Goodman
## Created: 21-05-02
##          from doc_updatsupp.R created 21-04-04 
##          from doc_updat.R created  20-11-15
##          from misig/doc_confi.R created 19-07-16
##          from misig/confi.R created 19-07-04
## Includes content adapted 
##           from workflow.R created 20-10-08
##           from misig/doc_readmesupp.R created 19-05-09
##
## Copyright (C) 2019-2021 Nat Goodman.
## 
## Specialized functions used in doc_updatsupp.R
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## --- Functions for updatsupp sections ---
sect_byage=
  function(ageid=1:4,where=param(where),what=param(what),datasrc=param(datasrc),
           objid='std',conf.prob=cq(conf,prob),
           ages=NULL,places=NULL,col=NULL) {
    what=what_var(what,choices=cq(cases,admits,icus,deaths,admdea));
    datasrc=datasrc_var(datasrc,choices=cq(cdc,doh));
    objid=match.arg(objid,choices=cq(std,cum,inc),several.ok=TRUE);
    objid=unique(ifelse(objid=='inc','std',objid));
    conf.prob=match.arg(conf.prob,choice=cq(conf,prob),several.ok=TRUE);
    ## expand variables and add conf.prob to cdc cases
    cases=merge(expand.grid(ageid=ageid,where=where,what=what,datasrc=datasrc,objid=objid,
                            stringsAsFactors=FALSE),
                data.frame(what='cases',datasrc='cdc',conf.prob,stringsAsFactors=FALSE),
                all.x=TRUE);
    ## prune invalid what, datasrc combinations
    cases=subset(cases,subset=(what!='icus'|datasrc=='cdc'));  
    withrows(cases,case,{
      ages=ageid_var(ageid,datasrc);
      places=where_var(where,places);
      if (what!='admdea') objs=list(get_obj(datasrc,what,objid))
      else objs=lapply(cq(admits,deaths),function(what) get_obj(datasrc,what,objid))
      ## make sure obj(s) contains desired places
      ok=all(sapply(objs,function(obj) places%<=%places(obj)));
      if (!ok) return();
      if (is.null(col)) col=col_agesupp(ages,datasrc);
      labels=if(datasrc=='doh') setNames(names(places),places) else setNames(places,places);
      objid.label=if(objid=='std') 'inc' else objid;
      objid.title=if(objid.label=='inc') 'Weekly' else 'Cumulative';
      what.label=if(what=='cases'&datasrc=='cdc'&conf.prob=='prob') 'cpcases' else what;
      what.title=
        if(what=='cases'&datasrc=='cdc'&conf.prob=='prob') paste('confirmed+probable',what)
        else if(what=='admdea') 'admits and deaths' else what;
      if (param(verbose))
        print(paste('+++ plotting',datasrc,what.label,objid.label,'by age',nv(ageid)));
      pfx=paste(sep='_','byage',what.label,objid.label,datasrc,ageid);
      title.pfx=paste(objid.title,what.title,"per million by age in");
      if (what!='admdea') {
        obj=objs[[1]];
        if (what=='cases'&datasrc=='cdc') obj=cdc_cases(obj,conf.prob);
        data=data_cvdat(obj,places=places,ages=ages,per.capita=TRUE);
        ymax=max(data[,-1],na.rm=TRUE);
        figblk_start();
        sapply(places,function(place) {
          dofig(paste(sep='_',pfx,place),
                plot_cvdat(
                  obj,places=place,ages=ages,col=col,per.capita=TRUE,lwd=2,ymax=ymax,
                  title=figtitle(paste(title.pfx,labels[place]))));
        });
      } else {
        ## plots admits, deaths together
        data=data_cvdat(objs,places=places,ages=ages,per.capita=TRUE);
        ymax=max(data[,-1],na.rm=TRUE);
        ylab=paste(tolower(objid.title),"admits and deaths per million");
        figblk_start();
        sapply(places,function(place) {
          dofig(paste(sep='_',pfx,place),
                plot_admdeasupp(objs,places=place,ages=ages,col=col,per.capita=TRUE,ymax=ymax,
                                ylab=ylab,title=figtitle(paste(title.pfx,labels[place]))));
        });
      }
    });
    cases;
  }
sect_byplace=
  function(where=param(where),what=param(what),datasrc=param(datasrc),
           objid='std',conf.prob=cq(conf,prob),
           places=NULL,col=NULL) {
    what=what_var(what,choices=cq(cases,admits,icus,deaths));
    datasrc=datasrc_var(datasrc);
    objid=match.arg(objid,choices=cq(std,cum,inc),several.ok=TRUE);
    objid=unique(ifelse(objid=='inc','std',objid));
    conf.prob=match.arg(conf.prob,choice=cq(conf,prob),several.ok=TRUE);
    ## expand variables and tack-on conf.prob to cdc cases
    cases=merge(expand.grid(where=where,what=what,datasrc=datasrc,objid=objid,
                            stringsAsFactors=FALSE),
                data.frame(what='cases',datasrc='cdc',conf.prob,stringsAsFactors=FALSE),
                all.x=TRUE);
    ## prune invalid what, datasrc combinations
    cases=subset(cases,
                 subset=((what%in%cq(cases,deaths)) |              # all have cases, deaths
                         (what%in%cq(admits,admdea)&datasrc%in%cq(doh,cdc)) | 
                         (what=='icus'&datasrc=='cdc')));
    ## prune invalid what, datasrc combinations
    cases=subset(cases,subset=(what!='icus'|datasrc=='cdc'));  
    withrows(cases,case,{
      places=where_var(where,places);
      obj=get_obj(datasrc,what,objid);
      ## make sure datasrc contains desired places
      if (!(places%<=%places(obj))) return();
      objid.label=if(objid=='std') 'inc' else objid;
      objid.title=if(objid.label=='inc') 'Weekly' else 'Cumulative';
      what.label=if(what=='cases'&datasrc=='cdc'&conf.prob=='prob') 'cpcases' else what;
      what.title=
        if(what=='cases'&datasrc=='cdc'&conf.prob=='prob') paste('confirmed+probable',what)
        else {if(what=='admdea') 'admits and deaths' else what}
      where.title=if(is.na(where)) 'Select'
                  else {if(grepl('^wa',where)) 'Washington'
                  else {if(grepl('^nonwa',where)) 'non-Washington'
                  else {if(where=='fav')'Favorite' else 'USA'}}}
      labels=names(places);
      if (param(verbose)) print(paste('+++ plotting',datasrc,what.label,objid.label,where));
      pfx=paste(sep='_','byplace',what.label,objid.label,datasrc,where);
      title.pfx=paste(objid.title,what.title,"per million from",toupper(datasrc));
      figblk_start();
      dofig(pfx,
            plot_cvdat(
              obj,places=places,ages='all',per.capita=TRUE,lwd=2,col=col,
              title=figtitle(paste(title.pfx,"in",where.title,"locations")),
              legends=list(labels=labels)));
      if (objid.label=='inc') {
        ## incude figures showing ragged raw data
        dofig(paste(sep='_',pfx,'ragged'),
              plot_finraw(
                datasrc=datasrc,what=what,places=places,ages='all',per.capita=TRUE,lwd=2,col=col,
                title=figtitle(paste(title.pfx,"in",where.title,"locations showing raw data")),
                legends=list(labels=labels)));
      }
    });
    cases;
  }
sect_bysrc=
  function(where=param(where),what=param(what),datasrc=param(datasrc),
           objid='std',conf.prob=cq(conf,prob),do.raw='auto',
           places=NULL,col=NULL,col.pal='d3') {
    if (is.null(where)&&is.null(places)) stop("'where' and 'places' both NULL. Nothing to plot");
    if (is.null(places)) places=unique(unlist(lapply(where,where_var)));
    what=what_var(what,choices=cq(cases,deaths)); # only ones that work here
    datasrc=datasrc_var(datasrc);
    objid=match.arg(objid,choices=cq(std,cum,inc),several.ok=TRUE);
    objid=unique(ifelse(objid=='inc','std',objid));
    conf.prob=match.arg(conf.prob,choice=cq(conf,prob),several.ok=TRUE);
    if (is.null(col)) {
      col=col_brew(length(datasrc)+1,col.pal); # +1 to include CDC conf+prob
      col.cp=tail(col,n=1);
      col=head(col,n=-1);                      # remove CDC conf+prob
    }
    do.raw=setNames(rep(do.raw,len=length(datasrc)),datasrc);
    ## expand variables
    cases=expand.grid(place=places,what=what,objid=objid,stringsAsFactors=FALSE);
    if (all(do.raw=='auto'))
      ## tack-on ragged to cases (only used for cdc)
      cases=merge(cases,data.frame(what='cases',ragged=c(TRUE,FALSE),stringsAsFactors=FALSE),
                  all.x=TRUE)
    else cases$ragged=NA;
    withrows(cases,case,{
      objid.label=if(objid=='std') 'inc' else objid;
      objid.title=if(objid.label=='inc') 'Weekly' else 'Cumulative';
      if (param(verbose)) print(paste('+++ plotting',what,objid.label,'bysrc',place));
      objs=lapply(datasrc,function(datasrc) get_obj(datasrc,what,objid));
      ## prune datasrc's and objs that don't contain place
      keep=sapply(seq_along(objs),function(i) place%in%places(objs[[i]]));
      datasrc=datasrc[keep];
      objs=objs[keep];
      datasrc.title=paste(collapse=", ",toupper(datasrc));
      labels=setNames(toupper(datasrc),datasrc);
      fname=paste(sep='_','bysrc',what,objid.label,place);
      ftitle=paste(objid.title,what,"per million from",datasrc.title,"in",place);
      col=col[keep];
      do.raw=do.raw[keep];
      do.raw=if(is.na(ragged)) ifelse(do.raw=='auto',sapply(objs,is_incremental),do.raw)
      else rep(ragged,length(do.raw));
      ## handle CDC conf+prob cases
      if (what=='cases'&&'cdc'%in%datasrc&&'prob'%in%conf.prob) {
        i=which(datasrc=='cdc');
        obj=cdc_cases(objs[[i]],'prob');
        objs=c(objs,list(obj));
        datasrc=c(datasrc,'cdc');
        col=c(col,col.cp);
        do.raw=c(do.raw,do.raw[i]);
        labels=c(labels,'CDC conf+prob');
      }
      objpairs=lapply(seq_along(objs),function(i) {
        obj=objs[[i]];
        obj.raw=if(do.raw[i]) get_obj(datasrc(obj),what,objid='raw') else NA;
        list(obj,obj.raw);
      });
      figblk_start();
      if (any(do.raw)) {
        fname=paste(sep='_',fname,'ragged');
        ftitle=paste(ftitle,'showing raw data');
      }
      dofig(fname,
              plot_pairs(objpairs=objpairs,places=place,per.capita=TRUE,title=figtitle(ftitle),
                col=col,legends=list(title='Source',labels=labels)));
    });
    cases;
  }
## process args and 'withrows' vars in 'sect' functions
## some can process multiple values at a time and can be used at top of function
## some can process one value at a time and SHOULD BE USED in 'withrows' body
ageid_var=function(ageid,datasrc,ages=NULL,choices=NULL) {
  if (length(ageid)>1) stop("Can only process one 'ageid' value at a time. Should call function inside 'withrows' body. ageid=",paste(collapse=', ',ageid));
  ageid=as.character(ageid);
  agel=get(paste(sep='.',datasrc,'ages'),envir=globalenv());
  if (is.null(choices)) choices=names(agel);
  if (ageid%in%choices) ages=agel[[ageid]]
  else if (is.null(ages))
    stop("Invalid 'ageid' for 'datasrc', and 'ages' is NULL: ",nv(ageid,datasrc,SEP=', '),
         ". Valid ageids for datasrc are ",paste(collapse=', ',choices));
  ages;
}
datasrc_var=
  function(datasrc.var,choices=param(datasrc),none.ok=TRUE,allnone.ok=FALSE,several.ok=TRUE) {
    unique(pmatch_choice(
      datasrc.var,choices,none.ok=none.ok,allnone.ok=allnone.ok,several.ok=several.ok));
    ##  bad=sapply(ml,function(m) length(m)==0)
    ## if (any(bad)) stop("Invalid datasrc(s): ",paste(collapse=', ',datasrc.var[bad]),
    ##                     ". Valid values: ",paste(collapse=', ',choices));
    ## unique(unlist(ml))
}
what_var=function(what.var,choices=param(what),none.ok=TRUE,allnone.ok=FALSE,several.ok=TRUE) {
  unique(pmatch_choice(
    what.var,choices,none.ok=none.ok,allnone.ok=allnone.ok,several.ok=several.ok));
  ## bad=sapply(ml,function(m) length(m)==0)
  ## if (any(bad)) stop("Invalid what(s): ",paste(collapse=', ',what.var[bad]),
  ##                    ". Valid values: ",paste(collapse=', ',choices));
  ## unique(unlist(ml))
}
where_var=function(where,places=NULL) {
  if (length(where)>1) stop("Can only process one 'where' value at a time. Should call function inside 'withrows' body. where=",paste(collapse=', ',where));
  places=get0(paste(sep='.','places',where),ifnotfound=places)
  if (is.null(places)) stop("No 'places.where' variable exists and 'places' is NULL: ",nv(where));
  places;
}

## return confirmed cases or confirmed+probable depending on conf.prob
cdc_cases=function(obj,conf.prob) if (conf.prob=='prob') edit(obj,USA=USA+'USA F') else obj;
## get object constructed by make_updatsupp_objs below
get_obj=function(datasrc,what,objid=NULL)
  get(paste(collapse='.',c(datasrc,what,objid)),envir=globalenv());

## --- Manage updatsupp objects and places ---
## make standard objects for doc_updatsupp and assign to global
## from workflow.R do_objs
make_updatsupp_objs=
  function(what=param(what),datasrc=param(datasrc),version='latest') {
    what=match.arg(what,several.ok=TRUE);
    datasrc=match.arg(datasrc,several.ok=TRUE);
    cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
    ## prune invalid what, datasrc combinations
    cases=subset(cases,
                 subset=((what%in%cq(cases,deaths)) |              # all have cases, deaths
                         (what=='admits'&datasrc%in%cq(doh,cdc)) | # doh, cdc have admits
                         (what=='icus'&datasrc=='cdc')));          # cdc has icus
    ## sorted county populations and names
    pop.wa=sort(pop_wa(),decreasing=TRUE);
    counties.wa=colnames(pop.wa);
    top5=head(counties.wa,n=5);
    bottom10=tail(counties.wa,n=10);
    ## Eastern WA, Western WA from Wikipedia
    easternwa=c("Adams","Asotin","Benton","Chelan","Columbia","Douglas","Ferry","Franklin",
                "Garfield","Grant","Kittitas","Klickitat","Lincoln","Okanogan","Pend Oreille",
                "Spokane","Stevens","Walla Walla","Whitman","Yakima");
    westernwa=c("Clallam","Clark","Cowlitz","Grays Harbor","Island","Jefferson","King","Kitsap",
                "Lewis","Mason","Pacific","Pierce","San Juan","Skagit","Skamania","Snohomish",
                "Thurston","Wahkiakum","Whatcom");
    withrows(cases,case,{
      if (param(verbose)) print(paste('+++ making',datasrc,what));
      case.env=environment();
      obj.src=raw(what,datasrc,version);
      if (datasrc!='cdc') {
        ## do edit first so downstream objects all comparable
        obj.edit=edit(obj.src,
                      SKP=Snohomish+King+Pierce,
                      North=Whatcom+Skagit,
                      South=Thurston+Lewis+Skamania,
                      West=Kitsap+Island+'San Juan',
                      East=Okanogan+Chelan+Kittitas+Yakima+Klickitat,
                      EastNotYakima=Okanogan+Chelan+Kittitas+Klickitat,
                      SUM=list(Top5=top5,Bottom10=bottom10,
                               EasternWA=easternwa,EasternWANotYakima=easternwa%-%'Yakima',
                               WesternWA=westernwa,WesternWANotKing=westernwa%-%'King'),
                      NEG=list(notKing='King',notSKP='SKP',notTop5='Top5',notBottom10='Bottom10'));
        if (datasrc=='doh')
          obj.edit=if(version(obj.src)<='21-03-07')
                     ## original ages
                     edit(obj.edit,
                          '20_59'='20_39'+'40_59',
                          '20_79'='20_39'+'40_59'+'60_79',
                          '40_79'='40_59'+'60_79',
                          '60_'='60_79'+'80_')
                   else ## new ages
                     edit(obj.edit,
                          '20_49'='20_34'+'35_49',
                          '20_64'='20_34'+'35_49'+'50_64',
                          '20_79'='20_34'+'35_49'+'50_64'+'65_79',
                          '35_64'='35_49'+'50_64',
                          '35_79'='35_49'+'50_64'+'65_79',
                          '50_79'='50_64'+'65_79',
                          '65_'='65_79'+'80_');
      } else {                          # datasrc is 'cdc'
        obj.edit=edit(obj.src,
                      Confirmed=USA,Probable='USA F','All Cases'=USA+'USA F',
                      '0_19'='0_9'+'10_19',
                      '20_49'='20_29'+'30_39'+'40_49',
                      '20_69'='20_29'+'30_39'+'40_49'+'50_59'+'60_69',
                      '20_79'='20_29'+'30_39'+'40_49'+'50_59'+'60_69'+'70_79',
                      '50_69'='50_59'+'60_69',
                      '50_79'='50_59'+'60_69'+'70_79',
                      '60_'='60_69'+'70_79'+'80_',
                      '70_'='70_79'+'80_');
      }
      ## 'raw' really means 'edited', 'weekly, incremental'
      obj.raw=if(datasrc=='doh') obj.edit else weekly(incremental(obj.edit));
      ## 'cum'. convert jhu, nyt to weekly so dates will match doh
      obj.cum=if(is_incremental(obj.edit)) cumulative(obj.edit) else weekly(obj.edit);
      obj.roll=roll(obj.raw);
      ## 'dly' is 'edited', 'daily', 'incremental'. only useful for sources with daily data
      obj.dly=switch(datasrc,
                     doh=daily(obj.edit),
                     jhu=incremental(obj.edit),
                     nyt=incremental(obj.edit),
                     cdc=obj.edit);
      ## NG 21-07-15: using different fit.units for cases and others was bad idea
      ##   jagged non-case plots actually caused by rounding of per.capita results
      ## for fit, use 1 day for cases, 10.5 days (1.5 weeks) for deaths
      ## fit.unit=switch(what,cases=1,admits=7,icus=7,deaths=10.5);
      ## obj.fit=fit(obj.raw,fit.unit=fit.unit);
      obj.fit=fit(obj.raw);                          # default method=sspline, fit.unit=1
      if(datasrc=='doh')  {
        obj.extra=extra(obj.raw);
        obj.rollx=roll(obj.extra);
        obj.std=obj.fitx=fit(obj.extra);
      } else {
        obj.std=obj.fit;
      }
      ## iterate over objects. get id, assign to global
      names.local=grep('^obj\\.',ls(case.env),value=TRUE);
      sapply(names.local,function(name) {
        objid=sub('^obj\\.','',name);
        name=paste0('obj.',objid);
        obj=get(name,envir=case.env,inherits=FALSE);
        obj$id=objid;
        assign(paste(sep='.',datasrc,what,objid),obj,envir=globalenv())
        if (objid=='std') 
          ## also assign 'std' to variable w/o suffix, eg, doh.cases
          assign(paste(sep='.',datasrc,what),obj,envir=globalenv());
      });
    });
    cases;  
  }
## remove superflous objects - either because they were created by mistake or to start clean
## if id is set, only removes those objects, else all that fit the pattern
rm_updatsupp_objs=function(what=param(what),datasrc=param(datasrc),
                           objid=NULL,rm.std=is.null(objid)) {
  what=match.arg(what,several.ok=TRUE)%-%'admdea'; # admea irrelevant here
  datasrc=match.arg(datasrc,several.ok=TRUE);
  if (length(what)==0||length(datasrc)==0) invisible(NULL); # nothing to remove
  names.all=ls(globalenv());
  pat=paste0('^(',paste(collapse='|',datasrc),')','\\.',
             '(',paste(collapse='|',what),')','\\.',
             '(',paste(collapse='|',objid),')');
  names1=grep(pat,names.all,value=TRUE);
  rm(list=names1,envir=globalenv());
  if (rm.std) {
    pat=paste0('^(',paste(collapse='|',datasrc),')','\\.',
               '(',paste(collapse='|',what),')$');
    names2=grep(pat,names.all,value=TRUE);
    rm(list=names2,envir=globalenv());
    invisible(c(names1,names2));
  } else invisible(names1);
}
## make standard places for doc_updatsupp and assign to global
make_updatsupp_places=function() {
 places.wa1=setNames(cq(state,King,Snohomish,Pierce),
                      c('Washington state','Seattle (King County)',
                       'Snohomish (North of Seattle)','Pierce (South of Seattle)'));
 places.wa2=setNames(cq(SKP,North,South,West,EastNotYakima,Yakima),
                      c('Seattle Metro (SKP)','North of SKP','South of SKP',
                        'West of SKP','East of SKP (except Yakima)','Yakima'));
 places.wa3=setNames(cq(EasternWA,EasternWANotYakima,WesternWA,WesternWANotKing),
                     c('Eastern Washington','Eastern Washington (except Yakima)',
                       'Western Washington','Western Washington (except King)'));
 places.wa=c(places.wa1,places.wa2,places.wa3);

 places.nonwa1=setNames(cq('Ann Arbor',Boston,'San Diego',DC),
                        cq('Ann Arbor','Boston','San Diego','Washington DC'));
 places.nonwa2=setNames(cq('Big Island',Fairbury,'Cape Cod','Mackinac Island',Omaha,Austin),
                        cq('Big Island',Fairbury,'Cape Cod','Mackinac Island',Omaha,Austin));
 places.nonwa=c(places.nonwa1,places.nonwa2);

 places.all=c(places.wa,places.nonwa);
 places.fav=cq(King,'San Juan','Ann Arbor','Mackinac Island',DC,Fairbury);
 places.wall=places(doh.cases);
 places.nonwall=places(jhu.cases)%-%places.wall;

 places.usa=setNames('USA','USA');

 assign_global();                       # assign all locals to global
 vars=ls();
 where=sub('^places\\.','',vars);
 param(where=where);
 places.updatsupp<<-vars;               # for rm_updatsupp_places
 vars;
}
rm_updatsupp_places=function() {
  if (exists('places.updatsupp',envir=globalenv())) {
    vars=places.updatsupp;
    rm(list=c(places.updatsupp,'places.updatsupp'),envir=globalenv());
    vars;
  } else NULL;
}
## make standard age lists for doc_updatsupp and assign to global
make_updatsupp_ages=function(datasrc=cq(doh,cdc)) {
  sapply(datasrc,function(datasrc) {
    if (datasrc=='doh') {
      version=version(doh.cases.src);
      if (version<='21-03-07') {
        ## original ages
        agel=list(c('0_19','20_39','40_59','60_79','80_'),
                  c('0_19','20_59','60_'),
                  c('0_19','20_39','40_79','80_'),
                  NULL);
      } else {
        agel=list(c('0_19','20_34','35_49','50_64','65_79','80_'),
                  c('0_19','20_49','50_64','65_'),
                  c('0_19','20_34','35_64','65_79','80_'),
                  c('0_19','20_49','50_79','80_'));
      }}
    else {                              # datasrc is 'cdc'
        agel=list(c('0_9','10_19','20_29','30_39','40_49','50_59','60_69','70_79','80_'),
                  c('0_9','10_19','20_69','70_79','80_'),
                  c('0_9','10_19','20_49','50_69','70_79','80_'),
                  c('0_9','10_19','20_49','50_59','60_69','70_'));
      }
    names(agel)=seq_along(agel);      # so ageids like '1' will work
    assign(paste(sep='.',datasrc,'ages'),agel,envir=globalenv());
  });
  ages.updatsupp<<-paste(sep='.',datasrc,'ages');
  ages.updatsupp;
}
rm_updatsupp_ages=function() {
  if (exists('ages.updatsupp',envir=globalenv())) {
    vars=ages.updatsupp;
    rm(list=c(ages.updatsupp,'ages.updatsupp'),envir=globalenv());
    vars;
  } else NULL;
}

## make colors for ages used here. adapted from col_ages in doc_updat.R
## TODO: handle cdc ages
col_agesupp=
  function(ages,datasrc=cq(doh,cdc),do.young=FALSE,do.old=TRUE,
           col.pal='rainbow',skip.beg=2,skip.end=0,  # for mid-ages
           col.young=head(cq(red,hotpink),n=2),      # for cdc young ages
           col.old=tail(cq(grey60,black),n=2)        # for old ages
           ) {
    datasrc=match.arg(datasrc);
    ## separate ages into young (<20), mid, old (last 2 >= 60)
    ## note age_starts function is in meta.R
    old=if(do.old) Filter(function(age) age_starts(age)>=60,tail(ages,n=2)) else NULL;
    young=if(do.young) Filter(function(age) age_starts(age)<20,head(ages,n=2)) else NULL;
    mid=ages%-%c(young,old);            # delete young, old if any
    ## make colors for each group
    col.young=setNames(head(col.young,n=length(young)),young);
    col.old=setNames(tail(col.old,n=length(old)),old)
    col.mid=col_brew(mid,col.pal,skip.beg=skip.beg,skip.end=skip.end);
    col=c(col.young,col.mid,col.old);
    ## col=rep(col,each=2);
    col;
  }
## --- Plot admits and deaths data together ---
## hack to plot admits and deaths data together. adapted from plot_admdea in doc_updat.R
## TODO: handle cdc
plot_admdeasupp=
  function(objs=list(doh.admits,doh.deaths),
           places='state',ages=NULL,per.capita=TRUE,title=NULL,ylab=NULL,ymax=NULL,
           where.legend='topleft',
           lwd.admits=2,lwd.deaths=3,lwd=c(lwd.admits,lwd.deaths),
           lty.admits='dotted',lty.deaths='solid',lty=c(lty.admits,lty.deaths),
           col=NULL) {
    if (is.null(ages)) ages=ages1;
    if (is.null(title)) 
      title=paste(collapse=' ',
                  c('Weekly admits and deaths',
                    if(per.capita) 'per million' else NULL,
                    'for',places));
    if (is.null(ylab))
      ylab=paste(collapse=' ',
                 c('weekly admits and deaths',if(per.capita) 'per million' else NULL))
    if (is.null(col)) col=col_agesupp(ages=ages);
    plot_cvdat(objs,places=places,ages=ages,per.capita=per.capita,
               title=title,ylab=ylab,ymax=ymax,where.legend=where.legend,
               lty=lty,lwd=lwd,col=rep(col,each=2),
               legend=list(list(labels=cq(admits,deaths),lty=lty,lwd=lwd,col='black'),
                           list(labels=age_label(ages,fmt='legend'),lty='solid',lwd=2,col=col)));
  }

## --- Plot pairs of objects for same datasrc ---
## hack to plot pairs of objects together for same datasrc, typically processed and raw data 
## similar to plot_finraw in doc_updat.R
## datasrc, what, ids define obj pairs 
## objpairs is list of object pairs (or NA): overrides choices from previous args
plot_pairs=
  function(what=param(what),datasrc=param(datasrc),places='state',ages='all',per.capita=TRUE,
           objid=NULL,objpairs=list(),
           ## id=cq(src,edit,raw,cum,dly,roll,fit,extra,fitx,std),
           col=NULL,col.pal='d3',
           type1='l',type2='p',type=c(type1,type2),
           lwd1=2,lwd2=0.375*lwd1,lwd=c(lwd1,lwd2),
           lty1='solid',lty2='dotted',lty=c(lty1,lty2),
           pch1=20,pch2=20,pch=c(pch1,pch2),...) {
    if (length(objpairs)==0) {
      ## create objpairs from other args
      what=if(missing(what)) 'cases' else match.arg(what,several.ok=TRUE);
      what=what%-%'admdea';           # admea irrelevant here, so prune it
      datasrc=if(missing(datasrc)) 'jhu' else match.arg(datasrc,several.ok=TRUE);
      if (is.null(objid)) objid=cq(std,raw);
      ## id=if(missing(id)) cq(std,raw) else unique(match.arg(id,several.ok=TRUE));
      ## if (length(id)!=2) stop ("'id' must have two elements, not ",length(id),
      ##                          ". id=",paste(collapse=', ',id));
      ## figure out object pairs
      cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
      ## prune invalid what, datasrc combinations
      cases=subset(cases,
                   subset=((what%in%cq(cases,deaths)) |              # all have cases, deaths
                           (what=='admits'&datasrc%in%cq(doh,cdc)) | # doh, cdc have admits
                           (what=='icus'&datasrc=='cdc')));          # cdc has icus
      objpairs=withrows(cases,case,{
        obj1=get_obj(datasrc,what,objid[1]);
        obj2=get_obj(datasrc,what,objid[2]);
        list(obj1,obj2);
      });
    } else {
      ## use objpairs as is. set cases to empty data frame
      len=length(objpairs)
      if (all(sapply(objpairs,function(obj) is_cvdat(obj)||is.na(obj)))) {
        ## special case: objpairs is list of objects. group into pairs
        if (len%%2) stop("When 'objpairs' is list of objects, its length must be even, not ",len);
        objpairs=lapply(seq(1,len-1,by=2),function(i) c(objpairs[i],objpairs[i+1]));
      } 
      ## make sure objpairs valid
      bad=which(!sapply(objpairs,function(pair)
        length(pair)==2&&{
          obj1=pair[[1]]; obj2=pair[[2]];is_cvdat(obj1)&&(is_cvdat(obj2)||is.na(obj2))}));
      if (any(bad))
        stop("These elements of 'objpairs' are invalid: ",paste(collapse=',',bad));
    }
    ## flatten objpairs to get y range
    objs=do.call(c,objpairs);
    objs=objs[!is.na(objs)];
    data=data_cvdat(objs,places=places,ages=ages,per.capita=per.capita);
    ymax=max(data[,-1],na.rm=TRUE);
    objs1=do.call(c,lapply(objpairs,function(pair) pair[1]));
    objs2=do.call(c,lapply(objpairs,function(pair) pair[2]));
    ## compute pch1, pch2 because need to rep to correct length
    pch1=rep(pch[1],length(objs1)*length(places)*length(ages));
    pch2=rep(pch[2],length(objs2)*length(places)*length(ages));
    ## do the plots!
    if (is.null(col)) col=col_brew(length(objpairs),col.pal);
    col1=col[!is.na(objs1)];
    objs1=objs1[!is.na(objs1)];
    col2=col[!is.na(objs2)];
    objs2=objs2[!is.na(objs2)];
    plot_cvdat(objs1,places=places,ages=ages,per.capita=per.capita,ymax=ymax,col=col1,
               type=type[1],lwd=lwd[1],lty=lty[1],pch=pch1,...);
    if (length(objs2))
      plot_cvdat(objs2,places=places,ages=ages,per.capita=per.capita,add=TRUE,col=col2,
                 type=type[2],lwd=lwd[2],lty=lty[2],pch=pch2,...);
    length(objpairs);
  }
