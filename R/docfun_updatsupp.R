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
sect_base=
  function(where=cq(wa,nonwa),what=cq(cases,admits,deaths,admdea),datasrc=cq(doh,jhu,nyt),
           places1=NULL,places2=NULL) {
    where=match.arg(where);
    what=match.arg(what,several.ok=TRUE);
    datasrc=match.arg(datasrc,several.ok=TRUE);
    where.label=if(where=='wa') 'Washington' else 'non-Washington';
    if (where=='nonwa') datasrc=datasrc%-%'doh';
    if (length(datasrc)==0)
      stop(paste0("Bad news: 'datasrc' empty after filtering for ",nv(where),
                  ". Should have been caught earlier"));
    where.ids=paste0(where,1:2);
    if (is.null(places1)) places1=get(paste(sep='.','places',where.ids[1]),envir=globalenv());
    if (is.null(places2)) places2=get(paste(sep='.','places',where.ids[2]),envir=globalenv());
    labels1=names(places1);
    labels2=names(places2);
    cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
    ## only 'doh' has 'admits'. prune others. 'admdea' doesn't work here, yet, so prune it, oo
    cases=cases[cases$what!='admits'|cases$datasrc%in%cq(doh),]
    cases=cases[cases$what!='admdea',]
    withrows(cases,case,{
      if (param(verbose)) print(paste('+++ plotting',datasrc,what,where.label));
      pfx=paste(sep='_',what,datasrc);
      obj=get(paste(sep='.',datasrc,what),envir=globalenv());
      title.pfx=paste("Weekly",what,"per million from",toupper(datasrc));
      figblk_start();
      dofig(paste(sep='_',pfx,where.ids[1]),
            plot_cvdat(
              obj,places=places1,ages='all',per.capita=TRUE,lwd=2,
              title=figtitle(paste(title.pfx,"in core",where.label,"locations")),
              legends=list(labels=labels1)));
      dofig(paste(sep='_',pfx,where.ids[2]),
            plot_cvdat(
              obj,places=places2,ages='all',per.capita=TRUE,lwd=2,
              title=figtitle(paste(title.pfx,"in supp",where.label,"locations")),
              legends=list(labels=labels2)));
      ## incude figures showing ragged raw data
      dofig(paste(sep='_',pfx,where.ids[1],'ragged'),
            plot_finraw(
              datasrc=datasrc,what=what,places=places1,ages='all',per.capita=TRUE,lwd=2,
              title=figtitle(paste(title.pfx,"in core",where.label,"locations showing raw data")),
              legends=list(labels=labels1)));
      dofig(paste(sep='_',pfx,'wa2','ragged'),
            plot_finraw(
              datasrc=datasrc,what=what,places=places.wa2,ages='all',per.capita=TRUE,lwd=2,
              title=figtitle(paste(title.pfx,"in supp",where.label,"locations showing raw data")),
              legends=list(labels=labels2)));
    });
    cases;
  }
sect_byage=
  function(aid=1,what=cq(cases,admits,deaths,admdea),datasrc=cq(doh),
           places=NULL,ages=NULL,col=NULL) {
    aid=as.character(aid);
    what=match.arg(what,several.ok=TRUE);
    datasrc=match.arg(datasrc);
    if (is.null(places)) places=places.wa;
    if (is.null(ages)) {
      agevar=paste0('ages',aid);
      ages=get0(agevar,envir=globalenv());
      if (is.null(ages)) {
        if (param(verbose))
          print(paste('+++ no ages to plot:',agevar,'is NULL or does not exist'));
        return(what);
      }}
    if (is.null(col)) col=col_agesupp(ages=ages);
    labels=setNames(names(places),places);
    sapply(what,function(what) {
      if (what=='admdea') return();         # handled below
      if (param(verbose)) print(paste('+++ plotting',what,'by age',nv(aid)));
      pfx=paste(sep='_',what,'byage',aid);
      obj=get(paste(sep='.',datasrc,what),envir=globalenv());
      title.pfx=paste("Weekly",what,"per million by age in");
      data=data_cvdat(obj,places=places,ages=ages,per.capita=TRUE);
      ymax=max(data[,-1],na.rm=TRUE);
      figblk_start();
      sapply(places,function(place) {
        dofig(paste(sep='_',pfx,place),
              plot_cvdat(
                obj,places=place,ages=ages,col=col,per.capita=TRUE,lwd=2,ymax=ymax,
                title=figtitle(paste(title.pfx,labels[place]))));
      });
    });
    if ('admdea'%in%what) {
      ## plots admits, deaths together
      if (param(verbose)) print('+++ plotting admits and death together by age');
      pfx=paste(sep='_','admdea','byage',aid);
      title.pfx="Weekly admits and deaths per million by age in";
      data=data_cvdat(list(doh.admits,doh.deaths),places=places,ages=ages,per.capita=TRUE);
      ymax=max(data[,-1],na.rm=TRUE);
      sapply(places,function(place) {
        dofig(paste(sep='_',pfx,place),
              plot_admdeasupp(places=place,ages=ages,col=col,per.capita=TRUE,ymax=ymax,
                          title=figtitle(paste(title.pfx,labels[place]))));
      });
    }
    what;
  }
## --- Manage updatsupp objects and places ---
## make standard objects for doc_updatsupp and assign to global
## from workflow.R do_objs
make_updatsupp_objs=
  function(what=cq(cases,admits,deaths),datasrc=cq(doh,jhu,nyt),version='latest') {
    cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
    ## only 'doh' has 'admits'. prune others
    cases=cases[(cases$what!='admits'|cases$datasrc%in%cq(doh)),]
    ## sorted county populations and names
    pop.wa=sort(pop_wa(),decreasing=TRUE);
    counties.wa=colnames(pop.wa);
    top5=head(counties.wa,n=5);
    bottom10=tail(counties.wa,n=10);
    withrows(cases,case,{
      if (param(verbose)) print(paste('+++ making',datasrc,what));
      case.env=environment();
      obj.src=raw(what,datasrc,version);
      ## do edit first so downstream objects all comparable
      obj.edit=edit(obj.src,
                    SKP=Snohomish+King+Pierce,
                    North=Whatcom+Skagit,
                    South=Thurston+Lewis+Skamania,
                    West=Kitsap+Island+'San Juan',
                    East=Okanogan+Chelan+Kittitas+Yakima+Klickitat,
                    EastNotYakima=Okanogan+Chelan+Kittitas+Klickitat,
                    SUM=list(Top5=top5,Bottom10=bottom10),
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
      ## 'raw' really means 'editted', 'weekly, incremental'
      obj.raw=if(datasrc=='doh') obj.edit else weekly(incremental(obj.edit));
      ## 'cum'. convert jhu, nyt to weekly so dates will match doh
      obj.cum=if(datasrc=='doh') cumulative(obj.edit) else weekly(obj.edit);
      obj.roll=roll(obj.raw);
      ## for fit, use 1 day for cases, 10.5 days (1.5 weeks) for deaths
      fit.unit=switch(what,cases=1,admits=7,deaths=10.5);
      obj.fit=fit(obj.raw,fit.unit=fit.unit);
      if(datasrc=='doh')  {
        obj.extra=extra(obj.raw);
        obj.rollx=roll(obj.extra);
        obj.std=obj.fitx=fit(obj.extra,fit.unit=fit.unit);
      } else {
        obj.std=obj.fit;
      }
      ## iterate over objects. get id, assign to global
      names.local=grep('^obj\\.',ls(case.env),value=TRUE);
      sapply(names.local,function(name) {
        id=sub('^obj\\.','',name);
        name=paste0('obj.',id);
        obj=get(name,envir=case.env,inherits=FALSE);
        obj$id=id;
        assign(paste(sep='.',datasrc,what,id),obj,envir=globalenv())
        if (id=='std') 
          ## also assign 'std' to variable w/o suffix, eg, doh.cases
          assign(paste(sep='.',datasrc,what),obj,envir=globalenv());
      });
    });
    cases;  
  }
## remove superflous objects - either because they were created by mistake or to start clean
## if id is set, only removes those objects, else all that fit the pattern
rm_updatsupp_objs=function(what=cq(cases,admits,deaths),datasrc=cq(doh,jhu,nyt),
                           id=NULL,rm.std=is.null(id)) {
  what=match.arg(what,several.ok=TRUE);
  datasrc=match.arg(datasrc,several.ok=TRUE);
  if (length(what)==0||length(datasrc)==0) invisible(NULL); # nothing to remove
  names.all=ls(globalenv());
  pat=paste0('^(',paste(collapse='|',datasrc),')','\\.',
             '(',paste(collapse='|',what),')','\\.',
             '(',paste(collapse='|',id),')');
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
  places.wa2=setNames(cq(SKP,North,South,West,EastNotYakima),
                      c('Seattle Metro (SKP)','North of SKP','South of SKP',
                        'West of SKP','East of SKP (except Yakima)'));
  places.wa=c(places.wa1,places.wa2);

  places.nonwa1=setNames(cq('Ann Arbor',Boston,'San Diego',DC),
                         cq('Ann Arbor','Boston','San Diego','Washington DC'));
  places.nonwa2=setNames(cq('Big Island',Fairbury,'Cape Cod','Mackinac Island',Omaha,Austin),
                         cq('Big Island',Fairbury,'Cape Cod','Mackinac Island',Omaha,Austin));
 places.nonwa=c(places.nonwa1,places.nonwa2);
 assign_global(places.wa1,places.wa2,places.wa,places.nonwa1,places.nonwa2,places.nonwa);
}
## make standard age lists for doc_updatsupp and assign to global
make_updatsupp_ages=function() {
  version=version(doh.cases.src);
  if (version<='21-03-07') {
    ## original ages
    ages1=c('0_19','20_39','40_59','60_79','80_');
    ages2=c('0_19','20_59','60_');
    ages3=c('0_19','20_39','40_79','80_');
    ages4=NULL;
  } else {
    ages1=c('0_19','20_34','35_49','50_64','65_79','80_');
    ages2=c('0_19','20_49','50_64','65_');
    ages3=c('0_19','20_34','35_64','65_79','80_');
    ages4=c('0_19','20_49','50_79','80_');
  }
  assign_global(ages1,ages2,ages3,ages4);
}

## make colors for ages used here. adapted from col_ages in doc_updat.R
col_agesupp=
  function(ages=NULL,
           col1.pal='rainbow',skip.beg=2,skip.end=0,col2=cq(grey60,black),col2.n=length(col2)) {
    if (is.null(ages)) ages=ages1;
    ## separate ages into old (last 2 >= 60) vs young-ish
    old=Filter(function(age) age_starts(age)>=60,tail(ages,n=2)); # age_starts in meta.R
    young=ages%-%old;
    col2=setNames(tail(col2,n=length(old)),old)
    col1=col_brew(young,col1.pal,skip.beg=skip.beg,skip.end=skip.end);
    col=c(col1,col2);
    ## col=rep(col,each=2);
    col;
  }
## --- Plot admits and deaths data together ---
## hack to plot admits and deaths data together. adapted from plot_admdea in doc_updat.R
plot_admdeasupp=
  function(places='state',ages=NULL,per.capita=TRUE,title=NULL,ylab=NULL,ymax=NULL,
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
    plot_cvdat(list(doh.admits,doh.deaths),places=places,ages=ages,per.capita=per.capita,
               title=title,ylab=ylab,ymax=ymax,where.legend=where.legend,
               lty=lty,lwd=lwd,col=rep(col,each=2),
               legend=list(list(labels=cq(admits,deaths),lty=lty,lwd=lwd,col='black'),
                           list(labels=age_label(ages,fmt='legend'),lty='solid',lwd=2,col=col)));
  }

## --- Plot pairs of objects for same datasrc ---
## hack to plot pairs of objects together for same datasrc, typically processed and raw data 
## similar to plot_finraw in doc_updat.R
## datasrc, what, ids define obj pairs not explicitly given in objpairs
## objpairs is list of object pairs: overrides choices from previous args
## TODO: add this to plot_cvdat!
plot_pairs=
  function(what=cq(cases,deaths),datasrc=cq(doh,jhu,nyt),
           id=cq(src,edit,raw,cum,roll,fit,extra,fitx),
           objpairs=list(),
           places='state',ages='all',per.capita=TRUE,
           type2=cq(lines,points),
           lwd=2,lwd1=lwd,lwd2=0.375*lwd1,
           lty1='solid',lty2='dotted',pch=20,...) {
    if (is.null(what)||is.null(datasrc)||is.null(id)) {
      ## use objpairs as is. set cases to empty data frame
      cases=data.frame(what=character(),datasrc=character(),objpair=integer(),
                       stringsAsFactors=FALSE);
    } else {
      what=if(missing(what)) 'cases' else match.arg(what,several.ok=TRUE);
      datasrc=if(missing(datasrc)) 'jhu' else match.arg(datasrc,several.ok=TRUE);
      id=if(missing(id)) cq(std,raw) else unique(match.arg(id,several.ok=TRUE));
      if (length(id)!=2) stop ("'id' must have two elements, not ",length(id),
                               ". id=",paste(collapse=', ',id));
      type2=unique(match.arg(type2,several.ok=TRUE));
      if (length(type2)!=2) stop ("'type2' must have two elements, not ",length(type2),
                                  ". type2=",paste(collapse=', ',type2));
      ## figure out object pairs
      cases=expand.grid(what=what,datasrc=datasrc,objpair=NA,stringsAsFactors=FALSE);
      ## only 'doh' has 'admits'. prune others
      cases=cases[(cases$what!='admits'|cases$datasrc%in%cq(doh)),]
      rownames(cases)=with(cases,paste(sep='.',what,datasrc));
    }
    ## override cases from objpairs
    ## special case" objpairs is itself a pair of objects
    if (length(objpairs)==2&all(sapply(objpairs,function(obj) is_cvdat(obj))))
      objpairs=list(objpairs);
    sapply(seq_along(objpairs),function(i) { 
      objpair=objpairs[[i]];
      if (length(objpair)==0) return(); # skip empty ones
      if (length(objpair)!=2)
        stop("each objpair must contain 2 objects, not ",length(objpair));
      ## objpair is singleton or pair. if pair, must have same 'what' and 'datasrc'
      what=unique(sapply(objpair,function(obj) what(obj)));
      datasrc=unique(sapply(objpair,function(obj) datasrc(obj)));
      if (length(what)>1) 
        stop("each objpair must contain objects with same 'what', not ",
             paste(collapse=', ',what));
      if (length(datasrc)>1) 
        stop("each objpair must contain objects with same 'datasrc', not ",
             paste(collapse=', ',datasrc));
      ## override case for this 'what', 'datasrc'
      cases[paste(sep='.',what,datasrc),]<<-c(what,datasrc,i);
    });
    ## get obj pairs for cases still not handled
    withrows(cases,case,{
      if (is.na(objpair)) {
        i=length(objpairs)+1;
        obj1=get(paste(sep='.',datasrc,what,id[1]),envir=globalenv())
        obj2=get(paste(sep='.',datasrc,what,id[2]),envir=globalenv())
        objpairs[[i]]<<-list(obj1,obj2);
        cases[paste(sep='.',what,datasrc),]<<-c(what,datasrc,i)
        ## BREAKPOINT('plot_pairs: bottom of cases loop')
      }
      });
    ## BREAKPOINT('plot_pairs: after creating cases')
    ## flatten objpairs to get y range
    objs=do.call(c,objpairs);
    data=data_cvdat(objs,places=places,ages=ages,per.capita=per.capita);
    ymax=max(data[,-1],na.rm=TRUE);
    objs1=do.call(c,lapply(objpairs,function(pair) pair[1]));
    objs2=do.call(c,lapply(objpairs,function(pair) pair[2]));
    ## BREAKPOINT('plot_pairs: after extracting objs1, objs2')
    plot_cvdat(objs1,places=places,ages=ages,per.capita=per.capita,ymax=ymax,
               lwd=lwd1,lty=lty1,...);
    if ('lines'%in%type2)
      plot_cvdat(
        objs2,places=places,ages=ages,per.capita=per.capita,add=TRUE,lwd=lwd2,lty=lty2);
    if ('points'%in%type2)
      plot_cvdat(
        objs2,places=places,ages=ages,per.capita=per.capita,add=TRUE,lwd=lwd2,lty=lty2,
        type='p',pch=rep(pch,length(objs2)*length(places)*length(ages)));
    cases;
  }
