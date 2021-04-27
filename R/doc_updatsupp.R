#################################################################################
##
## Author:  Nat Goodman
## Created: 21-04-04 
##          from doc_updat.R created  20-11-15
##          from misig/doc_confi.R created 19-07-16
##          from misig/confi.R created 19-07-04
## Includes content adapted 
##           from workflow.R created 20-10-08
##           from misig/doc_readmesupp.R created 19-05-09
##
## Copyright (C) 2019-2021 Nat Goodman.
## 
## Generate figures and tables for updat (weekly data update) supplement
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## --- Generate Figures and Tables for updat suplement ---
doc_updatsupp=function(sect=NULL,need.objs=TRUE,need.init=TRUE,
                       what=cq(cases,admits,deaths),datasrc=cq(doh,jhu,nyt),version='latest',
                       figs.all=FALSE,...) {
  if (is.null(version)||version=='latest') version=max(sapply(datasrc,latest_version));
  if (need.objs) make_updatsupp_objs(what=what,datasrc=datasrc,version=version);
  ## if (need.init) init_doc(doc='updatsupp',version=version,sectpfx=TRUE,...);
  if (need.init) init_doc(doc='updatsupp',version=version,figlabel=FALSE,...);
  sect.all=cq(base.wa,age.wa,base.nonwa,ages,cmp.wa,cmp.nonwa);
  if (is.null(sect)) sect=sect.all else sect=pmatch_choice(sect,sect.all,start=FALSE);
  places.wa1=cq(state,King,Snohomish,Pierce);
  labels.wa1=setNames(c('Washington state','Seattle (King County)',
                       'Snohomish (North of Seattle)','Pierce (South of Seattle)'),
                      places.wa1);
  places.wa2=cq(SKP,North,South,West,EastNotYakima);
  labels.wa2=setNames(c('Seattle Metro (SKP)','North of SKP','South of SKP',
                        'West of SKP','East of SKP (except Yakima)'),
                      places.wa2);
  places.wa=c(places.wa1,places.wa2);
  labels.wa=c(labels.wa1,labels.wa2);
  places.nonwa1=cq('Ann Arbor',Boston,'San Diego',DC);
  labels.nonwa1=setNames(c('Ann Arbor','Boston','San Diego','Washington DC'),places.nonwa1);
  places.nonwa2=cq('Big Island',Fairbury,'Cape Cod','Mackinac Island',Omaha,Austin);
  labels.nonwa2=setNames(places.nonwa2,places.nonwa2);
  places.nonwa=c(places.nonwa1,places.nonwa2);
  labels.nonwa=c(labels.nonwa1,labels.nonwa2);

  sapply(sect,function(sect) {
    ## base wa
    if (sect=='base.wa') {
      cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
      ## only 'doh' has 'admits'. prune others
      cases=cases[(cases$what!='admits'|cases$datasrc%in%cq(doh)),]
      withrows(cases,case,{
        if (param(verbose)) print(paste('+++ plotting',datasrc,what,'Washington'));
        pfx=paste(sep='_',what,datasrc);
        obj=get(paste(sep='.',datasrc,what),envir=globalenv());
        title.pfx=paste("Weekly",what,"per million from",toupper(datasrc));
        figblk_start();
        dofig(paste(sep='_',pfx,'wa1'),
              plot_cvdat(
                obj,places=places.wa1,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in core Washington locations")),
                legends=list(labels=labels.wa1)));
        dofig(paste(sep='_',pfx,'wa2'),
              plot_cvdat(
                obj,places=places.wa2,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in supp Washington locations")),
                legends=list(labels=labels.wa2)));
        ## incude figures showing ragged raw data
        dofig(paste(sep='_',pfx,'wa1','ragged'),
              plot_finraw(
                datasrc=datasrc,what=what,places=places.wa1,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in core Washington locations showing raw data")),
                legends=list(labels=labels.wa1)));
        dofig(paste(sep='_',pfx,'wa2','ragged'),
              plot_finraw(
                datasrc=datasrc,what=what,places=places.wa2,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in supp Washington locations showing raw data")),
                legends=list(labels=labels.wa2)));
      });
    }
    ## base nonwa
    if (sect=='base.nonwa') {
      datasrc=datasrc%-%'doh';          # DOH is only Washington
      what=what%-%'admits';             # admits are only DOH
      cases=expand.grid(what=what,datasrc=datasrc,stringsAsFactors=FALSE);
      withrows(cases,case,{
        if (param(verbose)) print(paste('+++ plotting',datasrc,what,'non-Washington'));
        pfx=paste(sep='_',what,datasrc);
        obj=get(paste(sep='.',datasrc,what),envir=globalenv());
        title.pfx=paste("Weekly",what,"per million from",toupper(datasrc));
        figblk_start();
        dofig(paste(sep='_',pfx,'nonwa1'),
              plot_cvdat(
                obj,places=places.nonwa1,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in core non-Washington locations")),
                legends=list(labels=labels.nonwa1)));
        dofig(paste(sep='_',pfx,'nonwa2'),
              plot_cvdat(
                obj,places=places.nonwa2,ages='all',per.capita=TRUE,lwd=2,
                title=figtitle(paste(title.pfx,"in supp non-Washington locations")),
                legends=list(labels=labels.nonwa2)));
        ## incude figures showing ragged raw data
        dofig(paste(sep='_',pfx,'nonwa1','ragged'),
              plot_finraw(
                datasrc=datasrc,what=what,places=places.nonwa1,ages='all',per.capita=TRUE,lwd=2,
                title=
                  figtitle(paste(title.pfx,"in core non-Washington locations showing raw data")),
                legends=list(labels=labels.nonwa1)));
        dofig(paste(sep='_',pfx,'nonwa2','ragged'),
              plot_finraw(
                datasrc=datasrc,what=what,places=places.nonwa2,ages='all',per.capita=TRUE,lwd=2,
                title=
                  figtitle(paste(title.pfx,"in supp non-Washington locations showing raw data")),
                legends=list(labels=labels.nonwa2)));
      });
    }
    if (sect=='ages') {
      if ('doh'%notin%datasrc) stop("datasrc must contain 'doh' to generate age figures");
      datasrc='doh';
      ages=sort(ages(doh.cases)%-%'all');
      col=col_ages(ages=ages);
      places=places.wa;
      sapply(what,function(what) {
        if (param(verbose)) print(paste('+++ plotting','ages',what));
        pfx=paste(sep='_',what,'ages');
        obj=get(paste(sep='.',datasrc,what),envir=globalenv());
        title.pfx=paste("Weekly",what,"per million by age in");
        data=data_cvdat(obj,places=places,ages=ages,per.capita=TRUE);
        ymax=max(data[,-1],na.rm=TRUE);
        figblk_start();
        sapply(places,function(place) {
          dofig(paste(sep='_',pfx,place),
                plot_cvdat(
                  obj,places=place,ages=ages,col=col,per.capita=TRUE,lwd=2,ymax=ymax,
                  title=figtitle(paste(title.pfx,labels.wa[place]))));
        });
      });
      if (all(cq(admits,deaths)%in%what)) {
        ## plots admits, deaths together
        if (param(verbose)) print('+++ plotting ages admits and death together');
        pfx='admdea_ages';
        title.pfx="Weekly admits and deaths per million by age in";
        data=data_cvdat(list(doh.admits,doh.deaths),places=places,ages=ages,per.capita=TRUE);
        ymax=max(data[,-1],na.rm=TRUE);
        sapply(places,function(place) {
          dofig(paste(sep='_',pfx,place),
                plot_admdea(places=place,ages=ages,col=col,per.capita=TRUE,ymax=ymax,
                            title=figtitle(paste(title.pfx,labels.wa[place]))));
        });
      }
    }
    if (sect=='cmp.wa') {
    
    }
  })
}
  


## make objs doc_updatsupp needs
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
    ## BREAKPOINT('make_updatsupp_objs: before withrows')
    withrows(cases,case,{
      if (param(verbose)) print(paste('+++ making',datasrc,what));
      case.env=environment();
      obj.src=raw(what,datasrc,version);
      ## do edit first so downstream objects all comparable
      ## BREAKPOINT('make_updatsupp_objs: in withrows before edit')
      obj.edit=edit(obj.src,
                    SKP=Snohomish+King+Pierce,
                    North=Whatcom+Skagit,
                    South=Thurston+Lewis+Skamania,
                    West=Kitsap+Island+'San Juan',
                    East=Okanogan+Chelan+Kittitas+Yakima+Klickitat,
                    EastNotYakima=Okanogan+Chelan+Kittitas+Klickitat,
                    SUM=list(Top5=top5,Bottom10=bottom10),
                    NEG=list(notKing='King',notSKP='SKP',notTop5='Top5',notBottom10='Bottom10'));
      ## BREAKPOINT('make_updatsupp_objs: in withrows after edit')
      ## if (datasrc=='doh')
      ##   ## TODO: not sure I like these age groups. also want labels to include age ranges
      ##   obj.edit=if(version(obj.src)<='21-03-07')
      ##              ## original ages
      ##              edit(obj.edit,kids='0_19',young='20_39'+'40_59',old='60_79'+'80_')
      ##            else ## new ages
      ##              edit(obj.edit,kids='0_19',young='20_34'+'35_49'+'50_64',old='65_79'+'80_');
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
