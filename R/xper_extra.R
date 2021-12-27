#################################################################################
##
## Author:  Nat Goodman
## Created: 21-01-03 
##          from xperiment.R created 20-12-02
##
## Copyright (C) 2021 Nat Goodman.
##
## Experiment with 'extra' transform: check, validate, compare
## Vesrion circa 21-12
## Based on version circa 21-08-21 (now xper_extra.bak1.R)
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
###################################################################################
## standard objids (suffixes)
objids.xper=cq(all,src,raw,edit,fitr,extra,fit,fitx,extrax,fitxx);
## note 'edit', 'raw' synonyms.  'fitx', 'fit' synonyms.
## base objids for main tests. no 'extra' since we're testing 'extra' variants
objids.test=cq(src,raw,edit,fitr);
## 
places.xper=cq(state,King,Snohomish,Pierce);
ages.xper.new=c("all","0_19","20_34","35_49","50_64","65_79" ,"80_");
ages.xper.old=c("all","0_19","20_39","40_59","60_79","80_");

## top level analysis circa Dec 2021
do_xper_old=
  function(what=cq(cases,admits,deaths),places=places.xper,ages=ages.xper.new,per.capita=TRUE,
           obj.id1='fitx',obj.id2='fitr',final.id1='fitr',final.id2='fitr',
           wmin=1,wmax=15,ws=wmin:wmax,
           DO.RATIO=TRUE,SKIP.COLF=TRUE,OMIT.ROWS=0,KEEP.COLS=0,REDUCE=FALSE,...) {
    what=match.arg(what);
    obj.id1=match.arg(obj.id1,objids.xper);
    obj.id2=match.arg(obj.id2,objids.xper);
    if (!is.null(final.id1)) final.id1=match.arg(final.id1,objids.xper);
    if (!is.null(final.id2)) final.id2=match.arg(final.id2,objids.xper);
    objs1=get_xper_objs(what=what,objid=obj.id1,final=FALSE);
    objs2=get_xper_objs(what=what,objid=obj.id2,final=FALSE);
    ## final1=get_xper_objs(what=what,objid=obj.id1,final=TRUE);
    ## final2=get_xper_objs(what=what,objid=obj.id2,final=TRUE);
    final1=get_xper_objs(what=what,objid=final.id1,final=TRUE);
    final2=get_xper_objs(what=what,objid=final.id2,final=TRUE);
    cases=expand.grid(place=places,age=ages,stringsAsFactors=FALSE);
    param(verbose);
    out=withrows(cases,case,{
      if (verbose) print(paste('>>>',nv(what,place,age)));
      resid1=
        resid_xper(objs1,final1,places=place,ages=age,per.capita=per.capita,
                   DO.RATIO=DO.RATIO,SKIP.COLF=SKIP.COLF,OMIT.ROWS=OMIT.ROWS,KEEP.COLS=KEEP.COLS);
      wmat1=wmat(resid1,ws=ws);
      rms1=rms_wmat(wmat1,REDUCE=REDUCE);
      resid2=
        resid_xper(objs2,final2,places=place,ages=age,per.capita=per.capita,
                   DO.RATIO=DO.RATIO,SKIP.COLF=SKIP.COLF,OMIT.ROWS=OMIT.ROWS,KEEP.COLS=KEEP.COLS);
      wmat2=wmat(resid2,ws=ws);
      rms2=rms_wmat(wmat2,REDUCE=REDUCE);
      rms12=data.frame(rms1,rms2);
      colnames(rms12)=c(obj.id1,obj.id2);
      if (REDUCE) data.frame(place=place,age=age,rms12,stringsAsFactors=FALSE) else rms12;
    });
    if (REDUCE) out=do.call(rbind,out) else names(out)=paste(sep=';',cases$place,cases$age);
    out;
  }
## top level analysis circa Dec 2021
do_xper=do_xper_new=
  function(what=cq(cases,admits,deaths),places=places.xper,ages=ages.xper.new,per.capita=TRUE,
           objids=cq(fitr,fitx),finalids='fitr',OUT.VAR=cq(rms,neg),
           wmin=1,wmax=15,ws=wmin:wmax,
           DO.RATIO=TRUE,SKIP.COLF=TRUE,OMIT.ROWS=0,KEEP.COLS=0,REDUCE=FALSE) {
    what=match.arg(what);
    objids=match.arg(objids,objids.xper,several.ok=TRUE);
    if (!is.null(finalids)) finalids=match.arg(finalids,objids.xper,several.ok=TRUE);
    OUT.VAR=match.arg(OUT.VAR);
    ## recycle finalids to length of objids
    finalids=rep(finalids,length=length(objids));
    objs=lapply(objids,function(objid) get_xper_objs(what=what,objid=objid,final=FALSE));
    finals=lapply(finalids,function(objid) get_xper_objs(what=what,objid=objid,final=TRUE));
    cases=expand.grid(place=places,age=ages,stringsAsFactors=FALSE);
    ## setup MoreArgs for mapply in loop below
    resid.args=
      list(per.capita=per.capita,
           DO.RATIO=DO.RATIO,SKIP.COLF=SKIP.COLF,OMIT.ROWS=OMIT.ROWS,KEEP.COLS=KEEP.COLS);
    param(verbose);
    out=withrows(cases,case,{
      if (verbose) print(paste('>>>',nv(what,place,age)));
      out=lapply(seq_along(objids),function(i) {
        objid=objids[i];
        finalid=finalids[i];
        objs=get_xper_objs(what=what,objid=objid,final=FALSE);
        final=get_xper_objs(what=what,objid=finalid,final=TRUE);
        resid=resid_xper(objs,final,places=place,ages=age,per.capita=per.capita,
                         DO.RATIO=DO.RATIO,SKIP.COLF=SKIP.COLF,OMIT.ROWS=OMIT.ROWS,
                         KEEP.COLS=KEEP.COLS);
        wmat=wmat(resid,ws=ws);
        out=if(OUT.VAR=='rms') rms_wmat(wmat,REDUCE=REDUCE)
            else rapply(wmat,function(row) length(which(sign(row)==-1))/length(row));
        out;
      });
      out=do.call(cbind,out);
      colnames(out)=objids;
      if (REDUCE) data.frame(place=place,age=age,out,stringsAsFactors=FALSE) else out;
    });
    if (REDUCE) out=do.call(rbind,out) else names(out)=paste(sep=';',cases$place,cases$age);
    out;
  }


## top level function to compare fitr, fitx for one w circa Dec 2021
cmp_xper=
  function(what=cq(cases,admits,deaths),places=places.xper,ages=ages.xper.new,per.capita=TRUE,
           obj.id1='fitx',obj.id2='fitr',final.id1='fitr',final.id2='fitr',
           w=1,DO.RATIO=TRUE,SKIP.COLF=TRUE,OMIT.ROWS=0,KEEP.COLS=0,...) {
    xper=lapply(places,function(place)
      do_xper(what=what,places=place,ages=ages,ws=w,
              DO.RATIO=DO.RATIO,SKIP.COLF=SKIP.COLF,OMIT.ROWS=OMIT.ROWS,KEEP.COLS=KEEP.COLS,
              REDUCE=TRUE,...));
    xper=do.call(rbind,xper);
    xper[,3:4]=round(xper[,3:4],digits=3);
    xper;
  }
## top level plot functions circa Dec 2021
plots.xper=cq(objs,resid,wmat,rms);
do_plot=
  function(what=cq(cases,admits,deaths),places=places.xper,ages=ages.xper.new,per.capita=TRUE,
           obj.id1='fitx',final.id1='fitr',objid=obj.id1,finid=final.id1,
           wmin=1,wmax=15,ws=wmin:wmax,
           DO.RATIO=TRUE,SKIP.COLF=TRUE,OMIT.ROWS=0,KEEP.COLS=0,REDUCE=FALSE,
           plots=cq(objs,wmat),xmin=NULL,cex.legend=0.65,doc='xper',need.init=TRUE,...) {
    if (need.init) init_doc(doc=doc,figlabel=FALSE,...); # so dofig will work
    what=match.arg(what);
    objid=match.arg(objid,objids.xper);
    objs=get_xper_objs(what=what,objid=objid,final=FALSE);
    final=if(!is.null(finid)) {
      finid=match.arg(finid,objids.xper);
      get_xper_objs(what=what,objid=finid,final=TRUE);
    } else NULL;
    plots=match.arg(plots,plots.xper,several.ok=TRUE);
    cases=expand.grid(place=places,age=ages,stringsAsFactors=FALSE);
    param(verbose);
    out=withrows(cases,case,{
      if (verbose) print(paste('>>>',nv(what,place,age)));
     resid=
        resid_xper(objs,final,places=place,ages=age,per.capita=per.capita,
                   DO.RATIO=DO.RATIO,SKIP.COLF=SKIP.COLF,OMIT.ROWS=OMIT.ROWS,KEEP.COLS=KEEP.COLS);
      if (is.null(xmin)) xmin=as.character(as_date(colnames(resid)[2])-7);
      wmat=wmat(resid,ws=ws);
      rms=rms_wmat(wmat,REDUCE=FALSE);
      fname2=figname_xper(what,place,age,objid,(if (!is.null(finid)) finid else 'none'));
      sapply(plots,function(p) {
        fname=figname_xper(p,fname2);
        if (verbose) print(paste('+++ plotting',fname));
        switch(p,
               objs=dofig(fname,
                          plot_objs(objs,final,places=place,ages=age,per.capita=per.capita,
                                    xmin=xmin,cex.legend=cex.legend,...)),
               resid=dofig(fname,plot_resid(resid,xmin=xmin,cex.legend=cex.legend)),
               wmat=dofig(fname,plot_wmat(wmat,xmin=xmin,cex.legend=cex.legend)),
               rms=dofig(fname,{
                 plot(x=as.numeric(rownames(rms)),y=rms,xlab='w',type='l',lty='solid'); grid()})
               )
      });
      cases;
    });
  }

figname_xper=function(...) paste(sep='_',...);

## make object lists
make_xper_objs=
  function(what=cq(cases,admits,deaths),places=places.xper,ages='new',
           extrax.places=places.xper,extrax.ages=ages.xper,extrax.minobjs=15,
           objids=objids.xper,do.final=TRUE,vsn.min=NULL,vsn.max=NULL,versions=NULL) {
    what=match.arg(what,several.ok=TRUE);
    if (ages=='new') ages=ages.xper.new else ages=ages.xper.old;
    objids=match.arg(objids,several.ok=TRUE);
    ## deal with objids synonyms
    if ('raw'%in%objids) objids=objids%-%'edit';
    if ('fit'%in%objids) objids=objids%-%'fitx';
    verbose=param(verbose);
    sapply(what,function(what) {
      if ('src'%in%objids) {
        if (verbose) print(paste(">>> make",what,"src"));
        if (is.null(versions)) versions=list_versions('doh',what);
        versions=btwn_vsn(versions,vsn.min,vsn.max);
        objs.all=lapply(versions,function(vsn) raw(what,'doh',vsn));
        ## objs.src=lapply(versions,function(vsn) raw(what,'doh',vsn));
        ## filter to ones with ages we want
        objs.src=objs.all[sapply(objs.all,function(obj) ages(obj)%>=%ages)]
        assign(paste(sep='.','objs',what,'all'),objs.all,globalenv());
        assign(paste(sep='.','objs',what,'src'),objs.src,globalenv());
        if (do.final) {
          assign(paste(sep='.','final',what,'all'),objs.all[[length(objs.all)]],globalenv());
          assign(paste(sep='.','final',what,'src'),objs.src[[length(objs.src)]],globalenv());
        }
      }
      if ('raw'%in%objids) {
        if (verbose) print(paste(">>> make",what,"raw (aka edit)"));
        objs.src=get(paste(sep='.','objs',what,'src'),globalenv());
        objs.raw=lapply(objs.src,function(obj) {
          obj=edit(obj,KEEP=c(places,ages));
          if ('state'%notin%places) obj=edit(obj,DROP='state');
          if ('all'%notin%ages) obj=edit(obj,DROP='all');
          obj});
        assign(paste(sep='.','objs',what,'raw'),objs.raw,globalenv());
        assign(paste(sep='.','objs',what,'edit'),objs.raw,globalenv()); # 'edit' synonym for 'raw'
        if (do.final) {
          assign(paste(sep='.','final',what,'raw'),objs.raw[[length(objs.raw)]],globalenv());
          assign(paste(sep='.','final',what,'edit'),objs.raw[[length(objs.raw)]],globalenv());
        }
      }
      if ('extra'%in%objids)  {
        if (verbose) print(paste(">>> make",what,"extra"));
        objs.raw=get(paste(sep='.','objs',what,'raw'),globalenv());
        objs.extra=lapply(objs.raw,function(obj) extra(obj));
        assign(paste(sep='.','objs',what,'extra'),objs.extra,globalenv());
        if (do.final)
          assign(paste(sep='.','final',what,'extra'),objs.extra[[length(objs.extra)]],globalenv());
      } 
      if ('fitr'%in%objids)  {
        if (verbose) print(paste(">>> make",what,"fitr (fit of raw (non-extra))"));
        objs.raw=get(paste(sep='.','objs',what,'raw'),globalenv())
        objs.fitr=lapply(objs.raw,function(obj) fit(obj,fit.unit=7));
        assign(paste(sep='.','objs',what,'fitr'),objs.fitr,globalenv());
        if (do.final)
          assign(paste(sep='.','final',what,'fitr'),objs.fitr[[length(objs.fitr)]],globalenv());
      } 
      if (('fit'%in%objids)||('fitx'%in%objids))  {
        if (verbose) print(paste(">>> make",what,"fit (aka fitx)"));
        objs.extra=get(paste(sep='.','objs',what,'extra'),globalenv());
        objs.fit=lapply(objs.extra,function(obj) fit(obj,fit.unit=7));
        assign(paste(sep='.','objs',what,'fit'),objs.fit,globalenv());
        assign(paste(sep='.','objs',what,'fitx'),objs.fit,globalenv()); # 'fitx' synonym for 'fit'
        if (do.final) {
          assign(paste(sep='.','final',what,'fit'),objs.fit[[length(objs.fit)]],globalenv());
          assign(paste(sep='.','final',what,'fitx'),objs.fit[[length(objs.fit)]],globalenv());
        }
      }
      if ('extrax'%in%objids)  {
        if (verbose) print(paste(">>> make",what,"extrax"));
        objs.raw=get(paste(sep='.','objs',what,'raw'),globalenv());
        ## truncate to objs with enough earlier versions
        objs.ok=objs.raw[sapply(objs.raw,function(obj) ages(obj)%>=%extrax.ages)];
        objs.in=tail(objs.ok,n=-extrax.minobjs);
        if (!length(objs.in))
          stop("Hmm... No objects have enough older versions with the correct ages");
        objs.extra=lapply(objs.in,function(obj) {
          objs.mdl=objs.ok[sapply(objs.ok, function(obj.mdl) version(obj.mdl)<version(obj))];
          extra(obj,objs=objs.mdl,
                mdl.places=extrax.places,mdl.ages=extrax.ages,mdl.minobjs=extrax.minobjs);
        });
        assign(paste(sep='.','objs',what,'extrax'),objs.extra,globalenv());
        if (do.final)
          assign(
            paste(sep='.','final',what,'extrax'),objs.extra[[length(objs.extra)]],globalenv());
      } 
      if ('fitxx'%in%objids) {
        if (verbose) print(paste(">>> make",what,"fitxx"));
        objs.extra=get(paste(sep='.','objs',what,'extrax'),globalenv());
        objs.fit=lapply(objs.extra,function(obj) fit(obj,fit.unit=7));
        assign(paste(sep='.','objs',what,'fitxx'),objs.fit,globalenv());
        if (do.final) 
          assign(paste(sep='.','final',what,'fitxx'),objs.fit[[length(objs.fit)]],globalenv());
      }
    })
    if (verbose) print(">>> done");
    invisible(list(what=what,objids=objids));
  }
rm_xper_objs=function(what=cq(cases,admits,deaths),objids=objids.xper) {
  what=match.arg(what,several.ok=TRUE);
  objids=match.arg(objids,several.ok=TRUE);
  if (length(what)==0||length(objids)==0) invisible(NULL); # nothing to remove
  names.all=ls(globalenv());
  pat=paste0('^(','objs|final',')','\\.',
             '(',paste(collapse='|',what),')','\\.',
             '(',paste(collapse='|',objids),')');
  names=grep(pat,names.all,value=TRUE);
  if (length(names)>0) {
    if (param(verbose)) print(paste('>>> rm',paste(collapse=', ',names)));
    rm(list=names,envir=globalenv());
  }
  invisible(names);
}
get_xper_objs=function(what=cq(cases,admits,deaths),objid=objids.xper,final=FALSE) {
  what=match.arg(what);
  objid=match.arg(objid);
  name=paste(sep='.',(if (final) 'final' else 'objs'),what,objid);
  get(name,globalenv());
}
## plot xper object lists. wrapper for cvdat.
## VERY ROUGH - direct copy of what I did interactively
plot_objs=plot_xper=
  function(objs,obj.final=NULL,places='state',ages='all',per.capita=TRUE,xmin='2021-03-01',
           col=NULL,lty=NULL,lwd=NULL,...) {
    if (!is.null(obj.final)) objs=cla(objs,obj.final);
    n=length(objs);
    if (is.null(col)) col=c(col_brew(n-1,'rainbow'),'black');
    if (is.null(lty)) lty=c(rep('dotted',n-1),'solid');
    if (is.null(lwd)) lwd=c(rep(1,n-1),2);
    plot_cvdat(objs,places=places,ages=ages,per.capita=per.capita,xmin=xmin,
               col=col,lty=lty,lwd=lwd,...);
  }

## compute residuals fot xper object list
## VERY ROUGH
resid_xper=
  function(objs,obj.final=NULL,places='state',ages='all',per.capita=TRUE,
           DO.RATIO=FALSE,SKIP.COLF=TRUE,OMIT.ROWS=0,KEEP.COLS=0,...) {
    if (length(places)!=1) stop("resid_xper needs single place, not ",length(places)," places");
    if (length(ages)!=1) stop("resid_xper needs single age, not ",length(ages)," ages");
    ## BREAKPOINT('resid_xper: top',nv(places,ages,KEEP.COLS))
    n=length(objs);
    if (!is.null(obj.final)) {
      ## replace last obj in list if it has same version as obj.final. 
      if (SKIP.COLF&&version(objs[[n]])==version(obj.final)) objs[[n]]=obj.final
      else objs=cla(objs,obj.final);      # else append obj.final to list
    }
    n=length(objs)-1;                     # number of objs excluding final
    cnm=c(sapply(objs[1:n],function(obj) version(obj)),'final');
    data=data_cvdat(objs,places=places,ages=ages,per.capita=per.capita,cnm=cnm,...);
    if (OMIT.ROWS>0) data=head(data,n=-OMIT.ROWS);
    if (KEEP.COLS>0) {
      data=data[,c(1,((ncol(data)-KEEP.COLS):ncol(data)))];
      n=KEEP.COLS;
    }
    resid=data[,2:(n+1)]-data[,'final'];  # each cell minus final
    if (DO.RATIO) {
      final=data[,'final'];
      final[final==0]=NA;
      resid=as.data.frame(capply(resid,function(x) x/final));
    }
    resid=cbind(date=data$date,resid);        
    ## BREAKPOINT('resid_xper: end',nv(places,ages,KEEP.COLS))
    invisible(resid);
  }
rms=function(x) sqrt(mean(x^2,na.rm=T));
rms_xper=function(x,SKIP.COL1=TRUE,REDUCE=FALSE) {
  if (is_2d(x)) {
    if (SKIP.COL1) x=x[,-1];
    if (!REDUCE) capply(x,function(x) rms(x)) else rms(as.vector(x));
  } else rms(x);
}
rms_wmat=function(x,REDUCE=FALSE) {
  if (!REDUCE) rapply(x,function(row) rms(row)) else rms(as.vector(x));
}

## limit residuals to region used for model - ie, last wmax values for each version
## NG 21-12-15: optionally order rows by 'w' - so that row index and 'w' match
##              optionally use 'w' as rownames and remove from data
wmat=function(resid,wmax=15,ws=1:wmax,W.ORDER=TRUE,W.ROWNAMES=TRUE) {
  data=resid[,-1];
  vsns=colnames(data);
  ## base row for interesting region
  i.base=which(resid$date==as_date(vsns[1]))-1;
  i1=i.base+(1:length(vsns))-1;
  wmax=max(ws);
  i0=i1-wmax+1;
  mat=do.call(cbind,lapply(seq_along(vsns),function(j) data[i0[j]:i1[j],j,drop=FALSE]));
  colnames(mat)=vsns;
  w=wmax:1;
  if (W.ORDER) {
    mat=mat[wmax:1,,drop=FALSE];
    w=1:wmax;
  } 
  if (W.ROWNAMES) rownames(mat)=w
  else mat=cbind(w=w,mat);
  invisible(mat[ws,,drop=FALSE]);
}

## TODO: title, ylab, legend title
plot_resid=function(resid,rms=NULL,xmin='2021-03-01',...) {
  n=ncol(resid)-1;
  ## col=c(col_brew(n-1,'rainbow'),'black');
  col=col_brew(n,'rainbow');
  lty=c(rep('dotted',n-1),'solid');
  lwd=c(rep(1,n-1),2);
  xlim=if(!is.null(xmin)) as_date(c(xmin,tail(colnames(resid),n=1)));
  ylim=if(!is.null(rms)) ylim=range(c(resid[,-1],rms),na.rm=TRUE) else NULL;
  plotm(resid,col=col,lty=lty,lwd=lwd,legend='bottomleft',xlim=xlim,ylim=ylim,...);
  if (!is.null(rms)) lines(x=as_date(colnames(rms)),y=rms[1,],lty='solid',col='black');
}

## TODO: title, ylab, legend title. improve col, etc
plot_wmat=function(wmat,xmin='2021-03-01',ws=rownames(wmat),col=NULL,...) {
  data=data.frame(date=colnames(wmat),t(wmat),stringsAsFactors=FALSE,check.names=F);
  if (is.null(col)) col=col_ws(ws=ws);
  lty='solid';
  lwd=1;
  xlim=if(!is.null(xmin)) as_date(c(xmin,tail(data$date,n=1))) else NULL;
  plotm(data,col=col,lty=lty,lwd=lwd,legend='top',xlim=xlim,...);
  abline(h=c(-0.1,0,0.1),lty=cq(dotted,dotted,dotted),lwd=0.75,col='red');
}

## col_ws adapted from col_ages in docfun_updat
col_ws=
  function(wmax=15,ws=1:wmax,
           col1.pal='rainbow',skip.beg=2,skip.end=0,
           col2=cq(black,grey40,grey60),col2.n=length(col2)) {
    ws=as.character(ws);
    col2=setNames(col2,head(ws,n=col2.n));
    col1=col_brew(tail(ws,n=-col2.n),col1.pal,skip.beg=skip.beg,skip.end=skip.end);
    col=c(col2,col1);
    ## col=rep(col,each=2);
    col;
  }
ages_doh=function(obj=doh.cases.raw) sort(ages(obj)%-%'all');
  
## between allowing missing endpoint. used in new and old code
btwn_vsn=function(vsn,vsn.min=NULL,vsn.max=NULL) {
  if (is.null(vsn.min)&&is.null(vsn.max)) return(vsn);
  want=if (is.null(vsn.min)) vsn<=vsn.max
       else if (is.null(vsn.max)) vsn>=vsn.min
       else btwn_cc(vsn,vsn.min,vsn.max);
  vsn[want];
}
    
