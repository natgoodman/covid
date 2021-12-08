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
objids.xper=cq(src,raw,edit,fitr,extra,fit,fitx);
## 'edit', 'raw' synonyms.  'fitx', 'fit' synonyms. 

## make object lists
make_xper_objs=
  function(what=cq(cases,admits,deaths),places=cq(state,King,Snohomish,Pierce),ages='new',
          objids=objids.xper,vsn.min=NULL,vsn.max=NULL,versions=NULL) {
    what=match.arg(what,several.ok=TRUE);
    if (ages=='new') ages=c("all","0_19","20_34","35_49","50_64","65_79" ,"80_")
    else if (ages=='old') ages=c("all","0_19","20_39","40_59","60_79","80_");
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
        objs.src=lapply(versions,function(vsn) raw(what,'doh',vsn));
        ## filter to ones with ages we want
        objs.src=objs.src[sapply(objs.src,function(obj) ages(obj)%>=%ages)]
        assign(paste(sep='.','objs',what,'src'),objs.src,globalenv());
      } else objs.src=get(paste(sep='.','objs',what,'src'),globalenv());
      if ('raw'%in%objids) {
        if (verbose) print(paste(">>> make",what,"raw (aka edit)"));
        objs.raw=lapply(objs.src,function(obj) {
          obj=edit(obj,KEEP=c(places,ages));
          if ('state'%notin%places) obj=edit(obj,DROP='state');
          if ('all'%notin%ages) obj=edit(obj,DROP='all');
          obj});
        assign(paste(sep='.','objs',what,'raw'),objs.raw,globalenv());
        assign(paste(sep='.','objs',what,'edit'),objs.raw,globalenv()); # 'edit' synonym for 'raw'
      } else objs.raw=get(paste(sep='.','objs',what,'raw'),globalenv());
      if ('extra'%in%objids)  {
        if (verbose) print(paste(">>> make",what,"extra"));
        objs.extra=lapply(objs.raw,function(obj) extra(obj));
        assign(paste(sep='.','objs',what,'extra'),objs.extra,globalenv());
      } else objs.extra=get(paste(sep='.','objs',what,'extra'),globalenv());
      if ('fitr'%in%objids)  {
        if (verbose) print(paste(">>> make",what,"fitr (fit of raw (non-extra))"));
        objs.fitr=lapply(objs.raw,function(obj) fit(obj));
        assign(paste(sep='.','objs',what,'fitr'),objs.fitr,globalenv());
      } 
      if ('fit'%in%objids)  {
        if (verbose) print(paste(">>> make",what,"fit (aka fitx)"));
        objs.fit=lapply(objs.extra,function(obj) fit(obj));
        assign(paste(sep='.','objs',what,'fit'),objs.fit,globalenv());
        assign(paste(sep='.','objs',what,'fitx'),objs.fit,globalenv()); # 'fitx' synonym for 'fit'
      } 
      if (verbose) print(">>> done");
    })
    invisible(list(what=what,objids=objids));
  }
rm_xper_objs=function(what=cq(cases,admits,deaths),objids=objids.xper) {
  what=match.arg(what,several.ok=TRUE);
  objids=match.arg(objids,several.ok=TRUE);
  if (length(what)==0||length(objids)==0) invisible(NULL); # nothing to remove
  names.all=ls(globalenv());
  pat=paste0('^(','objs',')','\\.',
             '(',paste(collapse='|',what),')','\\.',
             '(',paste(collapse='|',objids),')');
  names=grep(pat,names.all,value=TRUE);
  if (length(names)>0) {
    if (param(verbose)) print(paste('>>> rm',paste(collapse=', ',names)));
    rm(list=names,envir=globalenv());
  }
  invisible(names);
}
## plot xper object lists. wrapper for cvdat.
## VERY ROUGH - direct copy of what I did interactively
plot_xper=function(objs,obj.last=NULL,places,ages,per.capita=TRUE,xmin='2021-03-01',...) {
  if (!is.null(obj.last)) objs=cla(objs,obj.last);
  n=length(objs);
  col=c(col_brew(n-1,'rainbow'),'black');
  lty=c(rep('dotted',n-1),'solid');
  lwd=c(rep(1,n-1),2);
  plot_cvdat(objs,places=places,ages=ages,per.capita=per.capita,xmin=xmin,
             col=col,lty=lty,lwd=lwd,...);
}

## compute residuals fot xper object list
## VERY ROUGH - direct copy of what I did interactively
resid_xper=function(objs,obj.last=NULL,places,ages,per.capita=TRUE,...) {
  if (!is.null(obj.last)) objs=cla(objs,obj.last);
  n=length(objs);
  data=data_cvdat(objs,places=places,ages=ages,per.capita=per.capita,...);
  resid=data[,-1]-data[,n+1];
  resid=cbind(data$date,resid);
  colnames(resid)=colnames(data);
  invisible(resid);
}
rms_xper=function(resid) {
  rms=capply(resid[,-1],function(x) sqrt(mean(x^2,na.rm=T)));
}
## TODO: title, ylab, legend title
plot_resid=function(resid,rms=NULL,xmin='2021-03-01',...) {
  n=ncol(resid)-1;
  col=c(col_brew(n-1,'rainbow'),'black');
  lty=c(rep('dotted',n-1),'solid');
  lwd=c(rep(1,n-1),2);
  xlim=if(!is.null(xmin)) as_date(c(xmin,tail(colnames(resid),n=1)));
  ylim=if(!is.null(rms)) ylim=range(c(resid[,-1],rms),na.rm=TRUE) else NULL;
  plotm(resid,col=col,lty=lty,lwd=lwd,legend='bottomleft',xlim=xlim,ylim=ylim,...);
  if (!is.null(rms)) lines(x=as_date(colnames(rms)),y=rms[1,],lty='solid',col='black');
}

  
## between allowing missing endpoint. used in new and old code
btwn_vsn=function(vsn,vsn.min=NULL,vsn.max=NULL) {
  if (is.null(vsn.min)&&is.null(vsn.max)) return(vsn);
  want=if (is.null(vsn.min)) vsn<=vsn.max
       else if (is.null(vsn.max)) vsn>=vsn.min
       else btwn_cc(vsn,vsn.min,vsn.max);
  vsn[want];
}
########################################
## OLD CODE below here
## make wmats using 'extra' objs for all but final column
make_wmats=
  function(what=cq(cases,admits,deaths),need.objs=TRUE,
           ## places, ages for building model
           mdl.places=param(extra.places),mdl.ages=param(extra.ages),
           wmax=param(extra.wmax),
           minobjs=param(extra.minobjs)*wmax,
           maxobjs=param(extra.maxobjs)*wmax,
           ## places, ages for testing model
           places=cq(state,King,Snohomish,Pierce,Adams,'San Juan'),
           ages=c('0_19','80_','all'),
           ## places=cq(state),ages=c('all'),
           ## min, max versions. NULL means compute from mdl.minobjs, mdl.maxobjs
           vsn.min=NULL,vsn.max=NULL) {
    what=match.arg(what);
    versions=list_versions('doh',what);
    n=length(versions);
    if (is.null(vsn.min)) vsn.min=versions[minobjs+1];
    if (is.null(vsn.max)) vsn.max=versions[n];
    places.keep=unique(c(mdl.places,places));
    ages.keep=unique(c(mdl.ages,ages));
    keep=unique(c(places.keep,ages.keep));
    ## BREAKPOINT('xper_objs: before filtering versions')
    versions=btwn_vsn(versions,vsn.min,vsn.max);
    if (need.objs) {
      objs.src=lapply(versions,function(vsn) raw(what,'doh',vsn));
      ## filter to ones with desired ages
      ## BREAKPOINT('xper_objs: before filtering objs.src')
      objs.src=objs.src[sapply(objs.src,function(obj) ages.keep%<=%ages(obj))];
      ## BREAKPOINT('xper_objs: after filtering objs.src')
      objs.raw=lapply(objs.src,function(obj) edit(obj,KEEP=keep));
      objs.extra=lapply(objs.raw,function(obj)
        extra(obj,mdl.places=mdl.places,mdl.ages=mdl.ages,
              wmax=wmax,mdl.minobjs=minobjs,mdl.maxobjs=maxobjs));
      assign_global(objs.src,objs.raw,objs.extra);
    }
    n=length(objs.src);
    versions=sapply(objs.src,version);
    vdates=as_date(versions);
    ## BREAKPOINT('xper_objs: after making objects')
    d.first=min(vdates);
    ## d.last=max(vdates)-7*wmax;
    d.last=max(vdates);
    obj=objs.raw[[n]];
    objs=objs.extra[-n];
    wmats=sapply(places,simplify=FALSE,function(place) 
      sapply(ages,simplify=FALSE,function(age) {
        ## BREAKPOINT('xper_objs: before wmat ',nv(age,place))
        wmat=extra_wmat(obj,objs,place,age,wmax=wmax,d.first=d.first,d.last=d.last);
        ## BREAKPOINT('xper_objs: after wmat ',nv(age,place))
        wmat;
      }));
    ## BREAKPOINT('xper_objs: after wmats loop');
    invisible(wmats);
  }
make_emats=function(wmats,err.type=param(extra.errtype)) {
  err.type=match.arg(err.type);
  err.type=switch(err.type,multiplicative='*',additive='+',err.type);
  emats=lapply(wmats,function(wmats) lapply(wmats,function(wmat) make_emat(wmat,err.type)));
  invisible(emats);
}
make_emat=function(wmat,err.type=param(extra.errtype)) {
  err.type=match.arg(err.type);
  err.type=switch(err.type,multiplicative='*',additive='+',err.type);
  emat=if(err.type=='*') wmat/wmat[,'final'] else wmat-wmat[,'final'];
  invisible(emat);
}
    
## plot one wmat or emat
plot_mat=plot_wmat=plot_emat=
  function(mats,place='state',age='all',ws=cq(1,3,5),xmin=NULL,
           what=cq(cases,admits,deaths),
           type.mat=c('wmat','mul emat','add emat',param(extra.errtype))) {
    if (is_list(mats)) {
      mat=mats[[place]][[age]];
      if (is.null(mat)) stop("Invalid place, age for 'mats': ",nv(place,age));
    } else mat=mats;
    if (!is.matrix(mat)) stop("'mat' has incorrect type. Should be matrix. Is ",class(wmat));
    ws=c(ws%-%'final','final');           # make sure 'final' is last
    if (length(ws%-%colnames(mat))) {
      valid=colnames(mat);
      bad=ws%-%valid;
      stop("'mat' missing w(s): ",paste(collapse=', ',bad),
           ". colnames(mat): ",paste(collapse=', ',valid));
    }
    if (!is.null(xmin)) mat=mat[rownames(wmat)>=xmin,];
    what=match.arg(what);
    type.mat=match.arg(type.mat);
    type.mat=switch(type.mat,
                    multiplicative='*','mul emat'='*',additive='+','add emat'='+',type.mat);
    title.mat=switch(type.mat,
                    '*'='multiplicative emat','+'='additive emat',type.mat);
    title=paste(title.mat,what,nv(SEP=', ',place,age));
    x=as_date(rownames(mat));
    y=mat[,ws];
    col=col_mat(ws);
    lwd=lwd_mat(ws);
    lty=lty_mat(ws);
    ylab=switch(type.mat, wmat='count','*'='extra / final','+'='extra - final');
    plotm(x=x,y=y,legend='topright',col=col,lwd=lwd,lty=lty,title=title,xlab='date',ylab=ylab);
  }
## plot one 'w' for one place, multiple ages across multiple wmats or emats
## works best for multiplicative emats
plot_w=
  function(mats,place='state',ages=NULL,w=1,xmin=NULL,
           what=cq(cases,admits,deaths),mulemat.only=TRUE,
           type.mat=c('*','wmat','mul emat','add emat',param(extra.errtype))) {
    if (!is_list(mats)) stop("'plot_w' needs list of 'mats'");
    ## grab first mat to check 'w'. blithely assume all mats have same structure
    mat=mats[[1]][[1]];
    if (is.null(mat)) stop("Looks like 'mats' empty or has wrong structure");
    if (!is.matrix(mat)) stop("'mat' has incorrect type. Should be matrix. Is ",class(wmat));
    if (w%notin%colnames(mat)) {
      valid=colnames(mat);
      stop("'w' not column of 'mat': ",nv(w),". colnames(mat): ",paste(collapse=', ',valid));
    }
    if (is.null(ages)) ages=names(mats[[1]]);
    what=match.arg(what);
    type.mat=match.arg(type.mat);
    type.mat=switch(type.mat,
                    multiplicative='*','mul emat'='*',additive='+','add emat'='+',type.mat);
    if (mulemat.only&&type.mat!='*')
      stop("'plot_w' works best for multiplicative emats. 'mulemat.only' is TRUE, but 'type.mat' is not '*'",nv(type.emat));
    title.mat=switch(type.mat,
                    '*'='multiplicative emat','+'='additive emat',type.mat);
    title=paste(title.mat,what,nv(SEP=', ',place,w));
    x=as_date(rownames(mat));
    y=do.call(cbind,lapply(emats[[place]],function(mat) mat[,w]));
    if (!is.null(xmin)) y=y[rownames(y)>=xmin,];
    x=as_date(rownames(y));
    col=col_ages(ages);
    lwd=2;
    lty='solid';
    ylab=switch(type.mat, wmat='count','*'='extra / final','+'='extra - final');
    plotm(x=x,y=y,legend='bottomleft',col=col,lwd=lwd,lty=lty,title=title,xlab='date',ylab=ylab);
  }
## make line properties (col, lwd, lty) for mat (emat or emat)
col_mat=col_wmat=col_emat=
  function(ws=cq(1,3,5,final),wmax=param(extra.wmax),do.final=TRUE,
           col.pal='rainbow',col.final='grey50',skip.beg=0,skip.end=0) {
    col=if(do.final) c(col_brew(wmax,col.pal,skip.beg=skip.beg,skip.end=skip.end),col.final)
        else col_brew(wmax+1,col.pal,skip.beg=skip.beg,skip.end=skip.end);
    col=setNames(col,c(seq_len(wmax),'final'));
    if (is.null(ws)) col else col[ws];
  }
lwd_mat=lwd_wmat=lwd_emat=
  function(ws=cq(1,3,5,final),wmax=param(extra.wmax),do.final=TRUE,
           lwd=2,lwd.ws=lwd,lwd.final=(lwd.ws+1)[1]) {
    lwd=if(do.final) c(rep(lwd.ws,wmax),lwd.final)
        else rep(lwd.ws,wmax+1);
    lwd=setNames(lwd,c(seq_len(wmax),'final'));
    if (is.null(ws)) lwd else lwd[ws];
  }
lty_mat=lty_wmat=lty_emat=
  function(ws=cq(1,3,5,final),wmax=param(extra.wmax),do.final=TRUE,
           lty='dotted',lty.ws=lty,lty.final='solid') {
    lty=if(do.final) c(rep(lty.ws,wmax),lty.final)
        else rep(lty.ws,wmax+1);
    lty=setNames(lty,c(seq_len(wmax),'final'));
    if (is.null(ws)) lty else lty[ws];
  }
col_ages=
  function(ages,do.all=TRUE,col.pal='rainbow',col.all='black',skip.beg=0,skip.end=0) {
    n=length(ages);
    col=if(do.all) c(col.all,col_brew(n-1,col.pal,skip.beg=skip.beg,skip.end=skip.end))
        else col_brew(n,col.pal,skip.beg=skip.beg,skip.end=skip.end);
    setNames(col,ages);
  }


#################### OLD CODE below here. Last used 21-03-19 ####################
## Make base objects. 
## vsn.min default is min version with enough earlier versions for 'extra'
## vsn.max default is max version with original age groups
XXXmake_xper_objs=
  function(vsn.min='20-05-31',vsn.max='21-02-28',
           places=cq(state,King,Snohomish,Pierce,Adams,'San Juan'),ages=NULL,
           idx=list(xorig=TRUE,xa=TRUE,xas=TRUE,xs=TRUE,
                    xadd=FALSE,xmul=FALSE,xadw=FALSE,xmdw=FALSE,xmed=FALSE,rx=FALSE,xr=FALSE,
                    end=FALSE         # placeholder for last idx
                    )) {
    vsn=list_versions('doh','cases');
    vsn.obj=btwn_vsn(vsn,vsn.min,vsn.max);
    idx=names(idx[idx==TRUE]);
    param(verbose);
    if (verbose) print('raw');
    objs.raw=lapply(vsn.obj,function(vsn) raw('cases','doh',vsn));
    if (verbose) print('edit');
    keep=c(places,ages);
    objs.edit=lapply(objs.raw,function(obj) edit(obj,KEEP=keep));
    ## if (verbose) print('roll');
    ## objs.roll=lapply(objs.edit,function(obj) roll(obj));
    env=environment();
    sapply(idx,function(id) {
      if (verbose) print(paste0('extra (',id,')'));
      objs=
        switch(id,
               xorig=lapply(objs.edit,function(obj)
                 extra(obj,versions=vsn[vsn<=version(obj)],
                       mdl.ages=ages(obj),mdl.places=places(obj))),
               xa=lapply(objs.edit,function(obj)
                 extra(obj,versions=vsn[vsn<=version(obj)],mdl.ages='all')),
               xas=lapply(objs.edit,function(obj)
                 extra(obj,versions=vsn[vsn<=version(obj)],mdl.ages='all',mdl.places='state')),
               xs=lapply(objs.edit,function(obj)
                 extra(obj,versions=vsn[vsn<=version(obj)],mdl.places='state')),
               ##########
               xadd=lapply(objs.edit,function(obj)
                 extra(obj,versions=vsn[vsn<=version(obj)],err.type='+',args=list(fmla='y~w'))),
               xmul=lapply(objs.edit, function(obj)
                 extra(obj,versions=vsn[vsn<=version(obj)],err.type='*',args=list(fmla='y~w'))),
               xadw=lapply(objs.edit,function(obj)
                 extra(obj,versions=vsn[vsn<=version(obj)],err.type='+',
                       args=list(fmla='y~date:w+w'))),
               xmdw=lapply(objs.edit,function(obj)
                 extra(obj,versions=vsn[vsn<=version(obj)],err.type='*',
                       args=list(fmla='y~date:w+w'))),
               xmed=lapply(objs.edit,function(obj)
                 extra(obj,versions=vsn[vsn<=version(obj)],err.type='+',method='wfun')),
               rx=lapply(objs.edit,function(obj) {
                 obj=roll(obj);
                 extra(obj,versions=vsn[vsn<=version(obj)]);
               }),
               xr={
                 name=paste0('objs.',idx[1]);
                 objs.extra=get(name);
                 lapply(objs.extra,function(obj) roll(obj));
               });
      name=name=paste0('objs.',id);
      assign(name,objs,env);
    });
    ids=c(cq(raw,edit,roll),idx);
    xper_setids(ids);
    xper_global(ids);
    ids;
  }

## pick objects from object lists. all must be same length
xper_pick=function(objs,i.pick=NULL,n.pick=NULL,vsn.pick=NULL) {
  n1=length(objs[[1]]);
  args.pick=sum(!sapply(list(i.pick,n.pick,vsn.pick),is.null));
  if (args.pick>1) stop("At most one of i.pick, n.pick, vsn.pick can be specified");
  if (args.pick==1) {
    if (!is.null(n.pick)) i.pick=pick(1:n1,n.pick)
    else if (!is.null(vsn.pick)) {
      vsn=sapply(objs[[1]],version);
      i.pick=which(vsn%in%vsn.pick);
    }
    objs=lapply(objs,function(objs) objs[i.pick]);
  }
  invisible(objs);
}
## plot (or construct data frame from) objects lists
## colors used for obj versions
## lty, lwd used for object lists
## TODO: design is backwards. xper_data should be central, plot added on top. maybe someday
xper_plotdata=
  function(objs=list(objs.raw,objs.xorig,objs.xa,objs.xas),i.pick=NULL,n.pick=NULL,vsn.pick=NULL,
           places='state',ages='all',do.data=FALSE,do.plot=!do.data,
           cex.legend=NULL,labels=NULL,pal='inferno',...) {
    objs=xper_pick(objs,i.pick,n.pick,vsn.pick);
    nl=length(objs);
    lty=rev(sapply(1:nl,function(i) rep(i,length(objs[[i]]))))
    lwds=seq(1,2,length=nl);
    lwd=sapply(1:nl,function(i) rep(lwds[i],length(objs[[i]])));
    objs=do.call(c,objs);                    # flatten objs into single list
    vsns=sort(unique(sapply(objs,version))); # get unique versions from all lists
    col1=setNames(rev(col_brew(length(vsns),pal)),vsns);
    col=sapply(objs,function(obj) col1[version(obj)]);
    if (is.null(labels)) labels=sapply(objs,function(obj) paste(version(obj),obj$id));
    legends=list(title='Version/ID',labels=labels);
    if (is.null(cex.legend)) cex.legend=min(0.8,max(0.35,1+(30-(length(labels)))/50));
    if (do.plot) {
      ok=system('pjtest')==0;
      if (!ok) stop('Cannot copy to Mac: reverse tunnel not running. Stopping before plot');
      plon();
      plot_cvdat(objs,places=places,ages=ages,col=col,lty=lty,lwd=lwd,
                 cex.legend=cex.legend,legends=legends,...);
      ploff();
    }
    if (do.data) {
      data=data_cvdat(objs,places=places,ages=ages);
      colnames(data)=c('date',sub(' ','.',labels));
      invisible(data)
    }
    else NULL;
  }
xper_data=function(...,do.data=TRUE) xper_plotdata(...,do.data=do.data)
xper_plot=xper_plotdata;

## set id in objects
xper_setids=function(ids) {
  parent.env=parent.frame(n=1);
  sapply(ids,function(id) {
    name=paste0('objs.',id);
    objs=get(name,parent.env);
    objs=lapply(objs,function(obj) {obj$id=id; obj});
    assign(name,objs,parent.env)});
  invisible();
}
## assign obj lists to global
xper_global=function(ids) {
  parent.env=parent.frame(n=1);
  sapply(ids,function(id) {
    name=paste0('objs.',id);
    objs=get(name,parent.env);
    assign(name,objs,globalenv())});
  invisible();
}
  

