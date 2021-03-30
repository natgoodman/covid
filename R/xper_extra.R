#################################################################################
##
## Author:  Nat Goodman
## Created: 21-01-03 
##          from xperiment.R created 20-12-02
##
## Copyright (C) 2021 Nat Goodman.
##
## Experiment with 'extra' transform: check, validate, compare
## Could be generalized to other transforms
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
###################################################################################
## Make base objects.
## vsn.min default is min version with enough earlier versions for 'extra'
## vsn.max default is max version with original age groups
xper_objs=
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
## between allowing missing endpoint
btwn_vsn=function(vsn,vsn.min=NULL,vsn.max=NULL) {
  want=if(is.null(vsn.min)&&is.null(vsn.max)) vsn
       else if (is.null(vsn.min)) vsn<=vsn.max
       else if (is.null(vsn.max)) vsn>=vsn.min
       else btwn_cc(vsn,vsn.min,vsn.max);
  vsn[want];
}

  

