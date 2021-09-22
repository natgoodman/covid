#################################################################################
##
## Author:  Nat Goodman
## Created: 20-07-13
##          from plot.R (now in R.BAK) created 20-05-06
##          from covid.R created circa 20-04-26
##
## Copyright (C) 2020-2021 Nat Goodman.
## 
## Plot cvdat objects
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Plot cvdat objects ----
## plot data from one or more cvdat objects
## places are either 'state' or counties
## for doh object, ages are age groups
## can plot per.capita results
## incompatible.ok controls whether edited objects must be compatible
##   (places, ages of interest edited the same way)
## col,lty, lwd are the usual R line properties
##   code can automatically compute suitable values from the input data
## col1, lty1, lwd1 used as defauls in auto-compute
## TODO: MORE COMMENTS!!
## ylab is label for y-axis. default: computed from object
## title is title. default: computed from object and places
## cex.title is what R calls cex.main. if 'auto' or NULL, use auto-scaling
## legend, where.legend tell whether and where to draw legend
## title.legend,labels.legend,cex.legend are title, text labels, and cex for legend
## xgrid, xformat, xaxis control appearance of x-axis.
##   defaults good for plots covering full date range circa Sep 2021. found by trial-and-error
##   xgrid is spacing of x grid lines and x-axis annotations - keyword or number of days
## vline,hline are vectors of x or y positions for extra vertical or horizontal lines
## vhlty, vhcol, vhlwd are lty, col, lwd for these extra lines
## vlab, hlab contol writing vline, hline values along axes
## vhdigits is number of digits for these values (only used for horizontal (y-axis) values)
plot_cvdat=
  function(objs,places='state',ages=NULL,per.capita=FALSE,per.mort=FALSE,
           incompatible.ok=param(incompatible.ok),
           type='l',add=FALSE,xlim=NULL,xmin=NULL,xmax=NULL,ylim=NULL,ymin=NULL,ymax=NULL,
           col=NULL,lty=NULL,lwd=NULL,
           col1='black',lty1='solid',lwd1=2,
           col.pal='d3',lty.range=c(1,6),lwd.range=c(1,3),
           col.blocks=1,lty.blocks=2,lwd.blocks=2:3,
           col.legend=col1,lty.legend=lty1,lwd.legend=lwd1,
           pch=20,ylab=NULL,title=NULL,cex.title=NA,
           legend=TRUE,where.legend='topleft',cex.legend=0.8,legends=list(),
           xgrid=cq(monthly,biweekly,weekly,quadweekly,semimonthly),xformat='%b',cex.axis=0.75,
           vline=NULL,hline=NULL,vhlty='dashed',vhcol='grey50',
           vhlwd=1,vlab=TRUE,hlab=TRUE,vhdigits=2,
           attrs=cq(unit,cumulative,what,datasrc,version,id,fit,roll,extra,edit),
           attrs.ylab=cq(what),
           attrs.legend=attrs,
           blocks.order=cq(obj,place,age,objs,places,ages),
           legend.order=NULL) {
    if (is_cvdat(objs)) objs=list(objs);
    places.all=Reduce(intersect,lapply(objs,function(obj) places(obj)));
    ages.all=Reduce(intersect,lapply(objs,function(obj) ages(obj)));
    if (length(places.all)==0) stop("No valid places for these objects. Nothing to plot");
    if (length(ages.all)==0) stop("No valid ages for these objects. Nothing to plot");
    if (is.null(places)) places=places.all
    else {
      bad=places %-% places.all;
      if (length(bad)>0)
        stop("Invalid places: ",paste(collapse=', ',bad),
             ".\nValid places for these objects are: ",paste(collapse=', ',places.all));
    }
    if (is.null(ages)) ages=ages.all
    else {
      bad=ages %-% ages.all;
      if (length(bad)>0)
        stop("Invalid ages: ",paste(collapse=', ',bad),
             ".\nValid ages for these objects are: ",paste(collapse=', ',ages.all));
    }
    if (per.capita&&per.mort) stop ("Cannot specify both 'per.capita' and 'per.mort'");
    series=data_series(objs,places,ages,incompatible.ok,attrs);
    if (per.capita) series=series_percap(series);
    if (per.mort) series=series_permort(series);
    ct=ct_attrs(series,attrs);
    blocks=series_blocks(series,ct,blocks.order,attrs);
    
    lprop=auto_lprop(series,blocks,ct,col=col,lty=lty,lwd=lwd,
                     col1=col1,lty1=lty1,lwd1=lwd1,
                     col.pal=col.pal,lty.range=lty.range,lwd.range=lwd.range,
                     col.blocks=col.blocks,lty.blocks=lty.blocks,lwd.blocks=lwd.blocks,
                     blocks.order=blocks.order,attrs=attrs);
    if (!add) {
      ## new plot. do everything
      if (is.null(xlim)) xlim=auto_xlim(series,xlim,xmin,xmax);
      if (is.null(ylim)) ylim=auto_ylim(series,ylim,ymin,ymax);
      if (is.null(ylab)) ylab=auto_ylab(series,attrs.ylab,per.capita,per.mort);
      if (is.null(title)) title=auto_title(series,ct,attrs,per.capita,per.mort);
      if (is.na(cex.title)) cex.title=cex_title(title);
      ## setup new plot
      plot(x=xlim,y=ylim,type='n',xaxt='n',xlab=NA,ylab=ylab,main=title,cex.main=cex.title);
    }
    ## add series to existing plot
    col=lprop$col;
    lty=lprop$lty;
    lwd=lprop$lwd;
    data=series$series;
    sapply(seq_along(data),function(i)
      if (type=='l') lines(x=data[[i]]$date,y=data[[i]]$y,col=col[i],lty=lty[i],lwd=lwd[i])
      else if (type=='p') points(x=data[[i]]$date,y=data[[i]]$y,col=col[i],pch=pch[i],lwd=lwd[i]));
    if (!add) {
      ## draw grid
      grid(nx=NA,ny=NULL) # draw y grid lines. we'll draw x ourselves at desired spacing
      if (is.numeric(xgrid)) xgrid=seq(xlim[1],xlim[2],xgrid)
      else {
        xgrid=match.arg(xgrid);
        if (xgrid=='monthly'||xgrid=='semimonthly') {
          mon.after=inc_month(xlim[2],1);
          monthly=seq(mon_day(xlim[1],1),mon_day(mon.after,1),by='1 month');
          if (xgrid=='semimonthly') {
            mon.before=inc_month(xlim[1],-1);
            mon.15=seq(mon_day(mon.before,15),mon_day(mon.after,15),by='1 month');
            semimonthly=sort(c(monthly,mon.15));
          }}
        xgrid=switch(xgrid,weekly=seq(xlim[1],xlim[2],7),
                     biweekly=seq(xlim[1],xlim[2],14),
                     quadweekly=seq(xlim[1],xlim[2],28),
                     semimonthly=semimonthly,
                     monthly=monthly,
                     stop('Bad news: unknown xgrid=',xgrid,'. Should have been caught earlier'));
      }
      abline(v=xgrid,col="lightgray",lty="dotted");
      ## abline(v=biweek,col="lightgray",lty="dotted");
      ## abline(v=c(mon.01,mon.15),col="lightgray",lty="dotted");
      ## axis line below adapted from stackoverflow.com/questions/4843969. Thx!
      ## axis(1,mon.01,format(mon.01,"%b-%d"),cex.axis=cex.axis);
      ## axis(1,mon.01,format(mon.01,xformat),cex.axis=cex.axis);
      ## axis(1,xgrid,format(xgrid,xformat),cex.axis=cex.axis);
      ## this code emits every other label
      axis(1,xgrid,labels=FALSE,cex.axis=cex.axis)
      i=seq(1,length(xgrid),by=2)
      axis(1,xgrid[i],format(xgrid[i],xformat),cex.axis=cex.axis,tick=F)
      
      ## plot extra lines & values if desired. nop if vline, hline NULL
      vhline(vline=vline,hline=hline,vlab=vlab,hlab=hlab,vhdigits=vhdigits,
             lty=vhlty,col=vhcol,lwd=vhlwd);
      ## draw legend if desired
      ## legend is location or T/F or manually created legend list
      if (is.null(legend)||(is.logical(legend)&&legend)||is.character(legend)) {
        if (is.character(legend)) where.legend=legend;
        legend=auto_legend(blocks=blocks,lprop=lprop,legend.order=legend.order,legends=legends,
                           blocks.order=blocks.order,attrs=attrs.legend,
                           col=col.legend,lty=lty.legend,lwd=lwd.legend);
     } else if (is.logical(legend)&&!legend) legend=list();
      if (length(legend)>0) add_legend(legend,where=where.legend,cex=cex.legend);
    }
  }

auto_title=
  function(series,ct=NULL,attrs=cq(unit,cumulative,what,datasrc,version,id,fit,roll,extra),
           per.capita=FALSE,per.mort=FALSE,SEP='&') {
    if (is.null(ct)) ct=ct_attrs(series,attrs);
    ## attrs=ct$sv.attrs[ct$sv.attrs!='obj'];    # don't want 'obj' in title
    attrs=ct$sv.attrs %-% cq(obj,series);     # don't want 'obj' or 'series' in title
    xattr=unique(series$xattr[,attrs]);
    if (('cumulative'%in%attrs)&&xattr$cumulative) xattr$unit=FALSE; # unit doesn't matter for cum
    vals=sapply(attrs,function(attr) name_label(attr,xattr[[attr]],fmt='title'),simplify=FALSE);
    if (per.capita&&'what'%in%attrs) vals$what=paste(vals$what,'per Million');
    if (per.mort&&'what'%in%attrs) vals$what=paste(vals$what,'Relative to Expected Mortality');
    terms=unlist(sapply(attrs,function(attr) paste_title(attr,vals[[attr]],SEP=SEP)));
    paste(collapse=' ',terms);
  }

auto_lprop=
  function(series,blocks=NULL,ct=NULL,
           col=NULL,lty=NULL,lwd=NULL,col1='black',lty1='solid',lwd1=2,
           col.pal='d3',lty.range=c(1,6),lwd.range=c(1,3),
           col.blocks=1,lty.blocks=1:2,lwd.blocks=2:3,
           blocks.order=cq(obj,place,age,objs,places,ages),
           attrs=cq(unit,cumulative,what,datasrc,version,fit,roll,extra)) {
    if (is.null(blocks)) {
    if (is.null(ct)) ct=ct_attrs(series,attrs);
    blocks=series_blocks(series,ct,blocks.order);
    }
    nseries=length(series$series);
    col=auto_col(blocks,col.blocks,nseries,col,col.pal,col1);
    lty=auto_lty(blocks,lty.blocks,nseries,lty,lty.range,lty1);
    lwd=auto_lwd(blocks,lwd.blocks,nseries,lwd,lwd.range,lwd1);
    cl(col,lty,lwd);
  }
auto_col=function(blocks,i,nseries,col,pal,col1) {
  if (!is.null(col)) return(list(col=rep(col,length=nseries)));
  nblocks=length(blocks);
  if (nblocks<min(i)) return(list(col=rep(col1,length=nseries),col.block=NULL));
  i=min(i);
  name=names(blocks)[i];
  blocks=blocks[[i]]$blocks;
  col.block=col_brew(names(blocks),pal);
  col=NULL;
  sapply(names(blocks),function(name) {
    block=blocks[[name]];
    col[block]<<-col.block[name];})
  list(col=col,col.block=col.block,col.name=name);
}
auto_lty=function(blocks,i,nseries,lty,lty.range,lty1) {
  if (!is.null(lty)) return(list(lty=rep(lty,length=nseries)));
  nblocks=length(blocks);
  if (nblocks<min(i)) return(list(lty=rep(lty1,length=nseries),lty.block=NULL));
  if (max(lty.range)>6) stop("'lty.range' must be <= 6, not ",max(lty.range),
                             ": extended lty not yet implemented. Sorry");
  i=if(nblocks>=max(i)) max(i) else nblocks;
  name=names(blocks)[i];
  blocks=blocks[[i]]$blocks;
  ## set lty.range to sequence, then extend (or shrink) to number of blocks
  if (length(lty.range)==2) lty.range=seq(lty.range[1],lty.range[2],by=1);
  lty.range=rep(lty.range,length=length(blocks));
  lty.block=setNames(lty.range,names(blocks));
  lty=NULL;
  sapply(names(blocks),function(name) {
    block=blocks[[name]];
    lty[block]<<-lty.block[name];})
  list(lty=lty,lty.block=lty.block,lty.name=name);
}
auto_lwd=function(blocks,i,nseries,lwd,lwd.range,lwd1) {
  if (!is.null(lwd)) return(list(lwd=rep(lwd,length=nseries)));
  nblocks=length(blocks);
  if (nblocks<min(i)) return(list(lwd=rep(lwd1,length=nseries)));
  i=if(nblocks>=max(i)) max(i) else nblocks;
  name=names(blocks)[i];
  blocks=blocks[[i]]$blocks;
  ## set lwd.range to sequence of length number of blocks
  ## or extend (or shrink) to number of blocks
  if (length(lwd.range)==2) lwd.range=seq(lwd.range[1],lwd.range[2],length=length(blocks))
  else lwd.range=rep(lwd.range,length=length(blocks));
  lwd.block=setNames(lwd.range,names(blocks));
  lwd=NULL;
  sapply(names(blocks),function(name) {
    block=blocks[[name]];
    lwd[block]<<-lwd.block[name];})
  list(lwd=lwd,lwd.block=lwd.block,lwd.name=name);
}
## set xlim from series or xmin, xmax

auto_xlim=function(series,xlim=NULL,xmin=NULL,xmax=NULL) {
  ## do it this way so R won't convert dates to ints. sigh...
  if (is.null(xlim)) {
    if (!is.null(xmin)&&!is.null(xmax)) xlim=c(xmin,xmax)
    else {
      xlim=range(do.call(c,lapply(series$series,function(series) {
        r=range(series$date); r[is.finite(r)]})));
      if (is.null(xlim)) stop('All data series are empty. Nothing to plot')
      if (!is.null(xmin)) xlim[1]=xmin;
      if (!is.null(xmax)) xlim[2]=xmax;
    }}
  as_date(xlim);
}
auto_ylim=function(series,ylim=NULL,ymin=NULL,ymax=NULL) {
  if (is.null(ylim)) {
    if (!is.null(ymin)&&!is.null(ymax)) ylim=c(ymin,ymax)
    else {
      ylim=range(do.call(c,lapply(series$series,function(series) {
        y=series$y;
        y=y[!is.na(y)];
        if (length(y)==0) NULL else range(y);
      })));
      ## r=range(series$y); r[is.finite(r)]})));
      if (is.null(ylim)) stop('All data series are empty. Nothing to plot')
      if (!is.null(ymin)) ylim[1]=ymin;
      if (!is.null(ymax)) ylim[2]=ymax;
    }}
 ylim;
}
## NG 21-08-26: simplify ylab to just 'what' and per.capita,per.mort
auto_ylab=function(series,attrs=cq(what),per.capita=FALSE,per.mort=FALSE,SEP='&') {
  xattr=series$xattr;
  attrs=attrs%&%colnames(xattr);
  xattr=unique(xattr[,attrs,drop=FALSE]);
  ## if (all(xattr$cumulative)) xattr$unit=FALSE; # unit doesn't matter for cum
  vals=sapply(attrs,function(attr) name_label(attr,xattr[[attr]],fmt='ylab'),simplify=FALSE);
  if (per.capita) vals$what=paste(vals$what,'per million');
  if (per.mort) vals$what=paste(vals$what,'relative to mortality');
  terms=unlist(sapply(attrs,function(attr) paste_label(vals[[attr]],SEP=SEP)));
  paste(collapse=' ',terms);
}

## there can be up to 3 legend blocks
## with the caution that graphs with more than 2 blocks may be inscrutable
## block names can include final 's' - seems a likely typo otherwise
## 'obj' is shorthand for all object attrs
## for now, object attrs are all-or-none.
## TODO: use specific object attrs for block, eg, 'datasrc'
##       this means there could be left over attributes to be used in a final block
auto_legend=
  function(series=NULL,blocks=NULL,ct=NULL,lprop,legend.order=NULL,legends=list(),
           blocks.order=cq(obj,place,age,objs,places,ages),
           attrs=cq(unit,cumulative,what,datasrc,version,fit,roll,extra),
           col='black',lty='solid',lwd=2,SEP=', ') {
    if (length(legends)!=0&&!is_list(legends[[1]])) legends=list(legends);
    if (is.null(blocks)) {
      if (is.null(ct)) ct=ct_attrs(series,attrs);
      blocks=series_blocks(series,ct,blocks.order);
    }
    if (is.null(lprop$col.block)) col=lprop$col;
    if (is.null(lprop$lty.block)) lty=lprop$lty;
    if (is.null(lprop$lwd.block)) lwd=lprop$lwd;
    if (length(blocks)==0) {
      ## use legends as given with lprop's added
      default=list(col=col,lty=lty,lwd=lwd);
      legend=lapply(legends,function(legend) fill_defaults(default,legend));
      return(legend);
    }
    if (is.null(legend.order)) legend.order=names(blocks);
    if (length(legends)!=0) names(legends)=legend.order;
    ## legend=lapply(blocks[legend.order],function(block) {
    legend=lapply(legend.order,function(name) {
      block=blocks[[name]];
      xattr=block$xattr;
      attrs=colnames(xattr);
      legend=legends[[name]];
      title=legend$title;
      if (is.null(title)) {
        terms=sapply(attrs,function(attr) ltitle_label(attr),simplify=FALSE);
        title=paste(collapse=SEP,unlist(terms));
      }
      labels=legend$labels;
      if (is.null(labels)) labels=unlist(withrows(xattr,row,paste_legend(row)));
      col=if(identical(lprop$col.name,name)) lprop$col.block else col;
      lty=if(identical(lprop$lty.name,name)) lprop$lty.block else lty;
      lwd=if(identical(lprop$lwd.name,name)) lprop$lwd.block else lwd;
      list(title=title,legend=labels,xattr=xattr,col=col,lty=lty,lwd=lwd);
    });
    names(legend)=legend.order;
    legend[!sapply(legend,is.null)];
  }
