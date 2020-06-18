#################################################################################
##
## Author:  Nat Goodman
## Created: created 20-05-06
##          from covid.R created circa 20-04-26
##
## Copyright (C) 2019 Nat Goodman.
## 
## Plot utility functions
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## wrappers for matplot. inspired by misig/plotm and related
## plot one or more counties from standard pseudo-object data
## col is names of one or more RColorBrewer palettes or vctor of colors
plotm_obj=
  function(obj,names=colnames(obj$data)[-1],type='l',add=FALSE,
           xlim=NULL,xmin=NULL,xmax=NULL,ylim=NULL,
           lty='solid',lwd=1,pch=20,col='Dark2',ylab=NULL,title=NULL,cex.title=NA,
           legend='topleft',title.legend=NULL,labels.legend=NULL,cex.legend=0.8) {
    if (is.null(ylab)) ylab=ylab_obj(obj);
    if (is.null(title)) title=title_obj(obj);
    if (is.na(cex.title)) cex.title=cex_title(title);
    data=obj$data;
    x=data[,1];
    names=intersect(names,colnames(data)[-1]);
    y=data[,names,drop=FALSE];
    if (all(col %in% rownames(brewer.pal.info))) col=col_brew(col,n=length(names));
    if (!add) {
      ## new plot. do everything
      xlim=xlim_obj(obj,xlim,xmin,xmax);
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
          if (is.null(labels.legend)) labels.legend=names
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
## plot one or more counties from one or more standard pseudo-objects
## adapted from misig/plotmg which was adapted from repwr/plot_ragm
## ... are objects to plot
## names are counties to be plotted
## lty, lwd, pch are per-object
## note that col is name of RColorBrewer palette
plotm_objs=
  function(objs,names=NULL,type='l',add=FALSE,xlim=NULL,xmin=NULL,xmax=NULL,ylim=NULL,
           lty='solid',lwd=1,pch=20,col='Dark2',ylab=NULL,title=NULL,cex.title=NA,
           legend='topleft',title.legend='Place',legends=list(),cex.legend=0.8) {
    names.all=Reduce(intersect,lapply(objs,function(obj) colnames(obj$data)[-1]));
    names=if(is.null(names)) names.all else intersect(names,names.all);
    if (length(names)==0) stop("No names. Nothing to plot");
    plot1=length(names)==1;
    tl=title_legend(objs);     # split obj attrs by single vs multi valued
    if (is.null(ylab)) ylab=ylab_objs(objs);
    if (is.null(title)) {
      title=title_objs(tl$title);
      if (plot1) title=paste(title,'for',names[1]);
    }
    if (is.na(cex.title)) cex.title=cex_title(title);
    ## if only one name, use colors for objs, else for places
    ncols=if(plot1) length(objs) else length(names);
    col=if(all(col %in% rownames(brewer.pal.info))) col_brew(col,n=ncols);
    ## extend line properties to number of objs
    repv(lty,lwd,pch,LENGTH.OUT=length(objs));
    if (!add) {
      ## setup new plot
      xlim=xlim_objs(objs,xlim,xmin,xmax);
      if (is.null(ylim)) ylim=range(sapply(objs,function(obj) range(obj$data[,-1])));
      matplot(x=xlim,y=ylim,type='n',xaxt='n',xlab=NA,ylab=ylab,main=title,cex.main=cex.title);
    }
    ## add objs to existing plot
    sapply(seq_along(objs),function(i) {
      obj=objs[[i]]; lty=lty[i]; lwd=lwd[i]; pch=pch[i];
      if (plot1) col=col[i];
      plotm_obj(obj,names,type=type,add=TRUE,lty=lty,lwd=lwd,pch=pch,col=col);
    })
    if (!add) {
      grid(nx=NA,ny=NULL) # draw y grid lines. we'll draw x ourselves at first day of month
      days=seq(xlim[1],xlim[2],1);
      mon.01=mon_day(days,1);
      mon.15=mon_day(days,15);
      abline(v=c(mon.01,mon.15),col="lightgray",lty="dotted");
      ## axis line below adapted from stackoverflow.com/questions/4843969. Thx!
      axis(1,mon.01,format(mon.01,"%b-%d"),cex.axis=0.8)
      ## draw legend if desired. legend is location or T/F
      if (is.null(legend)) legend=FALSE
      else if (is.logical(legend)&&legend) where.legend='topleft'
      else if (!is.logical(legend)) {
        where.legend=legend;
        legend=TRUE;
      }
      if (legend) {
        ## construct list of legend args
        ## there are 2 legend blocks when more than 1 name
        ##   1) depicts counties (names). uses col
        ##   2) depicts objs. uses other line properties
        ## skip block #1 when only one name and use colors in block #2
        ## legends, if provided, overrides it all
        if (length(legends)==0) {
          legend1=list(title=title.legend,labels=names,lty=lty[1],lwd=lwd[1],col=col);
          col2=if(plot1) col else rep('black',length(objs));
          legend2=list(title=ltitle_objs(tl$legend),labels=legend_objs(tl$legend),
                       lty=lty,lwd=lwd,col=col2);
          legends=if(plot1) list(legend2) else list(legend1,legend2);
        }
        add_legend(legends,where=where.legend,cex=cex.legend);
      }}
    invisible()
  }
## TODO: plot cases and deaths together

## compute and plot lagged correlation
plot_lag=
  function(obj1,obj2,name='state',lag.max=28,title=NULL,cex.title=NA,
           xlab=NULL,ylab='correlation') {
    lag=lag(obj1,obj2,name,lag.max);
    ## use short title, since plot.acf seems to ignore cex.main
    ## if (is.null(title)) title=paste('Lagged Correlation of',title_objs(list(obj1,obj2)))
    if (is.null(title)) title=paste('Lagged Correlation of',
                                    title_objs(list(obj1,obj2),names=cq(what,datasrc)));
    if (is.na(cex.title)) cex.title=cex_title(title);
    if (is.null(xlab)) {
      unit=obj1$unit;                     # note lag function checks unit1==unit2
      xlab=paste0('lag (in ',if(unit==1) 'days' else 'weeks',')');
    }
    plot(lag,xlab=xlab,ylab=ylab,main=title,cex.main=cex.title);
    invisible()
  }

## adapted from www.r-bloggers.com/dates-in-r-and-the-first-day-of-the-month. Thx!
mon_day=function(date,day) as.Date(paste(sep='-',unique(format(date,format="%Y-%m")),day))

## set xlim from objs or xmin, xmax
xlim_objs=function(objs,xlim=NULL,xmin=NULL,xmax=NULL) {
  ## do it this way so R won't convert dates to ints. sigh...
  if (is.null(xlim)) {
    if (!is.null(xmin)&&!is.null(xmax)) xlim=c(xmin,xmax)
    else {
      xlim=range(do.call(c,lapply(objs,function(obj) range(obj$data$date))));
      if (!is.null(xmin)) xlim[1]=xmin;
      if (!is.null(xmax)) xlim[2]=xmax;
    }}
  as.Date(xlim);
}
xlim_obj=function(obj,...) xlim_objs(list(obj),...)

## ylab, title, legend labels for plotm_objs
## objs can be list of objs or data frame from objs_attr
## ylab uses unit,cumulative,what
ylab_objs=function(objs,names=cq(unit,cumulative,what),SEP='&') {
  if (is_list(objs)) xattr=objs_attr(objs,names)
  else xattr=objs;
  names=intersect(names,colnames(xattr));
  terms=apply(xattr,2,function(labels) paste_label(labels,SEP=SEP));
  paste(collapse=' ',terms);
}
ylab_obj=function(obj,names=cq(unit,cumulative,what),SEP='&') {
  if (is_list(obj)) xattr=obj_attr(obj,names) else xattr=obj;
  ylab_objs(xattr,names=names,SEP=SEP);
}
## title usually uses names and data from title_legend
title_objs=function(objs,names=cq(unit,cumulative,what,datasrc,version,fit),SEP='&') {
  if (is_list(objs)) xattr=objs_attr(objs,names)
  else xattr=objs;
  names=intersect(names,colnames(xattr));
  terms=unlist(sapply(names,function(name) paste_title(name,xattr[[name]],SEP=SEP)));
  paste(collapse=' ',terms);
}
title_obj=function(obj,names=cq(unit,cumulative,what,datasrc,version,fit),SEP='&') {
  if (is_list(obj)) xattr=obj_attr(obj,names) else xattr=obj;
  title_objs(xattr,names=names,SEP=SEP);
}
## legend usually uses names and data from title_legend
legend_objs=function(objs,names=cq(datasrc,version,unit,cumulative,what,fit),SEP=', ') {
  if (is_list(objs)) xattr=objs_attr(objs,names)
  else xattr=objs;
  names=intersect(names,colnames(xattr));
  terms=apply(xattr,1,function(row) paste_legend(row[names],SEP=SEP))
  terms;
}
## legend title usually uses names from title_legend
ltitle_objs=function(objs,names=cq(datasrc,version,unit,cumulative,what,fit),SEP=' ') {
  if (is_list(objs)) xattr=objs_attr(objs,names)
  else xattr=objs; 
  names=intersect(names,colnames(xattr));
  terms=sapply(names,ltitle_label);
  paste(collapse=', ',terms);
}

## put single-valued obj params in title, multi-valued ones in legend
title_legend=function(objs,names=cq(datasrc,what,unit,cumulative,version,fit),SEP='&') {
  ## get data frame values from objs
  xattr=objs_attr(objs,names);
  counts=apply(xattr,2,function(x) if (all(is.na(x))) 0 else length(unique(x)));
  xtitle=xattr[,names[counts==1],drop=FALSE];
  xlegend=xattr[,names[counts>1],drop=FALSE];
  list(title=xtitle,legend=xlegend);
}
##### THIS ONE OBSOLETE
## plots case or death data from one data frame
## presently scale must be weekly
plotm_one=
  function(data,names=colnames(data)[-1],what=cq(cases,deaths),unit='weekly',
           ylab=NULL,title=NULL,cex.title=NA) {
    what=match.arg(what);
    if (is.null(ylab)) ylab=paste(t_label(unit),what);
    if (is.null(title)) title=tools::toTitleCase(ylab);
    if (is.na(cex.title)) cex.title=cex_title(title);
    x=data[,1];
    names=intersect(names,colnames(data)[-1]);
    y=data[,names];
    col=col_brew('Dark2',n=ncol(y));
    matplot(x,y,type='l',lty='solid',col=col,xlab=NA,ylab=ylab,xaxt='n',
            main=title,cex.main=cex.title);
    grid(nx=NA,ny=NULL) # draw y grid lines. we'll draw x ourselves at first day of month
    mon.01=mon_day(x,1)
    mon.15=mon_day(x,15)
    abline(v=c(mon.01,mon.15),col="lightgray",lty="dotted")
    ## axis line below adapted from stackoverflow.com/questions/4843969. Thx!
    axis(1,mon.01,format(mon.01,"%b-%d"),cex.axis=0.8)
    add_legend(where='topleft',col=col,labels=colnames(y),cex=0.7)
    invisible()
  }


