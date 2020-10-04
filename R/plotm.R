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

## plot multiple lines vs dates - my adaptation of matplot - adapted from misig/plotm
## x is vector or 2-dimensional matrix-like object of dates with same number of columns as y
## y is vector or 2-dimensional matrix-like object
##   like matplot, each line is column of x or y
## col, lty, lwd are the usual line properties
## title, cex.title are title and cex for title
## type is plot type - passed to matplot. 'n' also turns off extra lines, legend, grid
##   TODO: type='p' should cause legend to draw points instead of lines
## vline,hline are vectors of x or y positions for extra vertical or horizontal lines
## vhlty, vhcol, vhlwd are lty, col, lwd for these extra lines
## vlab, hlab contol writing vline, hline values along axes
## vhdigits is number of digits for these values
## smooth whether to smooth data to make plot prettier. uses same methods as 'fit' transform
##   aspline, ispline, sspline, spline, loess, linear, approx (sames as linear), step, none
##   default aspline
##   TRUE means aspline, FALSE means none
## smooth.args are args passed to smooth method.
##   note defaults for aspline, spline, loess
## legend tells whether and where to draw legend
##   TRUE or word like 'right' means draw legend, FALSE or NULL means no legend
## legend.title is legend title
## legend.args are further legend params
plotm=
  function(x,y,title='',cex.title='auto',type='l',add=FALSE,
           col=NULL,lty=NULL,lwd=NULL,xlab='date',ylab='y',xlim=NULL,ylim=NULL,
           col.pal='d3',lty.range=c(1,6),lwd.range=c(1,3),
           vline=NULL,hline=NULL,vhlty='dashed',vhcol='grey50',
           vhlwd=1,vlab=TRUE,hlab=TRUE,vhdigits=2,
           ## NG 20-01-02: redo 'smooth' logic again to set smooth.args default
           smooth=FALSE,
           smooth.args=list(args=list(),clamp=NULL,by=NULL,length.out=NULL,relative=TRUE),
           legend=if(is.vector(y)) FALSE else 'right',legend.title=NULL,
           legend.labels=if(is.vector(y)) NULL else colnames(y),
           legend.args=list(where=NULL,x=NULL,y=NULL,cex=0.8,
                            title=legend.title,labels=legend.labels,col=col,lty=lty,lwd=lwd),
           ...) {
    if (is.null(x)) stop("Nothing to plot: 'x' is NULL");
    if (is.null(y)) stop("Nothing to plot: 'y' is NULL");
    if (is_vector(x)) x=matrix(x)   # use is_vector to handle dates
    else if (length(dim(x))!=2) stop("'x' must be vector or 2-dimensional matrix-like object");
    if (is.vector(y)) y=matrix(y)
    else if (length(dim(y))!=2) stop("'y' must be vector or 2-dimensional matrix-like object");
    ##   like matplot, if x, y both have multiple columns, must be same number of columns
    if (ncol(x)>1&&ncol(y)>1&&ncol(x)!=ncol(y))
      stop("When 'x' and 'y' both have multiple columns, must have same number of columns");
    if (is.logical(smooth)) smooth=if(smooth) 'aspline' else 'none';
    if (smooth!='none') {
      with(smooth.args,{
        args=cl(
             switch(smooth,
                    aspline=list(method='improved'),
                    sspline=list(spar=NULL),
                    spline=list(method='monoH.FC'),
                    loess=list(span=0.75),
                    linear=list(method='linear',yleft=0,rule=2),
                    approx=list(method='linear',yleft=0,rule=2),
                    step=list(method='constant',yleft=0,rule=2),
                    list()),
          args);
        xy=smooth(x,y,method=smooth,args=args,
                  clamp=clamp,by=by,length.out=length.out,relative=relative,simplify='matrix')
        x=xy$x; y=xy$y;
      })}
    if (!add) {
      ## setup new plot
      if (is.null(cex.title)|cex.title=='auto') cex.title=cex_title(title);
      if (!is.null(xlim)) xlim=as_date(xlim);
      yrange=range(c(0,y),na.rm=TRUE);
      plot(x=range(x),y=yrange,type='n',xaxt='n',xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,
           main=title,cex.main=cex.title,...);
    }
    ## add columns to existing plot
    l=ncol(y);
    ## set col to RColorBrewer palette
    col=if(is.null(col)) col_brew(colnames(y),col.pal) else rep(col,length=l);
    ## set lty to lty.range then extend (or shrink) to l
    if (is.null(lty)) lty=seq(lty.range[1],lty.range[2],by=1);
    lty=rep(lty,length=l);
    lwd=if(is.null(lwd)) seq(lwd.range[1],lwd.range[2],length=l) else rep(lwd,length=l);
    ## compute 'mask' to remove NAs. code adapted from akima::aspline
    nax=apply(x,2,is.na);
    nay=apply(y,2,is.na);
    if (ncol(x)==1) nna=!(nax[,1]|nay) else nna=!(nax|nay);
    sapply(1:l,function(j) {
      nna=nna[,j];
      x=if(ncol(x)==1) x[nna,1] else x[nna,j];
      y=y[nna,j];
      if (type=='l') lines(x=x,y=y,col=col[j],lty=lty[j],lwd=lwd[j],...)
      else if (type=='p') points(x=x,y=y,col=col[j],lty=lty[j],lwd=lwd[j],...);
    });
    if (!add&&type!='n') {
      ## draw grid, extra lines, legend
      grid(nx=NA,ny=NULL) # draw y grid lines. we'll draw x ourselves at first day of month
      days=seq(min(x),max(x),1);
      mon.01=mon_day(days,1);
      mon.15=mon_day(days,15);
      abline(v=c(mon.01,mon.15),col="lightgray",lty="dotted");
      ## axis line below adapted from stackoverflow.com/questions/4843969. Thx!
      axis(1,mon.01,format(mon.01,"%b-%d"),cex.axis=0.8);
      ## plot extra lines & values if desired. nop if vline, hline NULL
      vhline(vline=vline,hline=hline,vlab=vlab,hlab=hlab,vhdigits=vhdigits,
             lty=vhlty,col=vhcol,lwd=vhlwd);
      ## draw legend if desired
      if (is.null(legend)) legend=FALSE
      else if (!is.logical(legend)) {
        legend.args$where=legend;
        legend=TRUE;
      }
      if (legend) do.call(plotm_legend,legend.args);
    }
  }
## wrapper for plotm that pulls values from data frame
## x (implicit) must be 'date'
## y column names
plotm_df=
  function(data,y=colnames(data)%-%'date',z,xlab='date',ylab=if(length(y)==1) y else 'y',...) {
    force(xlab); force(ylab);
    if (!is.data.frame(data)) stop ('data must be data frame');
    if ('date' %notin% colnames(data)) stop("'data' must contain 'date' column");
    if (!is_subset(y,colnames(data))) stop("'y' must contain column names");
    date=data$date;
    ydata=data[,y,drop=F];
    plotm(date,ydata,xlab=xlab,ylab=ylab,...);
  }
## draw plotm legend. adapted from repwr/mesr_legend
## labels and legend are synonyms
plotm_legend=
  function(where=NULL,x=NULL,y=NULL,cex=0.8,bty='n',
           title=NULL,title.col='black',
           col='black',lty='solid',lwd=1,labels=NULL,legend=labels,...) {
    if (is.null(legend)) return();      # nothing to draw
    if (is.null(x)) x=where;
    legend(x,y,bty=bty,legend=legend,cex=cex,col=col,lwd=lwd,lty=lty,
          title=title,title.col=title.col,...);
  }

## plot one or more places (tpyically, counties) from standard pseudo-object data
## for doh object
##  plot one or more age groups for one place or one age group for multiple places
##  can plot per.capita results (presently just 'state', 'King', 'Yakima'
## col is names of one or more RColorBrewer palettes or vector of colors
## lty, lwd are vectors of standard R line types and widths
## ylab is label for y-axis. default: computed from object
## title is title. default: computed from object and places
## cex.title is what R calls cex.main. if 'auto' or NULL, use auto-scaling
## legend, where.legend tell whether and where to draw legend
## title.legend,labels.legend,cex.legend are title, text labels, and cex for legend
## vline,hline are vectors of x or y positions for extra vertical or horizontal lines
## vhlty, vhcol, vhlwd are lty, col, lwd for these extra lines
## vlab, hlab contol writing vline, hline values along axes
## vhdigits is number of digits for these values (only used for horizontal (y-axis) values)
plotm_obj=
  function(obj,places='state',ages='all',plot.pct=FALSE,per.capita=FALSE,
           type='l',add=FALSE,xlim=NULL,xmin=NULL,xmax=NULL,ylim=NULL,
           lty='solid',lwd=1,pch=20,col='Dark2',ylab=NULL,title=NULL,cex.title=NA,
           legend=TRUE,where.legend='topleft',
           title.legend=NULL,labels.legend=NULL,cex.legend=0.8,
           vline=NULL,hline=NULL,vhlty='dashed',vhcol='grey50',
           vhlwd=1,vlab=TRUE,hlab=TRUE,vhdigits=2) {
    if (is.null(ylab)) ylab=ylab_obj(obj,plot.pct=plot.pct,per.capita=per.capita);
    tl=tl_obj(obj,places,ages);     # split obj attrs by single vs multi valued
    if (is.null(title)) title=title_obj(tl$title,plot.pct,per.capita);
    if (is.na(cex.title)) cex.title=cex_title(title);
    ## data=obj$data;
    data=data_obj(obj,places,ages,plot.pct,per.capita,pop=obj$pop)
    x=data[,1];
    places=places %&% colnames(data)[-1];
    y=data[,places,drop=FALSE];
    if (all(col %in% rownames(brewer.pal.info))) col=col_brew(n=length(places),col);
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
        if (!is.logical(legend)) {
          where.legend=legend;
          legend=TRUE;
        }
        if (legend) {
          if (is.null(labels.legend)) labels.legend=places
          add_legend(labels=labels.legend,where=where.legend,title=title.legend,
                     cex=cex.legend,col=col,lty=lty,lwd=lwd);
        }}
    } else {
      ## add to existing plot
      if (type=='l') matlines(x,y,lty=lty,lwd=lwd,col=col)
      else matpoints(x,y,lty=lty,lwd=lwd,col=col,pch=pch);
    }
    ## plot extra lines & values if desired. nop if vline, hline NULL
    vhline(vline=vline,hline=hline,vlab=vlab,hlab=hlab,vhdigits=vhdigits,
           lty=vhlty,col=vhcol,lwd=vhlwd);
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
    names=if(is.null(names)) names.all else names.all %&% names;
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
    col=if(all(col %in% rownames(brewer.pal.info))) col_brew(n=ncols,col);
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
    names=names %&% colnames(data)[-1];
    y=data[,names];
    col=col_brew(n=ncol(y),'Dark2');
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


