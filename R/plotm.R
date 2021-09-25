#################################################################################
##
## Author:  Nat Goodman
## Created: created 20-05-06
##          from covid.R created circa 20-04-26
##
## Copyright (C) 2019 Nat Goodman.
## 
## Wrappers for matplot. inspired by misig/plotm and related
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## plot multiple lines vs dates - my adaptation of matplot - adapted from misig/plotm
## x is vector or 2-dimensional matrix-like object of dates with same number of columns as y
## y is vector or 2-dimensional matrix-like object
##   like matplot, each line is column of x or y
## if y NULL, 1st column of x used for date, rest for y
## col, lty, lwd are the usual line properties
## title, cex.title are title and cex for title
## type is plot type - passed to matplot. 'n' also turns off extra lines, legend, grid
##   TODO: type='p' should cause legend to draw points instead of lines
## vline,hline are vectors of x or y positions for extra vertical or horizontal lines
## vhlty, vhcol, vhlwd are lty, col, lwd for these extra lines
## vlab, hlab contol writing vline, hline values along axes
## vhdigits is number of digits for these values
## xgrid, xformat, xyear control appearance of x-axis.
##   defaults good for plots covering full date range circa Sep 2021. found by trial-and-error
##   xgrid is spacing of x grid lines and x-axis annotations - keyword or number of days
##   xyear controls writing year on line below axis
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
  function(x=NULL,y=NULL,title='',cex.title='auto',type='l',add=FALSE,
           col=NULL,lty=NULL,lwd=NULL,ylab='count',xlim=NULL,ylim=NULL,
           col.pal='d3',lty.range=c(1,6),lwd.range=c(1,3),
           xgrid=cq(monthly,biweekly,weekly,quadweekly,semimonthly),xformat='%b',xyear=TRUE,
           cex.axis=0.75,
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
    if (is.null(y)) {
      y=x[,-1,drop=FALSE];
      x=x[,1,drop=FALSE];
    }
    x=as_date(x);
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
      xrange=range(x);
      yrange=range(c(0,y),na.rm=TRUE);
      plot(x=xrange,y=yrange,type='n',xaxt='n',xlab=NA,ylab=ylab,xlim=xlim,ylim=ylim,
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
    if (type!='n') {
      ## plot extra lines & values if desired. nop if vline, hline NULL
      vhline(vline=vline,hline=hline,vlab=vlab,hlab=hlab,vhdigits=vhdigits,
             lty=vhlty,col=vhcol,lwd=vhlwd);
    }
    if (!add&&type!='n') {
      ## draw grid, x-axis labels
      xstyle(as_date(xrange),xgrid=xgrid,xformat=xformat,xyear=xyear,cex=cex.axis);
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

