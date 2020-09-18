#################################################################################
##
## Author:  Nat Goodman
## Created: 20-05-06
##          from clapi/R/plot_util.R created 20-03-22
##          from plot_nudge.R created 20-03-19
##          from misisg/R/plot.R created 19-01-09
##          uses code from repwr/R/plot.R created 18-05-03
##
## Copyright (C) 2019 Nat Goodman.
## 
## Plot utility functions
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
library(RColorBrewer);
## ---- Plot Utility Functions ----
## auto-scale title
cex_title=function(title) {
  xwidth=par('pin')[1];                 # width of plot region in inches
  xyplt=par('plt');                     # dimensions of plot region as fraction of plot
  xplt=xyplt[2]-xyplt[1];               # width of plot region as fraction of plot
  min(1,xplt*xwidth/strwidth(title,units='inches'));
}

## draw one or more legends. adapted from misig/multi_legend, repwr/ragm_legend
## legends is list of legend.args - arguments to my legend wrapper
##   or equivalently base R legend
## where, x, y are starting position
##   where can be keyword (eg, 'right'), coordinate (used as x), or vector of x, y
##   vector is typically from previous call
## others used as defaults in each legend
add_legend=
  function(legends,where=NULL,x=NULL,y=NULL,cex=0.8,bty='n',
           title=NULL,title.col='black',
           col='black',lty='solid',lwd=1,labels=NULL,legend=labels,...) {
    if (length(where)==1) x=where;
    if (length(where)==2) {x=where[1]; y=where[2];}
    default.args=
      list(cex=cex,bty=bty,title=title,title.col=title.col,col=col,lty=lty,lwd=lwd,
           ...);
    if (missing(legends)) do.call(legend_,c(list(x=x,y=y,legend=legend),default.args))
    else {
      want.args=funargs(legend_,graphics::legend);
      sapply(legends,function(legend.args) {
        if (is.null(legend.args)) return();
        legend.args=fill_defaults(default.args,c(list(x=x,y=y),legend.args));
        legend.args=legend.args[names(legend.args) %in% want.args];
        where.next=do.call(legend_,legend.args);
        ## <<- assigns to variables in outer scope, ie, function scope
        ##   from stackoverflow.com/questions/13640157. Thanks!
        ## could also use, eg, assign('x',where.next[1],envir=parent.frame(n=3))
        x<<-where.next[1];
        y<<-where.next[2];
      })}
    ## invisible();
    c(x,y);
  }
## draw one legend. wrapper for base R legend 
## adapted from misig/plotm_legend, repwr/mesr_legend
## labels and legend are synonyms
legend_=
  function(x,y,cex=0.8,bty='n',title=NULL,title.col='black',
           col='black',lty='solid',lwd=1,labels=NULL,legend=labels,seg.len=4,...) {
    if (is.null(legend)) return();      # nothing to draw
    where.next=graphics::legend(x,y,bty=bty,legend=legend,cex=cex,col=col,lty=lty,lwd=lwd,
                                seg.len=seg.len,title=title,title.col=title.col,...);
    x=where.next$rect$left;
    y=where.next$rect$top-where.next$rect$h;
    c(x,y);
  }
## col_brew moved to pal.R

## empty plot - just title & axes
plotempty=
  function(title='',cex.title='auto',xlab='x',ylab='y',xlim=c(0,1),ylim=c(0,1),
           xaxp=c(xlim,1),yaxp=c(ylim,1),...) {
    if (is.null(cex.title)|cex.title=='auto') cex.title=cex_title(title);
    plot(x=NULL,y=NULL,type='n',main=title,cex.main=cex.title,xlab=xlab,ylab=ylab,
         xlim=xlim,ylim=ylim,xaxp=xaxp,yaxp=yaxp,...);
    xylim=par('usr');                   # limits of disply region
    xmid=mean(xylim[1:2]);
    ymid=mean(xylim[3:4]);
    text(xmid,ymid,'PLOT DELIBERATELY LEFT BLANK',adj=c(0.5,0.5));
    invisible();
}
## helper functions to plot horizontal and vertical line segments
vhline=function(vline=NULL,hline=NULL,vlab=TRUE,hlab=TRUE,vhdigits=2,col=NA,cex=0.75,...) {
  xylim=par('usr');
  if (!is.null(vline)) {
    vline=as_date(vline);
    vline=vline[which(between(vline,as_date(xylim[1]),as_date(xylim[2])))];
  }
  if (!is.null(hline)) hline=hline[which(between(hline,xylim[3],xylim[4]))];
  abline(v=vline,h=hline,col=col,...);
  ## write vhline values along axes
  vline=vline[vlab];
  if (length(vline)>0)
    mtext(format(vline,"%b-%d"),side=1,at=vline,col=col,line=0.25,cex=cex*par('cex'));
  hline=hline[hlab];
  if (length(hline)>0)
    mtext(round(hline,vhdigits),side=2,at=hline,col=col,line=0.25,cex=cex*par('cex'));
}
hseg=
  function(y,x0=0,x,col='black',lty='solid',lwd=1,cex=0.75,text=NULL,
           label=list(text=text,side=2,at=y,col=col,line=0.25,cex=cex*par('cex'),las=1)) {
    segments(x0=x0,x1=x,y0=y,y1=y,col=col,lty=lty,lwd=lwd);
    if (!is.null(text)) do.call(mtext,label);
  }
hseg=
  function(x,y0=0,y,col='black',lty='solid',lwd=1,cex=0.75,text=NULL,
           label=list(text=text,side=1,at=x,col=col,line=0.25,cex=cex*par('cex'),las=1)) {
    segments(x0=x,x1=x,y0=y0,y1=y,col=col,lty=lty,lwd=lwd);
    if (!is.null(text)) do.call(mtext,label);
  }

## display color palette - from util.R
pal=function(col,border="light gray",...) {
 n=length(col)
 plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),axes=FALSE,xlab="",ylab="",...)
 rect(0:(n-1)/n,0,1:n/n,1,col=col,border=border)
}
