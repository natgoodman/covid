#################################################################################
##
## Author:  Nat Goodman
## Created: 21-09-25
##          from xper_xaxis.R created 21-09-20
##
## Copyright (C) 2021 Nat Goodman.
##
## Functions used in multiple top-level plots (ploc_cvdat, plotm)
## Presently just code to draw x-axis and related in plot_cvdat and plotm.
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
###################################################################################
## draw x grid and axis
xstyle=
  function(xlim,
           xgrid=cq(monthly,biweekly,weekly,quadweekly,semimonthly),xformat='%b',xyear=TRUE,
           cex=0.75,label.start=1,label.skip=2) {
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
      xgrid=switch(xgrid,
                       weekly=seq(xlim[1],xlim[2],7),
                       biweekly=seq(xlim[1],xlim[2],14),
                       quadweekly=seq(xlim[1],xlim[2],28),
                       semimonthly=semimonthly,
                       monthly=monthly,
                       stop('Bad news: unknown xgrid=',xgrid,'. Should have been caught earlier'));
    }
    ## draw grid lines
    abline(v=xgrid,col="lightgray",lty="dotted");
    ## draw x-axis ticks lined up to grid without labels
    axis(1,xgrid,labels=FALSE,cex.axis=cex);
    ## draw labels at desired tick
    i=seq(label.start,length(xgrid),by=label.skip);
    labels=format(xgrid[i],xformat);
    axis(1,xgrid[i],labels,cex.axis=cex,tick=F);
    if (xyear) xyear_(cex=cex);
    labels;
  }
## draw year annotations below main x-axis
xyear_=function(cex=0.75,line=3,lty='dotted',lwd=1,col='grey50') {
  xlim=as_date(par('usr')[1:2]);
  yr=unique(years(xlim));
  if (length(yr)>1) {
    yr=yr[1]:yr[2];
    yr.01=as_date(paste0(yr[-1],'-01-01'));
    xyear_tick_(yr.01,line=line);               # draw year ticks without labels
    bound=as.numeric(c(xlim[1],yr.01,xlim[2])); # bounds (endpoints) of year regions
  } else {
   bound=as.numeric(xlim);             # 1 year. region is entire plot
  }
  yline=line2user(line=line,side=1);   # y coordinate of year line
  wtext=strwidth(yr[1],cex=cex);       # width of year text (all same)
  wspace=strwidth(' ',cex=cex)/2;      # width of additional 1/2 space
  ## do it! annotate each region
  sapply(seq_along(yr),function(i) {
    x0=bound[i]; x1=bound[i+1];
    w=x1-x0;
    yr=yr[i];
    if (w>wtext) {
      ## there's room to write year text!
      xmid=(x0+x1)/2;
      text(xmid,yline,yr,cex=cex,xpd=TRUE);
      xline=c(x0,xmid-wtext/2-wspace,xmid+wtext/2+wspace,x1);
      x0=xline[c(1,3)];
      x1=xline[c(2,4)];
      segments(x0,yline,x1,yline,lty=lty,lwd=lwd,col=col,xpd=T);
    } else {
      ## just draw line
      segments(x0,yline,x1,yline,lty=lty,lwd=lwd,col=col,xpd=T);
    }
  });
  yr;
}
## draw year ticks without labels
xyear_tick_=function(at,line=3,height=0.5,lty='solid',lwd=1,col='black') {
  top=line2user(line-height/2,side=1);
  bot=line2user(line+height/2,side=1);
  segments(at,top,at,bot,lty=lty,lwd=lwd,col=col,xpd=T);
}
