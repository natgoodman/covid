#################################################################################
##
## Author:  Nat Goodman
## Created: 20-01-14
##          from Projects/FreeCell/script/free_cell.R created ~July 2014
##
## Copyright (C) 2020 Nat Goodman.
## 
## Analyze first Free Cell dataset
## Estimate "hidden" scores. Compute and plot empirical distributions
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## -- Analyze Free Cell score data circa July 2014 --

doc_hiddn=function() {
  frecl=read_frecl(1);
  ## phase 1. estimate hidden values from observed scores
  num.hid=frecl[1,'total']-1;                   # number of hidden scores
  num.obs=nrow(frecl);                          # number of observed scores
  num.all=num.hid+num.frecl;                    # number of scores in complete dataset
  ## run 'btr' functions and tack onto data
  frecl$btr.all=btr_all();
  frecl$btr.obs=btr_obs();
  frecl$btr.hid=btr_hid();
  ## consistency checks
  ##   1) for ties, btr.hids must be equal
  ##   2) btr.hid must be monotonic in score, ie, if S1<S2, then btr.hid1>=btr.hid2
  ## first do groups of ties
  by_score=split(frecl,frecl$score);
  counts=sapply(by_score,function(group) nrow(group));
  ties=by_score[which(counts>1)];
  bad=which(sapply(ties,function(group) any(diff(sort(group$btr.hid))!=0)))
  if (length(bad)>0) stop("failed consistency check for ties");
  ## now check rows with unique scores
  ## works to check all rows here once we know that ties are correct
  frecl.order=frecl[order(frecl$score,decreasing=F),]
  bad=which(diff(frecl.order$btr.hid)>0)
  if (length(bad)>0) stop("failed consistency check for non-ties");
  ## check for double counting of hid
  ## hid=number of hidden scores in each observed interval
  hid=diff(frecl.order$btr.hid);
  if (sum(hid)>num.hid) stop("failed consistency check for double counting");
  ## rename variables for next phase of analysis: make obs, btr.obs reflect complete obs data
  frecl=frecl.order;                         # frecl is ordered by score
  frecl$btr.obs=nrow(frecl)-rank(frecl$score,ties.method='max');
  frecl$btr.all=frecl$btr.obs+frecl$btr.hid;
  frecl$frq.all=1-frecl$btr.all/num.all;
  frecl$frq.hid=1-frecl$btr.hid/num.hid;
  frecl$frq.obs=1-frecl$btr.obs/num.obs;
  ## remove ties
  frecl=frecl[!duplicated(frecl$score),];
  
 save_data(phase1,data=frecl);
  ## phase 2. generate distribution functions for all, obs, hid
  
  frecl=subset(frecl,select=c(-rank,-total));
}

TODO=function() {
## remember to sort obs, remove ties, use 1-frq

  ## remove NAs. code adapted from akima::aspline
  ## CAUTION: must use '<-' not '=' or place assignment in extra parens ((na=is.na(y)))
  ##   see stackoverflow.com/questions/1741820 for explanation. gotta love R...
  if (any(na<-is.na(y))) x=x[!na]; y=y[!na];

}


## main idea in converting raw data into usuable distribution
##
## row i (score,rank): rank-1 entries have "better" scores in hidden and observed before i
## let btr_all(i)=rank-1: number of better scores in hidden and observed before i
##     btr_obs(i)   : number of better scores in observed before i
##     btr_hid(i)   : number of better scores in hidden
## then, btr_all(i)=btr_obs(i)+btr_hid(i) or
##       btr_hid(i)=btr_all(i)-btr_obs(i)
## btr_all=function(i,obs=get('obs',envir=parent.frame())) obs[i,'rank']-1;
btr_all=function(frecl=parent(frecl)) frecl$rank-1;
btr_obs=function(frecl=parent(frecl)) 
  c(0,sapply(2:nrow(frecl),function(i) length(which(frecl$score[1:(i-1)]>frecl$score[i]))));
btr_hid=function(frecl=parent(frecl)) with(frecl,btr.all-btr.obs)

########## stepfun
##### hidden
phid1=stepfun(rev(obs$score),c(1,rev(obs$frq_hid)),right=F);
dhid1=make_d(phid1);
qhid1=make_q(phid1);
rhid1=make_r(qhid1);
##### observed
pobs1=stepfun(rev(obs$score),c(1,rev(obs$frq_obs)),right=F);
dobs1=make_d(pobs1);
qobs1=make_q(pobs1);
robs1=make_r(qobs1);
##### all
pall1=stepfun(rev(obs$score),c(1,rev(obs$frq_all)),right=F);
dall1=make_d(pall1);
qall1=make_q(pall1);
rall1=make_r(qall1);
########## approxfun
##### hidden
phid2=approxfun(c(obs$score,1),c(obs$frq_hid,1),rule=2);
dhid2=make_d(phid2);
qhid2=make_q(phid2);
rhid2=make_r(qhid2); 
##### observed
pobs2=approxfun(c(obs$score,1),c(obs$frq_obs,1),rule=2);
dobs2=make_d(pobs2);
qobs2=make_q(pobs2);
robs2=make_r(qobs2);
##### all
pall2=approxfun(c(obs$score,1),c(obs$frq_all,1),rule=2);
dall2=make_d(pall2);
qall2=make_q(pall2);
rall2=make_r(qall2);
########## splinefun
########## cumulative distribution needs function generator to handle end intervals
make_p_spline=function(what=c('hid','obs','all'),obs=get('obs',envir=parent.frame())) {
  frq_what=paste(sep='_','frq',match.arg(what));
  sfun=splinefun(c(obs$score,1),c(obs[[frq_what]],1),method='monoH.FC');
  smax=max(obs$score);
  function(s) ifelse(s<=1,1,ifelse(s>=smax,0,sfun(s)));
}
##### hidden
phid3=make_p_spline(what='hid');
dhid3=make_d(phid3);
qhid3=make_q(phid3);
rhid3=make_r(qhid3);
##### observed
pobs3=make_p_spline(what='obs');
dobs3=make_d(pobs3);
qobs3=make_q(pobs3);
robs3=make_r(qobs3);
##### all
pall3=make_p_spline(what='all');
dall3=make_d(pall3);
qall3=make_q(pall3);
rall3=make_r(qall3);

################################################################################
########## phase 3. make some ugly, but hopefully illustrative, plots
################################################################################
## do plotting under a conditional so they are only run interactively

do_plot=F;
if (do_plot) {
  ##### plot approxfun versions
  dev.new()
  x=seq(0,120000,by=10000)
  dif_hid=abs(diff(phid2(x)))
  dif_obs=abs(diff(pobs2(x)))
  dif=data.frame(hid2=dif_hid,obs2=dif_obs)
  rownames(dif)=x[2:length(x)]
  barplot(t(dif),beside=T,col=c('green','red'),border=NA); grid()
  legend(x='top',legend=c('hid2','obs2'),fill=c('green','red'));

  x=seq(0,120000,by=5000)
  dif_hid=abs(diff(phid2(x)))
  dif_obs=abs(diff(pobs2(x)))
  dif=data.frame(hid2=dif_hid,obs2=dif_obs)
  rownames(dif)=x[2:length(x)]
  barplot(t(dif),beside=T,col=c('green','red'),border=NA); grid()
  legend(x='top',legend=c('hid2','obs2'),fill=c('green','red'));

  dev.new()
  x=seq(0,120000,by=1000)
  dif_hid=abs(diff(phid2(x)))
  dif_obs=abs(diff(pobs2(x)))
  dif=data.frame(hid2=dif_hid,obs2=dif_obs)
  rownames(dif)=x[2:length(x)]
  barplot(t(dif),beside=T,col=c('green','red'),border=NA); grid()
  legend(x='top',legend=c('hid2','obs2'),fill=c('green','red'));

  dev.new();
  plot(x=NULL,y=NULL,xlim=c(0,120000),ylim=c(0,5e-5),type='n'); # ylim empirically determined
  grid()
  invisible(replicate(10,lines(density(rhid2(1000)),col='green')))
  invisible(replicate(10,lines(density(robs2(1000)),col='red')))
  legend(x='top',legend=c('hid2','obs2'),fill=c('green','red'));

  ##### plot splinefun versions
  dev.new()
  x=seq(0,120000,by=10000)
  dif_hid=abs(diff(phid3(x)))
  dif_obs=abs(diff(pobs3(x)))
  dif=data.frame(hid3=dif_hid,obs3=dif_obs)
  rownames(dif)=x[2:length(x)]
  barplot(t(dif),beside=T,col=c('green','red'),border=NA); grid()
  legend(x='top',legend=c('hid3','obs3'),fill=c('green','red'));

  dev.new()
  x=seq(0,120000,by=5000)
  dif_hid=abs(diff(phid3(x)))
  dif_obs=abs(diff(pobs3(x)))
  dif=data.frame(hid3=dif_hid,obs3=dif_obs)
  rownames(dif)=x[2:length(x)]
  barplot(t(dif),beside=T,col=c('green','red'),border=NA); grid()
  legend(x='top',legend=c('hid3','obs3'),fill=c('green','red'));

  dev.new()
  x=seq(0,120000,by=1000)
  dif_hid=abs(diff(phid3(x)))
  dif_obs=abs(diff(pobs3(x)))
  dif=data.frame(hid3=dif_hid,obs3=dif_obs)
  rownames(dif)=x[2:length(x)]
  barplot(t(dif),beside=T,col=c('green','red'),border=NA); grid()
  legend(x='top',legend=c('hid3','obs3'),fill=c('green','red'));

  dev.new();
  plot(x=NULL,y=NULL,xlim=c(0,120000),ylim=c(0,5e-5),type='n'); # ylim empirically determined
  grid()
  invisible(replicate(10,lines(density(rhid3(1000)),col='green')))
  invisible(replicate(10,lines(density(robs3(1000)),col='red')))
  legend(x='top',legend=c('hid3','obs3'),fill=c('green','red'));

  ## compare interpolation methods
  dev.new()
  x=seq(0,120000,by=10000)
  dif_hid1=abs(diff(phid1(x)))
  dif_hid2=abs(diff(phid2(x)))
  dif_hid3=abs(diff(phid3(x)))
  dif=data.frame(hid1=dif_hid1,hid2=dif_hid2,hid3=dif_hid3)
  rownames(dif)=x[2:length(x)]
  barplot(t(dif),beside=T,col=c('green','yellow','red'),border=NA); grid()
  legend(x='top',legend=c('hid1','hid2','hid3'),fill=c('green','yellow','red'));

  dev.new()
  x=seq(0,120000,by=5000)
  dif_hid1=abs(diff(phid1(x)))
  dif_hid2=abs(diff(phid2(x)))
  dif_hid3=abs(diff(phid3(x)))
  dif=data.frame(hid1=dif_hid1,hid2=dif_hid2,hid3=dif_hid3)
  rownames(dif)=x[2:length(x)]
  barplot(t(dif),beside=T,col=c('green','yellow','red'),border=NA); grid()
  legend(x='top',legend=c('hid1','hid2','hid3'),fill=c('green','yellow','red'));

  dev.new()
  x=seq(0,120000,by=1000)
  dif_hid1=abs(diff(phid1(x)))
  dif_hid2=abs(diff(phid2(x)))
  dif_hid3=abs(diff(phid3(x)))
  dif=data.frame(hid1=dif_hid1,hid2=dif_hid2,hid3=dif_hid3)
  rownames(dif)=x[2:length(x)]
  barplot(t(dif),beside=T,col=c('green','yellow','red'),border=NA); grid()
  legend(x='top',legend=c('hid1','hid2','hid3'),fill=c('green','yellow','red'));

}
