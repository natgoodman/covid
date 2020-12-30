#################################################################################
##
## Author:  Nat Goodman
## Created: 20-08-16
##
## Copyright (C) 2020 Nat Goodman.
## 
## Adjust DOH data from recent weeks. This data is incomplete due to reporting
## and other delays. The solution is to add 'extra' data.
##
## The code here constructs functions that serve as correction factors
## Top-level 'extra' function in tranform.R calls these functions to correct the data
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
###################################################################################
extrafun=function(obj,objs,places,ages,method=cq(lm,wfun),args=list(),err.type,wmax,mulmax) {
  method=match.arg(method);
  ## set default args. actual arg values override
  args=cl(
    switch(method,
           lm=list(fmla='y~w',w.asfactor=TRUE),
           wfun=list(wfun=colMedians),
           stop("Bad news; unknown 'method' ",method,'. Should have been caught earlier!')),
    args);
  versions=sapply(objs,version);
  objs=objs[order(versions)];
  versions=sort(versions);
  vdates=as_date(versions);
  w=seq_len(wmax);
  ## dates wth enough versions are vmin-1*7..vmax-6*7
  vdates=as_date(sapply(objs,vdate));
  d.first=min(vdates)-7;
  d.last=max(vdates)-7*wmax;

  fun=sapply(ages,simplify=FALSE,function(age)
    sapply(places,simplify=FALSE,function(place) {
      wmat=extra_wmat(obj,objs,place,age,vdates,wmax,d.first,d.last);
      if (err.type=='*') werr=extra_errmul(wmat,mulmax) else werr=extra_erradd(wmat);
      if (any(!is.finite(werr))) stop('Bad news: bad values in werr: ',nv(age,place));
      fun=extrafun1(werr,w,method,args);
    }));
    invisible(fun);
  }
extrafun1=function(werr,w,method,args) {
  fun=with(args, {
    if (method=='lm') {
      wide=data.frame(date=as_date(rownames(werr)),werr[,w,drop=FALSE]);
      colnames(wide)=c('date',w);
      data=
        reshape(wide,varying=list(2:ncol(wide)),dir='long',idvar='date',v.names='y',timevar='w');
      rownames(data)=NULL;
      if (w.asfactor) data$w=as.factor(data$w) else data$w=as.numeric(data$w);
      mdl=lm(fmla,data=data,model=FALSE);
      fun=function(date,w) {
        dw=data.frame(date=date,w=w)
        if (w.asfactor) dw$w=as.factor(dw$w);
        pred=suppressWarnings(predict(mdl,dw));
      }
    } else if (method=='wfun') {
      ## only other model type at present returns vector of values keyed by w
      wvec=setNames(wfun(werr),colnames(werr));
      fun=function(date,w) wvec[w];
    } else {
      stop("Bad news; unknown 'method' ",method,'. Should have been caught earlier!');
    }
  });
  invisible(fun);
}
## compute error matrix from wmat: additive, multiplicative resp.
extra_erradd=function(wmat) {
  final=wmat[,'final'];
  werr=capply(wmat,function(col) final-col);
}
extra_errmul=function(wmat,mulmax) {
  final=wmat[,'final'];
  capply(wmat,function(col) {
    col=ifelse(col==0,1,col);
    err=ifelse(col>=final,1,final/col);
    ifelse(err>mulmax,mulmax,err);
  });
}
## use functions from extra_fun to adjust data
extraadj=function(obj,fun,places,ages,err.type,wmax,mulmax) {
  ## define dates and corresponding ws for prediction
  ## want dates from vdate(obj)-7*wmax to vdate(obj)-7
  d.first=vdate(obj)-7*wmax
  d.last=vdate(obj)-7;
  d.pred=seq(d.first,d.last,by=7);
  w.pred=wmax:1;
  data1=sapply(ages,simplify=FALSE,function(age) {
    data=obj$data[[age]];
    data1=data[data$date<d.first,];
  });
  data2=sapply(ages,simplify=FALSE,function(age) {
    data=obj$data[[age]];
    data2=data[data$date>=d.first,];
    dates2=data2$date;
    counts2=do.call(cbind,sapply(places,simplify=FALSE,function(place) {
      fun=fun[[age]][[place]];
      pred=fun(d.pred,w.pred);
      extraadj1(pred,data2[,place],err.type,mulmax);
    }));
    data2=cbind(date=dates2,as.data.frame(round(counts2)));
  });
  data=sapply(ages,simplify=FALSE,function(age) rbind(data1[[age]],data2[[age]]));
  invisible(data);
}
extraadj1=function(pred,counts,err.type,mulmax) {
  if (err.type=='+') {
    ## clamp pred to positve
    pred=pmax(0,pred);
    pred+counts;
  } else {
    ## clamp pred to [1,mulmax]
    pred=pmax(1,pmin(mulmax,pred));         # clamp to [ex.min,1]
    pred*counts;
  }
}
## returns matrix of counts by 'w' for one 'place' and 'age'
extra_wmat=
  function(obj,objs,place='state',age='all',
           vdates=as_date(sapply(objs,vdate)),
           wmax=param(extra.wmax),      
           d.first=min(vdates)-7,
           d.last=max(vdates)-7*wmax) {
    data=data_cvdat(c(objs,list(obj)),places=place,ages=age);
    colnames(data)=c('date',as.character(vdates),'final');
    ## limit data to dates covered by 1:wmax versions
    data=data[with(data,date>=d.first&date<=d.last),]
    counts=data[,-1];
    dates=data$date;
    wmat=matrix(NA,nrow=nrow(counts),ncol=wmax,dimnames=list(as.character(dates),1:wmax));
    sapply(seq_along(dates),function(i)
      sapply(seq_along(vdates),function(j) {
        w=extra_wf(vdates[j],dates[i],wmax);
        if (!is.na(w)) {
          wmat[i,w]<<-counts[i,j]
          }
      }));
    wmat=cbind(wmat,final=data$final);
    invisible(wmat)
  }
## vdate, date vectors
extra_wf=function(vdate,date,wmax=6) {
  w=(vdate-date)/7;
  ifelse((w>0&w<=wmax),w,NA);
}
