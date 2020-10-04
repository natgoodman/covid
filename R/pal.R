#################################################################################
##
## Author:  Nat Goodman
## Created: 20-09-15
##          from util_plot.R created 20-05-06
##          from clapi/R/plot_util.R created 20-03-22
##          from plot_nudge.R created 20-03-19
##          from misisg/R/plot.R created 19-01-09
##          uses code from repwr/R/plot.R created 18-05-03
##
## Copyright (C) 2019 Nat Goodman.
## 
## Functions for making color palettes (vectors of colors)
## Wraps several CRAN classes as well as base R palettes
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
library(RColorBrewer);
library(ggsci);
library(viridis);
library(wesanderson);

## ---- Color Palette Utility Functions ----
## make color vectors from color schemes (aka palettes)
## depite name, wraps other color packages in addition to RColorBrewer
## n - number of colors
## palettes - palette names or list of palette vectors
##   can be simple name if unambiguous, or 'pkg:pal', 'pal;subpal', 'pkg:pal;subpal'
##   or can be list of (pkg,pal,subpal) vectors
## names - names to index colors. if set, overrides n
## skip - number of colors to skip. most useful for sequential and diverging palettes
##   if 1 number, skip first; if 2 skip first and last; if 3 skip first, middle, last
##   can be single number or vector or list of one vector per pallete
## skip.beg - number of colors to skip at beginning
## skip.mid - number of colors to skip in middle. for diverging palettes
## skip.end - number of colors to skip at end
## rev - most useful for sequential palettes so darker colors first
## ramp.ok - okay to interpolate additional colors using colorRampPalette
##   default: TRUE for sequential and diverging, FALSE for categorical
col_brew=
  function(n=0,palettes='Dark2',names=NULL,
           skip=NULL,skip.beg=0,skip.mid=0,skip.end=0,rev=FALSE,ramp.ok=NA) {
    if ((length(palettes)==0)||(missing(n)&&missing(names))) return(NULL);
    ## pal_find returns subset of pal.info (1 row per palette)
    pals=pal_find(palettes);
    if (is.character(n)) names=n;
    if (!is.null(names)) n=length(names);
    n.pal=nrow(pals);
    ## compute number of colors we want for each palette
    pals$n=if(n.pal==1) n else as.integer(table(cut(1:n,n.pal)));
    ## add skip params after convertin into 2d of skip.beg,skip.mid,skip.end
    pals=cbind(pals,skip_pal(n.pal,skip,skip.beg,skip.mid,skip.end));
    ## add rev as is
    pals$rev=rev;
    ## add ramp.ok after filling defaults
    pals$ramp.ok=sapply(1:n.pal,function(i) 
      if (!is.na(ramp.ok[i])) ramp.ok else if (pals$type[i]=='cat') FALSE else TRUE);
    col=do.call(c,withrows(pals,row,
                           col_brew1(n,pkg,pal,subpal,type,mincols,maxcols,
                                     skip.beg,skip.mid,skip.end,rev,ramp.ok)))
    ## if too many colors, use 1st n
    if (n<length(col)) col=head(col,n);
    setNames(col,names);
  }
col_brew1=
  function(n,pkg,pal,subpal,type,mincols,maxcols,skip.beg,skip.mid,skip.end,rev,ramp.ok) {
    if (n==0) return(NULL);
    m=sum(n,skip.beg,skip.mid,skip.end)
    ## make sure m is in range supported by palette
    m=max(mincols,min(m,maxcols));
    ## get the colors
    col=switch(pkg,
               RColorBrewer=pal_rcbrewer(m,pal),
               ggsci=pal_ggsci(m,pal,subpal),
               viridis=pal_viridis(m,pal),
               wesanderson=pal_wesanderson(m,pal),
               yarrr=pal_yarrr(m,pal),
               base=pal_base(m,pal),
               stop('Bad news: unknown palette pkg=',pkg,'. Should have been caught in init_pal'));
    if (skip.beg>0) col=tail(col,-skip.beg);
    if (skip.end>0) col=head(col,-skip.end);
    if (skip.mid>0) {
      mid=length(col)/2;
      lo=ceiling(mid-skip.mid/2);
      hi=lo+skip.mid-1;
      col=col[-(lo:hi)];
    }
    if (rev) col=rev(col);
    ## if want more colors than in palette, colorRampPalette will make more if allowed
    if (n>length(col)) {
      if (ramp.ok) col=colorRampPalette(col)(n)
      else stop("Want more colors than palette provides (including skipped colors) and 'ramp.ok' is FALSE: ",nvq(n,m,maxcols));
    }
    col;
  }

## return subset of pal.info (1 row per palette)
## stop if any palette not found
## palettes - palette names or list of palette vectors
##   can be simple name if unambiguous, or 'pkg:pal', 'pal;subpal', 'pkg:pal;subpal'
##   or can be list of (pkg,pal,subpal) vectors
pal_find=function(palettes,pal.info=param(pal.info)) {
  if (is.null(pal.info)) pal.info=init_pal();
  if (!is.list(palettes)) palettes=as.list(palettes);
  ## parse palette strings into vectors
  pals=lapply(palettes,function(palette)
    ## if element has length 1, assume palette string else vector
    if (length(palette)==1) pal_parse(palette) else palette);
  ## lookup palette vectors and return rows of pal.info
  do.call(rbind,lapply(pals,function(pvec) {
    with(as.list(pvec),{
      if (!is.na(pkg)) pal.info=pal.info[pal.info$pkg==pkg,];
      if (!is.na(pal)) pal.info=pal.info[pal.info$pal==pal,];
      if (!is.na(subpal)) pal.info=pal.info[pal.info$subpal==subpal,];
      if (nrow(pal.info)==0) stop('palette ',nvq(pkg,pal,subpal,SEP=', '),' not found');
      if (nrow(pal.info)>1) {
        ## multiple rows. but are they all subpals of same pal?
        if (nrow(unique(pal.info[,cq(pkg,pal),drop=FALSE]))==1) pal.info=pal.info[1,,drop=FALSE]
        else stop('Multiple palettes found for ',nvq(pkg,pal,subpal,SEP=', '));
      }
      pal.info;
    })}));
}
## parse 'pal' string into vector
## palette can be 'pal', 'pal;subpal', 'pkg:pal;subpal'
pal_parse=function(palette) {
  m=regexec('([^:;]+)([:;]{0,1})([^:;]*)([:;]{0,1})([^:;]*)',palette,perl=T);
  terms=regmatches(palette,m)[[1]];
  ## skip first term - always entire string
  terms=terms[-1];
  ## drop empty terms
  terms=do.call(c,lapply(terms,function(term) if (nchar(term)>0) term else NULL));
  ## initialize components to NA
  out=c(pkg=NA,pal=NA,subpal=NA);
  ## well-formed pals have 1, 3, or 5 terms
  ## can also have +1 if string ends with punctuation
  nterms=length(terms);
  if (nterms<=2) out['pal']=terms[1]
  else if (nterms<=4) {
      if (terms[2]==':') out[cq(pkg,pal)]=terms[c(1,3)]
      else if (terms[2]==';') out[cq(pal,subpal)]=terms[c(1,3)]
      else stop('Unrecognized pal format: ',pal);
  }
  else if (nterms<=6) out[cq(pkg,pal,subpal)]=terms[c(1,3,5)]
  else stop('Unrecognized pal format: ',pal);
  out;
}
## convert col_brew skip params into data frame or matrix of skip.beg,skip.mid,skip.end
skip_pal=function(n.pal,skip,skip.beg,skip.mid,skip.end) {
  if (!is.null(skip)) {
    if (is_list(skip)) {
      ## list of vectors. turn into matrix and fall into 2d code below
      ncol=max(sapply(skip,length));
      if (ncol>3)
        stop("Some element of 'skip' is too long: '",ncol,
             ". When list, each element of 'skip' must have <=3 values");
      skip=sapply(skip,function(skip) fill(skip,LENGTH=ncol,FILL=0));
    }
    if (is_2d(skip)) {
      ncol=ncol(skip);
      if (ncol>3)
        stop("'skip' has too many columns: '",ncol,". When 2d, 'skip' must have <=3 columns");
      skip.beg=skip[,1];                 # first column is skip.beg
      if (ncol>=2) skip.end=skip[,ncol]; # last column is skip.end
      if (ncol==3) skip.mid=skip[,2];    # middle column is skip.mid
    } else {
      ## should be vector
      if (!is.vector(skip)) stop("Unrecognized 'skip' format: not list, 2d, or vector");
      len=length(skip);
      if (len>3)
        stop("'skip' has too many values: '",len,". When vector, 'skip' must have <=3 values");
      skip.beg=skip[1];               # first value is skip.beg
      if (len>=2) skip.end=skip[len]; # last value is skip.end
      if (len==3) skip.mid=skip[2]; # middle value is skip.mid
    }}
  ## suppressWarnings to shut up complaints about vector lengths
  skip=suppressWarnings(cbind(skip.beg=skip.beg,skip.mid=skip.mid,skip.end=skip.end));
  fillr(skip,LENGTH=n.pal,FILL=0)
}

pal_rcbrewer=RColorBrewer::brewer.pal;
## for now, only works with default subpal
pal_ggsci=function(n,pal,subpal) {
  if (is.na(subpal)) subpal=1;
  ## code below copied verbatim from ggsci pal_nejm - other pal functions identical
  alpha=1;
  raw_cols = ggsci:::ggsci_db[[pal]][[subpal]]
  raw_cols_rgb = col2rgb(raw_cols)
  alpha_cols = rgb(raw_cols_rgb[1L, ], raw_cols_rgb[2L, ], 
                   raw_cols_rgb[3L, ], alpha = alpha * 255L, names = names(raw_cols), 
                   maxColorValue = 255L)
  if (n>length(alpha_cols)) stop('asked for too many colors')
  alpha_cols[1:n]
  ## scales::manual_pal(unname(alpha_cols))
}
pal_viridis=function(n,pal) viridis(n,option=pal)
## need as.vector to turn off 'clever' print which plots pal...
pal_wesanderson=function(n,pal) as.vector(wes_palette(pal,n,));
## yarrr uses palette definitions from yarrr R/piratepal_function.R
pal_yarrr=function(n,pal) unlist(yarrr.info[[pal]])
pal_base=function(n,pal) {
  ## remove '.colors' suffix if present
  pal=sub('.colors','',pal);
  switch(pal,
         rainbow=rainbow(n),
         heat=heat.colors(n),
         terrain=terrain.colors(n),
         topo=topo.colors(n),
         cm=cm.colors(n),
         stop('Bad news: unknown palette ',pal,'. Should have been caught earlier'));
}

## init pal.info param
## columns pkg,pal,subpal,type,mincols,maxcols,skip,rev
init_pal=function(pkgs=cq(RColorBrewer,ggsci,viridis,wesanderson,yarrr,base)) {
  pkgs=match.arg(pkgs,several.ok=TRUE);
  ## super-easy way to init empty data frame from //stackoverflow.com/questions/10689055. Thx!
  pal.info=rbind(data.frame(),do.call(rbind,lapply(pkgs,function(pkg)
    cbind(pkg=pkg,
          switch(pkg,
                 RColorBrewer=initpal_rcbrewer(),
                 ggsci=initpal_ggsci(),
                 viridis=initpal_viridis(),
                 wesanderson=initpal_wesanderson(),
                 yarrr=initpal_yarrr(),
                 base=initpal_base(),
                 stop('Bad news: unknown palette pkg=',pkg,'. Should have been caught earlier')),
          stringsAsFactors=FALSE))));
  param(pal.info=pal.info);
}
initpal_rcbrewer=function() {
  rcbrewer=RColorBrewer::brewer.pal.info;
  ## convert RColorBrewer categories to our type
  type=as.character(rcbrewer$category);
  type=ifelse(type=='qual','cat',type);
  pal.info=data.frame(pal=rownames(rcbrewer),subpal=NA,type=type,
                      mincols=3,maxcols=rcbrewer$maxcolors,
                      stringsAsFactors=FALSE);
  pal.info;
}
## TODO: some ggsci palettes are continuous, aka, sequential.
## see https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html#npg
initpal_ggsci=function() {
  ggsci=ggsci:::ggsci_db;
  pal.info=do.call(rbind,lapply(names(ggsci),function(pal) {
    type=switch(pal,gsea='div',material='seq','cat');
    do.call(rbind,lapply(names(ggsci[[pal]]),function(subpal)
      data.frame(pal=pal,subpal=subpal,type=type,mincols=1,
                 maxcols=length(ggsci[[pal]][[subpal]]),
                 stringsAsFactors=FALSE)))}));
  ## fix igv;alternating. special case
  pal.info[pal.info$pal=='igv'&pal.info$subpal=='alternating','type']='div';
  pal.info;
}
initpal_viridis=function() {
  pal.info=do.call(rbind,lapply(cq(viridis,magma,plasma,inferno),function(pal)
    data.frame(pal=pal,subpal=NA,type='seq',mincols=1,maxcols=Inf,stringsAsFactors=FALSE)));
  pal.info;
}
initpal_wesanderson=function() {
  wes=wesanderson::wes_palettes;
  pal.info=do.call(rbind,lapply(names(wes),function(pal)
    data.frame(pal=pal,subpal=NA,type='cat',mincols=1,
               maxcols=length(wes[[pal]]),stringsAsFactors=FALSE)));
  pal.info;
}
initpal_yarrr=function() {
  yarrr=yarrr.info;
  pal.info=do.call(rbind,lapply(names(yarrr),function(pal)
    data.frame(pal=pal,subpal=NA,type='cat',mincols=1,
               maxcols=length(yarrr[[pal]]),stringsAsFactors=FALSE)));
  pal.info;
}

## for base allow palette names w/ and w/o '.colors' 
initpal_base=function() {
  pal.info=do.call(rbind,lapply(cq(rainbow,heat,terrain,topo),function(pal)
    data.frame(pal=pal,subpal=NA,type='seq',mincols=1,maxcols=Inf,
               stringsAsFactors=FALSE)));
  pal.info=rbind(pal.info,do.call(rbind,lapply(cq(cm),function(pal)
    data.frame(pal=pal,subpal=NA,type='div',mincols=1,maxcols=Inf,
               stringsAsFactors=FALSE))));
  ## repeat with '.colors' tacked on
  pal.info2=pal.info;
  pal.info2$pal=paste0(pal.info$pal,'.colors');
  rbind(pal.info,pal.info2);
}
