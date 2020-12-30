#################################################################################
##
## Author:  Nat Goodman
## Created: 20-07-13
##
## Copyright (C) 2020 Nat Goodman.
## 
## Generate data series and blocks from cvdat objects
## Used by plot_cvdat and data_cvdat
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
###################################################################################
data_series=
  function(objs,places='state',ages=NULL,incompatible.ok=param(incompatible.ok),
           attrs=cq(unit,cumulative,what,datasrc,version,id,fit,roll,extra,edit)) {
    if (is_cvdat(objs)) objs=list(objs);
    if (is.null(places)) stop("'places' cannot be NULL: nothing to select!");
    ## if (identical(ages,'all')) ages=NULL;
    if (!(is.null(ages)||identical(ages,'all'))) {
      datasrc=sapply(objs,function(obj) obj$datasrc);
      bad=unique(datasrc[datasrc!='doh']);
      if (length(bad)>0) 
        stop("Only doh objects have ages, not ",paste(collapse=', ',bad)," objects");
    }
    ## check whether edited objects are compatible
    cmp_pops(objs,places,ages,incompatible.ok=incompatible.ok);
    ## get data frame values from objs
    xattr=objs_attr(objs,attrs,label=FALSE);
    xattr=cbind(obj=seq_along(objs),xattr);  # tack 'obj' to front of xattr
    xattr=expand_df(xattr,place=places,age=ages);
    xattr=cbind(series=1:nrow(xattr),xattr); # tack 'series' to front of xattr
    series=withrows(xattr,case,{
      obj=objs[[obj]];
      data=obj$data;
      if (obj$datasrc=='doh') data=if(is.null(ages)) data$all else data[[age]];
      if (place %notin% colnames(data)[-1])
        stop(nv(place)," not valid for ",obj$datasrc," objects");
      series=data.frame(date=data$date,y=data[[place]])
    });
    list(objs=objs,xattr=xattr,series=series)
  }
ct_attrs=function(series,attrs=cq(unit,cumulative,what,datasrc,version,id,fit,roll,extra,edit)) {
  xattr=series$xattr;
  attrs=c(cq(series,obj),attrs,cq(place,age));
  attrs=attrs %&% colnames(xattr);
  xattr=xattr[,attrs];
  count=apply(xattr,2,function(x) if (all(is.na(x))) 0 else length(unique(x)));
  sv.attrs=attrs[count==1];
  mv.attrs=attrs[count>1];
  list(xattr=xattr,sv.attrs=sv.attrs,mv.attrs=mv.attrs);
}
series_percap=function(series,pop0=param(pop)) {
  xattr=series$xattr;
  objs=series$objs;
  if (is.null(pop0)) pop0=load_pop();
  ## if ('age' %notin% colnames(xattr)) xattr$age='all';
  if ('age' %notin% colnames(xattr)) age='all';
  data=series$series;
  series=withrows(xattr,row,{
    data=data[[series]];
    pop=objs[[obj]]$pop;
    if (is.null(pop)) pop=pop0;
    pop=pop[age,place];
    if (is.null(pop)) pop=NA;
    data.frame(date=data$date,y=round(1e6*data$y/pop));
  });
  list(objs=objs,xattr=xattr,series=series);
}

## there can be up to 3 lines blocks
## with the caution that graphs with more than 2 blocks may be inscrutable
## block names can include final 's' - seems a likely typo otherwise
## 'obj' is shorthand for all object attrs
## for now, object attrs are all-or-none.
## TODO: use specific object attrs for block, eg, 'datasrc'
##       this means there could be left over attributes to be used in a final block
## BUG: crashes if input contains multiple objects with same attributes.
series_blocks=
  function(series,ct=NULL,blocks.order=cq(obj,place,age,objs,places,ages),
           attrs=cq(unit,cumulative,what,datasrc,version,fit,roll,extra,edit)) {
    if (is.null(ct)) ct=ct_attrs(series,attrs);
    blocks.order=match.arg(blocks.order,several.ok=TRUE);
    blocks.order=unique(sub('s$','',blocks.order));
    attrs=ct$mv.attrs;
    xattr=series$xattr[,attrs];
    blocks=sapply(blocks.order,function(block) {
      if (block %in% attrs) {
        if (block=='obj') attrs=attrs%-%cq(series,obj,place,age)
        else attrs=block;
        bxattr=xattr[,attrs,drop=FALSE];
        xattr.grp=split(xattr,bxattr,drop=TRUE);
        blocks=lapply(xattr.grp,function(xattr) xattr$series);
        blocks=blocks[order(sapply(blocks,min))];
        list(blocks=blocks,xattr=unique(bxattr));
      }
    },simplify=FALSE);
    blocks[!sapply(blocks,is.null)];
  }

