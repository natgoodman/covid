#################################################################################
##
## Author:  Nat Goodman
## Created: 20-08-16
##
## Copyright (C) 2020 Nat Goodman.
## 
## Sandbox code for 'edit' transform.
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
###################################################################################

edit=function(data,...) {
  dots=match.call(expand.dots=FALSE)$...
  places=dots$places
  BREAKPOINT('after dots')

  names=names(dots);
  parent.env=parent.frame(n=1);
  if (is.null(names)) names=rep('',length(dots));
  y=sapply(seq_along(dots),function(i) {
    y=names[i];
    BREAKPOINT('top of sapply')
      if (nchar(y)==0) {
        y=dots[[i]];
        c=class(y);
        if (c=='call') y=eval(y,parent.env);
        y=as.character(y)
      } else {
        BREAKPOINT('inside else')
        val=eval(dots[[i]],envir=data,enclos=parent.env);
        data[,y]<<-val;
        y;
      }
  })
  BREAKPOINT('bottom')
  data[,y,drop=FALSE]
}

foo=function(data,places=list()) {
  places=pryr::subs(places);
  pldots=pryr::dots(places)
  BREAKPOINT('pldots')
  pl=eval(places,data)
  BREAKPOINT('pl')

  names=names(dots);
  parent.env=parent.frame(n=1);
  if (is.null(names)) names=rep('',length(dots));
  y=sapply(seq_along(dots),function(i) {
    y=names[i];
    BREAKPOINT('top of sapply')
      if (nchar(y)==0) {
        y=dots[[i]];
        c=class(y);
        if (c=='call') y=eval(y,parent.env);
        y=as.character(y)
      } else {
        BREAKPOINT('inside else')
        val=eval(dots[[i]],envir=data,enclos=parent.env);
        data[,y]<<-val;
        y;
      }
  })
  BREAKPOINT('bottom')
  data[,y,drop=FALSE]
}

subset.foo <- function (x, subset, select, drop = FALSE, ...)
{
    r <- if(missing(subset))
        rep_len(TRUE, nrow(x))
    else {
        e <- substitute(subset)
        r <- eval(e, x, parent.frame())
        if(!is.logical(r)) stop("'subset' must be logical")
        r & !is.na(r)
    }
    vars <- if(missing(select))
        TRUE
    else {
        nl <- as.list(seq_along(x))
        names(nl) <- names(x)
        eval(substitute(select), nl, parent.frame())
    }
    ## PR#15823 suggested that sometimes which(r) would be faster,
    ## but this is not intended for programmatic use and the
    ## difference is tens of ms on a 1 million-row data frame.
    x[r, vars, drop = drop]
}
