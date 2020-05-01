source('lib/util_file.R');
source('lib/util_plot.R');
suppressPackageStartupMessages(library(RMySQL));

## used to set argument from global defaults
default=function(what,default=NULL)
  mget(what,envir=parent.frame(),inherits=T,ifnotfound=list(default))[[1]];

## not in - based on example in RefMan - must exist somewhere but...
"%notin%"=function(x,table) match(x,table,nomatch=0)==0

## get name of Rscript script. adapted from optparse source
scriptname=function() {
  script=sub("--file=","", grep("--file=",commandArgs(),value=TRUE)[1]);
  if (is.na(script)) script='interactive';
  script;
}
## project is last component of working directory. used for setting database
project=function() basename(getwd());
## connect to standard database
db_connect=function(database=project()) dbConnect(MySQL(),database);

## convert Rmd files to R
## if weeks is NULL, do all weeks
## if replace is F, do not purl file if output alrady exists
purl.files=
  function(dir='rexamples/edXGenomics',weeks=NULL,replace=F,verbose=default('verbose',F)) {
  library(knitr);
  if (is.null(weeks)) weeks=find.files(dir,pattern.yes='^week');
  for (week in weeks) {
    if (!grepl('^week',week)) week=paste(sep='','week',week); # prepend 'week' if not there
    dir.week=file.path(dir,week);
    for (file in find.files(dir.week,suffix='Rmd')) {
      input=file.path(dir.week,file);
      purl.inplace(input,replace,verbose);
    }}
}
## purl one file in-place -- ie putting output in same directory as input
purl.inplace=function(input,replace=F,verbose=default('verbose',F)) {
  output=filename(desuffix(input,suffix='Rmd'),suffix='R');
  if (file.exists(output) & !replace) {
    if (verbose) print(paste('skipping',input))
  } else {
    if (verbose) print(paste('purling',input,'to',output));
    purl(input=input,output=output,quiet=T);
  }
}

#################### alias for browser so it'll be easier to find ####################
BROWSER=browser;
BREAKPOINT=browser;

## get index of median element of vector. analogous to which.min, which.max
which.med=function(x) {mid=ceiling(length(x)/2); which(x==sort(x)[mid])[1]}

#################### specialized versions of apply ####################
## apply f to sub-matrices of e defined by groups
gapply=function(groups,e,f) {
  t(sapply(groups,function(g) {
    eg=e[g,];
    if (length(dim(eg))>=2) apply(eg,2,function(column) f(column))
    else sapply(eg,function(column) f(column));
  }))
}
## apply f to columns of e. simple wrapper for apply
capply=function(e,f) if (!is.null(dim(e))) apply(e,2,function(column) f(column)) else f(e)
## apply f to rows of e. simple wrapper for apply
rapply=function(e,f) if (!is.null(dim(e))) apply(e,1,function(row) f(row)) else f(e)

