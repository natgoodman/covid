#################################################################################
##
## Author:  Nat Goodman
## Created: 20-07-13
##          from code in util_file.R created 20-05-03
##          from dat.R created 20-05-02
##          from frecl/R/dat.R created 20-01-17
##          from misg/R/datman.R created 19-01-01
##          from repwr/R/datman.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Save and load utilities
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Save and Load ----
## save data in RData and optionally txt formats
save_=function(data,base=NULL,save,save.txt=FALSE,file=NULL,suffix=cq(txt,RData)) {
  if (is.null(base)&&is.null(file)) stop("No place to save data: 'base' and 'file' are both NULL");
  if (!is.null(file)) {
    if (!file.exists(file)) stop(paste('File',file,'does not exist'));
  } else {
    base=desuffix(base,suffix);
    file=filename(base=base,suffix='RData');
  }
  if ((is.na(save)&!file.exists(file))|(!is.na(save)&save)) {
    save(data,file=filename(base=base,suffix='RData'));
    if (save.txt) {
      file=filename(base=base,suffix='txt');
      if (length(dim(data))==2) write.table(data,file=file,sep='\t',quote=F,row.names=F)
      ## code for saving non-table data adapted from save_tbl
      else if (is.list(data)) {
        sink(file); print(data); sink();
      }
      else if (is.vector(data)) {
        names=names(data);
        if (!is.null(names)) {
          data=data.frame(name=names,value=as.character(data));
          write.table(data,file=file,sep='\t',quote=F,row.names=F);
        } else writeLines(as.character(data),file);
      }
      else stop(paste('Unabe to save text for class',class(data),'. Sorry'));
    }
  }
}
## load data from RData file
load_=function(base=NULL,file=NULL,suffix=cq(txt,RData)) {
  if (is.null(base)&&is.null(file)) stop("No place to get data: 'base' and 'file' are both NULL");
  if (is.null(file)) file=resuffix(base,old.suffix=suffix,suffix='RData');
  what=load(file=file);                 # what is name of saved data
  get(what);                            # return it
}
## read data from txt file - mostly for testing - not guaranteed to work
read_=function(base=NULL,file=NULL,suffix=cq(txt,RData),stringsAsFactors=FALSE,...) {
  if (is.null(base)&&is.null(file)) stop("No place to get data: 'base' and 'file' are both NULL");
  if (is.null(file)) file=resuffix(base,old.suffix=suffix,suffix='txt');
  read.delim(file,stringsAsFactors=stringsAsFactors,...);
}
