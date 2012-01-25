getAberrations <- function(aberrations) 
{ # {{{ take a list, data.frame, or matrix and tabulate the aberrations in it 
  switch(class(aberrations),
         list=unique(unlist(aberrations)),
         data.frame=names(aberrations)[which(colSums(abs(aberrations))!=0)],
         matrix=colnames(aberrations)[which(colSums(abs(aberrations))!=0)])
}
