frozenSet <- function(set, sep='|')
{ # {{{ turn a gene set into a hash key 
  paste(sort(set), collapse=sep) 
}
