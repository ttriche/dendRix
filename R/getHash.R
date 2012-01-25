getHash <- function(aberrations) 
{ # {{{ convert a list, data.frame, or matrix into a key-value hash

  require(hash) 
  if(class(aberrations) == 'list') return(hash(aberrations))
  if(class(aberrations) == 'matrix') mutnames = colnames(aberrations)
  if(class(aberrations) == 'data.frame') mutnames = names(aberrations)
  subjects = rownames(aberrations)
  names(subjects) = subjects
  hash(lapply(subjects, function(x) {
    negnames = posnames = c()
    positive = which( as.numeric(aberrations[x,]) > 0 )
    negative = which( as.numeric(aberrations[x,]) < 0 )
    if(length(negative)>0) negnames = paste(mutnames[negative], 'Neg', sep='')
    if(length(positive)>0) posnames = mutnames[positive]
    return( c( posnames, negnames ) )
  }))

}
