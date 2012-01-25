\name{frozenSet}
\alias{frozenSet}
\title{
  frozenSet: flatten a set into a hash key (or more generally, a name)
}
\description{
  The sole functionality of the frozenset() class in Dendrix appeared to be as
  a hashable key, so this function simply replicates that functionality in R.
}
\usage{
  frozenSet(set)
}
\arguments{
  \item{set}{
    A collapsible object, typically a vector or Vector. 
  }
  \item{sep}{
    The character to separate items in the serialized object.  Default is |.
  }
}
\value{
  A string, with the entries sorted in alphabetical order.
}
\author{
  Tim Triche, Jr.
}
\examples{
 
  ## instead of |
  separator = ' '

  ## pickle a random sample of letters into a hashable key 
  foo = frozenSet( letters[ sample(1:26, 10) ], sep=separator )

  ## now recover the original (sorted, de-duplicated) sample
  bar = strsplit( foo, separator, fixed=TRUE )[[1]]

  ## and print it
  print(bar)

}
