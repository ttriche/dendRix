\name{measure}
\alias{measure}
\title{
  Loop through a gene set or sets and compute its measure
}
\description{
  This is an integral function for the Dendrix sampler and as such should be C++
}
\usage{
measure(genes_collection1, genes_collection2, sample_mutatedGenes, c)
}
\arguments{
  \item{genes_collection1}{
    A set of aberrations.
  }
  \item{genes_collection2}{
    Another set of aberrations.
  }
  \item{sample_mutatedGenes}{
    Per-sample mutation tabulation.
  }
  \item{c}{
    A constant (default 0.5)
  }
}
\value{
  The measure of the set.
}
\author{
  Tim Triche, Jr.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
