\name{dendRix}
\alias{dendRix}
\title{
  Run the MCMC sampler for dendRix.
}
\description{
  DE Novo DRIver eXclusivity... seek it out via MCMC sampling over mutation sets
}
\usage{
dendRix(aberrations = NULL, K = 2, minFreq = 1, numIter = 999999, keep = 999, analyzed = NULL, stepLength = 999, c = 0.5)
}
\arguments{
  \item{aberrations}{
  A list, data.frame, or matrix (rows are subjects, columns are aberrations).  
  }
  \item{K}{
  Cardinality of the sets to consider (should be deprecated soon).
  }
  \item{minFreq}{
  Minimum frequency to include an aberration (default is 1).
  }
  \item{numIter}{
  How many iterations to run the sampler for.
  }
  \item{keep}{
  How many of the top hits to retain.
  }
  \item{analyzed}{
  A list of aberrations to analyze (or NULL).
  }
  \item{stepLength}{
  How many steps to burn through between samples.
  }
  \item{c}{
  A constant.
  }
}
\value{
  A list of the most visited (asymptotically, the weightiest) sets.
}
\author{
  Tim Triche, Jr.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
