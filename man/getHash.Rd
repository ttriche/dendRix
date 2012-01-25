\name{getHash}
\alias{getHash}
\title{
  Turn a list, data.frame, or matrix of aberrations into a hash. 
}
\description{
  Pretty much what the title says.  
  Turn an adjacency matrix into an hashed adjacency list, 
  or turn a list into a proper (invertible!) hash object.
}
\usage{
getHash(aberrations)
}
\arguments{
  \item{aberrations}{
    A list, data.frame, or matrix of aberrations.
    Cells can be -1, 0, 1, T, or F for data.frames and matrices.
  }
}
\value{
  A hash (see package?hash for more details).
}
\author{
  Tim Triche, Jr.
}

\examples{
  
  data(AML.mutations)
  getHash(AML.mutations)

}
