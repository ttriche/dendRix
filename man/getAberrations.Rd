\name{getAberrations}
\alias{getAberrations}
\title{
  Retrieve the unique aberrations present in a group of samples/patients.
}
\description{
  Depending on whether a list, matrix, or data.frame is passed to the function,
  it extracts either the unique elements (of a list), the column names (of a 
  matrix), or the names (of a data.frame) where at least one sample is aberrant.
}
\usage{
  getAberrations(aberrations)
}
\arguments{
  \item{aberrations}{
  A list, matrix, or data.frame with aberrations and status for each subject.
  }
}
\value{
  A character vector.
}
\author{
  Tim Triche, Jr.
}

\examples{
  
  data(AML.mutations)
  getAberrations(AML.mutations)

}
