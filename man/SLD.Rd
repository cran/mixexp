\name{SLD}
\alias{SLD}
\title{ This function creates simplex lattice mixture designs 
}
\description{
This function creates simplex lattice designs in unconstrained mixture experiment space.
}
\usage{
SLD(fac, lev)
}
\arguments{
  \item{fac}{ This is the number of factors, this must be between 2 and 12} 
  \item{lev}{ This is the number of levels, this must be between 2, and 5.}
}
\value{
 \item{SL}{This is a data frame containing the simplex lattice design. The columns are labeled
 x1, x2 ...xn, where n is the number of mixture variables. }
}
\references{
1. Cornell, J. A.  \emph{Experiments with Mixtures: Models and Analysis of Mixture Data}, John Wiley \& Sons, New York, third edition, 2002.

2. "John Lawson, Cameron Willden (2016).",
        "Mixture Experiments in R Using mixexp.",
        "Journal of Statistical Software, Code Snippets, 72(2), 1-20.",
        "doi:10.18637/jss.v072.c02"
}

\author{ John S. Lawson \email{lawson@byu.edu}}
\examples{
des<-SLD(3,2)

des<-SLD(4,3)
}
\keyword{datagen }
