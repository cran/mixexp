\name{Vertcen}
\alias{Vertcen}
\title{ Loads compiled fortran in shared file cnvrt}
\description{This function loads and runs the compiled fortran code cnvrt. cnvrt is Piepel's 1988 JQT fortran code for extreme vertices designs. }
\usage{ 
Vertcen(ndm,nvrr,ncon2,rtheta2) 
}
\arguments{
  \item{ndm}{ This is the order of centroids desired (0=none, 1=edge centroids, 2=face centroids etc.)}
  \item{nvrr}{ This is the number of mixture variables ( maximum is 12)}
  \item{ncon2}{ This is the number of constraints (maximum is 45)}
  \item{rtheta2}{ This is the constraint matrix stored as a vector of columns.}
}
\value{ 
\item{rxvt }{ This is the matrix of vertices and centroids stored as a vector of columns.  }
}
\references{
1. Piepel, G. F. "Programs for Generating Extreme Vertices and Centroids of Linearly Consrtained Experimental Regions"
\emph{Journal of Quality Technology}, Vol 20, No. 2, pp. 125-139, 1988.

}
\note{ This function is called by the function crtave.}
\author{ John S. Lawson \email{lawson@byu.edu}}
\keyword{interface}
