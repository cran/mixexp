\name{Nrows}
\alias{Nrows}
\title{ Loads compiled fortran in shared file cnvrt and returns the number of rows in the resulting design}
\description{This function loads and runs the compiled fortran code cnvrt. cnvrt is Pieple's 1988 JQT fortran code for extreme vertices designs. }
\usage{ 
Nrows(ndm,nvrr,ncon2,rtheta2) 
}
\arguments{
  \item{ndm}{ This is the order of centroids desired (0=none, 1=edge centroids, 2=face centroids etc.)}
  \item{nvrr}{ This is the number of mixture variables ( maximum is 12)}
  \item{ncon2}{ This is the number of constraints (maximum is 45)}
  \item{rtheta2}{ This is the constraint matrix stored as a vector of columns.}
}
\value{ nvrtr
\item{nvrtr }{ This is the number of rows in rxvt the matrix of extreme vertices and centroids }
}
\references{
1. Pieple, G. F. "Programs for Generating Extreme Vertices and Centroids of Linearly Consrtained Experimental Regions"
\emph{Journal of Quality Technology}, Vol 20, No. 2, pp. 125-139, 1988.

}
\note{ This function is called by the function crtave.}
\author{ John S. Lawson \email{lawson@byu.edu}}
\keyword{}
