\name{Eflags}
\alias{Eflags}
\title{ Loads compiled fortran in shared file cnvrt and returs the error messages}
\description{This function loads and runs the compiled fortran code cnvrt and prints error messages. cnvrt is Pieple's 1988 JQT fortran code for extreme vertices designs. }
\usage{ 
Eflags(ndm,nvrr,ncon2,rtheta2) 
}
\arguments{
  \item{ndm}{ This is the order of centroids desired (0=none, 1=edge centroids, 2=face centroids etc.)}
  \item{nvrr}{ This is the number of mixture variables ( maximum is 12)}
  \item{ncon2}{ This is the number of constraints (maximum is 45)}
  \item{rtheta2}{ This is the constraint matrix stored as a vector of columns.}
}
\value{
\item{ifa }{ This is the vector of error flags. A negative value for flag 1 indicates that there are
inconsistent constraints. A negative value for flag2 indicates there are two many vertices and centroids,
this program only works when # vertices + # centroids <=1000. A negative value for flag 3 indicates
an error encountered when calling subroutine allnr. }
}
\references{
1. Pieple, G. F. "Programs for Generating Extreme Vertices and Centroids of Linearly Consrtained Experimental Regions"
\emph{Journal of Quality Technology}, Vol 20, No. 2, pp. 125-139, 1988.

}
\note{ This function is called by the function crtave.}
\author{ John S. Lawson \email{lawson@byu.edu}}
\keyword{interface}
