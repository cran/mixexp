\name{crvtave}
\alias{crvtave}
\title{ This function creates an extreme vertices design 
}
\description{
This function calls the function Vertcen which uses Piepel's (1988) fortran 
code (cnvrt) for generating extreme vertices and centroids of linearly constrained 
mixture experimental regions.
}
\usage{
crvtave(ndm,conmx)
}
\arguments{
  \item{ndm}{ is an integer representing the highest order of centroids requested. 
An overall centroid is always included, 0 indicates no other centroids will be created,
1 indicates edge centroids are requested, 2 indicates face centroids, etc.} 
  \item{conmx}{ This is the matrix of constraints.
  }
}
\value{
 \item{vtcn}{This is a data frame containing the extreme vertices design. The columns are labeled
 x1, x2 ...xn, where n is the number of mixture variables. The last column is labeled dimen and it
 indicates the order of centroid where 0 is an extreme vertex, 1 is an edge centroid, 2 is a face
 centroid, and n is the overall centroid. }
}
\references{
1. Pieple, G. F. "Programs for Generating Extreme Vertices and Centroids of Linearly Constrained Experimental Regions" 
\emph{Journal of Quality Technology}, Vol 20, No. 2, pp. 125-139, 1988.

}
\note{ This function calls the function Eflags to get error messages from cnvrt, the function Vertcen
to get the extreme vertices and centroids from cnvrt, and the function Nrows to get the number of vertices
and centroids from cnvrt.}
\author{ John S. Lawson \email{lawson@byu.edu}}
\examples{

data(conmx)
crvtave(1,conmx)

}
\keyword{datagen }
