\name{Fillv}
\alias{Fillv}
\title{This function duplicates SAS macro adxfill.}
\description{This function creates interior points in a mixture design by averaging all 
possible pairs of design points. }
\usage{ 
Fillv(nfac,des) 
}
\arguments{
  \item{nfac}{an integer representing the number of mixture variables in the design}
  \item{des}{a data frame containing a mixture design created by one of the functions SLD, SCD or Xvert}
 }
\author{ John S. Lawson \email{lawson@byu.edu}}
\examples{

# Example 1 fills interior of Simplex Lattice Design
des<-SLD(3,3)
DesignPoints(des)
des2<-Fillv(3,des)
DesignPoints(des2)

# Example 2 fills interior of Simplex Centroid Design
des<-SCD(4)
Fillv(4,des)

# Example 3 fills interior of Extreme vertices design
ev<-Xvert(3,uc=c(.1,.1,1.0),lc=c(0,0,0),ndm=1)
ev2<-Fillv(3,ev)

}
\keyword{datagen}
