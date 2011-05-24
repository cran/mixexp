\name{Xvert}
\alias{Xvert}
\title{This function creates an extreme vertices design in a constrained mixture space.}
\description{This function calls the function crvtave to create an extreme vertices design in a constrained mixture space. 
If there are only three factors the function DesignPoints is called to plot the results. }
\usage{ 
Xvert(nfac,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,ndm) 
}
\arguments{
  \item{nfac}{an integer representing the number of mixture variables in the design}
  \item{x1}{a vector containing lower and upper constraints on x1}
  \item{x2}{a vector containing lower and upper constraints on x2}
  \item{x3}{a vector containing lower and upper constraints on x3}
  \item{x4}{a vector containing lower and upper constraints on x4}
  \item{x5}{a vector containing lower and upper constraints on x5}
  \item{x6}{a vector containing lower and upper constraints on x6}
  \item{x7}{a vector containing lower and upper constraints on x7}
  \item{x8}{a vector containing lower and upper constraints on x8}
  \item{x9}{a vector containing lower and upper constraints on x9}
  \item{x10}{a vector containing lower and upper constraints on x10}
  \item{x11}{a vector containing lower and upper constraints on x11}
  \item{x12}{a vector containing lower and upper constraints on x12}
  \item{ndm}{an integer representing the highest order of centroids requested. An overall centroid is always included, 0 indicates no other centroids will be created, 1 indicates edge centroids are requested, etc.} 
}

\references{
1. Pieple, G. F. "Programs for Generating Extreme Vertices and Centroids of Linearly Consrtained Experimental Regions"
\emph{Journal of Quality Technology}, Vol 20, No. 2, pp. 125-139, 1988.

}
\note{ This function calls crvtave. If the number of factors is 3, the  function DesignPoints is called to graph the results. }
\author{ John S. Lawson \email{lawson@byu.edu}}
\examples{

# Polvoron Example from Lawson
des<-Xvert(3,x1=c(0,.8),x2=c(.10,.95),x3=c(.05,.50),ndm=2)

# Exercise 11.3 DAE with SAS
Xvert(3,x1=c(.2,.8),x2=c(.1,.4),x3=c(.1,.5))

#Snee Marquardt(1976) example
Xvert(8,x1=c(.1,.45),x2=c(.05,.50),x3=c(0,.10),x4=c(0,.4),x5=c(.1,.6),x6=c(.05,.2),
      x7=c(0,.05),x8=c(0,.05),ndm=0)
	  
# Example page 465
exvert<-Xvert(4,x1=c(.124,.188),x2=c(.064,.128),x3=c(.374,.438),x4=c(.374,.438),ndm=2)


}
\keyword{}
