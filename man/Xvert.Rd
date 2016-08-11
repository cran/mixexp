\name{Xvert}
\alias{Xvert}
\title{This function creates an extreme vertices design in a constrained mixture space.}
\description{This function calls the function crvtave to create an extreme vertices design in a constrained mixture space. 
If there are only three factors the function DesignPoints is called to plot the results. }
\usage{ 
Xvert(nfac=3,uc=c(0,0),lc=c(0,0),nlc=0,lb=c(0,0),ub=c(0,0),coef,ndm=0,plot=TRUE,
                 cornerlabs = c("x1","x2","x3"), axislabs = c("x1","x2","x3"),
                 pseudo=TRUE)  
}
\arguments{
  \item{nfac}{an integer representing the number of mixture variables in the design. Maximum nfac=12}
  \item{uc}{a vector of length nfac containing upper constraints on each mixture component}
  \item{lc}{a vector of length nfac containing lower constraints on each mixture component}
  \item{nlc}{the number of linear constraints, the default is zero}
  \item{lb}{a vector of length nlc containing the lower bounds for the linear constraints}
  \item{ub}{a vector of length nlc containing the upper bounds for the linear constraints}
  \item{coef}{an nlc by nfac matrix containing the coefficients of the components of the linear constraints}  
  \item{ndm}{an integer representing the highest order of centroids requested. An overall centroid is always included, 0 indicates no other centroids will be created, 1 indicates edge centroids are requested, etc.} 
  \item{plot}{a logical variable indicating whether a plot of the design is desired when there are only 3 components. Default is TRUE}
  \item{cornerlabs}{ This is a vector of text labels for the x1, x2 and x3 vertices. Use when there are only 3 components for plotting.}
  \item{axislabs}{ This is a vector of text labels for the x1, x2 and x3 axis. Use when there are only 3 components for plotting.}
  \item{pseudo}{logical variable, when TRUE plot in pseudo component space when there are lower constraints. }
}

\references{
1. Piepel, G. F. "Programs for Generating Extreme Vertices and Centroids of Linearly Consrtained Experimental Regions"
\emph{Journal of Quality Technology}, Vol 20, No. 2, pp. 125-139, 1988.

2. "John Lawson, Cameron Willden (2016).",
        "Mixture Experiments in R Using mixexp.",
        "Journal of Statistical Software, Code Snippets, 72(2), 1-20.",
        "doi:10.18637/jss.v072.c02"
}
\note{ This function calls crvtave. If the number of factors is 3, the  function DesignPoints is called to graph the results. }
\author{ John S. Lawson \email{lawson@byu.edu}}
\examples{

# Polvoron Example from Lawson
des<-Xvert(3,uc=c(.8,.95,.50),lc=c(0,.10,.05),ndm=1,plot=FALSE)


#Snee Marquardt(1976) example
Xvert(8,uc=c(.45,.50,.10,.4,.6,.2,.05,.05),lc=c(.1,.05,0,0,.1,.05,0,0),ndm=0)
    
# Example page 465
exvert<-Xvert(4,uc=c(.188,.128,.438,.438),lc=c(.124,.064,.374,.374),ndm=2)

# Example from Piepel 1988
coef<-matrix(c(.85,.9,1,.7,0,1),ncol=3,byrow=TRUE)
des<-Xvert(3,lc=c(.1,.1,0),uc=c(.5,.7,.7),nlc=2,lb=c(.9,.4),ub=c(.95,0),coef,ndm=1,plot=FALSE)

}
\keyword{datagen}
