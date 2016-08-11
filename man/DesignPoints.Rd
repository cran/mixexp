\name{DesignPoints}
\alias{DesignPoints}
\title{This function plots design points and or constraints in the simplex mixture space, given a data frame containing the design or vextors x, y, and z of the same length that contain the mixture components in the design.}
\description{ This function plots design points and or constraints in the simplex mixture space. It calls the function MixturePlot that does the actual plotting.}
\usage{ 
DesignPoints(des = NULL,nmxcmp=3, x = NULL, y = NULL, z = NULL,x1lower=0,x1upper=0,
                         x2lower=0, x2upper=0, x3lower=0,x3upper=0,
                         cornerlabs = c("x3","x2","x1"),
                         axislabs=c("x1","x2","x3"),pseudo=FALSE)
}
\arguments{
  \item{des}{data frame containing x1 x2 and x3 coordinates of data points to be plotted}
  \item{nmxcmp}{interger indicating the number of mixture components in the design}
  \item{x}{ vector of x3 coordinates of design points to be plotted} 
  \item{y}{ vector of x2 coordinates of design points to be plotted}
  \item{z}{ vector of x1 coordinates of design points to be plotted}
  \item{x1lower}{lower constraint on x1}
  \item{x1upper}{upper constraint on x1}
  \item{x2lower}{lower constraint on x2}
  \item{x2upper}{upper constraint on x2}
  \item{x3lower}{lower constraint on x3}
  \item{x3upper}{upper constraint on x3}
  \item{axislabs}{ This is a vector of text labels for the x1, x2 and x3 axis.}
  \item{cornerlabs}{ This is a vector of text labels for the x1, x2 and x3 vertices.}
  \item{pseudo}{logical variable, when TRUE plot is made in pseudo component space bounded by the lower constraints of each component. }
}

\references{
1. Piepel, G. F. "Programs for Generating Extreme Vertices and Centroids of Linearly Consrtained Experimental Regions"
\emph{Journal of Quality Technology}, Vol 20, No. 2, pp. 125-139, 1988.

2. "John Lawson, Cameron Willden (2016).",
        "Mixture Experiments in R Using mixexp.",
        "Journal of Statistical Software, Code Snippets, 72(2), 1-20.",
        "doi:10.18637/jss.v072.c02"
}
\note{ This function calls MixturePlot. If either des and x,y,z are missing no design points will be plotted, and if x1lower, x1upper, etc. are all zero no constraints will be plotted. If there are more than 3 columns of mixture components in des, only the first 3 will be plotted ignoring the others. }
\author{ John S. Lawson \email{lawson@byu.edu}}
\examples{
dat<-SCD(3)
DesignPoints(des=dat)

x1<-c(1,0,0,.5,.5, 0,.33333)
x2<-c(0,1,0,.5,0,.5,.33333)
x3<-c(0,0,1,0,.5,.5,.33333)
DesignPoints(x=x3,y=x2,z=x1)

dat<-data.frame(x1,x2,x3)
DesignPoints(des=dat)

}
\keyword{hplot}
