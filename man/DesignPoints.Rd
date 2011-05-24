\name{DesignPoints}
\alias{DesignPoints}
\title{This function plots design points and or constraints in the simplex mixture space.}
\description{ This function plots design points and or constraints in the simplex mixture space. It calls the function MixturePlot that does the actual plotting.}
\usage{ 
DesignPoints(des,x,y,z,x1lower,x1upper,x2lower,x2upper,x3lower,x3upper)  
}
\arguments{
  \item{des}{data frame containing x1 x2 and x3 coordinates of data points to be plotted}
  \item{x}{ vector of x3 coordinates of design points to be plotted} 
  \item{y}{ vector of x2 coordinates of design points to be plotted}
  \item{z}{ vector of x1 coordinates of design points to be plotted}
  \item{x1lower}{ This is the lower constraint on x1}
  \item{x1upper}{ This is the upper constraint on x1}
  \item{x2lower}{ This is the lower constraint on x2}
  \item{x2upper}{ This is the upper constraint on x2}
  \item{x3lower}{ This is the lower constraint on x3}
  \item{x3upper}{ This is the upper constraint on x3}
}

\references{
1. Pieple, G. F. "Programs for Generating Extreme Vertices and Centroids of Linearly Consrtained Experimental Regions"
\emph{Journal of Quality Technology}, Vol 20, No. 2, pp. 125-139, 1988.

}
\note{ This function calls MixturePlot. If either des and x,y,z are missing no design points will be plotted, and if x1lower, x1upper, etc. are all zero no constraints will be plotted. }
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

DesignPoints(x1lower=0,x1upper=.8,x2lower=.10,x2upper=.95,x3lower=.05,x3upper=.50)  
}
\keyword{}
