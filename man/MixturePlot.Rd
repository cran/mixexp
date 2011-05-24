\name{MixturePlot}
\alias{MixturePlot}
\title{ This function makes contour plots in the simplex mixture space.
}
\description{
This function makes contour plots in the simplex mixture space, it also can draw constraint lines and show design points.
}
\usage{
MixturePlot(x,y,z,w,des,n.breaks,res,lims,color.palette,constrts,
            contrs,cols,despts,mod,x3lab,x2lab,x1lab,corner.labs)
}
\arguments{
  \item{x}{x3 locations for known points} 
  \item{y}{x2 locations for known points} 
  \item{z}{x1 locations for known points} 
  \item{w}{y locations for known points} 
  \item{des}{data frame with x1,x2,x3, and y locations for known points} 
  \item{n.breaks}{number of breaks between levels} 
  \item{res}{number of color blocks between 0 and 1 of x} 
  \item{lims}{vector of lower and upper constraints for x1,x2,x3}
  \item{color.palette}{is the color palette to use}
  \item{constrts}{if TRUE constraints found in lines will be added to the graph} 
  \item{contrs}{if TRUE contour lines will be added to the graph}   
  \item{cols}{if TRUE regions between contour lines will be colored} 
  \item{despts}{if TRUE plots the design points in data frame des} 
  \item{mod}{is an indicator for the model 1=linear, 2=quadratic, 3=special cubic} 
  \item{x3lab}{label for the x3 axis}  
  \item{x2lab}{label for the x2 axis}  
  \item{x1lab}{label for the x1 axis} 
  \item{corner.labs}{labels for x3, x2 and x1 vertices}    
}

\references{
1. Cornell, J. A.  \emph{Experiments with Mixtures: Models and Analysis of Mixture Data}, John Wiley & Sons, New York, third edition, 2002.

2. See R Ternary Level Plot Function http://www.siftp.net/index.shtml

}

\author{ John S. Lawson \email{lawson@byu.edu}}
\examples{

##Usage and Examples - Example from page 458 DAE with SAS
dat = data.frame(
          "x1"=c(1,.8,.6,.5,.5,.33333,.3,.3,.1,.1,0,0,0),
          "x2"=c(0,.1,.2,0,.5,.33333,.2,.5,.1,.8,0,.5,1),
          "x3"=c(0,.1,.2,.5,0,.33333,.5,.2,.8,.1,1.0,.5,0),
          "y"=c(48.7,49.5,50.2,52.8,49.3,51.1,52.7,50.3,60.7,49.9,64.9,53.5,50.6) 
                 )
MixturePlot(dat$x3,dat$x2,dat$x1,dat$y, x3lab="Fraction x3",
  x2lab="Fraction x2", x1lab="Fraction x1", corner.labs=c("x3","x2","x1"),
  constrts=FALSE,contrs=TRUE,cols=TRUE, mod=2,n.breaks=9)

# Weed control example from Lawson & Erjavec
x1<-c(1,0,0,.5,.5,0,.33333,.33333,.33333)
x2<-c(0,1,0,.5,0,.5,.33333,.33333,.33333)
x3<-c(0,0,1,0,.5,.5,.33333,.33333,.33333)
y<-c(73,68,80,77,86,75,92,93,88)
des<-data.frame(x1,x2,x3,y)
MixturePlot(des=des,x3lab="Fraction C",x2lab="Fraction B",
   x1lab="Fraction A",corner.labs=c("C","B","A"),mod=3,n.breaks=5,cols=TRUE)

}
\keyword{ }
