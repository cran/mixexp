\name{ModelPlot}
\alias{ModelPlot}
\title{This function makes contour plots of a user supplied function in the simplex mixture space.
}
\description{
This function makes contour plots in the simplex mixture space, it also can draw constraint lines and show design points.
}
\usage{
ModelPlot(user.func = NULL, dimensions = list(x1=NULL,x2=NULL,x3=NULL),
          slice=NULL,lims=rep(0,6), constraints = FALSE,
          constraint.pars = list(lty=2,lwd=2),
          contour = FALSE, contour.pars = list(lwd=0.5,cex.lab=1.3),
          cuts = 10,at = NULL, res=300, pseudo=FALSE, 
          fill=FALSE, color.palette = heat.colors, 
          main=NULL, axislabs=c("Fraction X1","Fraction X2","Fraction X3"),
          axislab.pars = list(),
          axislab.offset=0,
          cornerlabs = c("X1", "X2", "X3"),
          cornerlab.pars = list(),
          grid=TRUE, grid.pars = list(col='darkgrey',lty=3,lwd=0.5),
          colorkey = FALSE,
          labels=TRUE, label.style="align", ...)
}
\arguments{
  \item{user.func}{function supplied by the user that takes as arguments a dataframe called 'grid' and returns the predictions.  Typically, this will be a wrapper function for the predict() (e.g. predict(model,newdata=grid)). Additional arguments for the user.func besides 'grid' will typically be used to fix values of process variables.}  
  \item{dimensions}{list argument that specifies the mixture variables to be plotted on the ternary plot. Values must correspond to variable names from the user-supplied model.}
  \item{slice}{list argument that specifies the value of fixed mixture components.}
  \item{lims}{vector of lower and upper constraints for ternary plot components (top, left, right).}
  \item{constraints}{if TRUE constraints found in lims will be added to the graph.}
  \item{constraint.pars}{list of graphical parameters controlling the appearance of the constraint lines.}
  \item{contour}{if TRUE contour lines will be added to the graph.}
  \item{contour.pars}{list of graphical parameters controlling the appearance of the contour lines.}
  \item{cuts}{number of breaks between levels (used for contours if 'at' not specified).}
  \item{at}{list of contour levels (e.g. at=c(1,3,5,10) will draw contours at those heights). Overrides cuts argument.}
  \item{res}{resolution of the grid.  Corresponds to number equally spaced values along the baseline of the simplex.}
  \item{pseudo}{if TRUE uses pseudo components to zoom in on constrained region. Will create the smallest equilateral triangle that still contains the whole constrained region.}
  \item{fill}{if TRUE regions between contour lines will be colored.}
  \item{color.palette}{is the color palette to use.}
  \item{main}{character value for main title or list containing character value and graphical parameters (e.g. main=list("main title",cex=2)).}
  \item{axislabs}{character vector of axis labels for ternary components.}
  \item{axislab.pars}{list of graphical parameters controlling the appearance of the axislabels.}
  \item{axislab.offset}{numeric value that creates or eliminates space between the angled axislabels and the tickmarks. Prevents axis labels from overlapping with tickmarks.  Typically, absolute value would not exceed 0.05. }
  \item{cornerlabs}{character vector of corner labels for x1, x2 and x3 vertices.}
  \item{cornerlab.pars}{list of graphical parameters controlling the appearance of the axislabels.}
  \item{grid}{logical argument.  If true, adds grid lines to the ternary plot.}
  \item{grid.pars}{list of graphical parameters controlling the appearance of the gridlines.}
  \item{colorkey}{logical or list of parameters.  See levelplot documentation for more details.}
  \item{labels}{logical argument.  If true, labels contour lines.}
  \item{label.style}{controls placement of contour labels.  Choose from "mixed","flat", or "align." See panel.levelplot documentation for more details.}
  \item{...}{expressions evaluated in the context of the function that creates the grid. Typically, will be used to fix values of process variables.}
}

\references{
1. Cornell, J. A.  \emph{Experiments with Mixtures: Models and Analysis of Mixture Data}, John Wiley & Sons, New York, third edition, 2002.

2. See R Ternary Level Plot Function http://www.siftp.net/index.shtml

}

\author{Cameron Willden \email{ccwillden@gmail.com}}
\examples{
mite<-SCD(4)
yavg<-c(1.8,25.4,28.6,38.5,4.9,3.1,28.7,3.4,37.4,10.7,22.0,2.6,2.4,
11.1,0.8)
mitemd<-MixModel(cbind(mite,yavg),"yavg",c("x1","x2","x3","x4"),4)  
### plot of this model looks different than article
#mitemd<-lm(yavg~-1+x1+x2+x3+x4+x1:x2+x1:x3+x1:x4+x2:x3+x2:x4+x3:x4+ 
### plot of thhis model looks like article
#x1:x2:x3+x1:x2:x4+x1:x3:x4+x2:x3:x4+x1:x2:x3:x4, data=cbind(mite,yavg))
miteprd=function(grid)
 { ygrid=predict(mitemd,grid)
     return(ygrid)
 }
ModelPlot(miteprd,dimensions = list(x1="x1",x2="x2",x3="x3"), 
slice = list(x4=0.0), main="Dibrom = 0.0",
constraints=FALSE,contour=TRUE,at=c(5,10,15,20),fill=FALSE,
axislabs=c("Vendex","Omite","Kelthane"),
cornerlabs = c("X1", "X2", "X3"),pseudo=FALSE)

ModelPlot(miteprd,dimensions = list(x1="x1",x2="x2",x3="x3"), 
slice = list(x4=0.4), main="Dibrom = 0.4",
constraints=FALSE,contour=TRUE,cuts=5,fill=FALSE,
axislabs=c("Vendex","Omite","Kelthane"),
cornerlabs = c("X1", "X2", "X3"),pseudo=FALSE)


}
\keyword{hplot }






