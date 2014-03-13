\name{ModelPlot}
\alias{ModelPlot}
\title{This function makes contour plots of a user-supplied model in the simplex mixture space.
}
\description{
This function makes contour plots in the simplex mixture space.  It also can draw constraint lines and zoom in on pseudo component region.
}
\usage{
ModelPlot(model=NULL,user.func = NULL, dimensions = list(x1=NULL,x2=NULL,x3=NULL),
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
  \item{model}{an lm object, MixModel object, or any other model object that is compatible with the predict function, which is the mixture model to be plotted.}
  \item{user.func}{function supplied by the user that takes as arguments a dataframe called 'grid' and returns the predictions.  This argument has been deprecated in favor of the model argument. Typically, this will be a wrapper function for predict() (e.g. predict(model,newdata=grid)). Additional arguments for user.func can be passed using the ellipsis argument for ModelPlot.  Overrides model argument if both are specified.}  
  \item{dimensions}{list argument that specifies the mixture variables to be plotted on the ternary plot. Values must correspond to variable names from the user-supplied model.}
  \item{slice}{list argument that specifies the value of fixed mixture components.}
  \item{lims}{vector of lower and upper constraints for ternary plot components (TopLower, TopUpper, LeftLower, LeftUpper, RightLower, RightUpper).}
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
  \item{cornerlab.pars}{list of graphical parameters controlling the appearance of the axis labels.}
  \item{grid}{logical argument.  If true, adds gridlines to the ternary plot.}
  \item{grid.pars}{list of graphical parameters controlling the appearance of the gridlines.}
  \item{colorkey}{logical or list of parameters.  See levelplot documentation for more details.}
  \item{labels}{logical argument.  If true, labels contour lines.}
  \item{label.style}{controls placement of contour labels.  Choose from "mixed","flat", or "align." See panel.levelplot documentation for more details.}
  \item{...}{additional arguments passed to user.func}
}

\references{
1. Cornell, J. A.  \emph{Experiments with Mixtures: Models and Analysis of Mixture Data}, John Wiley & Sons, New York, third edition, 2002.

2. See R Ternary Level Plot Function http://www.siftp.net/index.shtml

}

\author{Cameron Willden \email{ccwillden@gmail.com}}
\examples{
# Cornell's (2002) Yarn elongation
x1<-c(1,1,.5,.5,.5,0,0,0,0,0,0,0,.5,.5,.5)
x2<-c(0,0,.5,.5,.5,1,1,.5,.5,.5,0,0,0,0,0)
x3<-c(0,0,0,0,0,0,0,.5,.5,.5,1,1,.5,.5,.5)
y<-c(11,12.4,15,14.8,16.1,8.8,10,10,9.7,11.8,16.8,16,17.7,16.4,16.6)
elong<-data.frame(x1,x2,x3,y)
testQ<-lm(y~-1+x1+x2+x3+x1:x2+x1:x3+x2:x3,data=elong)
ModelPlot(model = testQ,dimensions = list(x1="x1",x2="x2",x3="x3"), 
           main="Thread Elongation",constraints=FALSE,contour=TRUE,
           at=c(12, 13, 14, 15, 16, 17),fill=FALSE,
           axislabs=c("X1", "X2", "X3"),
           cornerlabs = c("X1", "X2", "X3"),pseudo=FALSE)
           

# Cornells famous fish patty experiment 
data(fishp)
fishmod2<-MixModel(fishp, "y", mixcomps=c("x1","x2","x3"),model=5,procvars=c("z1","z2","z3"))
ModelPlot(fishmod2,dimensions = list(x1="x1",x2="x2",x3="x3"), 
          slice = list(process.vars=c(z1=-1, z2=-1, z3=-1)), main="z1=-1, z2=-1, z3=-1",
          constraints=FALSE,contour=TRUE,cuts=10,fill=FALSE,
          axislabs=c("Fraction X1","Fraction X2","Fraction X3"),
          cornerlabs = c("X1", "X2", "X3"),pseudo=FALSE)
          
          
#### Kowalski Cornell and Vining Simplified model on data from Gallant et. al. (2008)
data(Burn)
testBNM<-MixModel(Burn,"y",mixcomps=c("Course","Fine","Binder"),model=6,procvars=c("z"))
ModelPlot(testBNM,dimensions = list(x1="Course",x2="Fine",x3="Binder"), 
          slice = list(process.vars=c(z=1)), lims=c(.403,.704,.166,.467,.130,.431), main="z=1",
          constraints=TRUE,contour=TRUE,cuts=5,fill=FALSE,
          axislabs=c("Fraction Course","Fraction Fine","Fraction Binder"),
          cornerlabs = c("Course", "Fine", "Binder"),pseudo=TRUE)

ModelPlot(testBNM,dimensions = list(x1="Course",x2="Fine",x3="Binder"), 
          slice = list(process.vars=c(z=-1)),  lims=c(.403,.704,.166,.467,.130,.431),main="z=-1",
          constraints=TRUE,contour=TRUE,cuts=5,fill=FALSE,
          axislabs=c("Fraction Course","Fraction Fine","Fraction Binder"),
          cornerlabs = c("Course", "Fine", "Binder"),pseudo=TRUE)

          
}
\keyword{hplot }






