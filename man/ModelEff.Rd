\name{ModelEff}
\alias{ModelEff}
\title{ This function creates mixture effect plots
}
\description{
This function makes effect plots using the Cox or Piepel directions in constrained mixture space.
}
\usage{
ModelEff(nfac=3,mod=1,nproc=0,dir=1,ufunc=mod,dimensions = list(NULL),
pvslice=c(1,1,1),lc=c(0,0,0,0,0,0,0,0,0,0,0,0), uc=c(1,1,1,1,1,1,1,1,1,1,1,1))
}
\arguments{
  \item{nfac}{The number of mixture components in the model (a number between 2 and 12)}			  
  \item{mod}{An integer representing the model to be traced: 1 for a linear model,
              2 for a quadratic model, and 3 for a cubic model, 4 for a special cubic model,
              5 for a mixture process variable model consisting of a full cross of quadratic 
              model in up to 5 mixture components and a linear model in up to 3 process variables,
              6 for Kowalski, Cornell and Vining's (2000) more parsimonious model for mixture
              process variable experiments. See the documentation for the MixModel function for a 
              description of the models.}
  \item{nproc}{The number of process variables in the model (a number between 1 and 3 for models 5 and 6)}
  \item{dir}{ an integer representing the direction for which the effect plot is made: 1 for
               Piepel direction, 2 for Cox direction.}
  \item{ufunc}{ A user function, this should an lm object created by the MixModel function. Any
               lm object will work if the terms are in the same order as the model produced by 
               the MixModel function.}
  \item{dimensions}{ A vector of names of mixture components in the lm object.}
  \item{pvslice}{A vector giving fixed values of the process variables.}
  \item{lc}{A vector giving the lower bounds of the mixture components.}
  \item{uc}{A vector giving the upper bounds of the mixture components.}
  
}
\value{
 \item{PX}{This is a matrix containing the coordinates of the effect plot traces that are plotted. }
}
\references{
1. Piepel, G. F. "Measuring Component Effects in Constrained Mixture Experiments" 
\emph{Technometrics}, Vol 25, pp. 97-105, 1982.

2. Lawson, J. and Willden, C. "Mixture Experiments in R , using mixexp"
\emph{JSS}, to appear.

3. Kowalski, S. M., Cornell, J. A. and Vining, G. G. "A  Model and Class of Designs for Mixture 
Experiments with Process Variables"
\emph{Communication in Statistics: Theory and Methods}, Vol 29, pp. 2255-2280.

}
\note{ This function calls the function crvtave to get the design centroid from cnvrt.}
\author{ John S. Lawson \email{lawson@byu.edu}}
\examples{

#Example p. 63-65 Cornell (control of Mites)
# Four Component Mixture
mite<-SCD(4)
yavg<-c(1.8,25.4,28.6,38.5,4.9,3.1,28.7,3.4,37.4,10.7,22.0,2.6,2.4,
        11.1,0.8)
mite<-cbind(mite,yavg)
miteSC<-MixModel(mite,"yavg",mixcomps=c("x1","x2","x3","x4"),model=4)
ModelEff(nfac=4,mod=4,nproc=0,dir=2,ufunc=miteSC,lc=c(0,0,0,0),uc=c(1,1,1,1))


# Cornell's (2002) Yarn elongation
x1<-c(1,1,.5,.5,.5,0,0,0,0,0,0,0,.5,.5,.5)
x2<-c(0,0,.5,.5,.5,1,1,.5,.5,.5,0,0,0,0,0)
x3<-c(0,0,0,0,0,0,0,.5,.5,.5,1,1,.5,.5,.5)
y<-c(11,12.4,15,14.8,16.1,8.8,10,10,9.7,11.8,16.8,16,17.7,16.4,16.6)
elong<-data.frame(x1,x2,x3,y)
testQ2<-MixModel(elong,"y",mixcomps=c("x1","x2","x3"),model=2)
ModelEff(nfac=3,mod=2,nproc=0,dir=2,ufunc=testQ2,lc=c(0,0,0),uc=c(1,1,1))

#### Kowalski Cornell and Vining Simplified model on data from Gallant et. al. (2008)
data(Burn)
testBNM<-MixModel(Burn,"y",mixcomps=c("Course","Fine","Binder"),model=6,procvars=c("z"))
ModelEff(nfac=3,mod=6,nproc=1,dir=1,ufunc=testBNM,dimensions = list(NULL), pvslice=c(1),
lc=c(.403,.166,.130),uc=c(.704,.412,.210))
ModelEff(nfac=3,mod=6,nproc=1,dir=1,ufunc=testBNM,dimensions = list(NULL), pvslice=c(-1),
lc=c(.403,.166,.130),uc=c(.704,.412,.210))



}
\keyword{hplot }
