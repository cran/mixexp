\name{Effplot}
\alias{Effplot}
\title{ This function creates mixture effect plots
}
\description{
This function makes effect plots using the Cox or Pieple directions in constrained mixture space.
}
\usage{
Effplot(des=NULL,nfac=3,mod=1,dir=1,ufunc=NULL,dimensions = list(NULL),
x1=c(0,1),x2=c(0,1),x3=c(0,1),x4=c(0,1),x5=c(0,1),x6=c(0,1),x7=c(0,1),
x8=c(0,1),x9=c(0,1),x10=c(0,1),x11=c(0,1),x12=c(0,1))
}
\arguments{
  \item{des}{ data frame containing the design points and response data for a mixture experiment. The 
              data frame must contain the variables x1, x2 ...xn for the mixture variables, 
              and y for the response. n must be between 2 and 12. Only effect
              plots for linear models can be made when the number of factors is greater than 6.}
  \item{nfac}{The number of mixture components in the model.}			  
  \item{mod}{ an interger representing the model to be traced: 1 for a linear model,
              2 for a quadratic model, and 3 for a special cubic model, 4 for a user model.}
  \item{dir}{ an interger representing the direction for which the effect plot is made: 1 for
               Pieple direction, 2 for Cox direction.}
  \item{ufunc}{ A user function, this should be an lm object.}
  \item{dimensions}{ A vector of names of mixture components in the lm object if it is supplied.}
  \item{x1}{a vector containing lower and upper constraints on x1.}
  \item{x2}{a vector containing lower and upper constraints on x2.}
  \item{x3}{a vector containing lower and upper constraints on x3.}
  \item{x4}{a vector containing lower and upper constraints on x4.}
  \item{x5}{a vector containing lower and upper constraints on x5.}
  \item{x6}{a vector containing lower and upper constraints on x6.}
  \item{x7}{a vector containing lower and upper constraints on x7.}
  \item{x8}{a vector containing lower and upper constraints on x8.}
  \item{x9}{a vector containing lower and upper constraints on x9.}
  \item{x10}{a vector containing lower and upper constraints on x10.}
  \item{x11}{a vector containing lower and upper constraints on x11.}
  \item{x12}{a vector containing lower and upper constraints on x12.}
}
\value{
 \item{PX}{This is a matrix containing the coordinates of the effect plot traces that are plotted. }
}
\references{
1. Piepel, G. F. "Measuring Component Effects in Constrained Mixture Experiments" 
\emph{Technometrics}, Vol 25, pp. 97-105, 1982.

}
\note{ This function calls the function crvtave to get the design centroid from cnvrt.}
\author{ John S. Lawson \email{lawson@byu.edu}}
\examples{




#Example from Li, Tolley, Lee(2010) response is perm
x1<-c(.572,.358,.286,.286,.286,.143,.357)
x2<-c(.214,.428,.500,.357,.214,.500,.500)
x3<-c(.214,.214,.214,.357,.500,.357,.143)
y<-c(7.7,18.4,24.2,9.8,5.9,23.0,19.4)
des<-data.frame(x1,x2,x3,y)
Effplot(des,2,2)


#Example from Snee, Marquart(1976)
x1<-c(.1,.1,.1,.15,.1,.1,.1,.4,.35,.30,.1,.45,.45,.45,.45,.45,.259,.259,.259,.259)
x2<-c(.5,.05,.5,.05,.05,.5,.05,.05,.05,.5,.5,.05,.2,.15,.25,.1,.222,.222,.222,.222)
x3<-c(0,0,0,0,.1,.1,.1,.1,.1,0,.1,0,0,0,.1,.1,.05,.05,.05,.05)
x4<-c(0,0,.1,.1,0,.1,.1,.1,.1,0,0,0,.1,.1,0,0,.05,.05,.05,.05)
x5<-c(.1,.55,.1,.6,.55,.1,.55,.1,.1,.1,.2,.45,.1,.1,.1,.1,.244,.244,.244,.244)
x6<-c(.2,.2,.2,.05,.2,.05,.05,.2,.2,.05,.05,.05,.05,.2,.05,.2,.125,.125,.125,.125)
x7<-c(.05,.05,0,.05,0,0,0,.05,.05,0,.05,0,.05,0,.05,0,.025,.025,.025,.025)
x8<-c(.05,.05,0,0,0,.05,.05,0,.05,.05,0,0,.05,0,0,.05,.025,.025,.025,.025)
y<-c(30,113,17,94,89,18,90,20,21,15,28,48,18,7,16,19,38,30,35,40)
des<-data.frame(x1,x2,x3,x4,x5,x6,x7,x8,y)
Effplot(des,mod=1,dir=1)



# Weed control example from Lawson & Erjavec
x1<-c(1,0,0,.5,.5,0,.33333,.33333,.33333)
x2<-c(0,1,0,.5,0,.5,.33333,.33333,.33333)
x3<-c(0,0,1,0,.5,.5,.33333,.33333,.33333)
y<-c(73,68,80,77,86,75,92,93,88)
des<-data.frame(x1,x2,x3,y)
Effplot(des,3)



# Polvoron Example from Lawson
des<-Xvert(x1=c(0,.8),x2=c(.10,.95),x3=c(.05,.50),ndm=1)
dat<-as.matrix(des)
# remove the edge centroid at the top
dat<-dat[c(1:6,8:11), ]
# add two more centroids
dat<-rbind(dat,dat[10, ],dat[10,])
# response vector
y<-c(5.75,3.69,5.33,5.68,3.85,3.83,5.88,5.87,5.23,6.54,6.82,6.41)
# make the data frame for plotting
des<-data.frame(dat[,1:3],y)
Effplot(des,3)


}
\keyword{hplot }
