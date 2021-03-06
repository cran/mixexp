\name{EffPlot}
\alias{EffPlot}
\title{ This function creates mixture effect plots
}
\description{
This function makes effect plots using the Cox or Piepel directions in constrained mixture space.
}
\usage{
EffPlot(des=NULL,nfac=3,mod=1,dir=1)
}
\arguments{
  \item{des}{ data frame containing the design points and response data for a mixture experiment. The 
              data frame must contain the variables x1, x2 ...xn for the mixture variables, 
              and y for the response. n must be between 2 and 12. Only effect
              plots for linear models can be made when the number of factors is greater than 6.}
  \item{nfac}{The number of mixture components in the model.}			  
  \item{mod}{ an interger representing the model to be traced: 1 for a linear model,
              2 for a quadratic model, and 4 for a special cubic model. For models other than these, use the ModelEff function.}
  \item{dir}{ an integer representing the direction for which the effect plot is made: 1 for
               Piepel direction, 2 for Cox direction.}
}
\value{
 \item{PX}{This is a matrix containing the coordinates of the effect plot traces that are plotted. }
}
\references{
1. Piepel, G. F. "Measuring Component Effects in Constrained Mixture Experiments" 
\emph{Technometrics}, Vol 25, pp. 97-105, 1982.

2. "John Lawson, Cameron Willden (2016).",
        "Mixture Experiments in R Using mixexp.",
        "Journal of Statistical Software, Code Snippets, 72(2), 1-20.",
        "doi:10.18637/jss.v072.c02".
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
EffPlot(des,2,2)


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
EffPlot(des,mod=1,dir=1)



# Weed control example from Lawson & Erjavec
x1<-c(1,0,0,.5,.5,0,.33333,.33333,.33333)
x2<-c(0,1,0,.5,0,.5,.33333,.33333,.33333)
x3<-c(0,0,1,0,.5,.5,.33333,.33333,.33333)
y<-c(73,68,80,77,86,75,92,93,88)
des<-data.frame(x1,x2,x3,y)
EffPlot(des,3)



# Polvoron Example from Lawson
des<-Xvert(3,uc=c(.8,.95,.50),lc=c(0,.10,.05),ndm=1,plot=FALSE)
dat<-as.matrix(des)
# remove the edge centroid at the top
dat<-dat[c(1:6,8:11), ]
# add two more centroids
dat<-rbind(dat,dat[10, ],dat[10,])
# response vector
y<-c(5.75,3.69,5.33,5.68,3.85,3.83,5.88,5.87,5.23,6.54,6.82,6.41)
# make the data frame for plotting
des<-data.frame(dat[,1:3],y)
EffPlot(des,3)

# Cornell's example of blending pesticides for control of mites (special cubic model)
mite<-SCD(4)
yavg<-c(1.8,25.4,28.6,38.5,4.9,3.1,28.7,3.4,37.4,10.7,22.0,2.6,2.4,
        11.1,0.8)
mite<-cbind(mite,yavg)
mite2<-mite
names(mite2)<-c("x1","x2","x3","x4","y")
EffPlot(des=mite2,mod=4,dir=2)
}
\keyword{hplot }
