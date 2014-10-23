\name{MixModel}
\alias{MixModel}
\title{Fit mixture and mixture process variable models.}
\description{
This function fits mixture models (1)-(3) and mixture process models (5)-(6)
described in Lawson and Wilden(2015) "Mixture Experiments in R, using mixexp", Journal 
Statistical Software http://www/jstatsoft.org/
} 
\usage{
MixModel(frame, response, mixcomps=NULL,model,procvars=NULL)
}
\arguments{
  \item{frame}{a data frame containing columns with the mixture components, process variables,
               and responses}
  \item{response}{a character variable containing the column name of the response variable in
                  frame to be fit}
  \item{mixcomps}{a character vector of column names of the mixture components in frame}
  \item{model}{ an integer in the range of 1 to 6, indicating the model to be fit}
  \item{procvars}{a character vector of column names of the process variables in frame to be 
                  included in the model. Leave this out if there are no process variables in 
                  frame}
          }        
\references{
1. Lawson, J. and Wilden, C., "Mixture Experiments in R, using mixexp", Journal of Statistical Software, 2015
           }  
\author{ John S. Lawson \email{lawson@byu.edu}}

\examples{
# example from Lawson(2014), quadratic model
library(daewr)
data(pest)
mixvars<-c("x1","x2","x3")
MixModel(pest,"y",mixvars,2)

# example from Myers and Montgomery(2002), special cubic model
library(mixexp)
etch<-SCD(3)
etch<-Fillv(3,etch)
etch<-rbind(etch[1:7, ],etch[1:3, ],etch[7, ], etch[etch$x1==2/3, ],
etch[etch$x2==2/3, ],etch[etch$x3==2/3, ])
erate<-c(540,330,295,610,425,330,800,560,350,260,850,710,640,460)
etch<-cbind(etch,erate)
mixvars<-c("x1","x2","x3")
response<-c("erate")
MixModel(etch,response,mixvars,4)

# example Mixture process variable model from Sahni, Pieple and Naes(2009)
library(daewr)
mixvars<-c("x1","x2","x3")
procvars<-c("z1","z2")
data(MPV)
MixModel(MPV,"y",mixvars,5,procvars)

# example of Kowalski, Cornell and Vining's new model for mixture-process experiments
library(mixexp)
data(Burn)
MixModel(Burn,"BR7.0",c("u1","u2",'u3'),6,"z")

          }
\keyword{regression}
 
