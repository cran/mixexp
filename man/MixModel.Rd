\name{MixModel}
\alias{MixModel}
\title{Fit mixture and mixture process variable models.}
\description{
This function fits mixture models (1)-(4) and mixture process models (5)-(6)
described in Lawson and Willden(2015) "Mixture Experiments in R, using mixexp", Journal 
Statistical Software http://www/jstatsoft.org/, and prints the correct R square and standard errors of model coefficients.
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
  \item{model}{ an integer in the range of 1 to 6, indicating the model to be fit:
  
  \eqn{1.  y=\sum_{i=1}^{q} \beta_i x_i + \epsilon.}{1. y = sum from 1 to q (\beta(i)x(i))  + \epsilon}.
  
  \eqn{2. y=\sum_{i=1}^{q} \beta_i x_i + \sum_{i=1}^{q-1} \sum_{j=i+1}^{q} \beta_{ij} x_i x_j + \epsilon.}{2. y = sum from i=1 to q (\beta(i)x(i)) + sum from i=1 to q-1 sum j=i+1 to q (\beta(ij)x(i)x(j)) + \epsilon}.
  
  
  \eqn{3. y=\sum_{i=1}^{q} \beta_i x_i + \sum_{i=1}^{q-1} \sum_{j=i+1}^{q} \beta_{ij} x_i x_j  + \sum_{i=1}^{q-1} \sum_{j=i+1}^{q} \delta_{ij} x_i x_j (x_i - x_j) + \sum_{i=1}^{q-2} \sum_{j=i+1}^{q-1} \sum_{k=j+1}^{q} \beta_{ijk} x_i x_j x_k + \epsilon.}{3. y = sum from i=1 to q (\beta(i)x(i)) + sum from i=1 to q-1 sum j=i+1 to q (\beta(ij)x(i)x(j)) + sum i=1 to q-1 sum j=i+1 to q (\delta(ij)x(i)x(j)(x(i)-x(j)) + sum i=1 to q-2 sum j=i+1 to q-1 sum k=j+1 to q (\beta(ijk)x(i)x(j)x(k)) + \epsilon}.
  
  \eqn{4. y=\sum_{i=1}^{q} \beta_i x_i + \sum_{i=1}^{q-1} \sum_{j=i+1}^{q} \beta_{ij} x_i x_j   + \sum_{i=1}^{q-2} \sum_{j=i+1}^{q-1} \sum_{k=j+1}^{q} \beta_{ijk} x_i x_j x_k + \epsilon.}{4. y = sum from i=1 to q (\beta(i)x(i)) + sum from i=1 to q-1 sum j=i+1 to q (\beta(ij)x(i)x(j))  + sum i=1 to q-2 sum j=i+1 to q-1 sum k=j+1 to q (\beta(ijk)x(i)x(j)x(k)) + \epsilon}.
  
  
  \eqn{5. y = ( \sum_{i=1}^{q} \beta_i x_i + \sum_{i=1}^{q-1} \sum_{j=i+1}^{q} \beta_{ij} x_i x_j )(\alpha_0 + \sum_{l=1}^{p} \alpha_l z_l + \sum_{l=1}^{p-1} \sum_{m=l+1}^{p} \alpha_{lm} z_l z_m ) + \epsilon}{5. y = (sum from i=1 to q (\beta(i)x(i)) + sum from i=1 to q-1 sum j=i+1 to q (\beta(ij)x(i)x(j))) (\alpha(0)+sum from l=1 to p (\alpha(l)z(l)) + sum from l=1 to p-1 sum from m=l+1 to p (\alpha(lm)z(l)z(m)))+ \epsilon}.
  
  \eqn{6. y=\sum_{i=1}^{q} \beta_i^{(0)} x_i + \sum_{i=1}^{q-1} \sum_{j=i+1}^{q} \beta_{ij}^{(0)} x_i x_j  + \sum_{k=1}^m \left[\sum_{i=1}^{q} \beta_i^{(1)} x_i \right]z_k + \sum_{k=1}^{m-1} \sum_{l=k+1}^m \alpha_{kl} z_k z_l + \sum_{k=1}^m \alpha_{kk} z_k^2 + \epsilon.}{6.  y = sum from i=1 to q (\beta^0(i)x(i)) + sum from i=1 to q-1 sum j=i+1 to q (\beta^0(ij)x(i)x(j)) + sum from k=1 to m[sum from i=1 to q \beta^1x(i)]z(k) + sum k=1 to m-1 sum from l=k+1 to m \alpha(kl)z(k)z(l) + sum from k=1 to m \alpha(kk)z^2(k)+ \epsilon}
  
  where \eqn{x_i}{x(i)} are mixture components, and \eqn{z_j}{z(j)} are process variables.
   
  }
\item{procvars}{a character vector of column names of the process variables in frame to be 
                  included in the model. Leave this out if there are no process variables in the
                  frame}
          }        
\references{
1. Lawson, J. and Willden, C., "Mixture Experiments in R, using mixexp", Journal of Statistical Software, 2015
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

#### Kowalski Cornell and Vining Simplified model on data from Gallant et. al. (2008)
data(Burn)
testBNM<-MixModel(Burn,"y",mixcomps=c("Course","Fine","Binder"),model=6,procvars=c("z"))


          }
\keyword{regression}
 
