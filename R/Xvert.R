Xvert = function(nfac=3,x1=c(0,1),x2=c(0,1),x3=c(0,1),x4=c(0,1),x5=c(0,1),x6=c(0,1),
x7=c(0,1),x8=c(0,1),x9=c(0,1),x10=c(0,1),x11=c(0,1),x12=c(0,1),nlc=0,lb=c(0,0),ub=c(0,0),coef=c(0,0),ndm=0)
{
# Create the constraints matrix for crvtave
ck<-cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
for (i in 1:12){
  cks<-ck[1,i]+(1-ck[2,i])
   if (cks!=0) {
    nfacc=i 
     } else {break}
 }
if (nfacc>nfac) {
         stop(" The number of upper and lower limits supplied exceeds the number of factors")
                }
v<-c(-x1[1],x1[2])
for (i in 2:nfac) {
 v<-c(v,-ck[1,i],ck[2,i])
                  }
#Creates conmx corresponding to upper and lower constraints on components
Ip<-diag(nfac)
In<--1*Ip
conmx<-interleave(Ip,In)
conmx<-cbind(conmx,v)
 
# Create constraint matrix for linear constraints
if (nlc>0) {
loc<-length(coef)
if (loc!=nfac*nlc) {
         stop(" The number of elements in coef is not equal to the number of linear constraints times the number of factors")
                             }
lolb<-length(lb)
if (lolb!=nlc) { 
         stop(" The number of lower bounds for linear constraints is not equal to the number of linear constraints")
                       }
loub<-length(ub)
if (loub!=nlc) { 
         stop(" The number of upper bounds for linear constraints is not equal to the number of linear constraints")
                       }
lincon<-matrix(coef,byrow=T,nrow=nlc)
#set negative of coef on top of coef in a matrix
nlinc<- -1*lincon
lincon<-rbind(nlinc,lincon)


# set upper bounds on top of negative of lower bounds in a vector
v<- ub
v<-c(v,-lb)

# add vector of bounds to the right of lincon
lincon<-cbind(lincon,v)

# append lincon to the bottom of conmx
conmx<-rbind(conmx,lincon)
            }

# delete rows where contraint is zero
conmx<-conmx[abs(conmx[,nfac+1])>0, ]

# calls crvtave to create exteme vertices design plus centroid
des<-crvtave(ndm,conmx)
des<-data.frame(des)

if (nfac==3) {
   if (nlc>0) {
   DesignPoints(des=des)
              }
   else { DesignPoints(des,x1lower=x1[1],x1upper=x1[2],x2lower=x2[1],x2upper=x2[2],x3lower=x3[1],x3upper=x3[2]) }
             }
return(des)
 }
####End Function #################################