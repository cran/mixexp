Xvert = function(nfac=3,x1=c(0,1),x2=c(0,1),x3=c(0,1),x4=c(0,1),x5=c(0,1),x6=c(0,1),
                 x7=c(0,1),x8=c(0,1),x9=c(0,1),x10=c(0,1),x11=c(0,1),x12=c(0,1),ndm=0)
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
library(gdata)
Ip<-diag(nfac)
In<--1*Ip
conmx<-interleave(Ip,In)
conmx<-cbind(conmx,v)


# calls crvtave to create exteme vertices design plus centroid
des<-crvtave(ndm,conmx)
des<-data.frame(des)

if (nfac==3) {
#DesignPoints(des[,3],des[,2],des[,1],-v[1], v[2],-v[3],v[4],-v[5],v[6])
DesignPoints(des=des,x1lower=-v[1], x1upper=v[2],x2lower=-v[3],x2upper=v[4],x3lower=-v[5],x3upper=v[6])
 }
return(des)
}

