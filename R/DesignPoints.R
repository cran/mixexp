DesignPoints = function(des=NULL,x=NULL,y=NULL,z=NULL,x1lower=0,x1upper=0,
                        x2lower=0,x2upper=0,x3lower=0,x3upper=0) 
{

# Checks to See if there are design points

check1<-is.null(des)
check2<-is.null(x)

#check wheter to print design points
if (check1 & check2) {
design=FALSE   } else {design=TRUE } 

  
  if (check2) {
# if not make dummy design points to send to MixturePlot function
x<-c(0,0,1)
y<-c(0,1,0)
z<-c(1,0,0)
              }
if (check1) {
            }else {

x<-des$x3
y<-des$x2
z<-des$x1
       }

# Makes dummy response points
w<-runif(length(x))


# Checks to see if there are constraints

Check<-x1lower+x1upper+x2lower+x2upper+x3lower+x3upper
  if (Check==0) {
cls<-c(rep(0,6))
constraints=FALSE
} else {
cls <-c(rep(0,6))
cls[1]<-x1lower
cls[2]<-x1upper
cls[3]<-x2lower
cls[4]<-x2upper
cls[5]<-x3lower
cls[6]<-x3upper
constraints=TRUE
        }

MixturePlot(x,y,z,w,x3lab="Fraction x3",
  x2lab="Fraction x2", x1lab="Fraction x1", corner.labs=c("x3","x2","x1"),lims=cls,
  constrts=constraints,contrs=FALSE,cols=FALSE, mod=1,n.breaks=9,despts=design)
} 
