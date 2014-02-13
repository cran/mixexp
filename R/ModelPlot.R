ModelPlot = function(user.func = NULL, dimensions = list(x1=NULL,x2=NULL,x3=NULL),slice=NULL,lims=rep(0,6),
					   constraints = FALSE,
					   constraint.pars = list(lty=2,lwd=2),
					   contour = FALSE, 
					   contour.pars = list(lwd=0.5,cex.lab=1.3),
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
  ##############################################   
  #Argument list
  # user.func - function supplied by the user that takes as arguments a dataframe called 'grid' and returns
  #       the predictions.  Typically, this will be a wrapper function for the predict() (e.g. predict(model,newdata=grid))
  # dimensions - list argument that specifies the mixture variables to be plotted on the ternary plot.
  #       Values must correspond to variable names from the user-supplied model.
  # slice - list argument that specifies the value of fixed mixture components
  # lims - vector of lower and upper constraints for ternary plot components
  # constraints - if TRUE constraints found in lims will be added to the graph
  # constraint.pars - list of graphical parameters controlling the appearance of the constraint lines
  # contour - if TRUE contour lines will be added to the graph
  # contour.pars - list of graphical parameters controlling the appearance of the contour lines
  # cuts - number of breaks between levels (used for contours if 'at' not specified)
  # at - list of contour levels (e.g. at=c(1,3,5,10) will draw contours at those heights).
  #		Overrides cuts argument
  # res - resolution of the grid.  Corresponds to number equally spaced values along the baseline of the simplex
  # pseudo - logical argument.  If TRUE uses pseudo components to zoom in on constrained region.
  # 		Will create the smallestequilateral triangle that still contains the whole constrained region.
  # fill - if TRUE regions between contour lines will be colored
  # color.palette - color palette to use if fill=TRUE
  # main - character value for main title or list containing character value and graphical parameters (e.g. main=list("main title",cex=2))
  # axislabs - character vector of axis labels for ternary components
  # axislab.pars - list of graphical parameters controlling the appearance of the axislabels
  # axislab.offset - numeric value that creates or eliminates space between the angled axislabels and the tickmarks
  #		Prevents axis labels from overlapping with tickmarks.  Typically, absolute value would not exceed 0.05.
  # cornerlabs - character vector of corner labels for x1, x2 and x3 vertices
  # cornerlab.pars - list of graphical parameters controlling the appearance of the axislabels
  # grid - logical argument.  If true, adds grid lines to the ternary plot
  # grid.pars - list of graphical parameters controlling the appearance of the gridlines
  # colorkey - logical or list of parameters.  See levelplot documentation for more details.
  # labels - logical argument.  If true, labels contour lines.
  # label.style - controls placement of contour labels.  Choose from "mixed","flat", or "align."
  #		See panel.levelplot documentation for more details.
  # ... additional arguments for user.func besides 'grid.'  Typically, will be used to fix values of process variables.
{
#maximum values given the components being held constant (e.g 4th component = 25%, max should be 75% for remaining components)
  if(is.null(slice)){
  	mx = 1
  }else{
	mx = 1-sum(unlist(slice))
  }
  if((constraints | pseudo) & sum(lims == rep(0,6)) == 6 ){
    stop("Component limits must be specified with the 'lims' argument to plot constraints or to use pseudo components")
  }
  if((constraints & (lims[1]>mx |lims[3]>mx | lims[5]>mx))){
    stop("Lower constraint must be less than mx or there is nothing in the constrained region")
  }
  
  if (is.null(user.func)) stop("There must be a user.func for ModelPlot to work")
  if (length(dimensions)!=3) stop("Ternary plot dimensions must be identified")

  if(is.logical(colorkey)){
    if(colorkey){
      colorkey=list(axis.line=list(col=1),axis.text=list(col=1))
    }
  }else{
    if(is.list(colorkey)){
	if(is.null(colorkey$axis.line)){
	  colorkey$axis.line = list(col=1)
	}
	if(is.null(colorkey$axis.text)){
	  colorkey$axis.text= list(col=1)
	}
    }else{
	warning("colorkey argument must be a logical or a list.  Argument will be ignored.")
	colorkey = FALSE
  }
}
  
  #add check that user specifies x1, x2, x3
  
  #Depends on the following libraries
  #library(lattice)
  #library(grid)
base = high = NULL
l.bnds <- lims[seq(1,5,by=2)]
if(sum(l.bnds>=mx)>0) stop("The lower bound on at least on component on the ternary plot is larger than the proportion of the mixture not occupied by the fixed component(s).  The constraints are not consistent.")

## Creation of Grid
#start in full pseudo space
trian <- expand.grid(base=seq(0,1,l=res), high=seq(0,sin(pi/3), l=res)) #height of equilateral triangle is sin(pi/3)
trian <- subset(trian, (base*sin(pi/3)*2)>=high) #dumps values outside of triangle on left side
trian <- subset(trian, ((1-base)*sin(pi/3)*2)>=high) #dumps values outside of triangle on right side

#new2 is data frame containing barycentric (triangular) coordinates converted from Cartesian coordinates
new2 <- data.frame(x1=trian$high*2/sqrt(3))
new2$x3 <- trian$base-trian$high/sqrt(3)
new2$x2 <- 1 - new2$x1-new2$x3
new2=new2[,c(1,3,2)]

#applies reverse pseudo component transform
if (!pseudo){
	l.bnds2 = rep(0,3) #makes all lower bounds equal to 0, pseudo transformation will be a dummy transform
}else{
l.bnds2 = l.bnds
}
sum.bnds <- sum(l.bnds2)

#convert grid values out of pseudo-component space for evaluation
new2$x3 <- l.bnds2[3]+(mx-sum.bnds)*new2$x3
new2$x2 <- l.bnds2[2]+(mx-sum.bnds)*new2$x2
new2$x1 <- l.bnds2[1]+(mx-sum.bnds)*new2$x1
#missing stuff for design point here if we want that functionality

names(new2)=unlist(dimensions)
if(!is.null(slice)){
	for(i in 1:length(slice)){
		new2 = cbind(new2,slice[i])
	}
}

#trian$w=user.func(grid=new2) #use this line for debugging
trian$w=user.func(grid=new2,...)

#fix upper bounds if 0
for(i in seq(2,6,by=2)){
	if(lims[i]==0) lims[i]=1
}

grade.trellis <- function(...){
	from=0.2
	to=0.8
	step=0.2
	#Grid lines
	x1 <- seq(from, to, step)
	x2 <- x1/2
	y2 <- x1*sqrt(3)/2
	x3 <- (1-x1)*0.5+x1
	y3 <- sqrt(3)/2-x1*sqrt(3)/2
	labx1 <- l.bnds2[3]+(mx-sum.bnds)*x1
	labx2 <- l.bnds2[2]+(mx-sum.bnds)*x1
	labx3 <- l.bnds2[1]+(mx-sum.bnds)*x1
	panel.segments(x1, 0, x2, y2,...)
	panel.text(x1, 0, label=labx1, pos=1)
	panel.segments(x1, 0, x3, y3,...)
	panel.text(x2, y2, label=rev(labx2), pos=2)
	panel.segments(x2, y2, 1-x2, y2,...)
	panel.text(x3, y3, label=rev(labx3), pos=4)
}

plot.constraints <- function(...){
	if(constraints) {
	###Plot Upper Constraints
	#Constraints on x1
	  x1 <- (lims[2]-l.bnds2[1])/(mx-sum.bnds)
	  x2 <- x1/2
	  y2 <- x1*sqrt(3)/2
	  if(x1 < 1) panel.segments(x2, y2, 1-x2, y2,...) #prevents plotting outside simplex in pseudo space
	#Constraints on x2
	  x1 <- 1-(lims[4]-l.bnds2[2])/(mx-sum.bnds)
	  x2 <- x1/2
	  y2 <- x1*sqrt(3)/2
	  if(x1 > 0) panel.segments(x1, 0, x2, y2,...)
	#Constraints on x3
	  x1 <- (lims[6]-l.bnds2[3])/(mx-sum.bnds)
	  x2 <- (1-x1)/2+x1
	  y2 <- sqrt(3)/2-x1*sqrt(3)/2
	  if(x1 < 1) panel.segments(x1, 0, x2, y2,...)
	###Plot Lower Constraints
	#Constraints on x1
	  x1 <- (lims[1]-l.bnds2[1])/(mx-sum.bnds)
	  x2 <- x1/2
	  y2 <- x1*sqrt(3)/2
	  if(x1 > 0) panel.segments(x2, y2, 1-x2, y2,...) #prevents plotting outside simplex in pseudo space
	#Constraints on x2
	  x1 <- 1-(lims[3]-l.bnds2[2])/(mx-sum.bnds)
	  x2 <- x1/2
	  y2 <- x1*sqrt(3)/2
	  if(x1 < 1) panel.segments(x1, 0, x2, y2,...)
	#Constraints on x3
	  x1 <- (lims[5]-l.bnds2[3])/(mx-sum.bnds)
	  x2 <- (1-x1)*0.5+x1
	  y2 <- sqrt(3)/2-x1*sqrt(3)/2
	  if(x1 > 0) panel.segments(x1, 0, x2, y2,...)
	}
}

#Build list of arguments for levelplot
if(is.null(at)){
levelplot.main = list(x = w~base*high, data = trian, aspect="iso", xlim=c(-0.1,1.1), ylim=c(-0.1,0.96),
 	         main=main,xlab=NULL,ylab=NULL, contour=contour, cuts=cuts, labels=labels, label.style=label.style,
		   pretty=TRUE, region=fill, col.regions = color.palette(n=cuts+1),
	         par.settings=list(axis.line=list(col=NA), axis.text=list(col=NA)),
		   colorkey=colorkey)
}else{
levelplot.main = list(x = w~base*high, data = trian, aspect="iso", xlim=c(-0.1,1.1), ylim=c(-0.1,0.96),
 	         main=main,xlab=NULL,ylab=NULL, contour=contour, at=at, labels=labels, label.style=label.style,
		   pretty=TRUE, region=fill, col.regions = color.palette(n=length(at)+1), 
	         par.settings=list(axis.line=list(col=NA), axis.text=list(col=NA)),
		   colorkey=colorkey)
}

levelplot.args1 = c(levelplot.main,contour.pars,panel=function(...){
	panel.levelplot(...)
	panel.segments(c(0,0,0.5), c(0,0,sqrt(3)/2), c(1,1/2,1), c(0,sqrt(3)/2,0),lwd=2)
	if(grid){
		do.call(grade.trellis,grid.pars)
	}
	do.call(plot.constraints,constraint.pars)
	do.call(panel.text,c(0, 0, label=cornerlabs[2], pos=2,cornerlab.pars))
	do.call(panel.text,c(1/2, sqrt(3)/2, label=cornerlabs[1], pos=3,cornerlab.pars))
	do.call(panel.text,c(1, 0, label=cornerlabs[3], pos=4,cornerlab.pars))
	do.call(panel.text,c(.5,-.075,axislabs[3],axislab.pars))
	do.call(panel.text,c(.15-axislab.offset,.5,axislabs[2],srt=60,axislab.pars))
	do.call(panel.text,c(.85+axislab.offset,.5,axislabs[1],srt=-60,axislab.pars))
	}                    
)

#produce the plot
p=do.call(levelplot,levelplot.args1)
print(p)
}
#Function over ####################################
