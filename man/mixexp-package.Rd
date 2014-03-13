\name{mixexp-package}
\alias{mixexp-package}
\docType{package}
\title{
This package contains functions for creating designs for mixture experiments and making graphical display of results of mixture experiments.
}
\description{The \pkg{mixexp} package provides functions for creating mixture experiment designs in an unconstrained simplex or constrained mixture space. Functions are also 
provided for making ternary contour plots, pictures of constrained regions, design points, and mixture effect plots.  


}

\details{
\tabular{ll}{
Package: \tab mixexp\cr
Type: \tab Package\cr
Version: \tab 1.2-1\cr
Date: \tab 2011-05-31\cr
License: \tab GPL2.0\cr
Dependencies: gdata, lattice, grid\tab \cr
LazyLoad: \tab yes\cr
Packaged: \tab 2011-05-31 19:54:07 UTC; Lawson\cr
Built: \tab R 2.12.1; i386-pc-mingw32; 2011-05-31 19:54:08 UTC; windows\cr
}

Index:
\preformatted{
conmx                   example constraint matrix used as input to function 
                        crvtave
crvtave                 function for creating extreme vertices designs and 
                        centroids; this function calls Eflags, Nrows, and Vertcen
DesignPoints            function for plotting design points and or mixture 
                        constraint in the simplex
Eflags                  function for calling Piepel''s fortran code cnvrt 
                        to create extreme vertices designs and prints any 
						                        error messages
Effplot                 function for making mixture effect plots given a design
MixturePlot             function for making contour plots in simplex region given a design
MixModel                function for fitting mixture models to data
ModelPlot               function for making contour plots of an equation in an lm object 
                        created by the lm function or the MixModel function
Nrows                   function for calling Piepel's fortran code cnvrt to 
                        create extreme vertices designs and returns the 
						                        number of rows in the resulting design
SCD                     function for creating Simplex Centroid Designs
SLD                     function for creating Simplex Lattice Designs
Vertcen                 function for calling Piepel's fortran code cnvrt 
                        to create extreme vertices designs and returns the 
						                        resulting design
Xvert                   function for creating extreme vertices design 
                        and centroids, this function calls crvtave
}
}
\author{
John Lawson <lawson@byu.edu> and Cameron Willden <ccwillden@gmail.com>

Maintainer: John Lawson <lawson@byu.edu>
}
\keyword{ package }
