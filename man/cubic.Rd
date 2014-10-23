\name{cubic}
\alias{cubic}
\title{Creates cubic terms for Scheffe' full cubic model (3)}
\description{ Creates cubic terms that are used by the function MixModel when fitting model (3)
}
\usage{
cubic(a, b)
}
\arguments{
  \item{a}{
input - vector of mixture component column in the data frame
}
  \item{b}{
input - another vector of mixture component column in the data frame
}
}
\value{
vector of elementwise a^2*b-a*b^2 function of terms in the a and b vector
}
\author{
John Lawson
}


