\name{athi$dpairs.legend} 
\alias{athi$dpairs.legend} 
\alias{athi_dpairs.legend} 
\title{Adding legend top or bottom to a \code{athi$dpairs} or \code{pairs} plot} 
\description{ 
    The function \code{athi$dpairs.legend} allows the user to place a legend outside of a  
  pairs or dpairs plot. 
} 
\usage{athi_dpairs.legend(labels, col='grey80', pch=15, cex=1)} 
\arguments{ 
  \item{labels}{ 
    txt labels to be plotted 
  } 
  \item{col}{ 
    colors for the plotting characters 
  } 
  \item{pch}{ 
    plotting symbol, default 15 
  } 
  \item{cex}{ 
    the character expansion for text and plotting characters, default: 1 
  } 
} 
\examples{ 
  data(iris) 
  par(omi = c(0.8, 0.4,0.8,0.4)) # reserve some space top and bottom 
  athi$dpairs(iris,col.box=2:4,col.xy=rep(c(2:4),each=50)) 
  athi$dpairs.legend(levels(iris$Species),col=2:4) 
  mtext('Iris Data',side=3,outer=TRUE,cex=2,line=1) 
} 
\seealso{ \code{\link[athi:athi-class]{athi-class}},  
\code{\link[athi:athi_dpairs]{athi$dpairs}}} 
 
