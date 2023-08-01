\name{athi$pastel} 
\alias{athi$pastel} 
\alias{athi_pastel} 
\title{Create up to 20 pastel colors.} 
\description{ 
This is an alternative color creation function for R versions before 3.6 where  
the function `hcl.colors` is not available. 
} 
\usage{ athi_pastel(n) } 
\arguments{ 
\item{n}{number of colors requested, must be within 2 and 20} 
} 
\value{Vector of colors in RGB codes of requested length 'n'} 
\examples{ 
athi$pastel(4) 
par(mai=c(0.2,0.2,0.2,0.1)) 
plot(1:20,col=athi$pastel(20),cex=3,pch=15) 
} 
 
