\name{athi$cv} 
\alias{athi$cv} 
\alias{athi_cv} 
\title{ coefficient of variation} 
\description{ 
Calculate the coefficient of variation. 
} 
\usage{ athi_cv(x,na.rm=FALSE) } 
\arguments{ 
\item{x}{vector with positive numerical values} 
\item{na.rm}{should NA's be removed, default: FALSE} 
} 
\value{numerical value for the coefficient of variation} 
\examples{ 
cv=athi$cv 
cv(rnorm(20,mean=100,sd=4)) 
cv(c(1,2,3,4)) 
cv(c(1,2,3,4,NA)) 
cv(c(1,2,3,4,NA),na.rm=TRUE) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class} 
} 
 
