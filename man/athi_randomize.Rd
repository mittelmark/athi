\name{athi$randomize} 
\alias{athi$randomize} 
\alias{athi_randomize} 
\title{Randomize data frame or matrix columns} 
\description{ 
  This function can be used to randomize the data within the same column. 
} 
\usage{ athi_randomize(x) } 
\arguments{ 
  \item{x}{ 
    either a matrix or data frame 
  } 
} 
\value{depending on the input either a data frame or matrix with randomized values} 
\examples{ 
data(iris) 
round(cor(iris[,1:4]),2) 
round(cor(athi$randomize(iris[,1:4])),2) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
 
