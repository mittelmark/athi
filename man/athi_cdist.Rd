\name{athi$cdist} 
\alias{athi$cdist} 
\alias{athi_cdist} 
\title{ correlation distances } 
\description{ 
    Calculate correlation distance. 
} 
\usage{ athi_cdist(x,method="pearson",type="abs") } 
\arguments{ 
  \item{x}{ 
    data frame or matrix  
  } 
  \item{method}{ 
    correlation measure, either 'pearson', 'spearman','kendall, default: 'pearson' 
  } 
  \item{type}{ 
    either absolute, squared or negative (negativly correlated items are far away, default: 'absolute' 
  } 
} 
\value{Object of class dist.} 
\examples{ 
    athi$cdist(t(iris[,1:4])) 
    athi$cdist(t(iris[,1:4]),method="spearman",type="square") 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
 
