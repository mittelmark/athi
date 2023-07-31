\name{athi$smartbind} 
\alias{athi$smartbind} 
\alias{athi_smartbind} 
\title{Combine two data frames via rbind even with different column names.} 
\description{This function combines two data frames or matrices even if they have different 
  column names. In each data frame or matrix missing columns are first filled up 
  with NA's. Thereafter data is fused using rbind. Column order is determined based  
  on the first given data. 
} 
\usage{ athi_smartbind(x,y) } 
\arguments{ 
  \item{x}{data frame or matrix} 
  \item{y}{data frame or matrix} 
} 
\value{data frame or matrix, depending on the given input where data from *x* and *y* are combined 
  using rbind. Missing columns in either of the data frame are filled up with NA's. 
} 
\examples{ 
ir1=cbind(rn1=rnorm(nrow(iris)),iris) 
ir2=cbind(iris,rn2=rnorm(nrow(iris),mean=10)) 
head(athi$smartbind(ir1,ir2)) 
tail(athi$smartbind(ir1,ir2)) 
} 
\seealso{ 
  \link[athi:athi-class]{athi-class} 
} 
 
