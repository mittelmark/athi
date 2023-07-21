\name{athi$introNAs} 
\alias{athi$introNAs} 
\alias{athi_introNAs} 
\title{ introduce missing values into data frames or matrices } 
\description{ 
  Introduces NA's into the given data frame or matrix with a specified proportion. 
} 
\usage{ athi_introNAs(x,prop=0.05) } 
\arguments{ 
  \item{x}{ 
    either a matrix or data frame 
  } 
  \item{prop}{ 
    proportion of data where NA's should be introduced randomly, default: 0.05 (5 percent) 
  } 
} 
\value{depending on the input either a data frame or matrix some NA's} 
\examples{ 
  data(iris) 
  ir=athi$introNAs(iris[,1:4],prop=0.1) 
  summary(ir) 
  dim(ir) 
  apply(ir,2,function (x) { length(which(is.na(x))) }) 
} 
 
