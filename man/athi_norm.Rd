\name{athi$norm} 
\alias{athi$norm} 
\alias{athi_norm} 
\title{Normalizes given data frame or matrix using the choosen method} 
\description{ 
  Introduces NA's into the given data frame or matrix with a specified proportion. 
} 
\usage{ athi_norm(x,method="z",ties.method="average") } 
\arguments{ 
  \item{x}{ 
    either a matrix or data frame 
  } 
  \item{method}{the method used for normalization, either 'z' for z-score,  
     'uv' for unit variance, 'fs' for feature scaling within 0 and 1 and  
     'q' for quantile normalization, 'mp' for median polish, default: 'z' 
  } 
  \item{ties.method}{if normalization is `(q)antile` how ties should be handled,  
     default `average` which leads to slightly different scales (default in preprocessCore::normalize.quantile, whereas `random` gives  
     the same scales for all columns, this leads as well to different values for samples which had  
     originally the same values, default: 'average' 
  } 
} 
\value{depending on the input either a data frame or matrix with normalized values} 
\examples{ 
  data(iris) 
  ir=athi$norm(iris[,1:4],method="uv") 
  apply(ir,2,sd) 
  summary(ir) 
  ir2=athi$norm(iris[,1:4],method="fs") 
  boxplot(ir2) 
  summary(athi$norm(iris[,1:4],method="q")) 
  # NA's are handled as well 
  ir=iris[,1:4] 
  ir[2,3]=NA 
  ir[1,4]=NA 
  ir[3,2]=NA 
  head(ir) 
  head(athi$norm(ir,method="q")) 
  summary(athi$norm(ir,method="q")) 
  sdata=read.table(text="4 3 6 4 7 
8 1 10 5 11 
6 2 7 8 8 
9 4 12 9 12 
7 5 9 6 10 
") 
athi$norm(sdata,method="mp") 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
