\name{athi$drop_na} 
\alias{athi$drop_na} 
\alias{athi_drop_na} 
\title{ Removes all rows where any of the columns contain a NA. } 
\description{ 
 In contrast to `na.omit`, the method `athi$drop_na` just checks the given columns  
 to delete rows which have any NA in these two columns in the given rows.  
 This mimics the `tidyr::drop_na` function. 
} 
\usage{ athi_drop_na(x,cols=NULL) } 
\arguments{ 
   \item{x}{matrix or data frame} 
   \item{cols}{the column names or ids, if not given all columns will be checked, default: NULL} 
} 
\value{returns data frame or matrix with rows containing NA's removed} 
\examples{ 
data(iris) 
ir=iris 
ir[c(1,3),1]=NA 
ir[2,2]=NA 
ir[4,4]=NA 
head(ir) 
head(na.omit(ir)) # removes all rows with an NA somewhere 
head(athi$drop_na(ir,1:2)) # just checks the first two columns 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
 
