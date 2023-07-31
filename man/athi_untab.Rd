\name{athi$untab} 
\alias{athi$untab} 
\alias{athi_untab} 
\title{Convert a contingency table to a data frame one item per row.} 
\description{This function takes a contingency table and expands it two a data frame 
  where every level combinations is created according to the number of entries in the 
  contingency table. You can reverse the process using the table command. The procedure is useful for 
  instance for performing sample bootstrapping. 
} 
\usage{ athi_untab(x) } 
\arguments{ 
\item{x}{contingency table or a matrix} 
} 
\value{data frame with two columns, column names are either the dimnames of the given table or 
            generic columnames like 'rowvar' and 'colvar'.} 
\examples{ 
M=matrix(c(4,3,1,2),ncol=2,dimnames=list( 
        size=c('small','tall'), 
        smoking=c('no','yes'))) 
M 
N=athi$untab(M) 
head(N) 
summary(N) 
table(N[,1],N[,2]) 
} 
\seealso{ 
  \link[athi:athi-class]{athi-class} 
} 
 
