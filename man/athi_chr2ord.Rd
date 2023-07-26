\name{athi$chr2ord} 
\alias{athi$chr2ord} 
\alias{athi_chr2ord} 
\title{ Map characters or factors to numbers } 
\description{ 
    Create ordinal numerical variables out of character or factor variables based on a given mapping. 
} 
\usage{ athi_chr2ord(x,map) } 
\arguments{ 
 \item{x}{character or factor vector} 
 \item{map}{list with keys for the given x vector and numbers for the matching values} 
} 
\value{numerical values for the mapping} 
\examples{ 
status=c("never","rare","often","always") 
x=sample(status,100,replace=TRUE) 
x=c(NA,x,NA) 
table(x,useNA='ifany') 
map=c(never=0, rare=1, often=2,always=3) 
table(athi$chr2ord(x,map),useNA='ifany') 
} 
