\name{athi$ref_table} 
\alias{athi$ref_table} 
\alias{athi_ref_table} 
\title{ reference tables } 
\description{ 
    Function to retrieve reference tables. 
} 
\usage{ athi_ref_table(sex,type) } 
\arguments{ 
  \item{sex}{ 
    character string, either "M" for male/boy of "F" for female  
  } 
  \item{type}{ 
    character string, either "height", "weight", "bmi", "muac" or "head" 
  } 
} 
\details{ 
    Some more details ... 
} 
\value{data frame with the reference value} 
\examples{ 
    athi$ref_table(sex="M",type="height") 
    head(athi$ref_table(sex="F",type="height")) 
} 
 
