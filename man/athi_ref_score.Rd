\name{athi$ref_score} 
\alias{athi$ref_score} 
\alias{athi_ref_score} 
\title{ age corrected z-scores based on WHO references } 
\description{ 
    Function to retrieve age corrected z-scores based on WHO references 
} 
\usage{ athi_ref_score(x,age,sex,type) } 
\arguments{ 
  \item{x}{ 
    the numerical value for height (cm) or weight (kg) or BMI or muac (mm) or head circumference (cm). 
  } 
  \item{age}{ 
    age in years, fractional numbers are possible  
  } 
  \item{sex}{ 
    character string, either "M" for male/boy of "F" for female  
  } 
  \item{type}{ 
    character string, either "height" or "weight" 
  } 
} 
\details{ 
    Some more details ... 
} 
\value{data frame with the reference value} 
\examples{ 
    athi$ref_score(100,age=4,sex="M",type="height") 
    athi$ref_score(100,age=4,sex="F",type="height") 
    head(athi$ref_table(sex="F",type="height")) 
} 
 
