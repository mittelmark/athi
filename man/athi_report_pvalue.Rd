\name{athi$report_pvalue} 
\alias{athi$report_pvalue} 
\alias{athi_report_pvalue} 
\title{ p-value reporting for papers } 
\description{ 
    Function for reporting a p-value either giving the three alpha thresholds,  
  <0.05, <0.01, or <0.001 or using the star syntax.  
} 
\usage{ athi_report_pvalue(x,star=FALSE) } 
\arguments{ 
  \item{x}{numerical p-value, vectors are as well possible} 
  \item{star}{boolean, should the one-three star syntax be used, default: FALSE} 
} 
\details{ 
    This function can be used to report p-values in papers. 
} 
\value{scalar or vector of character strings} 
\examples{ 
  athi$report_pvalue(1/10000) 
  athi$report_pvalue(1/10000,star=TRUE) 
  athi$report_pvalue(0.02,star=TRUE) 
  athi$report_pvalue(0.12,star=TRUE) 
  athi$report_pvalue(c(0.001,0.01,0.3,0.02)) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
 
