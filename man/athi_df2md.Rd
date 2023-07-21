\name{athi$df2md} 
\alias{athi$df2md} 
\alias{athi_df2md} 
\title{ Convert a data frame or a matrix into a Markdown table.} 
\description{ 
  This function can be used within Rmarkdown documents to display easily 
  a simple Markdown table. For more advanced use cases you should other commands 
  such as the kable method from the knitr package. 
} 
\usage{ athi_df2md(x,rownames=TRUE,caption='') } 
\arguments{ 
   \item{x}{matrix or data frame} 
   \item{rownames}{should  the rownames be displayed, default: TRUE} 
   \item{caption}{the caption for the table, it is just displayed below of the table, default: ''} 
} 
\value{prints to stdout} 
\examples{ 
data(swiss) 
athi$df2md(head(swiss)) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
 
