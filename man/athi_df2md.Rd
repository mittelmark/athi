\name{athi$df2md} 
\alias{athi$df2md} 
\alias{athi_df2md} 
\title{ Convert a data frame or a matrix into a Markdown table.} 
\description{ 
  This function can be used within Rmarkdown documents to display easily 
  a simple Markdown table. For more advanced use cases you should other commands 
  such as the kable method from the knitr package. 
} 
\usage{ athi_df2md(x,caption='', center=TRUE, rownames=TRUE) } 
\arguments{ 
   \item{x}{matrix or data frame} 
   \item{caption}{the caption for the table, it is just displayed below of the table, default: ''} 
   \item{center}{should the table and the optional caption been center, default: TRUE} 
   \item{rownames}{should  the rownames be displayed, default: TRUE} 
} 
\value{prints to stdout} 
\examples{ 
data(swiss) 
athi$df2md(head(swiss),caption="**Table X:** Swiss Data") 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
 
