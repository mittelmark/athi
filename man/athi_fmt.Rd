\name{athi$fmt} 
\alias{athi$fmt} 
\alias{athi_fmt} 
\title{ Python like formatting of strings using curly braces } 
\description{ 
 This is a function for formatted output avoiding a lot of quotes 
 during creation of strings based on variables. It mimics the Python format command. 
} 
\usage{ athi_fmt(x,...) } 
\arguments{ 
   \item{x}{character string, usually with curly braces as place holders for the variables} 
   \item{\ldots}{variable number of arguments used to replace the curly braces within x} 
} 
\value{returns the formatted string} 
\examples{ 
  athi$fmt('I can say {} {}!',"Hello", "World") 
  athi$fmt('I can say {2} {1}!',"World", "Hello") 
  athi$fmt("pi is '{}'!",sprintf("\%3.5f",pi)) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
 
