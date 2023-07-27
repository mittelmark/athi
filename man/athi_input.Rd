\name{athi$input} 
\alias{athi$input} 
\alias{athi_input} 
\title{ Readline line but working as well in Rscripts non-interactivly.} 
\description{ 
 Replacement for the readline function in non-interactive scripts.  
  As the readline function does only works in interactive mode we need an alternative. 
} 
\usage{ athi_input(prompt) } 
\arguments{ 
   \item{prompt}{text displayed to ask for input of the user.character string, usually with curly braces as place holders for the variables} 
} 
\value{returns the entered string} 
\examples{ 
  \dontrun{ 
   x = as.numeric(athi$input("Enter a number: ")) 
  } 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
 
