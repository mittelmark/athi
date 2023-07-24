\name{athi$assoc_plot} 
\alias{athi$assoc_plot} 
\alias{athi_assoc_plot} 
\title{ create an association plot with Pearson residual coloring } 
\description{ 
  This function updates the standard assocplot function from the graphics package  
  with the ability to display residual colors. In blue and red are shown groups with  
  residuals above +4 or below -4 in light colors are shown residuals between 2 and 4 for positive and -4 and -2 for negative residuals. 
} 
\usage{ athi_assoc_plot(x,legend=TRUE,shade=TRUE,...) } 
\arguments{ 
   \item{x}{contingency table} 
   \item{legend}{should the residual table and the statistical values be shown at the bottom, default: TRUE} 
   \item{shade}{should residuals being colored, default: ZTUE} 
   \item{\ldots}{other arguments delegated to the default assocplot function} 
} 
\value{NULL} 
\examples{ 
 x <- margin.table(HairEyeColor, c(1, 2)) 
athi$assoc_plot(x) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}, \link[athi:athi_cor_plot]{athi$cor_plot}, \link[athi:athi_box_plot]{athi$box_plot} 
} 
 
