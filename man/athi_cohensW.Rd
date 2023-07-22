\name{athi$cohensW} 
\alias{athi$cohensW} 
\alias{athi_cohensW} 
\title{ Effect sizes measure for categorical data } 
\description{ 
    Calculate the effect size for 2x2 and larger contingency tables as well as for single variables. 
} 
\usage{ athi_cohensW(x,p=NULL) } 
\arguments{ 
 \item{x}{contingency table or vector of count data.} 
 \item{p}{expected proportions, required if x is a vector of count data, in case 'x' has length of two a single value,  
   can be given otherwise as many values as the length of x, default: NULL} 
} 
\details{ 
 The function `athi$cohensW` (omega) calculates the effect size for contingency tables.  
  Due to Cohen's rule of thumb values of around 0.1 are considered to stand  
  for small effects, values of around 0.3 represent medium effects and values  
  above 0.5 or higher represent large effects.  
 
  Please note that these rules of thumb are not useful for highly dependent outcome  
  variables (death for instance) these rules might not be useful and as well lower 
  values might be of practical relevance. 
} 
\value{numerical value, effect size Cohen's W (omega)} 
\examples{ 
data(Titanic) 
Titanic[1,1,,] 
athi$cohensW(Titanic[1,1,,]) 
# Data from New Eng. J. Med. 329:297-303, 1993 
azt=as.table(matrix(c(76,399,129,332), byrow=TRUE,ncol=2)) 
rownames(azt)=c("AZT","Placebo") 
colnames(azt)=c("DiseaseProgress", "NoDiseaseProgress") 
athi$cohensW(azt) 
# number of boys (25) and girls (15) in a hospital which deviates  
# from 50/50 or 51/49 ratios 
prop.table(c(25,15)) 
(prop.table(c(25,15))-0.5)*2 
athi$cohensW(c(25,15),p=0.5) 
athi$cohensW(c(25,15),p=c(0.51,0.49)) 
# most extrem case 40 boys and 0 girls 
athi$cohensW(c(40,0),p=0.5) 
athi$cohensW(c(40,0),p=c(0.51,0.49)) # max value here around 2*0.49 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class},\link[athi:athi_cohensD]{athi$cohensD}  
} 
 
