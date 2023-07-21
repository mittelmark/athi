\name{athi$impute} 
\alias{athi$impute} 
\alias{athi_impute} 
\title{ impute missing values } 
\description{ 
  Replaces missing values with a reasonable guess by different imputation methods. 
} 
\usage{ athi_impute(x,method="rpart",k=5,cor.method="spearman") } 
\arguments{ 
  \item{x}{ 
    either a matrix or data frame 
  } 
  \item{method}{ 
    character string, the method used for replacing missing values, either 'mean',  
     'median', 'rpart' or 'knn', default: 'rpart' 
  } 
  \item{k}{ 
    for method 'knn' number of nearest neighbors to use, default: 5 
  }  
  \item{cor.method}{ 
    method to use in 'knn' imputation for using to create distance matrix 
  } 
} 
\details{ 
  This method aids in replacing missing values with a reasonable guess by different imputation methods such as  
  the simple and not recommended methods mean and median, where NA's are replaced with the  
  mean or median for this variable or the more recommended methods using rpart decision trees 
  or knn using a correlation distance based k-nearest neighbor approach.  
  The rpart method can be as well used to replace missing values for categorical variables. 
  In case of median and mean imputations for categorical variables the modus is used,  
  so missing values are replaced with the most often category. This is rarely reasonable. 
} 
\value{depending on the input either a data frame or matrix with NA's replaced by imputed values} 
\examples{ 
  data(iris) 
  ir=athi$introNAs(iris[,1:4],prop=0.1) 
  summary(ir) 
  ir=athi$impute(ir) 
  summary(ir) 
  ir=iris 
  ir[1,3]=NA; ir[2,4]=NA; ir[c(1,3),5]=NA 
  head(ir) 
  head(athi$impute(ir,method="rpart")) 
} 
 
