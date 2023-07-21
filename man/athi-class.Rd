\name{athi-class} 
\alias{athi-class} 
\alias{athi} 
\title{athi - functions for the Guelpe Summerschool} 
\description{This environment has some useful function required for the 
the Guelpe Summer school Human Biology and Publich Health - Data Analysis and Statistics. 
} 
\format{Object of class environment with some functions for statistics} 
\details{ 
\describe{ 
  \item{\link[athi:athi_impute]{athi$impute(x,method="rpart",k=5,cor.method="spearman")}}{impute missing values.} 
  \item{\link[athi:athi_introNAs]{athi$introNAs(x,prop="0.05")}}{introduce missing values.} 
  \item{\link[athi:athi_norm]{athi$norm(x,method="z",ties.method="average")}}{normalize data.} 
  \item{\link[athi:athi_randomize]{athi$randomize(x)}}{randomize column data within matrix or data frame.} 
  \item{\link[athi:athi_ref_score]{athi$ref_score(x,age,sex,type)}}{reference score for the given age, sex and type.} 
  \item{\link[athi:athi_ref_table]{athi$ref_table(sex,type)}}{reference table for WHO for the given sex and measure type.} 
} 
} 
\examples{ 
attach(athi) 
ref_score(100,age=4,sex="M",type="height") 
head(ref_table(sex="F",type="height")) 
} 
\author{Detlef Groth <email: dgroth@uni-potsdam.de>} 
