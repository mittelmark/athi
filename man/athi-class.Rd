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
  \item{\link[athi:athi_cdist]{athi$cdist(x,method="spearman",type="abs")}}{Calculate correlation distances.} 
  \item{\link[athi:athi_cohensD]{athi$cohensD(x,g,paired=FALSE)}}{Calculate effect size for difference between two means.} 
  \item{\link[athi:athi_cohensW]{athi$cohensW(x,p=NULL)}}{Calculate effect size for categorical data.} 
  \item{\link[athi:athi_corr]{athi$corr(x,method="pearson",use="pairwise.complete.obs")}}{Calculate pairwise correlations and the statistics.} 
  \item{\link[athi:athi_cor_plot]{athi$cor_plot(x,y,method="pearson",...)}}{Extended version of the xy-plot with main statistics in the title.} 
  \item{\link[athi:athi_df2md]{athi_df2md(x,caption='',rownames=TRUE)}}{Print a matrix or data frame as a Markdown table} 
  \item{\link[athi:athi_eta_squared]{athi$eta_squared(x,y=NULL)}}{Calculate effect size for ANOVA.} 
  \item{\link[athi:athi_impute]{athi$impute(x,method="rpart",k=5,cor.method="spearman")}}{impute missing values.} 
  \item{\link[athi:athi_introNAs]{athi$introNAs(x,prop="0.05")}}{introduce missing values.} 
  \item{\link[athi:athi_lm_plot]{athi$lm_plot(x,y=NULL,data=NULL,...)}}{plot a linear model with confidence intervals.} 
  \item{\link[athi:athi_mds_plot]{athi$mds_plot(x,method="euclidean",...)}}{plot a multidimensional scaling.} 
  \item{\link[athi:athi_norm]{athi$norm(x,method="z",ties.method="average")}}{normalize data.} 
  \item{\link[athi:athi_randomize]{athi$randomize(x)}}{randomize column data within matrix or data frame.} 
  \item{\link[athi:athi_ref_score]{athi$ref_score(x,age,sex,type)}}{reference score for the given age, sex and type.} 
  \item{\link[athi:athi_ref_table]{athi$ref_table(sex,type)}}{reference table for WHO for the given sex and measure type.} 
  \item{\link[athi:athi_report_pvalue]{athi$report_pvalue(p,star=FALSE)}}{report a p-value using the significance thresholds.} 
} 
All methods are given in to forms: first they are collected in an environment `athi` which allow you to save this object for instance as 
a RDS file and then give it away with your analysis to allow other users to redo or extend the analysis without having to install the library.  
Secondly the methods have the athi prefix followed by an underline which is compatbible with the default R documentation system. 
} 
\examples{ 
# list all methods 
ls(athi) 
athi$ref_score(100,age=4,sex="M",type="height") 
head(athi$ref_table(sex="F",type="height")) 
} 
\author{Detlef Groth <email: dgroth@uni-potsdam.de>} 
