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
  \item{\link[athi:athi_assoc_plot]{athi$assoc_plot(x,legend=NULL,shade=TRUE)}}{extended version of the assocplot with main statistic and Pearson residuals (plot)} 
  \item{\link[athi:athi_bootstrap]{athi$bootstrap(x,FUN=NULL,n=1000,...)}}{resampling for the given data set and function (stats)} 
  \item{\link[athi:athi_box_plot]{athi$box_plot(x,y=NULL)}}{extended version of the boxplot with main statistic values on top (plot)} 
  \item{\link[athi:athi_chr2ord]{athi$chr2ord(x,map)}}{map characters or factors to numbers with comfort (data)} 
  \item{\link[athi:athi_cdist]{athi$cdist(x,method="spearman",type="abs")}}{calculate correlation distances (data)} 
  \item{\link[athi:athi_cohensD]{athi$cohensD(x,g,paired=FALSE)}}{effect size for difference between two means (stats)} 
  \item{\link[athi:athi_cohensW]{athi$cohensW(x,p=NULL)}}{effect size for categorical data (stats)} 
  \item{\link[athi:athi_corr]{athi$corr(x,method="pearson",use="pairwise.complete.obs")}}{pairwise correlations and their statistics (stats)} 
  \item{\link[athi:athi_cor_plot]{athi$cor_plot(x,y,method="pearson",...)}}{extended version of xy-plot with main statistics (plot)} 
  \item{\link[athi:athi_corr_plot]{athi$corr_plot(x,...)}}{plot matrices with pairwise correlations (plot)} 
  \item{\link[athi:athi_df2md]{athi$df2md(x,caption='',rownames=TRUE)}}{print a matrix or data frame as a Markdown table (data)} 
  \item{\link[athi:athi_drop_na]{athi$drop_na(x,cols=NULL)}}{drop rows if the given columns contain NAs in this row for certain columns (data)} 
  \item{\link[athi:athi_epsilon_squared]{athi$epsilon_squared(x,y)}}{effect size for Kruskal test (stats)} 
  \item{\link[athi:athi_eta_squared]{athi$eta_squared(x,y=NULL)}}{effect size for ANOVA (stats)} 
  \item{\link[athi:athi_fmt]{athi$fmt(x,...)}}{formatted string output (data)} 
  \item{\link[athi:athi_impute]{athi$impute(x,method="rpart",k=5,cor.method="spearman")}}{impute missing values (data)} 
  \item{\link[athi:athi_input]{athi$input(prompt)}}{readline replacement for scripts (programming)} 
  \item{\link[athi:athi_introNAs]{athi$introNAs(x,prop="0.05")}}{introduce missing values (data)} 
  \item{\link[athi:athi_kroki]{athi$kroki(text,mode="ditaa",...)}}{draw diagrams and flowcharts using kroki webservice (plot)} 
  \item{\link[athi:athi_lm_plot]{athi$lm_plot(x,y=NULL,data=NULL,...)}}{plot a linear model with confidence intervals (plot)} 
  \item{\link[athi:athi_mi]{athi$mi(x,y=NULL,norm=FALSE)}}{mutual information (stats)} 
  \item{\link[athi:athi_mds_plot]{athi$mds_plot(x,method="euclidean",...)}}{plot a multidimensional scaling (plot)} 
  \item{\link[athi:athi_norm]{athi$norm(x,method="z",ties.method="average")}}{normalize data (data)} 
  \item{\link[athi:athi_randomize]{athi$randomize(x)}}{randomize column data within matrix or data frame (data)} 
  \item{\link[athi:athi_ref_score]{athi$ref_score(x,age,sex,type)}}{reference score for the given age, sex and type (data)} 
  \item{\link[athi:athi_ref_table]{athi$ref_table(sex,type)}}{reference table for WHO for the given sex and measure type (daa)} 
  \item{\link[athi:athi_report_pvalue]{athi$report_pvalue(p,star=FALSE)}}{report a p-value using the significance thresholds (stats)} 
  \item{\link[athi:athi_sem]{athi$sem(x,na.rm=TRUE)}}{standard error of the mean (stats)} 
  \item{\link[athi:athi_smartbind]{athi$smartbind(x,y)}}{combine two data frame even if the have different column names (data)} 
  \item{\link[athi:athi_textplot]{athi$textplot(x)}}{write the data for a data frame or matrix into a plot (plot)} 
  \item{\link[athi:athi_untab]{athi$untab(x)}}{expand a contingency table to a data frame one item per row (data)} 
  \item{\link[athi:athi_venn]{athi$venn(x)}}{Venn diagram for logical relations between two and three sets (plot)} 
  \item{\link[athi:athi_wilcoxR]{athi$wilcoxR(x,y)}}{effect size r, for a wilcox test object (stats)} 
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
