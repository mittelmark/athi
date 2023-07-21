#' \docType{data}
#' \name{athi-class}
#' \alias{athi-class}
#' \alias{athi}
#' \title{athi - functions for the Guelpe Summerschool}
#' \description{This environment has some useful function required for the
#' the Guelpe Summer school Human Biology and Publich Health - Data Analysis and Statistics.
#' }
#' \format{Object of class environment with some functions for statistics}
#' \details{
#' \describe{
#'   \item{\link[athi:athi_impute]{athi$impute(x,method="rpart",k=5,cor.method="spearman")}}{impute missing values.}
#'   \item{\link[athi:athi_introNAs]{athi$introNAs(x,prop="0.05")}}{introduce missing values.}
#'   \item{\link[athi:athi_norm]{athi$norm(x,method="z",ties.method="average")}}{normalize data.}
#'   \item{\link[athi:athi_randomize]{athi$randomize(x)}}{randomize column data within matrix or data frame.}
#'   \item{\link[athi:athi_ref_score]{athi$ref_score(x,age,sex,type)}}{reference score for the given age, sex and type.}
#'   \item{\link[athi:athi_ref_table]{athi$ref_table(sex,type)}}{reference table for WHO for the given sex and measure type.}
#' }
#' }
#' \examples{
#' attach(athi)
#' ref_score(100,age=4,sex="M",type="height")
#' head(ref_table(sex="F",type="height"))
#' }
#' \author{Detlef Groth <email: dgroth@uni-potsdam.de>}

athi=new.env()

#' \name{athi$ref_table}
#' \alias{athi$ref_table}
#' \alias{athi_ref_table}
#' \title{ reference tables }
#' \description{
#'     Function to retrieve reference tables.
#' }
#' \usage{ athi_ref_table(sex,type) }
#' \arguments{
#'   \item{sex}{
#'     character string, either "M" for male/boy of "F" for female 
#'   }
#'   \item{type}{
#'     character string, either "height", "weight", "bmi", "muac" or "head"
#'   }
#' }
#' \details{
#'     Some more details ...
#' }
#' \value{data frame with the reference value}
#' \examples{
#'     athi$ref_table(sex="M",type="height")
#'     head(athi$ref_table(sex="F",type="height"))
#' }
#' 

athi$ref_table <- function (sex,type) {
    if (sex %in% c("M","male","Male","boy")) {
        sex = "boys"
    } else if (sex %in% c("F","female","Female","girl")) {
        sex =  "girls"
    }
    tab=utils::read.table(base::file.path(base::system.file(package="athi"),"references",
                                          paste(sex,"-",type,".tab",sep="")),sep="\t",header=TRUE)
    return(tab)
}

#' \name{athi$ref_score}
#' \alias{athi$ref_score}
#' \alias{athi_ref_score}
#' \title{ age corrected z-scores based on WHO references }
#' \description{
#'     Function to retrieve age corrected z-scores based on WHO references
#' }
#' \usage{ athi_ref_score(x,age,sex,type) }
#' \arguments{
#'   \item{x}{
#'     the numerical value for height (cm) or weight (kg) or ...
#'   }
#'   \item{age}{
#'     age in years, fractional numbers are possible 
#'   }
#'   \item{sex}{
#'     character string, either "M" for male/boy of "F" for female 
#'   }
#'   \item{type}{
#'     character string, either "height" or "weight"
#'   }
#' }
#' \details{
#'     Some more details ...
#' }
#' \value{data frame with the reference value}
#' \examples{
#'     athi$ref_score(100,age=4,sex="M",type="height")
#'     athi$ref_score(100,age=4,sex="F",type="height")
#'     head(athi$ref_table(sex="F",type="height"))
#' }
#' 

athi$ref_score <- function (x,age,sex,type) {
    this=athi
    type=tolower(type)
    sex=tolower(sex)
    if (!type %in% c("height","weight","bmi","muac","head")) {
        stop("Unknown type, known types are 'height', 'weight', 'bmi', 'muac' or 'head'!")
    }
    res = c()
    tabs=list()
    for (s in c("boys","girls")) {
        tabs[[s]] = this$ref_table(s,type)
    }
    if (length(x)>1) {
        if (length(age)==1) {
            age=rep(age,length(x)) 
            sex=rep(sex,length(x))
        }
    }
    sex=gsub("ma?l?e?","boys",sex)
    sex=gsub("fe?m?a?l?e?","girls",sex)
    sex[!(sex %in% c("girls","boys"))]=NA
    for (i in 1:length(x)) {
        if (is.na(sex[i])) {
            res=c(res,NA)
            next
        }
        tab=tabs[[sex[i]]]
        idx0=which(tab$Age==age[i])
        if (length(idx0)==0) {
            idx1=which(tab$Age<age[i])
            idx1=idx1[length(idx1)]
            idx2=which(tab$Age>age[i])[1]
            if (!is.numeric(idx1) | !is.numeric(idx2)) {
                res=c(res,NA)
                next
            }
            r=(tab$Age[idx2]-age[i])/(tab$Age[idx2]-tab$Age[idx1])
            M=tab[idx1,'M']*r+tab[idx2,'M']*(1-r)
            if (type %in% c("weight","bmi","muac","head")) {
                L=tab[idx1,'L']*r+tab[idx2,'L']*(1-r)
                S=tab[idx1,'S']*r+tab[idx2,'S']*(1-r)
                z=(((x[i]/M)^L)-1)/(L*S)
                res=c(res,z)
            } else {
                SD=tab[idx1,'SD']*r+tab[idx2,'SD']*(1-r)
                z = (x[i]-M)/SD
                res=c(res,z)
            }
        } else {
            M=tab[idx0,'M']
            if (any(type %in% c("weight","bmi","muac","head"))) {
                L=tab[idx0,'L']
                S=tab[idx0,'S']
                z=(((x[i]/M)^L)-1)/(L*S)
                res=c(res,z)
            } else {
                SD=tab[idx0,'SD']
                z = (x[i]-M)/SD
                res=c(res,z)
            }
        }
    }
    return(res)
}

#' \name{athi$impute}
#' \alias{athi$impute}
#' \alias{athi_impute}
#' \title{ impute missing values }
#' \description{
#'   Replaces missing values with a reasonable guess by different imputation methods.
#' }
#' \usage{ athi_impute(x,method="rpart",k=5,cor.method="spearman") }
#' \arguments{
#'   \item{x}{
#'     either a matrix or data frame
#'   }
#'   \item{method}{
#'     character string, the method used for replacing missing values, either 'mean', 
#'      'median', 'rpart' or 'knn', default: 'rpart'
#'   }
#'   \item{k}{
#'     for method 'knn' number of nearest neighbors to use, default: 5
#'   } 
#'   \item{cor.method}{
#'     method to use in 'knn' imputation for using to create distance matrix
#'   }
#' }
#' \details{
#'   This method aids in replacing missing values with a reasonable guess by different imputation methods such as 
#'   the simple and not recommended methods mean and median, where NA's are replaced with the 
#'   mean or median for this variable or the more recommended methods using rpart decision trees
#'   or knn using a correlation distance based k-nearest neighbor approach. 
#'   The rpart method can be as well used to replace missing values for categorical variables.
#'   In case of median and mean imputations for categorical variables the modus is used, 
#'   so missing values are replaced with the most often category. This is rarely reasonable.
#' }
#' \value{depending on the input either a data frame or matrix with NA's replaced by imputed values}
#' \examples{
#'   data(iris)
#'   ir=athi$introNAs(iris[,1:4],prop=0.1)
#'   summary(ir)
#'   ir=athi$impute(ir)
#'   summary(ir)
#'   ir=iris
#'   ir[1,3]=NA; ir[2,4]=NA; ir[c(1,3),5]=NA
#'   head(ir)
#'   head(athi$impute(ir,method="rpart"))
#' }
#' 

athi$impute <- function (x,method="rpart",k=5,cor.method="spearman")  {   
    if (method %in% c("mean","median")) {
        for (i in 1:ncol(x)) {
            # integer is as well numeric :) so 
            if (is.numeric(x[,i])) {
                idx=which(is.na(x[,i]))
                if (method == "mean") {
                    x[idx,i]=mean(x[,i],na.rm=TRUE)
                } else if (method == "median") {
                    x[idx,i]=stats::median(x[,i],na.rm=TRUE)
                }  
            } else {
                # TODO: modus (?)
                warning(paste("Only numerical columns can be imputed with mean and median! Column",colnames(x)[i], "is however non-numeric!"))
            }
        }
    } else if (method == "rpart") {
        # TODO: refinement for many variables, 
        # take only variables with high absolute correlation
        # into account if more than 10 variables take top 10
        data=x
        idata=data
        for (i in 1:ncol(data)) {
            idx = which(!is.na(data[,i]))
            if (length(idx) == nrow(data)) {
                next
            }
            if (is.factor(data[,i])) { 
                model=rpart(formula(paste(colnames(data)[i],"~.")), 
                            data=as.data.frame(data[idx,]),
                            method="class")
                x2 = predict(model,newdata=as.data.frame(data[-idx,]),
                             type="class")
            } else {
                model=rpart(formula(paste(colnames(data)[i],"~.")), 
                            data=as.data.frame(data[idx,]))
                x2 = predict(model,newdata=as.data.frame(data[-idx,]))
            }

            idata[-idx,i]=x2
        }
        return(idata)
    } else if (method == "knn") {
        if (ncol(x) < 4) {
            stop("knn needs at least 4 variables / columns")
        }
        data.imp=x
        D=as.matrix(1-((cor(t(data.imp),use="pairwise.complete.obs")+1)/2))
        #D=as.matrix(1-cor(t(data.imp),use="pairwise.complete.obs"method=cor.method))
        for (i in 1:ncol(x)) {
            idx=which(is.na(x[,i]))
            idxd=which(!is.na(x[,i]))
            for (j in idx) {
                idxo=order(D[j,])
                idxo=intersect(idxo,idxd)
                mn=mean(x[idxo[1:k],i])
                data.imp[j,i]=mn
            }
        }
        return(data.imp)
    } else {
        stop("Unknown method, choose either mean, median, knn or rpart")
    } 
    return(x)
}
#' \name{athi$introNAs}
#' \alias{athi$introNAs}
#' \alias{athi_introNAs}
#' \title{ introduce missing values into data frames or matrices }
#' \description{
#'   Introduces NA's into the given data frame or matrix with a specified proportion.
#' }
#' \usage{ athi_introNAs(x,prop=0.05) }
#' \arguments{
#'   \item{x}{
#'     either a matrix or data frame
#'   }
#'   \item{prop}{
#'     proportion of data where NA's should be introduced randomly, default: 0.05 (5 percent)
#'   }
#' }
#' \value{depending on the input either a data frame or matrix some NA's}
#' \examples{
#'   data(iris)
#'   ir=athi$introNAs(iris[,1:4],prop=0.1)
#'   summary(ir)
#'   dim(ir)
#'   apply(ir,2,function (x) { length(which(is.na(x))) })
#' }
#' 

athi$introNAs <- function (x,prop=0.05) {
    df=FALSE
    # changed class(x)[1] == "data.frame" to is.data.frame(x)
    if (is.data.frame(x)) {
         # TODO: deal with different datatypes in 
         # different columns with data frames
         dts=c()
        for (i in 1:ncol(x)) {
          dts=c(dts,class(x[,i]))
        }
        x=as.matrix(x)
        df=TRUE
    }
    n=as.integer(nrow(x)*ncol(x)*prop)
    idx=sample(1:(nrow(x)*ncol(x)),n)
    x[idx]=NA
    if (df) {
        x=as.data.frame(x)
        for (i in 1:ncol(x)) {
            if (dts[i]=="factor") {
                x[,i]=as.factor(x[,i])
            } else {
                class(x[,i])=dts[i]
            }
        }
    }
    return(x)
}    
#' \name{athi$norm}
#' \alias{athi$norm}
#' \alias{athi_norm}
#' \title{Normalizes given data frame or matrix using the choosen method}
#' \description{
#'   Introduces NA's into the given data frame or matrix with a specified proportion.
#' }
#' \usage{ athi_norm(x,method="z",ties.method="average") }
#' \arguments{
#'   \item{x}{
#'     either a matrix or data frame
#'   }
#'   \item{method}{the method used for normalization, either 'z' for z-score, 
#'      'uv' for unit variance, 'fs' for feature scaling within 0 and 1 and 
#'      'q' for quantile normalization, 'mp' for median polish, default: 'z'
#'   }
#'   \item{ties.method}{if normalization is `(q)antile` how ties should be handled, 
#'      default `average` which leads to slightly different scales (default in preprocessCore::normalize.quantile, whereas `random` gives 
#'      the same scales for all columns, this leads as well to different values for samples which had 
#'      originally the same values, default: 'average'
#'   }
#' }
#' \value{depending on the input either a data frame or matrix with normalized values}
#' \examples{
#'   data(iris)
#'   ir=athi$norm(iris[,1:4],method="uv")
#'   apply(ir,2,sd)
#'   summary(ir)
#'   ir2=athi$norm(iris[,1:4],method="fs")
#'   boxplot(ir2)
#'   summary(athi$norm(iris[,1:4],method="q"))
#'   # NA's are handled as well
#'   ir=iris[,1:4]
#'   ir[2,3]=NA
#'   ir[1,4]=NA
#'   ir[3,2]=NA
#'   head(ir)
#'   head(athi$norm(ir,method="q"))
#'   summary(athi$norm(ir,method="q"))
#'   sdata=read.table(text="4 3 6 4 7
#' 8 1 10 5 11
#' 6 2 7 8 8
#' 9 4 12 9 12
#' 7 5 9 6 10
#' ")
#' athi$norm(sdata,method="mp")
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }


athi$norm <- function (x,method="z",ties.method="average")  {
    this=athi
    if (method %in% c("z","fs","uv")) {
        for (i in 1:ncol(x)) {
            if (is.numeric(x[,i])) {
                if (method == "z") {
                    x[,i]=(x[,i]-mean(x[,i],na.rm=TRUE))/sd(x[,i],na.rm=TRUE)
                } else if (method == "uv") {
                    x[,i]=x[,i]/sd(x[,i])
                } else if (method == "fs") {
                    x[,i]=(x[,i]-min(x[,i],na.rm=TRUE))/(max(x[,i],
                                                             na.rm=TRUE)-min(x[,i],na.rm=TRUE)) 
                } 
            } 
        } 
    } else if (method %in% c("mp")) { 
        x=x-medpolish(x,trace.iter=FALSE)$residuals
    } else if (method %in% c("q", "qn","quantiles")) {
        # TODO: handling of NA's
        if (any(is.na(x))) {
            # replace NA's with mean
            x2=this$impute(x,method="knn")
            svals=apply(apply(x2,2,sort),1,mean) # vector
            rmtx=apply(x2,2,rank,ties.method=ties.method) # matrix
            for (i in 1:ncol(x2)) {
                x2[,i]=svals[rmtx[,i]]
                # at the end only change non-NA's
                x[!is.na(x[,i]),i]=x2[!is.na(x[,i]),i]
            }
        } else {
            # no NA's handling
            # 4-5 lines of cod, 
            # Should we really need to install a Bioconductor package 
            # for this functionality?? Yes, the Bioconductor package is in C, so faster!
            # But ... 
            svals=apply(apply(x,2,sort),1,mean) # vector
            rmtx=apply(x,2,rank,ties.method=ties.method) # matrix
            for (i in 1:ncol(x)) {
                x[,i]=svals[rmtx[,i]]
            }
        }
    }  else  {
        stop("Unknown method, choose one of: z, uv, fs, mp  or q")
    }
    return(x)    
}

#' \name{athi$randomize}
#' \alias{athi$randomize}
#' \alias{athi_randomize}
#' \title{Randomize data frame or matrix columns}
#' \description{
#'   This function can be used to randomize the data within the same column.
#' }
#' \usage{ athi_randomize(x) }
#' \arguments{
#'   \item{x}{
#'     either a matrix or data frame
#'   }
#' }
#' \value{depending on the input either a data frame or matrix with randomized values}
#' \examples{
#' data(iris)
#' round(cor(iris[,1:4]),2)
#' round(cor(athi$randomize(iris[,1:4])),2)
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#'

athi$randomize <- function (x) {
    for (i in 1:ncol(x)) {
        x[,i]=sample(x[,i])
    }
    return(x)
}

athi_impute = athi$impute
athi_introNAs = athi$introNAs
athi_ref_score = athi$ref_score
athi_ref_table = athi$ref_table
athi_norm = athi$norm
athi_randomize = athi$randomize
