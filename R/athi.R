#' \docType{data}
#' \name{athi-class}
#' \alias{athi}
#' \alias{athi-class}
#' \title{athi - functions for the Guelpe Summerschool}
#' \description{This environment has some useful function required for the
#' the Guelpe Summer school Human Biology and Publich Health - Data Analysis and Statistics.
#' }
#' \usage{athi}
#' \format{Object of class environment with some functions for statistics}
#' \details{
#' \describe{
#'   \item{\link[athi:athi_impute]{athi$impute(x,method="rpart",k=5,cor.method="spearman")}}{impute missing values.}
#'   \item{\link[athi:athi_introNAs]{athi$introNAs(x,prop="0.05")}}{introduce missing values.}
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
#' \title{ reference tables }
#' \description{
#'     Function to retrieve reference tables.
#' }
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
#' \title{ age corrected z-scores based on WHO references }
#' \description{
#'     Function to retrieve age corrected z-scores based on WHO references
#' }
#' \usage{ athi$ref_score(x,age,sex,type) }
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
#' \usage{ athi$impute(x,method="rpart",k=5,cor.method="spearman") }
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
                model=rpart::rpart(formula(paste(colnames(data)[i],"~.")), 
                            data=as.data.frame(data[idx,]),
                            method="class")
                x2 = predict(model,newdata=as.data.frame(data[-idx,]),
                             type="class")
            } else {
                model=rpart::rpart(formula(paste(colnames(data)[i],"~.")), 
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
#' \usage{ athi$introNAs(x,prop=0.05) }
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
