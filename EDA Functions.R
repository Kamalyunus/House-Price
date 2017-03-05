
#The Summary_num function searches for all numeric/integer columns in dataset and return basic summary statistcs in data frame
Summary_num<-function(df) {
  
  #Search Numeric/Integer columns
  num<- vector(mode = "character")
  for (var in 1:ncol(df)) {
    if (class(df[[var]])=="numeric" || class(df[[var]])=="integer") {
      num <- c(num,names(df[var]))
    }
  }
  
  dfnum     <- subset(df,select=num)
  D         <- sapply(dfnum, function(x) as.numeric(x,na.rm=TRUE))
  DD        <- as.data.frame(D)
  
 #Basic Summary 
  options(digits = 3)
  n                   <- sapply(DD, function(x) sum(!is.na(x)))
  mean                <- sapply(DD, function(x) round(mean(x,na.rm=TRUE), digits = 1))
  sd                  <- sapply(DD, function(x) round(sd(x,na.rm=TRUE), digits = 1))
  max                 <- sapply(DD, function(x) max(x,na.rm=TRUE))
  min                 <- sapply(DD, function(x) min(x,na.rm=TRUE))
  range               <- max - min
  nzero               <- sapply(DD, function(x) length(which(x == 0)))
  nunique             <- sapply(DD, function(x) length(unique(x)))
  outliersummary      <- t(sapply(DD, function(x) {
    iqr          <- IQR(x,na.rm = TRUE,type = 4)
    lowerbound   <- quantile(x,0.25,na.rm=TRUE)-(1.5*iqr)
    upperbound   <- quantile(x,0.75,na.rm=TRUE)+(1.5*iqr)
    noofoutliers <- length  (which(x > upperbound | x <lowerbound))
    return(c(iqr,lowerbound,upperbound,noofoutliers))
  }))
  d2                  <- cbind.data.frame(n,mean,sd,max,min,range,nunique,nzero,outliersummary)
  colnames(d2)        <- c("N","Mean","SD","Max","Min","Range","NUnique","NZeros","IQR","Lower","Upper","NOutlier")
  
  #missing value computation
  Miss      <- sapply(dfnum, function(x) sum(is.na(x)) )
  Miss      <- as.data.frame(Miss)
  d3        <- cbind(d2,Miss)
  n1        <- nrow(dfnum)
  missPer   <- round((Miss/n1)*100, digits=1)
  d3        <- cbind(d3,missPer)
  colnames(d3)[ncol(d3)] <- "%Miss"
  
  #percentile value computation
  q         <- sapply(DD, function(x) round(quantile(x, c(.01,.05,.25,.5,.75,.95, .99),na.rm=TRUE), digits = 1 ))
  q         <- as.data.frame(q)
  q         <- t(q)
  d3        <- cbind(d3,q)
  
  return(d3)
  
}

#The Count_Type function returns number of numeric and character columns in dataset
Count_Type<-function(df) {
  
  num        <- vector(mode = "character")
  char       <- vector(mode = "character")
  for (var in 1:ncol(df)) {
    if (class(df[[var]]) == "numeric" || class(df[[var]])=="integer") {
      num    <- c(num, names(df[var]))
    }else if (class(df[[var]]) == "factor" || class(df[[var]]) == "character") {
      char   <- c(char, names(df[var]))
    }
  }
  
  return(paste("Total:",ncol(df),"Num:",length(num),"Char:",length(char)))
}

#The Univariate_num function searches for all numeric/integer columns in dataset and return histogram/density plots
Univariate_num<-function(df, outlier_flag) {
  
  #Search Numeric/Integer columns
  num<- vector(mode = "character")
  for (var in 1:ncol(df)) {
    if (class(df[[var]])=="numeric" || class(df[[var]])=="integer") {
      num <- c(num,names(df[var]))
    }
  }
  
  dfnum     <- subset(df,select=num)
  D         <- sapply(dfnum, function(x) as.numeric(x,na.rm=TRUE))
  DD        <- as.data.frame(D)
  
  #Plots
  
  for (var in 1:ncol(DD)) {
    if (outlier_flag==1) {
      iqr          <- IQR(DD[,var],na.rm = TRUE,type = 4)
      lowerbound   <- quantile(DD[,var],0.25,na.rm=TRUE)-(1.5*iqr)
      upperbound   <- quantile(DD[,var],0.75,na.rm=TRUE)+(1.5*iqr)
      X            <- as.data.frame(subset(DD[,var],DD[,var] < upperbound & DD[,var] >lowerbound))
      colnames(X) <- names(DD[var])
      if (nrow(X)!=0) {
        Plots<- ggplot(X, aes(X[,1])) +      
        geom_histogram(aes(y = ..density.., alpha = 0.8), bins = 40, show.legend = FALSE) +      
        geom_density(color="blue", size = 1) +      
        scale_x_continuous(names(DD[var])) + ylab("Density Plot") + ggtitle(paste("Variable Univariate (Outlier Treatment):",names(DD[var])))
      print(Plots)
      }
    } else{
      Plots<-ggplot(DD, aes(DD[var])) +      
        geom_histogram(aes(y = ..density.., alpha = 0.8), bins = 40, show.legend = FALSE) +      
        geom_density(color="blue", size = 1) +      
        scale_x_continuous(names(DD[var])) + ylab("Density Plot") + ggtitle(paste("Variable Univariate:",names(DD[var])))
      print(Plots)
    }
      
  }
}


#Bivariate_num functions takes dependent variable and plot scatter plots (dependent variable: numeric) or Box plots (dependent variable: Character)
Bivariate_num<-function(df, dep, outlier_flag) {
  df=train
  #Search Numeric/Integer columns
  num<- vector(mode = "character")
  for (var in 1:ncol(df)) {
    if (class(df[[var]])=="numeric" || class(df[[var]])=="integer") {
      num <- c(num,names(df[var]))
    }
  }
  
  dfnum     <- subset(df,select=num)
  D         <- sapply(dfnum, function(x) as.numeric(x,na.rm=TRUE))
  DD        <- as.data.frame(D)
  
  #Plots
  for (var in 1:ncol(DD)) {
    if (outlier_flag==1) {
      iqr          <- IQR(DD[,var],na.rm = TRUE,type = 4)
      lowerbound   <- quantile(DD[,var],0.25,na.rm=TRUE)-(1.5*iqr)
      upperbound   <- quantile(DD[,var],0.75,na.rm=TRUE)+(1.5*iqr)
      X            <- cbind(df[dep],DD[,var])
      X            <- X%>%filter(X[,2] < upperbound & X[,2] >lowerbound)
      colnames(X) <- c(dep,names(DD[var]))
      if (nrow(X)!=0) {
        if(names(DD[var])!=dep) {
          if(class(df[[dep]])=="numeric" || class(df[[dep]])=="integer") {
            Plots<-ggplot(X, aes(X[,2],X[,1])) +      
              geom_point(aes(alpha=0.9),color="red", show.legend = FALSE) +      
              geom_smooth(color="blue", size = 1, se=FALSE) +      
              scale_x_continuous(names(X[2])) + ylab(dep) + ggtitle(paste("Bivariate (Outlier Treated):",dep, "Vs.",names(X[2])))
            print(Plots)
          } else {
            Plots<-ggplot(X, aes(X[,1],X[,2])) +      
              geom_boxplot(outlier.colour = "red") +      
              xlab(dep) + ylab(names(X[2])) + ggtitle(paste("Bivariate (Outlier Treated):",dep, "Vs.",names(X[2])))
            print(Plots)
          }
        }
      }
    } else{
      if(names(DD[var])!=dep) {
        if(class(df[[dep]])=="numeric" || class(df[[dep]])=="integer") {
          Plots<-ggplot(DD, aes(DD[,var],df[[dep]])) +      
            geom_point(aes(alpha=0.9),color="red", show.legend = FALSE) +      
            geom_smooth(color="blue", size = 1, se=FALSE) +      
            scale_x_continuous(names(DD[var])) + ylab(dep) + ggtitle(paste("Bivariate:",dep, "Vs.",names(DD[var])))
          print(Plots)
        } else {
          Plots<-ggplot(DD, aes(df[[dep]],DD[,var])) +      
            geom_boxplot(outlier.colour = "red") +      
            xlab(dep) + ylab(names(DD[var])) + ggtitle(paste("Bivariate:",dep, "Vs.",names(DD[var]),"(Outliers shown in Red)"))
          print(Plots)
        }
      }
      
    }
    
  }
}

#The Summary_char function searches for all numeric/integer columns in dataset and return basic summary statistcs in data frame
Summary_char <- function(df){
  
  char       <- vector(mode = "character")
  for (var in 1:ncol(df)) {
    if (class(df[[var]]) == "factor" || class(df[[var]]) == "character") {
      char   <- c(char, names(df[var]))
    }
  }
  
  if (length(char)!=0){
    dfchar   <- subset(df, select=char)
    n        <- sapply(dfchar, function(x) sum(!is.na(x)))
    n        <- data.frame(n)
    colnames(n) <- "N"
    
    n1       <- nrow(df)
    
    #missing value computation
    Miss     <- sapply(dfchar, function(x) sum(is.na(x)))
    Miss     <- as.data.frame(Miss)
    g3       <- cbind(n, Miss)
    perc     <- round((Miss/n1)*100, digits = 1)
    m3       <- cbind(g3, perc)
    colnames(m3)[ncol(m3)] <- "%Miss"
    
    #top-5 level count
    topfivelevel <- function(x){
      tbl_x             <- table(x)
      topfive           <- sort(tbl_x, decreasing = TRUE)[1:ifelse(length(tbl_x) >= 5, yes = 5, no = length(tbl_x))]
      topfivelevelcount <- paste0(names(topfive), ":", topfive)
    }
    
    Unique_num     <- sapply(dfchar, function(x) length(unique(x)))
    Unique_num     <-as.data.frame(Unique_num)
    unique_val <- sapply(dfchar, function(x) paste0(topfivelevel(x), collapse = ", "))
    unique_val <- as.data.frame(unique_val)
    m4         <- cbind(m3, Unique_num, unique_val)
    colnames(m4)[ncol(m4)] <- "Top5Levels:Count"
    
    return(m4)
  }
}


