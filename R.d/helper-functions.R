
#' Figure path.
#' 
#' Return the relative path for a figure to be used using either \code{pdf()} or \code{dev.copy2pdf()}. In 
#' this case the folder for figures should be one folder up from the current working directory and be called 
#' 'Figures.d'.
#' 
#' @param file.name The name of the PDF file that will contain the figure.
#' @param single Boolean flag that can be used in conjunction with the \code{onefile} parameter of the 
#'   \code{pdf} function (see examples).
#'   
#' @return A string with the appropriate path.
#' 
#' @examples
#' data(AirPassengers)
#' plot(AirPassengers)
#' dev.copy2pdf(file=fig.path("some-name"))
#' 
#' data(AirPassengers)
#' pdf(file=fig.path("some-name", single=F), onefile=F)
#' plot(AirPassengers)
#' plot(stl(AirPassengers, s.window="periodic"))
#' dev.off()
#' 
fig.path <- function(file.name, single=T) {
     if (single)
          return(paste0("../Figures.d/", file.name, ".pdf"))
     else
          return(paste0("../Figures.d/", file.name, "-%02d.pdf"))
}
# ===========================================================================================================

#' Calculate error metrics.
#' 
#' Calculate a given error between two vectors. By default it uses all values. However, if only a subset of
#' the vectors should be used, the indices to be used can be specified in the \code{ind} parameter.
#' 
#' @param a A vector.
#' @param b Another vector.
#' @param metric Metric to choose (default = RMSE)
#'        available: MAE (Mean Absolute Error) , RMSE (Root Mean Squared Error)
#'                   MAPE (Mean Absolute Percentage Error), RMSPE (Root Mean Squared Percentage Error),
#'                   ALL (all of the above)
#' @param ind Vector of indices to use in case only a subset of the data should be used for calculation.
#'   Leave empty to use all data.
#' 
#' @return The desired error metric.
#' 
metrics <- function(a, b, metric = "RMSE", ind=NULL) {
     if (!is.null(ncol(a)) | !is.null(ncol(b)))
          stop("Can only handle univariate vectors.")
     
     if (class(a)=="character" | class(b)=="character")
          stop("Cannot calculate metrics of strings.")
     
     if (length(a) != length(b))
          stop("Vectors do NOT have the same length.")         
       
     if (is.null(ind)) {
          switch(metric,
            RMSE = { return(sqrt(mean( (a-b)^2 ))) },
            MAE = { return (mean(abs(a-b)))},
            MAPE = { return (100*mean(abs((a-b)/a)))},
            RMSPE = { return (sqrt(100*mean(((a-b)/a)^2)))},
            ALL = { return(c(RMSE = sqrt(mean( (a-b)^2 )),
                             MAE = mean(abs(a-b)),
                             MAPE = 100*mean(abs((a-b)/a)),
                             RMSPE = sqrt(100*mean(((a-b)/a)^2)) ) )},
            stop("No support for chosen metric")
          )
          
          
     } else {          
          if (any(ind<1 | ind>length(a)))
               stop("Invalid indices provided.")
          
          switch(metric,
                 RMSE = { return(sqrt(mean( (a[ind]-b[ind])^2 ))) },
                 MAE = { return (mean(abs(a[ind]-b[ind])))},
                 MAPE = { return (100*mean(abs((a[ind]-b[ind])/a[ind])))},
                 RMSPE = { return (sqrt(100*mean(((a[ind]-b[ind])/a[ind])^2)))},
                 ALL = { return(c(RMSE = sqrt(mean( (a[ind]-b[ind])^2 )),
                                  MAE = mean(abs(a[ind]-b[ind])),
                                  MAPE = 100*mean(abs((a[ind]-b[ind])/a[ind])),
                                  RMSPE = sqrt(100*mean(((a[ind]-b[ind])/a[ind])^2)) ) )},
                 stop("No support for chosen metric")
          )          
     }
}
# ===========================================================================================================

#' Create missing.
#' 
#' Takes a time series and removes entries based on an exponential distribution using the given seed and rate.
#' 
#' @param data A time series.
#' @param seed A random seed used for reproducibility.
#' @param rate The parameter used in the exponential distribution.
#' @param na.rm Boolean flag that indicates if entries should be removed (TRUE) or an NA should be inserted
#'   (FALSE).
#'   
#' @return A list with: \itemize {
#'   \item A time series with the same time and frequency as the provided data but with some values removed 
#'     (if \code{rate > 0}).
#'   \item A vector with the indices where values were removed.
#' }
create.missing <- function(data, rate, seed=NULL, na.rm=F) { 
     
     ## Only for time series
     if (class(data) != "ts") {
          stop("Provided data is not a time series.")
     }
     
     ## No missing data (pass-through)
     if (rate == 0) { 
          return(data)
     }
     
     ## Save original parameters
     t <- time(data)
     f <- frequency(data)
     
     ## Reproducibility if desired
     if (!is.null(seed))
          set.seed(seed)
     
     ## Initialize index
     a <- 0
     
     ## Indices of removed entries
     tempDelete <- numeric(0)
     
     while (a < length(data)) {
          
          ## 'ceiling' is to avoid possible zeros 
          a <- ceiling(a + rexp(1, rate))
          
          if ( a <= length(data) ) {
               data[a] <- NA
               tempDelete <- c(tempDelete, a)
          }
     }   
     
     ## Remove NAs
     if (na.rm) {
          warning("If you delete entries, the time attribute might no longer be valid.")
          data <- data[-tempDelete]
          data <- ts(data=data, start=t[1], frequency=f)
     }
     
     return(list(data=data, na.ind=tempDelete))
}
# ===========================================================================================================

#' Create lag matrix.
#' 
#' Helper function. Transform a univariate time series to a matrix with lags as columns. This effectively 
#' shortens the time series by an amount equal to \code{lags}.
#' 
#' @param data The time series.
#' @param lags The maximum amount of lags to be created.
#' 
#' @return A data frame with the lags in the columns.
#' 
create.lags <- function(data, lags = 0) {
     if (class(data) != "ts")
          stop("Data is not a time series.")
     
     if (lags < 1) {
          warning("No lags introduced.")
          return(data)
     }
     
     data.new <- data.frame(data[(lags+1):length(data)])
     
     cnames <- "x"
     for (i.lag in 1:lags) {
          ind <- (lags + 1 - i.lag) : (length(data) - i.lag)
          data.new <- cbind(data.new, data[ind])
          cnames <- c(cnames, paste("lag", as.character(i.lag), sep = "_"))
     }
     colnames(data.new) <- cnames
     
     return(data.new)
}
# ===========================================================================================================

#' Autoregressive IRMI
#' 
#' Uses the IRMI framework to perform autoregressive imputation of time series.
#' 
#' @param data The time series with missing values coded as NA.
#' @param lags Number of lags to be used.
#' @param seed Random seed to be used for reproducibility.
#' 
#' @return A time series with the same attributes as \code{data} but with its missing valus imputed.
#' 
ar.irmi <- function(data, lags=0, seed=NULL) {
     if (class(data) != "ts")
          stop("Data is not a time series.")
     
     if (!any(is.na(data))) {
          warning("No missing values encountered, no imputation performed.")
          return(data)
     }
     
     if (lags < 1)
          stop("The number of lags should be greater than zero for Autoregressive IRMI.")
     
     if (!require(VIM))
          stop("VIM package is required.")
     
     ## Save original parameters
     t <- time(data)
     f <- frequency(data)
     
     ## Transform ts to pseudo-multivariate matrix with lags
     data.lags <- create.lags(data, lags=lags)
     
     ## Which values are missing in the first row (i.e. they're just lags)
     ind.miss <- which(is.na(data.lags[1, , drop=T]), arr.ind=T)
     
     ## Imputation step
     if (!is.null(seed))
          set.seed(seed)
     
     fl <- file()
     sink(file=fl)
     data.lags <- irmi(data.lags)
     sink()
     close(fl)
     
     ## Calculate median for those missing values that are only lags {
     data.lags <- as.matrix(data.lags)
     
     if (any(ind.miss) & ind.miss[1] > 1) {
          for (i.miss in ind.miss) {
               if (i.miss < ncol(data.lags)) {               
                    val <- median(diag(data.lags[1:(1+ncol(data.lags)-i.miss), i.miss:ncol(data.lags)]))
                    diag(data.lags[1:(1+ncol(data.lags)-i.miss), i.miss:ncol(data.lags)]) <- val
               }
          }
     }
     ## }
     
     ## Extract the time series values
     new1 <- data.lags[1, ncol(data.lags):1, drop=T]
     names(new1) <- NULL
     
     new2 <- data.lags[2:nrow(data.lags), 1, drop=T]
     names(new2) <- NULL
     
     ## Construct imputed ts
     imputed.ts <- ts(data=c(new1, new2), start=t[1], frequency=f)
     
     return(imputed.ts)
}
# ===========================================================================================================

#' Linear models based on conditional trees.
#' 
#' Grow a conditional tree and fit different linear models to the data at the leaves.
#' 
#' @param formula Formula specifying the response and predictors.
#' @param data Data frame with the observations.
#' @param maxdepth Maximum depth of the conditional tree.
#' @param plot Whether to plot the conditional tree or not.
#' 
#' @return A list with: \itemize {
#'   \item The grown conditional tree.
#'   \item A list with the linear models for each leaf.
#'   \item A vector with the number of nodes that correspond to the leaves of the conditional tree.
#'   \item The fitted values.
#'   \item The residuals.
#' }
ctree.lm <- function(formula, data=NULL, maxdepth=0, plot=F) {
     if (is.null(data))
          stop("No data provided.")
     
     #if (class(data) != "ts")
     #     stop("Provided data is not a time series.")
     
     if (any(is.na(data)))
          stop("No support for missing data.")
     
     if (!require(party))
          stop("party package required.")
     
     if (!require(plyr))
          stop("plyr package required.")
     
     #if (lags < 1)
     #     stop("The number of lags should be greater than zero for ART.")
     #else
     #     data <- create.lags(data, lags=lags)
     
     #formula <- as.formula(paste(colnames(data)[1], "~ ."))
     
     ## Grow conditional tree
     fit.ct <- ctree(formula, data=data, controls=ctree_control(maxdepth=maxdepth))
     
     if (plot) 
          plot(fit.ct)
     
     ## Extract rules based on the text printed... (couldn't find a better way) {
     ct.output <- capture.output(fit.ct)
     ct.rules <- list(0)
     rule <- NULL
     prev.node <- 0
     i <- 1
     leaves <- NULL
     
     ## The printed output has the rules starting at line 8
     for (i.rule in 8:length(ct.output)) {
          ## Obtain current rule
          rule.row <- strsplit(gsub("^ *", "", ct.output[i.rule]), # remove leading spaces
                               "[);]")
          ## Obtain current node
          node <- as.numeric(rule.row[[1]][1])
          
          ## If we moved down the tree...
          if (node > prev.node) {
               ## If there is a 'less than' or 'greater than' sign, it is a rule to be extracted
               if (grepl("[<>]", rule.row[[1]][2])) {
                    rule <- c(rule, 
                              ## Insert empty strings for nodes with no rules (for consistency in later index)
                              rep("", node-prev.node-1), 
                              ## Remove empty spaces
                              sub(" ", "", rule.row[[1]][2]))
                    
               } else {
                    ## If there are no signs, then we are at a leaf
                    leaves <- c(leaves, node)
               }
               prev.node <- node # update previous node
          } else {
               ## If we moved up the tree, create a rule based on the current path from root to leaf
               ct.rules[[i]] <- paste(rule[rule != ""], collapse=" & ")
               i <- i+1
               rule <- rule[1:node]
               rule[node] <- sub(" ", "", rule.row[[1]][2]) # update path
               
               prev.node <- node # update previouos node
          }
          
          ## If the last node is reached, loop will not iterate again, so create the last rule
          if (i.rule == length(ct.output))
               ct.rules[[i]] <- paste(rule[rule != ""], collapse=" & ")
     }
     ## }
     
     ## Train the different linear models for each leaf
     attach(data)
     fit.ct.lm <- llply(ct.rules, function(x.rule) {
          ind <- eval(parse(text=x.rule))
          df <- data[ind,]
          
          fit <- lm(formula, data=df)
          
          return(fit)
     })
     detach(data)
     
     ## Return and order fitted values
     fitted.values <- unlist(sapply(fit.ct.lm, function(x) {x$fitted.values}))
     fitted.ind <- sort(as.numeric(names(fitted.values)), index.return=T)
     fitted.values <- fitted.values[fitted.ind$ix]
     
     ## Return and order residuals
     residuals <- unlist(sapply(fit.ct.lm, function(x) {x$residuals}))
     residuals.ind <- sort(as.numeric(names(residuals)), index.return=T)
     residuals <- residuals[residuals.ind$ix]
     
     ## List everything. Leaves are included because they are needed for prediction later on.
     return(list(ct=fit.ct, ct.lm=fit.ct.lm, leaves=leaves, 
                 fitted.values=fitted.values, residuals=residuals))
}
# ===========================================================================================================

#' Predict with linear models based on conditional tree
#' 
#' This function predicts new values based on the group of linear models obtained with the \code{ctree.lm}
#' function.
#' 
#' @param models The list returned by \code{ctree.lm}.
#' @param newdata The dataframe with the new set of observations. Column names must match the original data.
#' 
#' @return Predicted values.
#' 
ctree.lm.predict <- function(models=NULL, newdata=NULL) {
     if (!is.null(models)) {
          fit.ct <- models$ct
          fit.ct.lm <- models$ct.lm
          leaves <- models$leaves
     } else {
          stop("No models provided")
     }
     
     if (!is.null(newdata)) {          
          newdata.ind <- where(fit.ct, newdata=newdata)
          newdata.ind <- sapply(newdata.ind, function(x) {
               which(x==leaves, arr.ind=T)
          })
          
          newdata <- cbind(newdata, leaf=newdata.ind)
          
          prediction <- apply(newdata, 1, function(x) {
               predict(fit.ct.lm[[x["leaf"]]], newdata=as.data.frame(t(x))) # transpose to have a row-vector
          })
          
          names(prediction) <- NULL
          return(prediction)
          
     } else {
          stop("No new data found")
     }
}
# ===========================================================================================================

#' Autoregressive tree imputation.
#' 
#' Being developed, not sure if it'll work.
#' 
art.impute <- function(data=NULL, lags=0, trace=F) {
     if (is.null(data))
          stop("No data provided.")
     
     if (class(data) != "ts")
          stop("Provided data is not a time series.")
     
     if (!any(is.na(data))) {
          warning("No missing values encountered, no imputation performed.")
          return(data)
     }
     
     if (lags < 1)
          stop("Number of lags should be greater than zero.")
     
     if (!require(VIM))
          stop("VIM package is required.")
     
     #if (!require(lubridate))
     #     stop("lubridate package is required.")
     
     ## Save original parameters
     t <- time(data)
     f <- frequency(data)

     ## Initialize NAs 
     ## (kNN doesn't yield good results, maybe different initialization?)
     ## IRMI didn't help either
     
     data.init <- data.frame(data=unclass(data), 
                             month=rep_len(1:12, length.out=length(data)), 
                             year=floor(unclass(t)))
     imp.ind <- which(is.na(data), arr.ind=T)
     imp.ind <- imp.ind[imp.ind > lags] - lags # adjust to later access lag matrix
     data.init <- irmi(data.init)
     
     ## Create lag matrix
     data.lags <- create.lags(data=ts(data=data.init[,1], start=t[1], frequency=f),
                              lags=lags)
     
     ## Grow ART
     fit.art <- ctree.lm(x ~ ., data=data.lags, plot=trace)
     
     ## Re-calculate originally imputed values with the ART model
     imputed.vals <- ctree.lm.predict(models=fit.art, 
                                      newdata=data.lags[imp.ind,])
     
     ## Extract final time series
     data.lags[imp.ind,1] <- imputed.vals
     
     new1 <- unlist(data.lags[1, ncol(data.lags):1, drop=T]) # 'unlist' due to problems with data frame format
     names(new1) <- NULL
     
     new2 <- data.lags[2:nrow(data.lags), 1, drop=T]
     names(new2) <- NULL
     
     imputed.ts <- ts(data=c(new1, new2), start=t[1], frequency=f)
     return(imputed.ts)
}
# ===========================================================================================================

#' Mice 
#' 
#' MICE
#' 
#' @param data The time series with missing values coded as NA.
#' @param time time information about the complete ts
#' @param algorithm The algorithm to be used
#' @param seed Random seed to be used for reproducibility.
#' 
#' @return A time series with the same attributes as \code{data} but with its missing valus imputed.
#' 
mice.impute <- function(data,time, algorithm, seed=7) {
  if (is.null(data))
    stop("No data provided.")
  
  if (class(data) != "ts")
    stop("Provided data is not a time series.")
  
  if (!any(is.na(data))) {
    warning("No missing values encountered, no imputation performed.")
    return(data)
  }
  
  if (!require(mice))
       stop("mice package required.")
  
  temp <- create.lags(data, frequency(data)*2)
 # temp <- data
 # temp <- data.frame( a = as.numeric(temp),time) #sample,mean, rf , quadratic, norm.predict
  impu <- mice(temp, imputationMethod = algorithm)     #polr,lda,polyreg, cart,logreg ri error 
  temp <- complete(impu) 
  
 # out <- as.vector(temp$a)
 out <- as.vector(temp$x)
  return(out)
}

# ===========================================================================================================

#' Zoo 
#' 
#' 
#' @param data The time series with missing values coded as NA.
#' @param algorithm The algorithm to be used
#' @param seed Random seed to be used for reproducibility.
#' 
#' @return A time series with the same attributes as \code{data} but with its missing valus imputed.
#' 
zoo.impute <- function(data, algorithm, seed=7) {
  if (is.null(data))
    stop("No data provided.")
  
  if (class(data) != "ts")
    stop("Provided data is not a time series.")
  
  if (!any(is.na(data))) {
    warning("No missing values encountered, no imputation performed.")
    return(data)
  }
  
  if (!require(zoo))
       stop("zoo package required.")
  
  if (is.na(data[1])) {
    data[1] <- mean(data, na.rm = T)
  }
  
  switch(algorithm,
         locf = {return (na.locf(data))},
         approx = {return (na.approx(data, rule = 2))},
         aggregate = {return (na.aggregate(data))},
         StructTS = {return (na.StructTS(data))},
         stop("Not a valid algorithm.")
         )
    
}

