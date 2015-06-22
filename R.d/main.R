
library("TSA")
library("mice")
library("zoo")
library("ggplot2")
library("forecast")


source("helper-functions.R")

#Settings
sel.imputation.extra <- c("ar.irmi")
sel.imputation.zoo <- c("aggregate","StructTS","locf","approx")
sel.imputation.forecast <- c("interp")


sel.imputation <- c(sel.imputation.extra, sel.imputation.zoo, sel.imputation.forecast)

sel.rates <- c(0.1, 0.3, 0.5, 0.7)
sel.seeds <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)

sel.datasets <- c("beersales", "airpass", "SP","google")


#load all selected datasets
data(list = sel.datasets, envir = environment())
plots <- list()
results <- NULL

for (i.dataset in sel.datasets) 
{
     cat("\nDataset: ", i.dataset, sep="")
     
     for (i.rate in sel.rates) 
     {
          cat("\n\tRate: ", i.rate, sep="")
          
          for(i.seed in sel.seeds) 
          {
               cat("\n\t\tSeed: ", i.seed, sep="")
               set.seed(i.seed)
               
               ts.complete <- get(i.dataset)
               ts.missing <- create.missing(data=ts.complete, rate=i.rate)
               
               for (i.algoImp in sel.imputation)
               {
                    cat("\n\t\t\tAlgorithm: ", i.algoImp, sep="")
                    time.req <- proc.time()
                    
                    if (i.algoImp %in% sel.imputation.zoo) {
                         ts.imputed <- zoo.impute(ts.missing$data, i.algoImp)
                    } 
                    
                    else if (i.algoImp %in% sel.imputation.forecast) {
                      ts.imputed <- na.interp(ts.missing$data)
                    } 
                      
                    else if (i.algoImp %in% sel.imputation.extra) {
                         ts.imputed <- switch(i.algoImp,
                                              ar.irmi = {ar.irmi(ts.missing$data, lags=12)},
                                              stop("Not a valid algorithm.")
                         )
                    }
                    
                    time.req <- proc.time() - time.req
                    
                    errors.all <- metrics(ts.imputed, ts.complete, metric = "ALL")
                    names(errors.all) <- paste(names(errors.all), "All", sep="_")
                    errors.subset <- metrics(ts.imputed, ts.complete, metric = "ALL", ind=ts.missing$na.ind)
                    names(errors.subset) <- paste(names(errors.subset), "Subset", sep="_")
                    
                    series <- gl(n=2, k=length(ts.complete), labels=c("Original", "Imputed"))
                    
                    g.imp.ts <- ggplot(data.frame(x=rep(time(ts.complete), 2),
                                                  y=c(ts.complete, ts.imputed), 
                                                  series=series)) + 
                         geom_line(aes(x=x, y=y, size=series, colour=series, alpha=series)) +
                         labs(x="Time", y=i.dataset, title=i.algoImp) +
                         scale_size_discrete(range=c(0.5, 1)) + 
                         scale_alpha_discrete(range=c(1, 0.5)) +
                         scale_colour_manual(name="series", 
                                             values=c("Original" = "black", "Imputed" = "red")) +
                         geom_point(data=data.frame(x=time(ts.complete)[ts.missing$na.ind], 
                                                    y=ts.imputed[ts.missing$na.ind]), 
                                    aes(x=x, y=y), size=2, colour="blue", alpha=1)
                    
                    plots <- c(plots, list(g.imp.ts))
                    
                    result <- data.frame(Dataset=i.dataset,
                                         Algorithm=i.algoImp,
                                         Miss_Rate=i.rate,
                                         Seed=i.seed,
                                         Elapsed_Time=time.req["elapsed"],
                                         Amount_NAs=length(ts.missing$na.ind),
                                         t(errors.all), # must transpose to add correctly
                                         t(errors.subset),
                                         row.names=NULL)
                    
                    results <- rbind(results, result)
               } 
          } 
     } 
}

timestamp <- as.character(Sys.time())
timestamp <- strsplit(timestamp, " ")
timestamp <- paste(timestamp[[1]][1], 
                   paste(strsplit(timestamp[[1]][2], ":")[[1]], collapse="-"),
                   sep="_")
save(list=c("results", "plots"),
     file=paste0("../Results.d/Results_", timestamp, ".RData"))
