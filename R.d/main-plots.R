# ===========================================================================================================

fig.path <- function(file.name, single=T) {
     if (single)
          return(paste0("../Figures.d/", file.name, ".pdf"))
     else
          return(paste0("../Figures.d/", file.name, "-%02d.pdf"))
}

my.plot <- function(results, metric, onefile=T) {
     datasets <- unique(results$Dataset)
     
     plots <- list()
     for (i in seq_along(datasets)) {
          plots[[i]] <- ggplot(subset(results, Dataset==datasets[i]), 
                               aes_string(x="Amount_NAs", y=metric, colour="factor(Miss_Rate)")) +
               geom_point(size=3) + 
               geom_smooth(aes(group=1), colour="gold", size=1, method="loess", se=F) +
               facet_wrap(~Algorithm, scales="free_y") +
               labs(x="Amount of NAs", y=metric, title=paste(metric, "for", datasets[i])) +
               theme_bw(base_size=15)
          
          pdf(file=fig.path(paste0(datasets[i], "-", metric), single=onefile), 
              onefile=onefile, width=14)
          print(plots[[i]])
          dev.off()
     }
     names(plots) <- datasets
     
     return(plots)
}

# ===========================================================================================================

library(ggplot2)

load("../Results.d/Results_2015-04-25_07-16-16.RData")

# ===========================================================================================================

rmse.mean <- aggregate(results[,c("RMSE_All","RMSE_Subset")], 
                       by=list(Dataset=results$Dataset, 
                               Algorithm=results$Algorithm, 
                               Rate=results$Miss_Rate), 
                       FUN=mean)

mape.mean <- aggregate(results[,c("MAPE_All","MAPE_Subset")], 
                       by=list(Dataset=results$Dataset, 
                               Algorithm=results$Algorithm, 
                               Rate=results$Miss_Rate), 
                       FUN=mean)

time.mean <- aggregate(results[,c("Elapsed_Time")],
                       by=list(Dataset=results$Dataset,
                               Algorithm=results$Algorithm),
                       FUN=mean)

# ===========================================================================================================

g.rmse.all <- ggplot(rmse.mean, aes(x=Rate, y=RMSE_All, colour=Dataset)) +
     geom_line() +
     facet_wrap(~Algorithm, scales="free_y") +
     labs(x="Missing Rate", y="RMSE With Mean Between Seeds", title="Mean RMSE Using All Entries") +
     theme_bw(base_size=15) + 
     geom_point(data=results, aes(x=Miss_Rate, y=RMSE_All, shape=Dataset), size=2, colour="blue")
print(g.rmse.all)

g.rmse.subset <- ggplot(rmse.mean, aes(x=Rate, y=RMSE_Subset, colour=Dataset)) +
     geom_line() +
     facet_wrap(~Algorithm, scales="free_y") +
     labs(x="Missing Rate", y="RMSE With Mean Between Seeds", title="Mean RMSE Using Only Imputed Entries") +
     theme_bw(base_size=15) + 
     geom_point(data=results, aes(x=Miss_Rate, y=RMSE_Subset, shape=Dataset), size=2, colour="blue")
print(g.rmse.subset)

g.rmse.na <- ggplot(results, aes(x=Amount_NAs, y=RMSE_All, shape=Dataset, colour=factor(Miss_Rate))) +
     geom_point(size=3) + 
     geom_smooth(aes(group=1), colour="gold", size=1, method="loess", se=F) +
     facet_wrap(~Algorithm, scales="free_y") +
     labs(x="Amount of NAs", y="RMSE", title="RMSE Using All Entries") +
     theme_bw(base_size=15)
print(g.rmse.na)

g.rmse.na <- my.plot(results, "RMSE_All")
print(g.rmse.na)

g.rmse.na.all <- ggplot(results, aes(x=Amount_NAs, y=RMSE_All, shape=Dataset, colour=factor(Miss_Rate))) +
  geom_point(size=3) + 
  geom_smooth(aes(group=1), colour="gold", size=1, method="loess", se=F) +
  facet_wrap(~Algorithm, scales="free_y") +
  labs(x="Amount of NAs", y="RMSE", title="RMSE Using All Entries") +
  theme_bw(base_size=15)
print(g.rmse.na.all)

g.rmse.na.all <- my.plot(results, "RMSE_All")
print(g.rmse.na.all)


g.rmse.na.subset <- ggplot(results, aes(x=Amount_NAs, y=RMSE_Subset, shape=Dataset, colour=factor(Miss_Rate))) +
  geom_point(size=3) + 
  geom_smooth(aes(group=1), colour="gold", size=1, method="loess", se=F) +
  facet_wrap(~Algorithm, scales="free_y") +
  labs(x="Amount of NAs", y="RMSE", title="RMSE Using Imp Entries") +
  theme_bw(base_size=15)
print(g.rmse.na.subset)

g.rmse.na.subset <- my.plot(results, "RMSE_Subset")
print(g.rmse.na.subset)
# ===========================================================================================================

g.mape.all <- ggplot(mape.mean, aes(x=Rate, y=MAPE_All, colour=Dataset)) +
     geom_line() +
     facet_wrap(~Algorithm, scales="free_y") +
     labs(x="Missing Rate", y="MAPE With Mean Between Seeds", title="Mean MAPE Using All Entries") +
     theme_bw(base_size=15) + 
     geom_point(data=results, aes(x=Miss_Rate, y=MAPE_All, shape=Dataset), size=2, colour="blue")
print(g.mape.all)

g.mape.subset <- ggplot(mape.mean, aes(x=Rate, y=MAPE_Subset, colour=Dataset)) +
     geom_line() +
     facet_wrap(~Algorithm, scales="free_y") +
     labs(x="Missing Rate", y="MAPE With Mean Between Seeds", title="Mean MAPE Using Only Imputed Entries") +
     theme_bw(base_size=15) + 
     geom_point(data=results, aes(x=Miss_Rate, y=MAPE_Subset, shape=Dataset), size=2, colour="blue")
print(g.mape.subset)

###!!!
g.mape.subset <- ggplot(mape.mean, aes(x=Rate, y=MAPE_Subset, colour=Algorithm)) +
  geom_line() +
  facet_wrap(~Dataset, scales="free_y") +
  labs(x="Missing Rate", y="MAPE With Mean Between Seeds", title="Mean MAPE Using Only Imputed Entries") +
  theme_bw(base_size=15) + 
  geom_point(data=results, aes(x=Miss_Rate, y=MAPE_Subset, shape=Algorithm), size=2, colour="blue")
print(g.mape.subset)

g.mape.na.all <- ggplot(results, aes(x=Amount_NAs, y=MAPE_All, shape=Dataset, colour=factor(Miss_Rate))) +
     geom_point(size=3) + 
     geom_smooth(aes(group=1), colour="gold", size=1, method="loess", se=F) +
     facet_wrap(~Algorithm, scales="free_y") +
     labs(x="Amount of NAs", y="MAPE", title="MAPE Using All Entries") +
     theme_bw(base_size=15)
print(g.mape.na.all)

g.mape.na.all <- my.plot(results, "MAPE_All")
print(g.mape.na.all)


g.mape.na.subset <- ggplot(results, aes(x=Amount_NAs, y=MAPE_Subset, shape=Dataset, colour=factor(Miss_Rate))) +
  geom_point(size=3) + 
  geom_smooth(aes(group=1), colour="gold", size=1, method="loess", se=F) +
  facet_wrap(~Algorithm, scales="free_y") +
  labs(x="Amount of NAs", y="MAPE", title="MAPE Using Imp Entries") +
  theme_bw(base_size=15)
print(g.mape.na.subset)

g.mape.na.subset <- my.plot(results, "MAPE_Subset")
print(g.mape.na.subset)

# ===========================================================================================================

g.time <- ggplot(results, aes(x=Amount_NAs, y=Elapsed_Time, shape=Dataset, colour=factor(Miss_Rate))) +
     geom_point(size=3) + 
     #geom_smooth(aes(group=1), colour="gold", size=1, method="loess", se=F) +
     facet_wrap(~Algorithm, scales="free_y") +
     labs(x="Amount of NAs", y="Elapsed Time", title="Algorithm Running Time") +
     theme_bw(base_size=15)
print(g.time)

# ===========================================================================================================

pdf(file=fig.path("rmse-comparison", single=F), onefile=F, width=14)
print(g.rmse.all)
print(g.rmse.subset)
print(g.rmse.na)
print(g.rmse.na.subset)

dev.off()

pdf(file=fig.path("mape-comparison", single=F), onefile=F, width=14)
print(g.mape.all)
print(g.mape.subset)
print(g.mape.na.all)
print(g.mape.na.subset)

dev.off()

pdf(file=fig.path("running-times"), width=14)
print(g.time)
dev.off()
