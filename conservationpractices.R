library(stringr)
library(BiodiversityR)
library(cluster)
library(ape)
library(dendextend)
library(dplyr)


#----
#list of landuses that are in file names
landuse <- c('crop','otheragland', 'farmstead', 'forest', 'range', 'pasture')
#import all tables and label by landuse
for(u in 1:6){
df <- read.delim(paste0("data/",landuse[u],".txt"))
df$landuse <- landuse[u]
if (u == 1){
  df2 <- df}
else{
  df2 <- rbind(df2,df)}
}
df<- df2
rm(df2)
#move last column to front
col_idx <- grep("landuse", names(df))
df <- df[, c(col_idx, (1:ncol(df))[-col_idx])]
for (j in 1:(ncol(df)-4)){
  df[,j+4] <- as.numeric(df[,j+4])
  for (i in 1:nrow(df)){
    df[i,j+4] <- ifelse(is.na(df[i,j+4]), 0, df[i,j+4])}}
#select only 'P' practices
dfp <- df[grepl('P',df[,2]),]
dfp$rowtotal <- apply(dfp[,5:ncol(dfp)], MARGIN = 1, FUN = 'sum')
#make community dataset to compare landuses
plotinputs1 <- makecommunitydataset(dfp, row = 'landuse', column = 'Code', value = 'rowtotal', drop = TRUE)
jacdist <- as.data.frame(as.matrix(vegdist(plotinputs1, method='jaccard', binary=FALSE, na.rm=T)))
jactree <- agnes(jacdist, method='average')
filename <- "output/landusetree.png"
w <- 300
h <- 200
u <- 12
png(filename=filename,width = w, height = h, units = "px", pointsize = u)

par(mar = c(2,0,1,1))
plot(as.phylo(as.hclust(jactree)), main='simularity among practices', font=1, cex=0.84)

dev.off()

#summarize all practices
dfp2 <- df[grepl('P',df[,2]),]
dfp2agg <- aggregate(dfp2[,5:ncol(dfp2)], by=list(dfp2$Code), FUN = 'sum')
rownames(dfp2agg) <- paste0('x',dfp2agg[,1])
dfp2agg <- dfp2agg[,2:ncol(dfp2agg)]
eudist2 <- as.data.frame(as.matrix(vegdist(dfp2agg, method='euclidean', binary=FALSE, na.rm=T)))
jactree2 <- agnes(eudist2, method='average')

filename <- "output/practicetree.png"
w <- 800
h <- nrow(dfp2agg)*12+80
u <- 12
png(filename=filename,width = w, height = h, units = "px", pointsize = u)

par(mar = c(2,0,1,1))
plot(as.phylo(as.hclust(jactree2)), main='simularity among practices', font=1, cex=0.84)

dev.off()
