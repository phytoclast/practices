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
df$Code <- as.character(df$Code)
df$landuse <- landuse[u]
if (u == 1){
  df2 <- df}
else{
  df2 <- rbind(df2,df)}
}
df<- df2
df$Code <- paste0('x',df$Code)
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
#----
#summarize all practices
dfp2 <- df[grepl('P',df[,2]),]
dfp2agg <- aggregate(dfp2[,5:ncol(dfp2)], by=list(dfp2$Code), FUN = 'sum')
rownames(dfp2agg) <- dfp2agg[,1]
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

#----
#Combine landuse and resource triggers as variables for practices
translanduse <- t(plotinputs1)
translanduse <- as.data.frame(translanduse)
translanduse$practice <- rownames(translanduse)
dfp2agg$practice <- rownames(dfp2agg)
plotinputs3 <- merge(translanduse, dfp2agg, by='practice', all.x=TRUE, All.y=TRUE)
rownames(plotinputs3) <- plotinputs3$practice
plotinputs3 <- subset(plotinputs3, select = -c(practice))
plotinputs3 <- as.data.frame(scale(plotinputs3, center = FALSE, scale = TRUE)) #normalize each column

eudist <- as.data.frame(as.matrix(vegdist(plotinputs3[,1:ncol(plotinputs3)], method='euclidean', binary=FALSE, na.rm=T)))
#jacdist <- as.data.frame(as.matrix(vegdist(plotinputs3[,1:6], method='jaccard', binary=FALSE, na.rm=T)))
#disttotal <- (eudist/40 + jacdist)/2 #need to better normalize two matrices
tree3 <- agnes(eudist, method='average')

ngroups <- 8

groups <- cutree(tree3, k = ngroups)

Code <- names(groups)
clust <- unname(groups)
groupdf <- as.data.frame(cbind(Code, clust))
groupdf$clust <- (as.numeric(as.character(groupdf$clust)))
maxcluster <- max(groupdf$clust)
numberzeros <- nrow(groupdf[(groupdf$clust == 0),])
whichrecords <- which(groupdf$clust == 0)
if (nrow(groupdf[groupdf$clust == 0,]) != 0){
  for (i in 1:numberzeros){ #assign all zero clusters to unique cluster number.
    groupdf[whichrecords[i],]$clust <- maxcluster+i}}

newlabels <- tree3$order.lab
newlabels <- as.data.frame(newlabels)
newlabels$row <- row(newlabels)
newlabels <- merge(newlabels, groupdf, by.x='newlabels', by.y ='Code')
newlabels$newlabels <- paste(newlabels$clust, newlabels$newlabels)
newlabels <- newlabels[order(newlabels$row),1]
newtree <- tree3
newtree$order.lab <- newlabels

dend1 <- color_branches(as.hclust(newtree), k = ngroups)
dend1 <- color_labels(dend1, k = ngroups)

filename <- "output/practicelandusetree.png"
w <- 800
h <- nrow(plotinputs3)*12+80
u <- 12
png(filename=filename,width = w, height = h, units = "px", pointsize = u)

par(mar = c(2,0,1,2))
plot(dend1,  horiz = TRUE, main='simularity among practices', font=1, cex=0.84)

dev.off()
#----
#export groups
forexport <- merge(groupdf, unique(df[,3:4]), by='Code')
plotinputs3$Code <- rownames(plotinputs3)
forexport <- merge(forexport, plotinputs3, by='Code')

write.csv(forexport, 'output/groups.csv', row.names = FALSE)