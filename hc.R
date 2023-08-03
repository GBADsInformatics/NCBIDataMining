##############################################################
#Script for hierarchical clustering of breeds and disease 
#Script written by Cassandre Pyne for GBADs
##############################################################

#load necessary packages 
library(stringr)
library(tm)
library(cluster)
library(dbscan)
library(fpc)
library(ggplot2)
library(factoextra)

setwd("/Users/cassandrepyne/Documents/GBADs")
data <- read.csv("MASTER_DATA_coordinates_FINAL_AUG_5.csv", sep = ",", header = T)
spp_list <- read.csv("gbads_spp.csv", sep = ",", header = T)
spp_list <- tolower(spp_list$species)
data$spp_combined <- tolower(paste(data$species, data$host.species))
data$spp_combined <- str_extract(data$spp_combined, paste(spp_list, collapse="|"))

#extract entries for required species 
data <- data[which(data$spp_combined=="ovis aries" | data$spp_combined == "sheep" | data$spp_combined == "bos taurus" | data$spp_combined == "bovine" | data$spp_combined == "cow" | data$spp_combined == "cattle" | data$spp_combined == "livestock" | data$spp_combined == "gallus gallus" | data$spp_combined == "chicken" | data$spp_combined == "hen" | data$spp_combined == "poultry" | data$spp_combined == "sus scrofa" | data$spp_combined == "pig" | data$spp_combined == "capra hircus" | data$spp_combined == "goat" | data$spp_combined == "lama glama" | data$spp_combined == "llama" | data$spp_combined == "camel" | data$spp_combined == "equid" | data$spp_combined == "equine" | data$spp_combined == "equus ferus caballus" | data$spp_combined == "equus ferus"| data$spp_combined == "equus caballus" | data$spp_combined == "equus kiang" | data$spp_combined == "equus zebra" | data$spp_combined == "equus asinus x equus caballus" | data$spp_combined == "equus przewalskii" | data$spp_combined == "equus hemionus kulan" | data$spp_combined == "equus asinus" | data$spp_combined == "donkey"), ]

#clean breed data by replacing common unnecessary phrases 
data$breed <- gsub("strain: ", "", data$breed)
data$breed <- gsub("isolate: ", "", data$breed)
data$breed <- gsub("breed: ", "", data$breed)
data$breed <- gsub("label: ", "", data$breed)
data$breed <- gsub("transcriptome; ", "", data$breed)
data$breed <- gsub("ecotype: ", "", data$breed)
data$breed <- gsub("cultivar: ", "", data$breed)

#extract unique breed data and continue cleaning 
breeds <- unique(as.character(data$breed))
breeds <- sort(breeds)
breeds_cleaned <- str_replace_all(breeds, "[^[:alnum:]]", " ")
breeds_cleaned <- gsub('[[:digit:]]+', '', breeds_cleaned)
breeds_cleaned <- gsub("  ", " ", breeds_cleaned)
breeds_cleaned <- breeds_cleaned[!is.na(breeds_cleaned)]
breeds_cleaned <- gsub('\\b\\w{1,2}\\b','', breeds_cleaned)
breeds_cleaned <- breeds_cleaned[-which(breeds_cleaned =="")]
breeds_cleaned <- breeds_cleaned[-which(breeds_cleaned ==" ")]
breeds_cleaned <- gsub("  ", " ", breeds_cleaned)
breeds_cleaned <- trimws(breeds_cleaned)
breeds_cleaned <- tolower(breeds_cleaned)
breeds_cleaned <- unique(breeds_cleaned)

#combine disease data with host disease data to get a complete disease list 
data$comb_disease <- paste0(data$disease, " ", data$host.disease)
data$comb_disease <- gsub("  ", "", data$comb_disease)
data$comb_disease <- trimws(data$comb_disease)
data$comb_disease <- tolower(data$comb_disease)
disease <- unique(data$comb_disease)

#convert breed and disease data into corpus objects 
corpus = tm::Corpus(tm::VectorSource(breeds_cleaned))
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, stemDocument)

corpus = tm::Corpus(tm::VectorSource(disease))
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, stemDocument)

#convert corpus to Document Term Matrix object
dtm <- DocumentTermMatrix(corpus)

#convert to a matrix for analysis and rename rownames to either breeds or disease 
m <- as.matrix(dtm)
rownames(m) <- breeds_cleaned
rownames(m) <- disease
#calculate distance matrix 
d <- dist(m)

save(d, file = "d.Rdata")
load(file = "d.Rdata")

#############################################
#agglomerative clustering 
groups <- hclust(d, method="ward.D")

#divisive clustering 
div_clust <- diana(as.matrix(d), diss = TRUE, keep.diss = TRUE)

save(div_clust, file = "div_clust.Rdata")
load(file = "div_clust.Rdata")

#############################################
#cluster analysis 
#############################################
#code from Towards Data Science article 'Hierarchical Clustering on Categorical Data in R' 
#https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995
#this function calculates cluster stats for elbow and silhouette analysis 
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
    }
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}

#agglomerative clustering 
stats.df.agg <- cstats.table(d, groups, 11)
stats.df.agg
save(stats.df.agg, file = "data.Rdata")
load(file = "data.Rdata")
stats.df.agg <- stats.df.divisive
###silhouette plot
ggplot(data = data.frame(t(stats.df.agg)), aes(x=cluster.number, y = avg.silwidth)) + geom_point() + geom_line() + labs(x = "Num.of clusters", y = "Average silhouette width") + theme(plot.title = element_text(hjust = 0.5))
###elbow plot
ggplot(data = data.frame(t(stats.df.agg)), aes(x=cluster.number, y = within.cluster.ss)) + geom_point() + geom_line() + labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") + theme(plot.title = element_text(hjust = 0.5))

#divisive clustering 
stats.df.div <- cstats.table(d, div_clust, 11)
stats.df.div 
save(stats.df.div, file = "dataDIV.Rdata")
load(file = "dataDIV.Rdata")
###silhouette plot
ggplot(data = data.frame(t(stats.df.div)), aes(x=cluster.number, y = avg.silwidth)) + geom_point() + geom_line() + labs(x = "Num.of clusters", y = "Average silhouette width") + theme(plot.title = element_text(hjust = 0.5))
###elbow plot
ggplot(data = data.frame(t(stats.df.div)), aes(x=cluster.number, y = within.cluster.ss)) + geom_point() + geom_line() + labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") + theme(plot.title = element_text(hjust = 0.5))


#########################################################
#choose value of k 
#########################################################
##values of k for cluster information (change depending on breed vs disease)
k = 11
k = 7
g <- cutree(groups, k)

#investigate stats and visualizations depending on value of k
table(g)
plot(groups, cex = 0.6)
rect.hclust(groups, k = 4, border = 2:5)
rect.hclust(groups, k = 7, border = 2:5)

#split data into k number of clusters
gg <- hcut(d, k = 4, method="complete")
gg <- hcut(d, k = 7, method="complete")
fviz_cluster(list(data = as.matrix(d), cluster = g))
fviz_dend(gg, show_labels=FALSE, rect = TRUE)
fviz_cluster(gg)

l <- t(rbind(breeds_cleaned, g))
l <- order(l$g)
#l <- read.csv("clustering.csv", sep = ",", header = T)
l <- data.frame(l)
l <- l[order(l$g),]

####################################################################################

#create file with breed, known species, cluster number
data <- data[which(data$spp_combined=="ovis aries" | data$spp_combined == "sheep" | data$spp_combined == "bos taurus" | data$spp_combined == "bovine" | data$spp_combined == "cow" | data$spp_combined == "cattle" | data$spp_combined == "livestock" | data$spp_combined == "gallus gallus" | data$spp_combined == "chicken" | data$spp_combined == "hen" | data$spp_combined == "poultry" | data$spp_combined == "sus scrofa" | data$spp_combined == "pig" | data$spp_combined == "capra hircus" | data$spp_combined == "goat" | data$spp_combined == "lama glama" | data$spp_combined == "llama" | data$spp_combined == "camel" | data$spp_combined == "equid" | data$spp_combined == "equine" | data$spp_combined == "equus ferus caballus" | data$spp_combined == "equus ferus"| data$spp_combined == "equus caballus" | data$spp_combined == "equus kiang" | data$spp_combined == "equus zebra" | data$spp_combined == "equus asinus x equus caballus" | data$spp_combined == "equus przewalskii" | data$spp_combined == "equus hemionus kulan" | data$spp_combined == "equus asinus" | data$spp_combined == "donkey"), ]
dd <- cbind(data$breed, data$spp_combined)
colnames(dd) <- c("breed", "species")
rownames(dd) <- 1:nrow(dd)
dd <- data.frame(dd)
dd$breed <- gsub("strain: ", "", dd$breed)
dd$breed <- gsub("isolate: ", "", dd$breed)
dd$breed <- gsub("breed: ", "", dd$breed)
dd$breed <- gsub("label: ", "", dd$breed)
dd$breed <- gsub("transcriptome; ", "", dd$breed)
dd$breed <- gsub("ecotype: ", "", dd$breed)
dd$breed <- gsub("cultivar: ", "", dd$breed)
dd$breed <- str_replace_all(dd$breed, "[^[:alnum:]]", " ")
dd$breed <- gsub('[[:digit:]]+', '', dd$breed)
dd$breed <- gsub("  ", " ", dd$breed)
dd$breed <- dd$breed[!is.na(dd$breed)]
dd$breed <- gsub('\\b\\w{1,2}\\b','', dd$breed)
dd$breed <- dd$breed[-which(dd$breed =="")]
dd$breed <- dd$breed[-which(dd$breed ==" ")]
dd$breed <- gsub("  ", " ", dd$breed)
dd$breed <- trimws(dd$breed)
dd$breed <- tolower(dd$breed)
colnames(dd) <- c("breeds_cleaned", "species")

merge <- merge(dd, l, by="breeds_cleaned", all=TRUE)
v <- merge[-which(merge ==""),]
v <- unique(v)
v <- v[order(v$g),]
write.csv(v, "breeds_clusters_withSPP_11.csv")

####################################################################################

#visualize 
dendro <- as.dendrogram(groups)
dendro.col <- dendro %>%
  set("branches_k_color", k = k, value =   c("darkslateblue", "darkslategray4", "darkslategray3", "yellow2", "grey", "cyan3", "gold3", "aquamarine", "azure4", "cornsilk3")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("black")) %>% 
  set("labels_cex", 0.5) 
ggd1 <- as.ggdend(dendro.col)
ggd1 <- ggd1 %>% na.omit(ggd1$segments$col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 9")

#plot pie plot with breed and species information for each of the nine clusters
sppData <- read.csv("breeds_clusters_withSPP.csv", sep = ",", header = T)
one <- sppData[which(sppData$g==1),]
two <- sppData[which(sppData$g==2),]
three <- sppData[which(sppData$g==3),]
four <- sppData[which(sppData$g==4),]
five <- sppData[which(sppData$g==5),]
six <- sppData[which(sppData$g==6),]
seven <- sppData[which(sppData$g==7),]
eight <- sppData[which(sppData$g==8),]
nine <- sppData[which(sppData$g==9),]

par(mfrow=c(3,3))
pie(table(one$spp), main="cluster 1")
pie(table(two$spp), main="cluster 2")
pie(table(three$spp), main="cluster 3")
pie(table(four$spp), main="cluster 4")
pie(table(five$spp), main="cluster 5")
pie(table(six$spp), main="cluster 6")
pie(table(seven$spp), main="cluster 7")
pie(table(eight$spp), main="cluster 8")
pie(table(nine$spp), main="cluster 9")

data <- read.csv("clusters_for_pie_charts.csv", sep = ",", header = T)

#calculaate number of breeds per cluster 
df <- data %>% 
  group_by(cluster) %>% 
  mutate(perc = `count` / sum(`count`)) %>% 
  mutate(labels = scales::percent(perc))

#percentage information for each cluster
d <- data
d$percentage <- df$labels
#write.csv(d, "d.csv")
d <- read.csv("d.csv", sep = ",", header = T)

dff <- d %>% group_by(cluster) %>% mutate(pos = cumsum(count)- count/2)
d$size_f = factor(d$cluster, levels=c('one','two','three','four', 'five', 'six', 'seven', 'eight', 'nine'))

