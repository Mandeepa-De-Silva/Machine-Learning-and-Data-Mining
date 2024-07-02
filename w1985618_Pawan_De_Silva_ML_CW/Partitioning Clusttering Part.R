library(NbClust)
library(ggpubr)
library(factoextra)
library(ggplot2)
library(caret)
library(cluster)
library(fpc)

#import whitewine data set 
whitewhine_data <- read.csv("Whitewine_v6.csv")

#calculate  total number of missing values
sum(is.na(whitewhine_data))
#Provide a summary of logicsal values which indicate the missing values or not
summary(is.na(whitewhine_data))

#store attributes from column 1 to 11
numarical_variables <- whitewhine_data[,-12]
#quality type of the 
quality_variable <- whitewhine_data$quality

#create a empty vector for store found outliers during the data analysis
Outliers <- c()

#graphical summary of the distribution 
boxplot(numarical_variables)

for(i in 1:11){
  column <- numarical_variables[, i]
  logical_vector <- column%in% boxplot.stats(column)$out
  current_column_out <- which(logical_vector == TRUE)
  Outliers <- c(Outliers,current_column_out)
}


eleminate <- unique(Outliers) #containing unique outliers 
eleminate <- sort(eleminate) #ascending order 
removed_data <- whitewhine_data[-eleminate,] #remove outliers

#after remove outliers from original data set
boxplot(removed_data[,-12])

#Scaling process for standardize the rage 
scaled_removed_data <- data.frame(scale(removed_data[,-12]))

#after scaling 
boxplot(scaled_removed_data)


#Automated Tools
#nbc automated tool
nbc <- NbClust(scaled_removed_data, distance = "euclidean", 
               method = "kmeans", min.nc = 2, max.nc = 10, index = "all")

#Elbow method  
set.seed(123)
fviz_nbclust(scaled_removed_data, kmeans, method = "wss")+
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow Method", x = "Number of clusters k",
       y= "within-cluster sum of squares(WSS)")

#Gap-static automated tool
set.seed(123)
gap_stat <- clusGap(scaled_removed_data, kmeans, K.max = 10,
                    nstart = 25, B =50)
fviz_gap_stat(gap_stat)

#silhouette Method
fviz_nbclust(scaled_removed_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette Method")


#k-means
#k-means clustering using 2 clusters based on sacaled data
kmean_2 <- kmeans(scaled_removed_data,2)

#visualization of the k-means clustering result
fviz_cluster(kmean_2, data = scaled_removed_data,
             palette= c("#ff4500", "#836fff"),
             geom = "point", ellipse.type = "convex", ggtheme = theme_bw()
             )

kmean_2

centers <- kmean_2$centers #cluster centers 
bss <- kmean_2$betweenss # amount of variation between cluster centers
tss <- kmean_2$totss # total amount variation  in the data
wss <- kmean_2$withinss # amount of variation within each cluster
kmean_2[["tot.withinss"]]
kmean_2[["betweenss"]]
kmean_2[["totss"]]

#for clustering effectiveness
ratio_bss_tss <- (bss/tss)*100 

#average silhouette width 
silh <- silhouette(kmean_2$cluster, dist(scaled_removed_data))
fviz_silhouette(silh, palette=c("#ff4500", "#836fff"))




#--------------------------#Principal Component Analysis#-----------------------

#calculate the principal components of scaled removed data
pca <- prcomp(scaled_removed_data)  
summary(pca)

#eigenvalues and eigenvectors
-pca$rotation #principal components loading
pca$sdev^2  #standard deviation of every PC

scree <- ggplot(data.frame(prop_var = pca$sdev^2/sum(pca$sdev^2),   #calculate the proportion of each PC
                           cum_var = cumsum(pca$sdev^2/sum(pca$sdev^2)),
                           PC = 1:length(pca$sdev)), #represent the PC 1 to total
                aes(x = PC, y = cum_var)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1,length(pca$sdev))) +
  xlab("Number of principal components") +
  ylab("Cumulative proportion of variance explained") +
  ggtitle("Scree plot")

scree

#get the first 7 PCs
transformed <- data.frame(pca$x[,1:7], class= removed_data[,-13])
transformed_data <- transformed[, 1:7]

#automated tools
#NbClust method
nbc_pca <- NbClust(transformed_data, distance="euclidean", 
                   method="kmeans", min.nc=2, max.nc=10, index="all")


#Elbow method with PCA

set.seed(26)
fviz_nbclust(transformed_data, kmeans, method="wss")+
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow Method for PCA Data",
       x='Number of Clusters K', y='within cluster sum of square')


#gap statistic method with PCA
set.seed(26)
gap_stat_pca <- clusGap(transformed_data, kmeans, K.max = 10,
                        nstart=25, B=50)
fviz_gap_stat(gap_stat_pca)

#silhouette method with PCA
fviz_nbclust(transformed_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method for PCA data")


#kmeans with PCA
pca_kmean_2 <- kmeans(transformed_data,2)

fviz_cluster(pca_kmean_2, data = transformed_data,
             palette= c("#ff7f00", "#ff4500"),
             geom = "point", ellipse.type = "convex", ggtheme = theme_bw()
             )
pca_kmean_2

pca_centers <- pca_kmean_2$centers
pca_bss <- pca_kmean_2$betweenss #amount variance between cluster centroids
pca_tss <- pca_kmean_2$totss   #total amount of variation in data
pca_wss <- pca_kmean_2$withinss   # amount of variance within the cluster
pca_kmean_2[["betweenss"]]
pca_kmean_2[["tot.withinss"]]
pca_kmean_2[["totss"]]
pca_kmean_2[["centers"]]

#clustering effectiveness after PCA
pca_ratio_bss_tss <- (pca_bss/pca_tss)*100
pca_ratio_bss_tss


#average silhouette score after PCA
pca_silh <- silhouette(pca_kmean_2$cluster, dist(transformed_data))
fviz_silhouette(pca_silh, palette= c("#ff7f00", "#ff4500"))

#quality of a clustering solution is better when the index value is higher
ch_index <- calinhara(transformed_data, pca_kmean_2$cluster)
print(ch_index)
