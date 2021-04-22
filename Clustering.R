#CLUSTERING
patient_data<- read.csv("patient.csv") 
str(patient_data) 
patient_data[["X"]]<-NULL 
patient_data[["payer_code"]]<-NULL 
library(cluster)

#scatter plot
plot(time_in_hospital~ diag_1, patient_data) 
?daisy
dis<- daisy(patient_data, metric = "gower") 
print(dis, digits = 3)
head(dis)

#complete linkage 
hc.c<-hclust(dis) 
plot(hc.c,hang = -1, cex = 0.6)

#average linkage
hc.a<- hclust(dis, method = "average")

#cluster membership 
member.c<-cutree(hc.c, 5) 
member.c

member.a<- cutree(hc.a,5) 
table(member.c, member.a)

#cluster means
aggregate(patient_data, list(member.c), mean)

#silhoutte plot
library(cluster)
windows() 
plot(silhouette(cutree(hc.c,5), dis))
table(member.a, patient_data$readmitted)

#dendextend 
library(dendextend) 
dend<-as.dendrogram(hc.c) 
d1=color_branches(dend,k=5) 
plot(d1)

plot(hc.c, type = "phylogram", show.tip.label = TRUE, edge.color = "red", edge.width = 1, edge.lty = 1, tip.color = "blue")


install.packages("devtools")
library(devtools) 
devtools::install_github("kassambara/factoextra") 
library(factoextra)

#clustering
clust_data<- patient_data
head(patient_num_data)
medians<- apply(clust_data,2,median)
mads<- apply(clust_data,2,mad)
clust_data = scale(clust_data,center=medians,scale=mads)

patient.dist<-dist(clust_data)
#hclust uses complete linkage
patient.clust<-hclust(patient.dist) 
plot(patient.clust,labels=patient_data$readmitted,main='Default from hclust')

#dendextend package 
install.packages("dendextend") 
install.packages("colorspace")

library(dendextend) 
library(colorspace)

k <- 4
cols <- rainbow_hcl(k)
dend <- as.dendrogram(patient.clust) 
dend <- color_branches(dend, k = k) 
plot(dend)
labels_dend <- labels(dend)
groups <- cutree(dend, k=4, order_clusters_as_data = FALSE) 
dends <- list()
for(i in 1:k) {
  labels_to_keep <- labels_dend[i != groups]
  dends[[i]] <- prune(dend, labels_to_keep) }
par(mfrow = c(2,2)) 
for(i in 1:k) {
  plot(dends[[i]],
       main = paste0("Tree number ", i))
}
groups.12 = cutree(patient.clust,12) 
table(groups.12)

#Try clustering 
library(dplyr) 
library(cluster) 
library(ggplot2)
?daisy

gower_dist<-daisy(patient_data, metric = "gower")
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)
#find most similar data pairs
patient_data[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),arr.ind = TRUE)[1, ], ]
sil_width <- c(NA) 
for(i in 2:10){
  pam_fit <- pam(gower_dist, 
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
#Silhouette analysis measures how well an observation is clustered and
#it estimates the average distance between clusters. The silhouette plot displays a 
#measure of how close each point in one cluster is to points in the neighboring clusters. 
#plot sil width, higher the better

plot(1:10, sil_width,
     xlab = "Number of clusters", 
     ylab = "Silhouette Width")
lines(1:10, sil_width)

