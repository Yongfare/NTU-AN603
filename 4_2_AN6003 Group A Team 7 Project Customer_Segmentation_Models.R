# Customer Segmentation Part- Team 7 
# Load useful packages
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
setwd("/Users/rainiem/Desktop/NTU MSBA/Analytics Strategy/Team Assignment and Project")
# Load data
df=read.csv("bank_customer_survey.csv",header=TRUE,stringsAsFactors=TRUE)
df1=df[c(1:40833),c(1:7)]
# Compute the Gower distance
gower_dist=daisy(df1,metric = "gower")
gower_matrix=as.matrix(gower_dist)
# Have a sense of the most similar clients
df1[which(gower_matrix == min(gower_matrix[gower_matrix != min(gower_matrix)]), arr.ind = TRUE)[1, ], ]
# Have a sense of the most dissimilar clients
df1[which(gower_matrix == max(gower_matrix[gower_matrix != max(gower_matrix)]), arr.ind = TRUE)[1, ], ]
# Calculate the Silhouette Width for cluster number K = 2,3,4,5
sil_width=c(NA)
for(i in 2:5){  
  pam_fit=pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i]=pam_fit$silinfo$avg.width  
}
# Plot the Silhouette Width for cluster number K = 2,3,4,5
plot(1:5, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:5, sil_width)
# From the plot of Silhouette Width,K=5 has the highest silhouette width.Let's pick K=5.
k=5
pam_fit=pam(gower_dist, diss = TRUE, k)
pam_results <- df1 %>% # %>%:pass the left hand side of the operator to the first argument of the right hand side of the operator
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
# Derive some common patterns for clients within a cluster
pam_results$the_summary
# Plot the cluster from a higher dimension to a lower dimension
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
``