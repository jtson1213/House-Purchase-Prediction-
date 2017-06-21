###########################################
# Cluster Analysis
###########################################

housing = read.csv("Final dataset_ver2.csv", header=T,sep=",")
housingNum=housing[,2:8]
housingNumScale=scale(housingNum)
head(housingNumScale)

###Choose optimal k based on elbow method
###First method to choose best k
set.seed(123)
k.max<-15
wss<-sapply(1:k.max, function(k){kmeans(housingNumScale,k,nstart = 10)$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v=3,lty=2)
###Second method to choose best k

library(factoextra)
library(ggplot2)
fviz_nbclust(housingNumScale, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

###k=3 is the best
###fit kmeans
fit = kmeans(housingNumScale, 3)
fit

###Hierarchical Clustering Graph:
###Compute pairewise distance matrices
housingNumTrans=t(housingNum)
dist.res <- dist(housingNumTrans, method = "euclidean")

###Hierarchical clustering results
fit = hclust(dist.res, method="ward.D2")

###Visualization of hclust-TreeMap, k=3
plot(fit)
rect.hclust(fit, k = 3, border = 2:4) 


#Cluster Analysis II:
# reading the data
dataset = read.csv("Final dataset_ver2.csv")
DemographicInfo = data.frame(Appended.Age.1=dataset$Appended.Age.1,
                             PROP_MRKTVAL=dataset$PROP_MRKTVAL,
                             PROP_LIVINGSQFT=dataset$PROP_LIVINGSQFT, 
                             PROP_BEDRMS=dataset$PROP_BEDRMS,
                             Home.Market.Value=dataset$Home.Market.Value,
                             Estimated.Income=dataset$Estimated.Income,
                             Wealth.Score=dataset$Wealth.Score, 
                             Purchasing.Power.Score=dataset$Purchasing.Power.Score
)

#log transform on numerical variable due to skewedness
DemographicInfo[,1]=log10(DemographicInfo[,1])
DemographicInfo[,2]=log10(DemographicInfo[,2])
DemographicInfo[,3]=log10(DemographicInfo[,3])
DemographicInfo[,4]=as.factor(DemographicInfo[,4])

# creating distance matrix
gower_dist <- daisy(DemographicInfo, metric = "gower", type = list(logratio = 3))

# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot sihouette width (higher is better)
plot(1:10, sil_width, xlab = "Number of clusters", ylab = "Silhouette Width")
lines(1:10, sil_width)

# Descriptive statistic
library(magrittr)
library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
pam_results <- DemographicInfo %>%
  #dplyr::select(-name) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

# Plot the clusters
library(Rtsne)
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = DemographicInfo$Wealth.Score)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
