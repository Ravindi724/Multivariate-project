#D/DBA/21/0018
#KHR Pabasara
#Multivariate Data Analysis - Assignment 01


############################## ~ Objective 01 ~ ##############################

#libraries
library(readxl)
library(tidyverse)

data<-read_excel("D:/Academic Stuff/3rd year/2nd sem/Multivariate/Assignment/CM 3052_project data (1).xlsx")

#Display the first few rows of the data set
head(data)

 
#Summary statistics for the data set
summary(data)

#Structure of the data set
str(data)

# Create a correlation matrix
correlation_matrix <- cor(data)

# Display the correlation matrix
correlation_matrix

# Visualize correlations with a heat map
library(ggplot2)
library(reshape2)

melted_corr_matrix <- melt(correlation_matrix)


ggplot(melted_corr_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  theme_minimal() +
  scale_fill_gradient2(low = "black", mid = "white", high = "blue", midpoint = 0) +
  labs(title = "Correlation Heatmap")

#checking null values
null_val <-sum(is.null(data))
null_val

#removing the first column which is well water sample_no
data<-data[-1]


# Performing of PCA
#calculate principal components (based on correlation matrix "scale = TRUE")
pca_result <- prcomp(data,scale=TRUE)
pca_result


# Extract eigenvalues
eigenvalues <- pca_result$sdev^2

# Create a scree plot
plot(1:length(eigenvalues), eigenvalues, type = "b", 
     xlab = "Principal Component", ylab = "Variance Explained",
     main = "Scree Plot")


############################### ~ Objective 02 ~ ##############################

#----------* hierarchical clustering*
#----------* NB clustering

# Install and load the factoextra package 
install.packages("factoextra")
library(factoextra)


fviz_nbclust(data, kmeans, method = "wss")


#method ="kmeans"
library(NbClust)
NbClust(data = data, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
        method = "kmeans", index = "all", alphaBeale = 0.1)

#method ="complete"
library(NbClust)

NbClust(data = data, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
        method = "complete", index = "all", alphaBeale = 0.1)


#method =average
library(NbClust)
NbClust(data = data, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
        method = "average", index = "all", alphaBeale = 0.1)

#method=ward.D2
library(NbClust)

NbClust(data = data, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
        method = "ward.D2", index = "all", alphaBeale = 0.1)

#optimal no of clusters =3

#Distance matrix
d_data <- dist(data)
d_data

#Dendrogram
wardse <- hclust(d_data,"ward.D2")
plot(wardse)

plot(wardse, hang = -1)

rect.hclust(wardse, k=3 , border="blue")
rect.hclust(wardse, k=3 , border= 2:5)



##-------- Display values include in each cluster

# Load necessary libraries
library(cluster)


# Sample data (replace with your data)
data <- data
 

# Calculate hierarchical clustering using Ward's linkage
hc <- hclust(dist(data), method = "ward.D2")


# Specify the desired number of clusters
k <- 3  # Adjust as needed

# Cut the dendrogram to define 'k' clusters
clusters <- cutree(hc, k)


# Print the data points that belong to each cluster
for (i in 1:k) {
  cat("Cluster", i, ":", which(clusters == i), "\n")
}


############################### ~ Objective 03 ~ ###############################

standard_values <- c(Be = 4, Cr = 100, Fe = 300, Ni = 20 , Cu = 1300, As = 10, Cd =5, Ba = 2000, Tl = 0.5, Pb = 15, U = 30)
standard_values

mean_values <- colMeans(data[, c("Be", "Cr", "Fe", "Ni", "Cu", "As", "Cd", "Ba", "Tl", "Pb", "U")])
mean_values



# Comparing of mean values with standard values
n=92 #no of samples
p=11 #no of variables
dset <- cbind(data)
S <- cov(dset)
S
x_bar = matrix(mean_values,c(11,1))
x_bar

mu_note =matrix(standard_values,c(11,1))
mu_note

#Test statistics 
T2_cal <- n*t(x_bar-mu_note)%*%solve(S)%*% (x_bar-mu_note)
T2_cal

Table_value =(n-1)*p/(n-p)*qf(0.95,p,n-p)
Table_value

