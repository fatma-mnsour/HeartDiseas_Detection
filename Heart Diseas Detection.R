# Clustering Heart Disease Patients

###################################
# Import Data
###################################
install.packages('factoextra')
install.packages("tidyverse")
install.packages("car")
install.packages('lattice')
#package of hirarcal
install.packages("dplyr")
library(readr)
#library of hirarcal
library(dplyr)
library(lattice)
library(ggplot2)
#Heartdiseas <- read_csv("Heartdiseas.txt")
Heartdiseas=read.delim("Heartdiseas.txt",sep=",")
View(Heartdiseas)

str(Heartdiseas)

###################################
# Preprocessing
###################################
# Remove id
Heartdiseas = Heartdiseas[ , !(names(Heartdiseas) %in% c("id"))]

# cleaning data
s = sum(is.na(Heartdiseas))
s
df <- na.omit(Heartdiseas)
d <- sum(duplicated(df))
d
df <- scale(Heartdiseas) 
head(df)
#statistics
summary(df) 
range(df) 
sd(df) 
cor(df) 
var(df)
#visualize

hist(Heartdiseas$age, data=df)

hist(Heartdiseas$cp, data = df)

boxplot(Heartdiseas$age, Heartdiseas$thalach, names = c("age", "thalach"))

boxplot(Heartdiseas$oldpeak ~ Heartdiseas$slope, data = df)

#barplot(Heartdiseas$age,Heartdiseas$thalach,data=df)
#barplot(Heartdiseas$age, beside=TRUE,scale(Heartdiseas$thalach,center = TRUE,scale = TRUE))
plot(Heartdiseas$age,Heartdiseas$thalach,data=df)
#barplot(Heartdiseas$cp,Heartdiseas$exang,data=df)
plot(Heartdiseas$cp, Heartdiseas$exang, data = df)


library(factoextra)
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
fviz_cluster(k2, data = df)
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

set.seed(250)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
library(tidyverse)
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# Set the seed so that results are reproducible
seed_val = 60
set.seed(seed_val, kind = "Mersenne-Twister", normal.kind = "Inversion")

# Select a number of clusters
k = 2

# Run the k-means algorithms
first_clust = kmeans(df, centers = k, nstart = 25)

# How many patients are in each group?
first_clust$size

# Set the seed
seed_val =50
set.seed(seed_val, kind = "Mersenne-Twister", normal.kind = "Inversion")

# Run the k-means algorithms
second_clust = kmeans(df, centers = k, nstart=25)

# How many patients are in each group?
second_clust$size


# Adding cluster assignments to the data
Heartdiseas['first_clust'] = first_clust$cluster
Heartdiseas['second_clust'] = second_clust$cluster

# Load ggplot2
library(ggplot2)

# Creating the plots of age and chol for the first clustering algorithm
plot_one = ggplot(Heartdiseas, aes(x =age, y = thalach, color = as.factor(first_clust))) + 
  geom_point()
plot_one 

# Creating the plots of age and chol for the second clustering algorithm
plot_two = ggplot(Heartdiseas, aes(x = age, y = thalach, color = as.factor(second_clust))) + 
  geom_point()
plot_two



########################
#Hierarchical Clustering
########################
#define linkage methods
library(cluster)
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)

# Finding distance matrix
distance_mat <- dist(df, method = 'euclidean')
distance_mat
# Fitting Hierarchical clustering Model
# to training dataset


#first hirarcal round
#set.seed(38)  # Setting seed
Hierar_cl <- hclust(distance_mat, method = "ward.D")
Hierar_cl
plot(Hierar_cl)
# Cutting tree by no. of clusters
fit1 <- cutree(Hierar_cl, k = 3 )
fit1
#second round
#set.seed(50)  # Setting seed
Hierar_c2 <- hclust(distance_mat, method = "single")
Hierar_c2
plot(Hierar_c2)
# Cutting tree by no. of clusters
fit2 <- cutree(Hierar_c2, k = 3 )
fit2

# Adding assignments of chosen hierarchical linkage
Heartdiseas['hc_clust'] = fit1

# Remove 'sex', 'first_clust', and 'second_clust' variables
hd_simple = Heartdiseas[, !(names(Heartdiseas) %in% c("sex", "Hierar_c1", "Hierar_c2"))]

# Getting mean and standard deviation summary statistics
clust_summary = do.call(data.frame, aggregate(. ~ hc_clust, data = hd_simple, function(x) c(avg = mean(x), sd = sd(x))))
clust_summary
# Choosing no. of clusters
# Cutting tree by height
abline(h = 110, col = "green")

rect.hclust(Hierar_cl, k = 3, border = "green")

###########
#conclution
###########
#kmeans when k = 2 (TRUE)
#hierarical when method = ward (TRUE)
#hierarical when method = single (FALSE)

