# Install the necessary packages: dplyr, cluster, clValid
#install.packages("dplyr")
#install.packages("cluster")
#install.packages("clValid")
#install.packages("vegan")

# Load the .csv files
finance <- read.csv("finance.csv", header=TRUE, sep=";")
legal <- read.csv("legal.csv", header=TRUE, sep=";")
marketing <- read.csv("marketing.csv", header=TRUE, sep=";")
sales <- read.csv("sales.csv", header=TRUE, sep=";")
purchase <- read.csv("purchase.csv", header=TRUE, sep=";")
production <- read.csv("production.csv", header=TRUE, sep=";")

# Bind all data from files into one variable 
# by using the bind_rows() method from dplyr library

library(dplyr)
measures <- bind_rows(finance, legal, marketing, sales, purchase, production)

# Let's get a summary of our data using the summary method
summary(measures)

# Set the RDG
set.seed(20)

# Do the kmeans calculation by calling kmeans()
# And storing the result in measures_km variable
measures_km <- kmeans(x=measures[,2:3], centers=3, nstart=20)

# Save the cluster number in the data set as column 'SIZE'
measures$SIZE <- as.factor(measures_km$cluster)

# Inspect 'clusters'
str(measures_km)

# Plot the height as function of weight using plot()
# Setting col = measures_km$cluster colors your points based on the corresponding object's cluster.
plot(x=measures$EMP_WEIGHT, y=measures$EMP_HEIGHT, xlab = "Weight", ylab = "Height", col = measures_km$cluster)

# Calculate cluster statistics using cluster.stats() in "fpc" library
library(fpc)
d <- dist(measures, method = "euclidean")
cluster.stats(d, measures_km$cluster)

# Initialise Within Cluster Sum of Squares ratio_ss
ratio_ss <- rep(0, 7)

# Finish the for-loop
for (k in 1:7) {
  
  # Apply k-means to measures: measures_kmexp
  measures_kmexp <- kmeans(x=measures[,2:3], centers=k, nstart=20)
  
  # Save the ratio between of WSS to TSS in kth element of ratio_ss
  ratio_ss[k] <- measures_kmexp$tot.withinss / measures_kmexp$totss
  
}

# Make a scree plot with type "b" and xlab "k"
plot(ratio_ss, type = "b", xlab = "k", ylab="WSS/TSS ratio")
