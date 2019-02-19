# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine$Type = NULL
scale(wine) # adjusting because there are multiple variables that use different scales
head(wine)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine) 


# Exercise 2:
#   * How many clusters does this method suggest? 
#     Two to four clusters; can go with three clusters
#   * Why does this method work? What's the intuition behind it? 
#     This code is a function that:
#     1.) Sets the cluster count to 15
#     2.) Counts the row in the data, subtracts one to identify each row, and then
#     ...multiples the value by itself to create squares of values
#     3.) We loop through each cluster, and the seed is set to ensure
#     ...consistent results for all users.
#     4.) The loop also sums the k-mean of the squared data 
#     5.) We then plot from 1 to 15 clusters in x and the sum squared values in y
#     6.) Finally, we label the plot.

# The intitution is that we are computing the sum of square total within each cluster with 
# ...wss (within-cluster sum of square) to minimize variation within each cluster.
# Once, we minimize variation, we can easily compare values per cluster and we want to look
# ...where the elbow is in the data because we are seeing where the large variance decreases 
# ...before leveling out. This is called the Elbow Method.

#   * Look at the code for wssplot() and figure out how it works

  

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# According to the majority rule, the best number of clusters is  2 


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

# fit.km <- kmeans(wine, centers=2, iter.max=10)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
data(wine, package="rattle.data")
ct.km <- table(wine$Type, fit.km$cluster)
ct.km 
library(flexclust)
randIndex(ct.km)
# The adjusted Rand index provides a measure of the agreement between two partitions, 
# adjusted for chance. It ranges from -1 (no agreement) to 1 (perfect agreement). 
# Agreement between the wine varietal type and the cluster solution is 0.4. 
# I would say this clustering can be improved.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
install.packages("cluster")
library(cluster)
#clusplot( ... ) # Need assistance here. I am not sure what this is asking me to do. This is
# ...problematic that this file is asking for information to be completed in the assignment
# ...that was not covered in the training. 
# This link does not provide clear guidance: 
# https://www.rdocumentation.org/packages/cluster/versions/2.0.7-1/topics/clusplot
