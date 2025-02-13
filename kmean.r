set.seed(42)

euclidean_distance <- function(a, b) {
  return(sqrt(sum((a - b) ^ 2)))
}

initialize_centroids <- function(X, k, initial_centroids) {
  return(initial_centroids)
}

assign_clusters <- function(X, centroids) {
  clusters <- c()
  for (i in 1:nrow(X)) {
    distances <- apply(centroids, 1, euclidean_distance, b = as.numeric(X[i, ]))
    clusters[i] <- which.min(distances)
  }
  return(clusters)
}

update_centroids <- function(X, clusters, k) {
  new_centroids <- matrix(nrow = k, ncol = ncol(X))
  for (i in 1:k) {
    cluster_points <- X[clusters == i, , drop = FALSE]
    if (nrow(cluster_points) > 0) {
      new_centroids[i, ] <- colMeans(cluster_points)
    } else {
      new_centroids[i, ] <- X[sample(nrow(X), 1), ]
    }
  }
  return(new_centroids)
}

k_means <- function(X, k, initial_centroids, max_iters = 100, tol = 1e-4) {
  centroids <- initialize_centroids(X, k, initial_centroids)
  
  for (i in 1:max_iters) {
    cat(sprintf("Iteration %d:\n", i))
    print(centroids) 
    
    clusters <- assign_clusters(X, centroids)
    new_centroids <- update_centroids(X, clusters, k)
    
    centroids <- as.matrix(centroids)
    new_centroids <- as.matrix(new_centroids)
    
    if (all(abs(new_centroids - centroids) < tol)) {
      cat("Converged!\n")
      break
    }
    
    centroids <- new_centroids
  }
  
  return(list(clusters = clusters, centroids = centroids))
}

k <- as.integer(readline(prompt = "Enter the number of clusters: "))
num_points <- as.integer(readline(prompt = "Enter the number of data points: "))

data <- data.frame(A = numeric(num_points), B = numeric(num_points))
for (i in 1:num_points) {
  data$A[i] <- as.numeric(readline(prompt = paste("Enter x-coordinate of point", i, ": ")))
  data$B[i] <- as.numeric(readline(prompt = paste("Enter y-coordinate of point", i, ": ")))
}

initial_centroids <- matrix(nrow = k, ncol = 2)
for (i in 1:k) {
  initial_centroids[i, 1] <- as.numeric(readline(prompt = paste("Enter x-coordinate of centroid", i, ": ")))
  initial_centroids[i, 2] <- as.numeric(readline(prompt = paste("Enter y-coordinate of centroid", i, ": ")))
}

X <- data[, c("A", "B")]

result <- k_means(X, k, initial_centroids)

clusters <- result$clusters
centroids <- result$centroids

for (i in 1:k) {
  cat(sprintf("Cluster %d:\n", i))
  print(X[clusters == i, ])
  cat("\n")
}

cat("Final Centroids:\n")
print(centroids)

plot(X$A, X$B, col = clusters, pch = 19, xlab = "A", ylab = "B", main = "K-Means Clustering")
points(centroids[, 1], centroids[, 2], col = "green", pch = 8, cex = 2)
