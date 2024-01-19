# libraries -------------------------------------------------------------------------------------------------------
library(shapr)
library(progressr)


# Setup -----------------------------------------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(123)

# Parameters
n_train <- 10000    # Number of training samples
n_explain <- 1000   # Number of explincands
n_features <- 3     # Number of features

# Mean and covariance matrix for the multivariate Gaussian distribution
mean_vector <- rep(0, n_features)
cov_matrix <- matrix(c(1, 0.5, 0.75, 0.5, 1, 0.2, 0.75, 0.2, 1), nrow = n_features, byrow = TRUE)
cov_matrix

# Generate x_train from multivariate Gaussian
x_train <- MASS::mvrnorm(n = n_train, mu = mean_vector, Sigma = cov_matrix)

# Generate x_explain from multivariate Gaussian with the same mean and covariance matrix as x_train
x_explain <- MASS::mvrnorm(n = n_explain, mu = mean_vector, Sigma = cov_matrix)

# Set column names
colnames(x_train) = colnames(x_explain) = paste0("X", seq(n_features))

# Display x_train and x_explain
pairs(x_train)
pairs(x_explain)

# Create some (very simple linear) response. To keep it simple, we do not add any noise now.
beta = c(3, -2, 1)
y_train = x_train %*% beta
y_explain = x_explain %*% beta

# Make data tables out of the training and test features + response
data_train = as.data.table(cbind(y_train, x_train))
data_explain = as.data.table(cbind(y_explain, x_explain))
colnames(data_train) = colnames(data_explain) = c("y", paste0("X", seq(n_features)))

# Train a linear model
model = lm(y ~ ., data = data_train)

# Set up progressr, so we get feedback from shapr
progressr::handlers(global = TRUE)
progressr::handlers('cli') # requires the 'cli' package

# Explain the model using conditional shapley values
explanation = shapr::explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = 0,
  n_batches = 7,
  n_samples = 1000)

max(abs(rowSums(explanation$shapley_values) - explanation$pred_explain))


# PCA -------------------------------------------------------------------------------------------------------------
# Compute principal components for x_train
pca_train <- prcomp(x_train, center = TRUE, scale. = FALSE)
x_train_pca = pca_train$x

# Look at some summary to see the importance of each principal component
summary(pca_train)
# pca_train$sdev == sqrt(diag(cov(x_train_pca)))

# Compute PC means and covariance
colMeans(x_train_pca) # Technically zero
cov(x_train_pca)

# Can also look at the pairs plot
pairs(x_train_pca)

# Use the same transformations on x_explain
x_explain_pca <- predict(pca_train, newdata = x_explain)
x_explain_pca
# Now, x_explain_pca contains the principal components of x_explain based on the transformations learned from x_train

# Compute PC means and covariance
colMeans(x_explain_pca) # Should be close to zero, but not exactly as we use the training data transformations
cov(x_explain_pca)

# Define the number of PCs to use
k = 3

# Use the inverse transformation to go back to the original space
aux_pca_scale = if (any(pca_train$scale == FALSE)) rep(1, ncol(pca_train$x)) else pca_train$scale
x_explain_pca_k <- x_explain_pca
x_explain_pca_k[, -(1:k)] = 0
x_explain_reconstructed = t(t(x_explain_pca_k %*% t(pca_train$rotation)) * aux_pca_scale + pca_train$center)

sqrt(sum((x_explain_reconstructed - x_explain)^2) / length(x_explain))


aux_pca_scale


x_explain_pca_k2 = x_explain_pca_k
x_explain_pca_k2[1:n_explain,1] = x_explain_pca_k2[n_explain:1,1]

tmp = t(t(x_explain_pca_k2 %*% t(pca_train$rotation)) * aux_pca_scale + pca_train$center)
pairs()
tmp[1,]
x_explain[1,]

# Now, original_space_x_explain contains the data in the original space



explanation_PCA = shapr::explain(
  model = model,
  x_explain = x_explain[1:4,],
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = 0,
  n_batches = 7,
  n_samples = 10,
  pca_rotation = pca_train$rotation,
  pca_scale = aux_pca_scale,
  pca_center = pca_train$center)


explanation_PCA$internal$parameters




compute_and_compare_new <- function(x_train, x_explain, k, scale. = TRUE) {
  # Step 0: tests
  if (ncol(x_train) != ncol(x_explain)) stop("`x_train` and `x_explain` have different number of features.")
  if (k <= 0 || k > nrow(x_train)) stop("`k` should be between 1 and the number of features.")

  # Step 1: Compute principal components for x_train
  pca_train <- prcomp(x_train, center = TRUE, scale. = scale.)

  # Step 2: Apply the transformation to x_explain
  x_explain_pca <- predict(pca_train, newdata = x_explain)

  # Step 3: Use the inverse transformation to go back to the original space
  aux_pca_scale = if (any(pca_train$scale == FALSE)) rep(1, ncol(pca_train$x)) else pca_train$scale
  x_explain_pca_k <- x_explain_pca
  x_explain_pca_k[, -(1:k)] = 0
  x_explain_reconstructed = t(t(x_explain_pca_k %*% t(pca_train$rotation)) * aux_pca_scale + pca_train$center)

  # NOTE: An alternative to the method above.
  # See "https://stackoverflow.com/questions/29783790/how-to-reverse-pca-in-prcomp-to-get-original-data".
  # scalling.matrix <- matrix(rep(pca_train$scale, nrow(x_explain)), ncol = ncol(x_explain), byrow = TRUE)
  # centering.matrix <- matrix(rep(pca_train$center, nrow(x_explain)), ncol = ncol(x_explain), byrow = TRUE)
  # x_explain_reconstructed2  = ((x_explain_pca_k %*% solve(pca_train$rotation)) * scalling.matrix) + centering.matrix
  # max(abs(x_explain_reconstructed - x_explain_reconstructed2))

  # Step 4: Compute a measure for how close x_explain_reconstructed is to x_explain
  reconstruction_error <- sqrt(sum((x_explain_reconstructed - x_explain)^2) / length(x_explain))

  # Return the results
  return(list(
    pca_train = pca_train,
    x_explain_pca_k = x_explain_pca_k,
    x_explain_reconstructed = x_explain_reconstructed,
    reconstruction_error = reconstruction_error
  ))
}


compute_and_compare_old <- function(x_train, x_explain, k, scale. = FALSE) {
  # Step 0: tests
  if (ncol(x_train) != ncol(x_explain)) stop("`x_train` and `x_explain` have different number of features.")
  if (k <= 0 || k > nrow(x_train)) stop("`k` should be between 1 and the number of features.")

  # Step 1: Compute principal components for x_train
  pca_train <- prcomp(x_train, center = TRUE, scale. = scale.)

  # Step 2: Apply the transformation to x_explain
  x_explain_pca <- predict(pca_train, newdata = x_explain)

  # Transformation back is influenced by if we used scaling or not.
  if (scale.) {
    # Step 3: Set all principal components except the first "k" principal components to zero
    x_explain_pca_k <- x_explain_pca
    x_explain_pca_k[, -(1:k)] = 0

    # Step 4: Invert the transformations back to the original variable space
    x_explain_reconstructed = t(t(x_explain_pca_k %*% t(pca_train$rotation)) * pca_train$scale + pca_train$center)

    # NOTE: An alternative to the method above.
    # See "https://stackoverflow.com/questions/29783790/how-to-reverse-pca-in-prcomp-to-get-original-data".
    # scalling.matrix <- matrix(rep(pca_train$scale, nrow(x_explain)), ncol = ncol(x_explain), byrow = TRUE)
    # centering.matrix <- matrix(rep(pca_train$center, nrow(x_explain)), ncol = ncol(x_explain), byrow = TRUE)
    # x_explain_reconstructed2  = ((x_explain_pca_k %*% solve(pca_train$rotation)) * scalling.matrix) + centering.matrix
    # max(abs(x_explain_reconstructed - x_explain_reconstructed2))

  } else {
    # Step 3: Extract the first "k" principal components
    x_explain_pca_k <- x_explain_pca[, 1:k]

    # Step 4: Invert the transformations back to the original variable space
    x_explain_reconstructed <- t(t(x_explain_pca_k %*% t(pca_train$rotation[, 1:k])) + pca_train$center)

    # NOTE: An alternative to the method above. Give equal results
    # sweep(x_explain_pca_k %*% t(pca_train$rotation[, 1:k]), 2, pca_train$center, "+")
  }

  # Step 5: Compute a measure for how close x_explain_reconstructed is to x_explain
  reconstruction_error <- sqrt(sum((x_explain_reconstructed - x_explain)^2) / length(x_explain))

  # Return the results
  return(list(
    pca_train = pca_train,
    x_explain_pca_k = x_explain_pca_k,
    x_explain_reconstructed = x_explain_reconstructed,
    reconstruction_error = reconstruction_error
  ))
}

sapply(1:3, function(k) compute_and_compare_old(x_train, x_explain, k = k, scale. = TRUE)$reconstruction_error)
sapply(1:3, function(k) compute_and_compare_old(x_train, x_explain, k = k, scale. = FALSE)$reconstruction_error)

sapply(1:3, function(k) compute_and_compare_new(x_train, x_explain, k = k, scale. = TRUE)$reconstruction_error)
sapply(1:3, function(k) compute_and_compare_new(x_train, x_explain, k = k, scale. = FALSE)$reconstruction_error)







