# In this file, we explore one simulation experiment to see how the sampling scheme of the coalitions
# effect the overall precision of the Shapley value estimates when we are only using a subset of the
# coalitions compared to using all of them.
#
# As we are not focused on estimating the contribution functions v(S), but rather which of the
# S's are important, we choose to create Gaussian data and use the Gaussian approach.
# We will likely get similar results for other settings, but we do not investigate that here
# as that will add extra variability and uncertainty. We want a simple case to investigate.

# The steps used in this function are:
# 1. Generate Gaussian data and train a predictive model.
# 2. Compute the true contribution functions v_{true}(S) and true Shapley values
#    for the test data set using ALL coalitions.
# 3. Compute estimated Shapley values using only a subset of the coalitions
#    where which S to include are chosen by different coalition sampling schemes.
#    Do this for many different schemes and repeat it for many repetitions.
# 4. Compare and plot the results.


# Libraries -------------------------------------------------------------------------------------------------------
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(mgcv)


# Parameters ------------------------------------------------------------------------------------------------------
# Number of features
M = 6

# Mean of the multivariate Gaussian distribution
mu = rep(0, times = M)

# Create the covariance matrix
rho = 0.3
sigma = matrix(rho, ncol = M, nrow = M) # Old
for (i in seq(1, M-1)) {
  for (j in seq(i+1, M))
    sigma[i,j] = sigma[j,i] = rho^abs(i-j)
}
diag(sigma) = 1
sigma


# Data generating -------------------------------------------------------------------------------------------------
# Define the number of training and test observations
n_train = 1000
n_test = 250

# Set seed for reproducibility
set.seed(1996)

# Make Gaussian data
data_train = data.table(rmvnorm(n = n_train, mean = mu, sigma = sigma))
data_test  = data.table(rmvnorm(n = n_test,  mean = mu, sigma = sigma))
colnames(data_train) = paste("X", seq(M), sep = "")
colnames(data_test) = paste("X", seq(M), sep = "")

# Generate the responses according through a simple process.
# True model: Y = 2 + 1*X1 + 0*X2 - 3*X3 - 1*X4 + 1.5*X4*X5^2
response_train = unlist(2 + 1*data_train[,1] + 0*data_train[,2] - 3*data_train[,3] -
                          1*data_train[,4] + 1.5*data_train[,5]*data_train[,6]^2)
response_test = unlist(2 + 1*data_test[,1] + 0*data_test[,2] - 3*data_test[,3] -
                         1*data_test[,4] + 1.5*data_test[,5]*data_test[,6]^2)

# Just take a look at pairs plot of the data and some histograms
pairs(data_train)
plot(data_train$X5, data_train$X6^2)
par(mfrow = c(1,2))
hist(response_train, breaks = 50)
hist(response_test, breaks = 50)
par(mfrow = c(1,1))

# Put together the data
data_train_with_response = copy(data_train)[,y := response_train]
data_test_with_response  = copy(data_test)[,y := response_test]



# Predictive model ------------------------------------------------------------------------------------------------
# Fit a GAM model. We include an interaction term between X4 and X5 as we know the true data generating process.
# Furthermore, we include the main effects for X4 and X5 as this is needed for the interaction spline.
# We use `s()` for the main effects and `ti` for the pairwise effects. This yields the best results.
# THIS WAS WHEN I ONLY HAD M=5 FEATURES
# predictive_model_s = gam(y ~ s(X1) + s(X2) + s(X3) + s(X4) + s(X5) + s(X4, X5), data = data_train_with_response)
# predictive_model_s
# predictive_model_ti = gam(y ~ ti(X1) + ti(X2) + ti(X3) + ti(X4) + ti(X5) + ti(X4, X5), data = data_train_with_response)
# predictive_model_ti
# predictive_model_s_ti = gam(y ~ s(X1) + s(X2) + s(X3) + s(X4) + s(X5) + ti(X4, X5), data = data_train_with_response)
# predictive_model_s_ti
# predictive_model_s_ti_2 = gam(y ~ s(X1) + s(X2) + s(X3) + ti(X4) + ti(X5) + ti(X4, X5), data = data_train_with_response)
# predictive_model_s_ti_2
# predictive_model_s_2 = gam(y ~ s(X1) + s(X2) + s(X3) + ti(X4, X5), data = data_train_with_response)
# predictive_model_s_2
# mean((predict(predictive_model_s, data_test_with_response) - data_test_with_response$y)^2)
# mean((predict(predictive_model_ti, data_test_with_response) - data_test_with_response$y)^2)
# mean((predict(predictive_model_s_ti, data_test_with_response) - data_test_with_response$y)^2)
# mean((predict(predictive_model_s_ti_2, data_test_with_response) - data_test_with_response$y)^2)
# mean((predict(predictive_model_s_2, data_test_with_response) - data_test_with_response$y)^2)
# mean((predict(predictive_model, data_test_with_response) - data_test_with_response$y)^2)

# Our final GAM model
predictive_model = gam(y ~ s(X1) + s(X2) + s(X3) + s(X4) + s(X5) + s(X6) + ti(X5, X6), data = data_train_with_response)
predictive_model

# Look at the accuracy of the model
cat(sprintf("Training MSE = %g and test MSE = %g.\n",
            mean((predict(predictive_model, data_train_with_response) - data_train_with_response$y)^2),
            mean((predict(predictive_model, data_test_with_response) - data_test_with_response$y)^2)))

# See that the model is a good model
{
  par(mfrow = c(1,2))
  plot(data_train_with_response$y, predict(predictive_model, data_train_with_response),
       xlab = "True response", ylab = "Estimated response", main = "Training data")
  abline(a = 0, b = 1, col = "red")
  plot(data_test_with_response$y, predict(predictive_model, data_test_with_response),
       xlab = "True response", ylab = "Estimated response", main = "Test data")
  abline(a = 0, b = 1, col = "red")
  par(mfrow = c(1,1))
}


# Get the prediction zero, i.e., the phi0 Shapley value.
prediction_zero = mean(response_train)


#

# True Shapley ----------------------------------------------------------------------------------------------------
# Create the true Shapley value explanations
true_explanations <- explain(
  model = predictive_model,
  x_explain = data_test,
  x_train = data_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  keep_samp_for_vS = TRUE,
  n_combinations = 2^M,
  n_samples = 2000,
  n_batches = 50,
  gaussian.mu = mu,
  gaussian.cov_mat = sigma,
  seed = 1
)

# Save them just in case
saveRDS(true_explanations,
        paste("~/PhD/Paper3/shapr/Paper3_rds_saves/Paper3_Experiment_M", M, "rho", rho, "true.rds", sep = "_"))
true_explanations =
  readRDS(paste("~/PhD/Paper3/shapr/Paper3_rds_saves/Paper3_Experiment_M", M, "rho", rho, "true.rds", sep = "_"))


# Estimated Shapley -----------------------------------------------------------------------------------------------
# Specify the number of repetitions we repeat the whole experiment.
n_repetitions = 10

# The number of MC samples
n_samples = 100

# The number of batches
n_batches = 10

# We start with 2 ad we used 1 for the true Shapley values above.
seed_start_value = 2

# First value where the coalition/combination sampling scheme has an effect, (first two are empty and full coalitions)
n_combinations_from = 2

# We increase by one each time
n_combinations_increment = 2

# Path to save the results temporary
tmp_save_path = paste("~/PhD/Paper3/shapr/Paper3_rds_saves/Paper3_Experiment_M", M, "rho", rho,
                      "MC", n_samples, "estimated_tmp.rds", sep = "_")

# Which coalition/combination sampling schemes to use
sampling_methods = c("unique",
                     "unique_paired",
                     "non_unique",
                     "chronological_order_increasing",
                     "chronological_order_decreasing",
                     "largest_weights",
                     "largest_weights_combination_size",
                     "smallest_weights",
                     "smallest_weights_combination_size")

# Compute the repeated estimated Shapley values using the different sampling methods
repeated_estimated_explanations = repeated_explanations(
  model = predictive_model,
  x_explain = data_test[1:100,],
  x_train = data_train,
  approach = "gaussian",
  gaussian.cov_mat = sigma,
  gaussian.mu = mu,
  prediction_zero = prediction_zero,
  keep_samp_for_vS = FALSE,
  n_repetitions = n_repetitions,
  n_samples = n_samples,
  n_batches = n_batches,
  seed_start_value = seed_start_value,
  n_combinations_from = n_combinations_from,
  n_combinations_increment = n_combinations_increment,
  use_precomputed_vS = TRUE,
  sampling_method = sampling_methods,
  save_path = tmp_save_path)

# Save them just in case
saveRDS(repeated_estimated_explanations,
        paste("~/PhD/Paper3/shapr/Paper3_rds_saves/Paper3_Experiment_M", M, "rho", rho, "MC", n_samples, "estimated.rds", sep = "_"))
repeated_estimated_explanations =
  readRDS(paste("~/PhD/Paper3/shapr/Paper3_rds_saves/Paper3_Experiment_M", M, "rho", rho, "MC", n_samples, "estimated.rds", sep = "_"))



# Result figures --------------------------------------------------------------------------------------------------
true_explanations2 = true_explanations
true_explanations$shapley_values = true_explanations$shapley_values[1:100,]


result_figures = aggregate_and_plot_results(repeated_explanations_list =
                                              repeated_estimated_explanations,
                                            true_explanations = true_explanations,
                                            evaluation_criterion = "MAE",
                                            plot_figures = FALSE,
                                            return_figures = TRUE,
                                            return_dt = TRUE)
result_figures$figures$figure_CI
result_figures$figures$figure_mean
result_figures$figures$figure_lines
result_figures$figures$figure_boxplot

# We see that it is better to use the paired version
aggregate_and_plot_results(repeated_explanations_list =
                             repeated_estimated_explanations[c("unique",
                                                               "unique_paired")],
                           true_explanations = true_explanations,
                           evaluation_criterion = "MAE",
                           plot_figures = FALSE,
                           return_figures = TRUE,
                           return_dt = TRUE)$figures$figure_CI

# We see that adding the coalitions in increasing and decreasing order is a bad idea
aggregate_and_plot_results(repeated_explanations_list =
                             repeated_estimated_explanations[c("unique",
                                                               "unique_paired",
                                                               "chronological_order_increasing",
                                                               "chronological_order_decreasing")],
                           true_explanations = true_explanations,
                           evaluation_criterion = "MAE",
                           plot_figures = FALSE,
                           return_figures = TRUE,
                           return_dt = TRUE)$figures$figure_CI

# Smarter to add the coalitions based on larger Shapley kernel weights
# Here we see that also adding those with small are better,
# but I think that is a conicidence for this particular setup.
aggregate_and_plot_results(repeated_explanations_list =
                             repeated_estimated_explanations[c("unique",
                                                               "unique_paired",
                                                               "largest_weights",
                                                               "smallest_weights")],
                           true_explanations = true_explanations,
                           evaluation_criterion = "MAE",
                           plot_figures = FALSE,
                           return_figures = TRUE,
                           return_dt = TRUE)$figures$figure_CI


aggregate_and_plot_results(repeated_explanations_list =
                             repeated_estimated_explanations[c("unique",
                                                               "largest_weights",
                                                               "largest_weights_combination_size",
                                                               "smallest_weights",
                                                               "smallest_weights_combination_size")],
                           true_explanations = true_explanations,
                           evaluation_criterion = "MAE",
                           plot_figures = FALSE,
                           return_figures = TRUE,
                           return_dt = TRUE)$figures$figure_CI


# The different sampling methods:
# unique:
# The default version in shapr. It samples



repeated_explanations_list$largest_weights$repetition_1$n_combinations_8$internal$objects$X
repeated_explanations_list$largest_weights$repetition_2$n_combinations_10$internal$objects$X

repeated_explanations_list$smallest_weights$repetition_1$n_combinations_8$internal$objects$X


sampling_methods = c("unique",
                     "unique_paired",
                     "non_unique",
                     "chronological_order_increasing",
                     "chronological_order_decreasing",
                     "largest_weights",
                     "largest_weights_combination_size",
                     "smallest_weights",
                     "smallest_weights_combination_size")

