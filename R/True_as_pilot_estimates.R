# In this file we are going to explore if there is any merit in the idea about using pilot estimates.
# To check this we will first make it easy for our self and assume that the pilot estimates are given
# to us. In this case, we will use the true contribution function values as the pilot estimates
library(data.table)
library(ggplot2)


# Load some stuff -------------------------------------------------------------------------------------------------
# We load the true explanations
explanation = readRDS("~/PhD/Paper3/shapr/Paper3_rds_saves/Paper3_Experiment_M_5_n_train_1000_n_test_250_rho_0_betas_2_1_0.25_-3_-1_1.5_true.rds")
explanation = readRDS("~/PhD/Paper3/shapr/Paper3_rds_saves/Paper3_Experiment_M_5_n_train_1000_n_test_250_rho_0.9_betas_2_1_0.25_-3_-1_1.5_true.rds")
explanation = readRDS("~/PhD/Paper3/shapr/Paper3_rds_saves/Paper3_Experiment_M_5_n_train_1000_n_test_250_rho_0_betas_0_1_1_1_1_1_true.rds")
explanation = readRDS("~/PhD/Paper3/shapr/Paper3_rds_saves/Paper3_Experiment_M_5_n_train_1000_n_test_250_rho_0.9_betas_0_1_1_1_1_1_true.rds")

# Extract the internal list from the explanation object
internal = explanation$internal

# Get the number of features
M = internal$parameters$n_features

# We extract the W and S matrices
W = explanation$internal$objects$W
S = explanation$internal$objects$S

# Extract the v(S) elements
dt_vS = explanation$internal$output$dt_vS

# The W matrix is the same as the R matrix in the papers.
# And it is such that Phi = W %*% v(S). We can check this. (Only column name that is different)
all.equal(as.data.table(t(W %*% as.matrix(internal$output$dt_vS[,-"id_combination"]))), explanation$shapley_values)



# Look at a single test obs ---------------------------------------------------------------------------------------
# Let us consider a single test observations first
test_obs_idx = 10
# vS_for_one_test_obs = as.matrix(dt_vS[,..test_obs_idx]) cannot add one here
vS_for_one_test_obs = as.matrix(dt_vS[,.SD, .SDcols = test_obs_idx+1])

# Let us element-wise multiply W together with the v(S) for the test observation.
W_and_vS_element_wise_product = sweep(x = W, MARGIN = 2, STATS = vS_for_one_test_obs, FUN = "*")

# We are then getting the terms that together sum to the Shapley values.
# So these are the W_ij*v(S)_j entries for j = 1,2,...,2^M
W_and_vS_element_wise_product

# That is, each row sums to the Shapley values.
rowSums(W_and_vS_element_wise_product)
t(W %*% vS_for_one_test_obs)
explanation$shapley_values[test_obs_idx,]

# Let us compute the cumulative sum
W_and_vS_cumsum = apply(W_and_vS_element_wise_product, 1, cumsum)

# We see that this adds up to the Shapley values
matplot(W_and_vS_cumsum, type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")
points(rep(2^M, M+1), explanation$shapley_values[test_obs_idx], col = seq(M))

# Compute the difference between the cumulative sum and the actual Shapley values with all the terms
W_and_vS_cumsum_diff = sweep(x = W_and_vS_cumsum, MARGIN = 2, STATS = as.matrix(explanation$shapley_values[test_obs_idx,]), FUN = "-")

# We see that the difference decreases relative linear. But some volatility
matplot(W_and_vS_cumsum_diff, type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley value difference")

# Can also look at the absolute difference
matplot(abs(W_and_vS_cumsum_diff), type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Absolute Shapley value difference")

# The point is that we above have added the coalitions in increasing order,
# but we want to see if we can find another
# order such that the error decreases much faster.


## Order each feature ----------------------------------------------------------------------------------------------
# IF we order each feature by itself.
W_and_vS_element_wise_product_without_null = W_and_vS_element_wise_product[-1,]

# Get the Shapley values based on all coalitions
rowSums(W_and_vS_element_wise_product_without_null)

# Get the Shapley values based on increasing the number of coalitions.
# Here we include the additional coalitions in the order they are made in the S matrix in shapr
apply(W_and_vS_element_wise_product_without_null, 1, cumsum)

# For each feature i, we sort the W_ij*v(S)_j entries for j = 1,2,...,2^M.
# NOTE THAT THIS IS DONE ON AN ABSOLUTE SCALE. (SHOULD I HAVE DONE IT ON REGULAR SCALE AND used mean as orign?)
W_and_vS_element_wise_product_without_null_sorted =
  apply(abs(W_and_vS_element_wise_product_without_null), 1, sort, decreasing = TRUE)

# We see that there are a quite wide spread in the values of the W_ij*v(S)_j terms.
# Ranging from approx 0.5 and down to 0.02.
W_and_vS_element_wise_product_without_null_sorted

# We can also order them, to see which of the coalitions have the largest value for the different coalitions
# NOTE THAT THIS IS DONE ON AN ABSOLUTE SCALE. (SHOULD I HAVE DONE IT ON REGULAR SCALE AND used mean as orign?)
W_and_vS_element_wise_product_without_null_ordered =
  apply(abs(W_and_vS_element_wise_product_without_null), 1, order, decreasing = TRUE)

# Here the ij'th element tells us the i'th most "important" coalition index for the j'th feature.
# Here "important" means having larger absolute value.
W_and_vS_element_wise_product_without_null_ordered

W_and_vS_elements_sorted_based_on_abs_value - W_and_vS_element_wise_product_without_null_sorted

W_and_vS_elements_sorted_based_on_abs_value = sapply(
  seq(M),
  function(col_ind) {
    W_and_vS_element_wise_product_without_null[col_ind, W_and_vS_element_wise_product_without_null_ordered[,col_ind]]})

# We see that we now have the elements sorted in
W_and_vS_elements_sorted_based_on_abs_value

# Get the Shapley values
colSums(W_and_vS_elements_sorted_based_on_abs_value)
W_and_vS_elements_sorted_based_on_abs_value

W_and_vS_elements_sorted_based_on_abs_value_cumsum = apply(W_and_vS_elements_sorted_based_on_abs_value, 2, cumsum)
W_and_vS_elements_sorted_based_on_abs_value_cumsum
matplot(W_and_vS_elements_sorted_based_on_abs_value_cumsum, type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")
points(rep(2^M, M), explanation$shapley_values[test_obs_idx, -"none"], col = seq(M))

W_and_vS_elements_sorted_based_on_abs_value_cumsum_diff =
  sweep(x = W_and_vS_elements_sorted_based_on_abs_value_cumsum,
        MARGIN = 2,
        STATS = as.matrix(explanation$shapley_values[test_obs_idx, -"none"]),
        FUN = "-")

matplot(W_and_vS_elements_sorted_based_on_abs_value_cumsum_diff,
        type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")

matplot(abs(W_and_vS_elements_sorted_based_on_abs_value_cumsum_diff),
        type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Abs. Shapley values")

plot(rowMeans(abs(W_and_vS_elements_sorted_based_on_abs_value_cumsum_diff)),
     type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "MAE (Shapley values)")



## Compare some ordering strategies --------------------------------------------------------------------------------
W_and_vS_element_wise_product_without_null_ordered_ordered =
  apply(abs(W_and_vS_element_wise_product_without_null), 1, function(x) order(order(x), decreasing = FALSE))
best_avarage_order = order(rowSums(W_and_vS_element_wise_product_without_null_ordered_ordered), decreasing = TRUE)

{
  par(mfrow = c(2,2))
  barplot(rowSums(abs(t(W_and_vS_element_wise_product_without_null))))
  barplot(rowSums(W_and_vS_element_wise_product_without_null_ordered_ordered))
  plot(rowSums(abs(t(W_and_vS_element_wise_product_without_null))),
       rowSums(W_and_vS_element_wise_product_without_null_ordered_ordered))
  plot(order(rowSums(abs(t(W_and_vS_element_wise_product_without_null))), decreasing = TRUE),
       order(rowSums(W_and_vS_element_wise_product_without_null_ordered_ordered), decreasing = TRUE))
  abline(a = 0, b = 1)
  par(mfrow = c(1,1))
}

# So can sort based on both methods. I.e., the ranking score and the actual values.



W_and_vS_element_wise_product_without_null_ordered_ordered
best_avarage_order
plot(best_avarage_order)

W_and_vS_elements_sorted_based_on_best_avarage_order = t(W_and_vS_element_wise_product_without_null[,best_avarage_order])
# Get the Shapley values
colSums(W_and_vS_elements_sorted_based_on_abs_value)
colSums(W_and_vS_elements_sorted_based_on_best_avarage_order)
explanation$shapley_values[test_obs_idx, -"none"]

W_and_vS_elements_sorted_based_on_best_avarage_order_cumsum =
  apply(W_and_vS_elements_sorted_based_on_best_avarage_order, 2, cumsum)
W_and_vS_elements_sorted_based_on_best_avarage_order_cumsum

matplot(W_and_vS_elements_sorted_based_on_best_avarage_order_cumsum,
        type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")
points(rep(2^M, M), explanation$shapley_values[test_obs_idx, -"none"], col = seq(M))


W_and_vS_elements_sorted_based_on_best_avarage_order_cumsum_diff =
  sweep(x = W_and_vS_elements_sorted_based_on_best_avarage_order_cumsum,
        MARGIN = 2,
        STATS = as.matrix(explanation$shapley_values[test_obs_idx, -"none"]),
        FUN = "-")

matplot(W_and_vS_elements_sorted_based_on_best_avarage_order_cumsum_diff,
        type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")

matplot(abs(W_and_vS_elements_sorted_based_on_best_avarage_order_cumsum_diff),
        type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Abs. Shapley values")

plot(rowMeans(abs(W_and_vS_elements_sorted_based_on_best_avarage_order_cumsum_diff)),
     type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "MAE (Shapley values)")





# Looking at the Kernel Shapley values ----------------------------------------------------------------------------
# Plot to just see the Shapley kernel weights of the
{
  par(mfrow = c(1,1))
  plot(explanation$internal$objects$X$shapley_weight, ylim = c(0, 1), type = "b",
       xlab = "Coalition index", ylab = "Shapley kernel weights", main = "Shapley kernel weights for the different coalitions")
}

# We see that the smallest and largest coalitions are weighted higher than the mid-sized coalitions,
# however, there are more of them. Nonetheless, if we sum the weights for each coalition size we see that
# there are still less likely to sample a mid sized coalition size even if there are more of them.
# This is because the Shapley kernel weight is given by (M-1)/(choose(M,|S|)*|S|*(M-|S|)), while the number
# of coalitions of size |S| is given by choose(M,|S|). That means that the probability of sampling a coalition
# of the different sizes are given by (M-1)/(|S|*(M-|S|)).
table(explanation$internal$objects$X$shapley_weight)
unique(explanation$internal$objects$X$shapley_weight*explanation$internal$objects$X$N)
(M-1)/(0:M*(M-0:M))


# We now progress to the setting where we look at the weights in W matrix which allows us to compute the
# Shapley values as a matrix product, i.e., Phi = W %*% v(s).
# Plot to look at how the weights changes with the different coalitions
{
  par(mfrow = c(3,2))
  for (row_idx in seq(nrow(W))) {
    plot(W[row_idx,], type = "l", xlab = "Coalition index", ylab = "W weight", main = paste0("feature: ", row_idx-1))
  }
}

# Here we remove first column related to phi0, as it is 1 for the empty coalitions and technically 0 for the rest.
# We see that the weights are symmetric.
{
  par(mfrow = c(1,2))
  matplot(t(W[-1,]), type = "b", lwd = 1.5, pch = 1, xlab = "Coalition index", ylab = "W weights", main = "Weights for Phi_j")
  abline(h = 0, col = "gray", lwd = 1.5)
  abline(v = 2^(M-1)+0.5, col = "gray", lwd = 1.5)
  legend("bottom", legend = seq(1,5), lty = 1:5, col = 1:5, lwd = 2, pch = 1)

  # Look at them in the absolute value too
  matplot(abs(t(W[-1,])), type = "b", pch = 1, lwd = 1.5, xlab = "Coalition index", ylab = "Absolute W weights", main = "Absolute weights for Phi_j", ylim = c(0, max(abs(t(W[-1,])))))
  abline(h = 0, col = "gray", lwd = 1.5)
  abline(v = 2^(M-1)+0.5, col = "gray", lwd = 1.5)
  legend("top", legend = seq(1,5), lty = 1:5, col = 1:5, lwd = 2,  pch = 1)
}

# These weights are then matrix multiplied with the v(s) to form R1 + R2 +... R(n_combinations) and these
# summed together form the Shapley values phi.
# So the point of article three is to not only use the Shapley kernel weights in the sampling procedure,
# but also to include pilot estimates of v(s), such that we can see if some of the R's are very important
# and that the corresponding coalition should be include when we properly estimate the Shapley values.
# Recall that we compute the R's at an individual basis, thus, we can get that different coalitions
# are important for different test observations.
# There are two ways to handle this problem:
#   1) Use a method which does not need to train a new method for each coalition, or one that does so fast.
#      Here we can use for example independence, empirical, parametric, vaeac or a fast regression method.
#   2) We can average over the test observations and use the coalitions which are the most important on average,
#      and then just focus on training good methods for these coalitions.





# Looking at R weights --------------------------------------------------------------------------------------------
# We will now look at a the size of the R's for some individuals
# Extract the contribution functions and the W matrix (phi = W %*% v)
dt_vS = explanation$internal$output$dt_vS
W = explanation$internal$objects$W

# Just to check that these multiplied together form the Shapley values
kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
dt_kshap <- data.table::as.data.table(kshap)
colnames(dt_kshap) = colnames(explanation$shapley_values)

# Check that they are the same as the shapr::explanaition function output
all.equal(dt_kshap, explanation$shapley_values)



# Looking at the v(S) ---------------------------------------------------------------------------------------------
{
  # Define some test observations
  test_observations = seq(200)
  test_observations = c(1,4,5,10,50,100,200)

  # We look at the v(s) for different test observations
  # Can also look at several at the same time.
  # We see that there is some variability
  # The line represents the E[f(x)] and the last coalition index shows f(x)
  par(mfrow = c(1,1))
  matplot(as.matrix(dt_vS[, -"id_combination"])[,test_observations],
          type = "b", lwd = 1.5, pch = 1, lty = 1,
          xlab = "Coalition index",
          ylab = "Contribution function v(S)",
          main = "V(s) for different test observations")
  abline(h = explanation$internal$parameters$prediction_zero)

  # The variability is further seen when we include all test observations
  matplot(as.matrix(dt_vS[, -"id_combination"]),
          type = "l", lwd = 1.5, lty = 1,
          xlab = "Coalition index",
          ylab = "Contribution function v(S)",
          main = "V(s) for different test observations")

  # Here we plot summaries for all test observations, as the previous plot is chaotic.
  {
    par(mfrow = c(1,1), bty = "l")
    dt_vS_summarized = cbind(
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, min),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, quantile, 0.05),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, quantile, 0.25),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, quantile, 0.5),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, quantile, 0.75),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, quantile, 0.95),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, max),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, mean)
    )
    matplot(dt_vS_summarized, col = c(2,3,4,5,4,3,2,1), lty = c(2,3,4,5,4,3,2,1), type = "b", pch = 1, lwd = 1.5,
            xlab = "Coalition index", ylab = "Contribution function v(S)",
            main = "Summarizes of the V(s) for all test observations")
    legend("topleft", legend = c("max and min", "0.05 and 0.95 quantile", "0.25 and 0.75 quantile", "median", "mean"),
           col = c(2,3,4,5,1), lty = c(2,3,4,5,1), pch = 1, lwd = 1.5, bty = "n")
  }
}

{
  ## min and max -----------------------------------------------------------------------------------------------------

  # Look at which coalitions index is which coalition.
  # Right now the beta coefficients are [1, 0.25, -3, -1, 1.5] and rho = 0.9
  # So we should expect to see that X3 and X5 are most important, especially X3.
  # I.e., if v(S) has a high value we see that we most likely condition on knowing X3.
  # Why is that reasonable?
  # While low values are more likely to condition on X4 and X5
  explanation$internal$objects$S[order(dt_vS_summarized[,1], decreasing = FALSE), ] # HERE WE LOOK AT MIN
  explanation$internal$objects$S[order(dt_vS_summarized[,7], decreasing = TRUE), ]  # HERE WE LOOK AT MAX

  # We see that x3 are included most in the minimum (the maximum will be ish identical)
  apply(explanation$internal$objects$S[order(dt_vS_summarized[,1], decreasing = FALSE), ], 2, cumsum)
  {
    par(mfrow = c(2,1))
    matplot(apply(explanation$internal$objects$S[order(dt_vS_summarized[,1]), ], 2, cumsum),
            lty = 1:5, pch = 1, type = "b", lwd = 2,
            xlab = "Number of coalitions", ylab = "Number of times the features has been included",
            main = "Min v(S)")
    legend("topleft", legend = paste("X", 1:M, sep = ""), lty = 1:5, pch = 1, col = 1:M, bty = "n")

    matplot(apply(explanation$internal$objects$S[order(dt_vS_summarized[,7], decreasing = TRUE), ], 2, cumsum),
            lty = 1:5, pch = 1, type = "b", lwd = 2,
            xlab = "Number of coalitions", ylab = "Number of times the features has been included",
           main = "Max v(S)")
    legend("topleft", legend = paste("X", 1:M, sep = ""), lty = 1:5, pch = 1, col = 1:M, bty = "n")
    par(mfrow = c(1,1))
  }

}


{
  ## mean ------------------------------------------------------------------------------------------------------------

  # What if we look at the mean? What do we see then?
  # It is hard to see in the dt_vS_summarized, but know it is very clear that S with
  # X3 included are the most important on average, which is reasonable
  explanation$internal$objects$S[order(dt_vS_summarized[,8], decreasing = FALSE), ]

  # Se which coalitions are furthest from the empty coalition, on absolute scale,
  # when considering the contribution function.
  {
    par(mfrow = c(1,2))
    barplot(dt_vS_summarized[, 8] - dt_vS_summarized[1, 8], xlab = "Coalition index",
            ylab = "Difference v(S) - v(ø)")
    barplot(abs(dt_vS_summarized[, 8] - dt_vS_summarized[1, 8]), xlab = "Coalition index",
            ylab = "Absolute difference: |v(S) - v(ø)|")
    par(mfrow = c(1,1))
  }

  # Lets get the coalitions that are the furthest from v(ø) = E[f(x)]
  explanation$internal$objects$S[order(abs(dt_vS_summarized[,8]-dt_vS_summarized[1,8]), decreasing = TRUE),]

  # Can then get a count of it and then plot it
  apply(explanation$internal$objects$S[order(abs(dt_vS_summarized[,8]-dt_vS_summarized[1,8]), decreasing = TRUE),],
        2, cumsum)
  {
    # Can also plot the development. See that X3 is always included in the beginning, and
    # often together with X1 and X3
    matplot(apply(explanation$internal$objects$S[order(abs(
      dt_vS_summarized[,8]-dt_vS_summarized[1,8]), decreasing = TRUE),], 2, cumsum),
            lty = 1:5, pch = 1, type = "b", lwd = 2,
            xlab = "Number of coalitions", ylab = "Number of times the features has been included")
    legend("topleft", legend = paste("X", 1:M, sep = ""), lty = 1:5, pch = 1, col = 1:M, bty = "n")
  }
}





# One test obs ----------------------------------------------------------------------------------------------------

# We just look at one test observation.
test_obs_idx = 16

# We want to find the contributions function that contributed the most.
test_obs_vs = as.matrix(dt_vS[, -"id_combination"])[,test_obs_idx]

# Plot the v(s) for the the different coalitions for the test observations
plot(test_obs_vs, type = "b", lwd = 1.5, pch = 1, xlab = "Coalition index", ylab = "Contribution function v(S)",
     main = "Contribution function for the different coalitions for one test observation")
abline(h = prediction_zero, col = "gray", lwd = 2)

# We go back and look at just the test observation.
# Can look at the final Shapley values in many ways.
t(W %*% test_obs_vs)
dt_kshap[test_obs_idx,]

# Compute the R's. I.e., W * v(s), but without adding them together.
# So not a matrix product, but rather an element wise multiplication.
R_matrix_for_one_individual = sweep(x = W, MARGIN = 2, STATS = test_obs_vs, FUN = "*")
# Instead of using sweep, we could have done: t(t(W)*test_obs_vs)
# This is equivalent. We take the column vector v(s) and elementwise multiply it with the
# rows in W.

# Can also do it for all test observations and store the results in a list,
# where each element in the list corresponds to the
R_matrix_list_all_individuals =
  lapply(seq(ncol(dt_vS[, -"id_combination"])),
         function (test_obs_idx) t(t(W)*as.matrix(dt_vS[, -"id_combination"])[,test_obs_idx]))
# Check that we got the same, and yes we did.
all.equal(R_matrix_for_one_individual, R_matrix_list_all_individuals[[test_obs_idx]])

# From Efficiency axiom, we have that Sum_{j=1}^M phi_j = v(M) - v(Ø) = f(x) - y_bar = f(x) - phi_0.
# Check that is correct.
# First version checks that Sum_{j=0}^M phi_j = f(x). Correct. Note that j starts at 0 here.
# Only machine tolerance error
sum(rowSums(R_matrix_for_one_individual))
sum(R_matrix_for_one_individual)
explanation$pred_explain[test_obs_idx]
all.equal(sum(rowSums(R_matrix_for_one_individual)), explanation$pred_explain[test_obs_idx])

# First version checks that Sum_{j=1}^M phi_j = f(x) - y_bar. Correct. Note that j starts at 1 here.
# Only machine tolerance error
sum(rowSums(R_matrix_for_one_individual)[-1])
explanation$pred_explain[test_obs_idx] - prediction_zero
all.equal(sum(rowSums(R_matrix_for_one_individual)[-1]), explanation$pred_explain[test_obs_idx] - prediction_zero)




# If we sum the R's, that is the same as doing the matrix multiplication and we get
rowSums(R_matrix_for_one_individual)
t(W %*% test_obs_vs)
dt_kshap[test_obs_idx,]
rowSums(t(t(W) * test_obs_vs))


# TODO: WHy does these become phi0=1 and then 0 for the feature. (SYMMETRY IN W)
rowSums(t(t(W) * test_obs_vs[1]))
rowSums(t(t(W) * 1))
rowSums(t(t(W)))
rowSums(W)



{
  par(mfrow = c(3,1))

  # Plot the v(s) for the the different coalitions for the test observations
  plot(test_obs_vs, type = "b", lwd = 1.5, pch = 1, xlab = "Coalition index",
       ylab = "Contribution function v(S)",
       main = "Contribution function for the different coalitions for one test observation")
  abline(h = prediction_zero, col = "gray", lwd = 2)


  # Look at the elements in the W matrix, before we multiply them with the v(S) values for
  # the test observation. Note that they are now perfectly symmetric.
  matplot(t(W[-1,]), type = "b", lwd = 1.5, pch = 1,
          xlab = "Coalition index", ylab = "W elements/terms",
          main = "Elements/terms in W matrix (phi = W %*% v(S))")
  abline(h = 0, col = "gray", lty = 1, lwd = 1)
  legend("bottom", legend = paste("phi_", 1:M, sep = ""), pch = 1, lty = 1, col = 1:M, bty = "n")


  # We look at the elements/terms that we add together to constitute the phi_j values
  # For the first test observation that I tried.
  # Note that they are now not perfectly symmetric.
  # This means that some of the coalitions will have had a larger impact.
  matplot(t(R_matrix_for_one_individual[-1,]), type = "b", lwd = 1.5, pch = 1,
          xlab = "Coalition index", ylab = "R elements/terms", main = "Elements/terms for Phi_j for a singel test observation")
  abline(h = 0, col = "gray", lty = 1, lwd = 1)
  legend("bottom", legend = paste("phi_", 1:M, sep = ""), pch = 1, lty = 1, col = 1:M, bty = "n")
}



# Which feature we want to look more into
# Note that we add 1 to investigate_feature_number as
# feature 0 (the phi0) counts as the first feature, so need to offset it by one.
investigate_feature_number = 5

#
which_test_obs = order(explanation$pred_explain, decreasing = TRUE)[1:10] # 10 with highest f(x)
which_test_obs = order(explanation$pred_explain, decreasing = FALSE)[1:10] # 10 with lowest f(x)
which_test_obs = c(order(explanation$pred_explain, decreasing = TRUE)[1:5],
                   order(explanation$pred_explain, decreasing = FALSE)[1:5]) # 10 lowest and highest
which_test_obs = seq(nrow(explanation$shapley_values))

# Plot the R values for all test observation for one particular feature.
{
  par(mfrow = c(2,1))
  matplot(1:2^M, sapply(R_matrix_list_all_individuals[which_test_obs], "[", investigate_feature_number+1,),
          type = "b", lty = rep(c(1,2), each = 5), # 1
          pch = rep(c(16,17), each = 5), #16
          xlab = "Coalition index",
          ylab = "R elements/terms",
          main = paste("Elements/terms for Phi_",investigate_feature_number,
                       " for all test observation", sep = ""))
  abline(h = 0)
  matplot(1:2^M, abs(sapply(R_matrix_list_all_individuals[which_test_obs], "[", investigate_feature_number+1,)),
          type = "b", lty = rep(c(1,2), each = 5),  # 16
          pch = rep(c(16,17), each = 5), #
          xlab = "Coalition index",
          ylab = "Absolute R elements/terms",
          main = paste("Absolute Elements/terms for Phi_",investigate_feature_number,
                       " for all test observation", sep = ""))
  abline(h = 0)

  # Here we have removed the v(empty) and v(M) since they are not estimated by the method.
  # But it is nice to include them to see their effect/magnitude.
  # matplot(2:(2^M-1), sapply(R_matrix_list_all_individuals, "[", investigate_feature_number+1,)[-c(1, 2^M),], type = "b", lty = 1, pch = 1,
  #         xlab = "Coalition index", ylab = "R elements/terms", main = paste("Elements/terms for Phi_",investigate_feature_number," for all test observation", sep = ""))
  # matplot(2:(2^M-1), abs(sapply(R_matrix_list_all_individuals, "[", investigate_feature_number+1,)[-c(1, 2^M),]), type = "b", lty = 1, pch = 1,
  #         xlab = "Coalition index", ylab = "Absolute R elements/terms", main = paste("Absolute Elements/terms for Phi_",investigate_feature_number," for all test observation", sep = ""))
  #
}



# Create a list of the indices
indices = seq(1, 2^M)






# Look at paired differences --------------------------------------------------------------------------------------


# Compute the R's. I.e., W * v(s), but without adding them together.
# So not a matrix product, but rather an element wise multiplication.
# Do it for all test observations and store the results in a list.
R_matrix_list_all_individuals =
  lapply(seq(ncol(dt_vS[, -"id_combination"])),
         function (test_obs_idx) t(t(W)*as.matrix(dt_vS[, -"id_combination"])[,test_obs_idx]))

# Alternate them such that we extract the smallest, then the largest, then the second smallest,
# then the second largest and so on.
alternating_indices = c(rbind(seq(1, 2^(M-1)), seq(2^M, 2^(M-1) + 1)))


# Change the order of the coalitions such that we have S (odd indices) and
# then S_bar (even indices), for all possible coalitions.
R_matrix_paired_order_list =
  lapply(seq(M), function (investigate_feature_number) {
    sapply(R_matrix_list_all_individuals,
           "[",
           investigate_feature_number + 1, )[alternating_indices,]
  })


# We compute the difference between the S and S_bar entries. Odd minus even indices.
R_matrix_paired_order_diff_list =
  lapply(seq_along(R_matrix_paired_order_list),
         function (feature_idx) {
           R_matrix_paired_order_list[[feature_idx]][seq(1, 2^M,2), ] -
             R_matrix_paired_order_list[[feature_idx]][seq(2, 2^M,2), ]
         })


# Convert it to a data.table
R_dt_paired_order_diff_list =
  lapply(seq_along(R_matrix_paired_order_diff_list),
         function (feature_idx) {
           data.table(id = factor(seq(nrow(explanation$internal$data$x_explain))),
                      D = t(R_matrix_paired_order_diff_list[[feature_idx]]))
         })

R_dt_paired_order_diff = data.table::rbindlist(R_dt_paired_order_diff_list,
                                               idcol = "id_feature",
                                               use.names = TRUE)
R_dt_paired_order_diff


# Change the column names
setnames(R_dt_paired_order_diff, c("id_feature", "id", paste0("D", seq(2^(M-1)))))
R_dt_paired_order_diff
str(R_dt_paired_order_diff)

# Go from wide data.table to long data.table format
R_dt_paired_order_diff_long = melt(R_dt_paired_order_diff,
                                   id.vars = c("id_feature", "id"),
                                   value.name = "Rij",
                                   variable.name = "id_combination_diff",
                                   variable.factor = TRUE)

# Plot the results
ggplot(data = R_dt_paired_order_diff_long[id %in% 1:100],
       aes(fill = id, y = Rij, x = id_combination_diff)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(rows = vars(id_feature))
#facet_wrap(vars(id_feature))

# Compute the mean of the Rij's summed over all test observations, and the same when also using the absolute value
# So, $\frac{1}{N_test}\sum_{j = 1}^N_test Rij$ and $\frac{1}{N_test}\sum_{j = 1}^N_test |Rij|$.
#colMeans(R_dt_paired_order_diff[,-"id"])
# R_dt_paired_order_diff_long[, lapply(.SD, mean), by = id_combination_diff, .SDcols = "Rij"]
R_dt = R_dt_paired_order_diff_long[, .(mean_Rij = mean(Rij),
                                      mean_abs_Rij = mean(abs(Rij))),
                                  by = list(id_combination_diff, id_feature)]
R_dt[, `:=` (order_mean_abs_Rij = order(order(mean_abs_Rij, decreasing = TRUE), decreasing = FALSE),
            id_combination_S = seq(1, 2^(M-1)),
            id_combination_Sbar = seq(2^M, 2^(M-1) + 1)),
     by = id_feature]


R_dt
ggplot(data = R_dt, aes(x = id_combination_diff, y = mean_abs_Rij)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(rows = vars(id_feature))
R_dt[id_feature == 1]

#
R_dt_summarized = R_dt[, mean(mean_abs_Rij), by = id_combination_diff][, setnames(.SD, "V1", "mean_abs_R")]
R_dt_summarized[, `:=` (mean_abs_R_ordered = order(order(mean_abs_R, decreasing = TRUE), decreasing = FALSE))]
R_dt_summarized

ggplot(data = R_dt_summarized, aes(x = id_combination_diff, y = mean_abs_R)) +
  geom_bar(position="dodge", stat="identity")

R_dt_summarized_sorted =

ggplot(data = R_dt_summarized, aes(x = forcats::fct_reorder(id_combination_diff, mean_abs_R),
                                   y = mean_abs_R)) +
  geom_hist(position="dodge", stat="identity")


R_dt_summarized %>%


ggplot(data = R_dt, aes(x = id_combination_diff, y = mean_Rij)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(rows = vars(id_feature))




data.table(id_combination_S = seq(1, 2^(M-1)),
           id_combination_Sbar = seq(2^M, 2^(M-1) + 1))


tmp_dt = data.table(tmp[,seq(10)])[, comb_idx := alternating_indices]
tmp_dt$comb_idx = factor(tmp_dt$comb_idx, levels = tmp_dt$comb_idx, ordered = TRUE)
data_dt = melt(tmp_dt,
               id.vars = "comb_idx",
               value.name = "Rij",
               variable.name = "Observation",
               variable.factor = TRUE)
str(data_dt)

ggplot(data = data_dt,
       aes(fill = Observation, y = Rij, x = comb_idx)) +
  geom_bar(position="dodge", stat="identity")


tmp_dt



test_obs_idx = 6
barplot(tmp[,test_obs_idx])
barplot((tmp[seq(1,2^M,2),] - tmp[seq(2,2^M,2)])[,test_obs_idx])
barplot(abs((tmp[seq(1,2^M,2),] - tmp[seq(2,2^M,2)])[,test_obs_idx]))

tmp_order = order(abs((tmp[seq(1,2^M,2),] - tmp[seq(2,2^M,2)])[,test_obs_idx]), decreasing = TRUE)
barplot((tmp[seq(1,2^M,2),] - tmp[seq(2,2^M,2)])[tmp_order,test_obs_idx])
par(mfrow = c(1,1))
tmp_order*2-1

# So a strategy is to use this paired order, going from left to right.
sapply(tmp_order, function(x) alternating_indices[(2*x-1):(2*x)])
alternating_indices
tmp_order*2-1






# Take a look at which coalitions corresponds to the different coalitions indices.
explanation$internal$objects$S
# True model: Y = 1 + 1*X1 + 0*X2 - 2*X3 + 1.5*X4*X5
# Here we
{
  par(mfrow = c(2,3))
  # In the figures below, we see that including the coalition in S gives positive R weights,
  # while they are negative when the feature is not in the coalitions S.
  for (investigate_feature_idx in seq(0,M)) {
    R_matrix_list_all_individuals_one_feature = sapply(R_matrix_list_all_individuals, "[", investigate_feature_idx+1,)
    matplot(1:2^M, rowMeans(R_matrix_list_all_individuals_one_feature), type = "b", lty = 1, pch = 1,
            xlab = "Coalition index", ylab = "R elements/terms",
            main = paste("Elements/terms for Phi_",investigate_feature_idx ," for all test observation", sep = ""))
    abline(h = 0)
  }

  rowMeans(R_matrix_list_all_individuals_one_feature) + rev(rowMeans(R_matrix_list_all_individuals_one_feature))

  for (investigate_feature_idx in seq(0,M)) {
    matplot(1:2^M, sapply(R_matrix_list_all_individuals, "[", investigate_feature_idx+1,), type = "b", lty = 1, pch = 1,
            xlab = "Coalition index", ylab = "R elements/terms",
            main = paste("Elements/terms for Phi_", investigate_feature_idx,
                         " for all test observation", sep = ""))
    abline(h = 0)
  }

  for (investigate_feature_idx in seq(0,M)) {
    matplot(1:2^M, abs(sapply(R_matrix_list_all_individuals, "[", investigate_feature_idx+1,)), type = "b", lty = 1, pch = 1,
            xlab = "Coalition index", ylab = "Absolute R elements/terms",
            main = paste("Absolute Elements/terms for Phi_", investigate_feature_idx ," for all test observation", sep = ""))
    abline(h = 0)
  }

  par(mfrow = c(1,1))
}








# We see that including the coalition S that only includes the feature of interest,
# and the coalition where that is the only one that is missing is important to include here.
# As they have high importance
# Note that they often seem to be relative negative of each other.
# This sort of supports the finding of Covert which recommends paired sampling.
# Because they often cancel each other out ish.
# TODO: do we see other results for other setups. What about less dependence. Will then middle coalitions
# which include the feature be more important?
investigate_feature_number = 2
plot(
  apply(
    abs(sapply(R_matrix_list_all_individuals, "[", investigate_feature_number+1,)[-c(1, 2^M),]), 1, mean),
  type = "b")

# Note that mean of all R elements tends to be close to zero.
# TODO: is this a coincidence or some deeper mathematical meaning?
mean(sapply(R_matrix_list_all_individuals, "[", investigate_feature_number+1,)[-c(1, 2^M),])

# sum(apply((sapply(R_matrix_list_all_individuals, "[", investigate_feature_number+1,)[-c(1, 2^M),]), 1, mean))


# We look at the absolute elements/terms that we add together to constitute the phi_j values
{
  par(mfrow = c(1,2))


  matplot(t(R_matrix_for_one_individual[-1,]),
          type = "b", lwd = 1.5, pch = 1,
          xlab = "Coalition index", ylab = "R elements/terms",
          main = "Elements/terms for Phi_j for a singel test observation")
  legend("top", legend = paste("phi_", 1:M, sep = ""), pch = 1, lty = 1, col = 1:M, bty = "n")
  abline(h = 0, col = "gray", lty = 1, lwd = 1)


  matplot(abs(t(R_matrix_for_one_individual[-1,])),
          type = "b", lwd = 1.5, pch = 1,
          xlab = "Coalition index", ylab = "Absolute R elements/terms",
          main = "Absolute elements/terms for Phi_j for a singel test observation",
          ylim = c(0, max(abs(t(R_matrix_for_one_individual[-1,])))))
  abline(h = 0, col = "gray", lty = 1, lwd = 1)
  legend("top", legend = paste("phi_", 1:M, sep = ""), pch = 1, lty = 1, col = 1:M, bty = "n")
}

explanation$internal$data$x_explain[test_obs_idx,]
explanation$internal$objects$S[14,]













# New stuff -------------------------------------------------------------------------------------------------------
# What I need to do.
# Compute the precomputed true contribution function, which we will use as pilot-estimates.
# Send these to the explain function, where we also specify that we use a pilot-estimate strategy.
# Then when sampling the coalitions to use, we use the pilot estimates.
# We can think of several strategies.
# 1. We can simply sample the coalitions which has the highest absolute Rij*v(S) values.
# Here we can do it on an individual basis and on average basis.
# 2. We can do paired sampling the coalitions which has the largest difference between the coalitions.
#






