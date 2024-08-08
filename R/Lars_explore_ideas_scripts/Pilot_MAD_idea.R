# In this file, we look at the Mean Absolute Difference (MAD) between the conditional expectation and the prediction
# and consider using these as weights for importance sampling when sampling the coalitions in aproximated SV.

#
library(data.table)

# Load some true explanation objects to see how the
explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Experiment_M_5_n_train_1000_n_test_250_rho_0.7_betas_2_10_0.25_-3_-1_1.5_true.rds")
explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_rds_saves/Paper3_Experiment_M_7_n_train_1000_n_test_250_rho_0.5_betas_0_1_1_1_1_1_1_1_true.rds")
explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Samp_VS_kernel_Xgboost_M_10_n_train_1000_n_test_1000_rho_0.2_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_true.rds")
explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Xgboost_M_8_n_train_1000_n_test_1000_rho_0_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_true.rds")
explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Xgboost_M_8_n_train_1000_n_test_1000_rho_0.5_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_true.rds")

explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Experiment_M_8_n_train_1000_n_test_1000_rho_0_betas_1_1_1_1_1_1_1_1_1_true.rds")
explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Experiment_M_8_n_train_1000_n_test_1000_rho_0.5_betas_1_1_1_1_1_1_1_1_1_true.rds")
explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Experiment_M_8_n_train_1000_n_test_1000_rho_0.9_betas_1_1_1_1_1_1_1_1_1_true.rds")
explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Experiment_M_8_n_train_1000_n_test_1000_rho_0.5_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_true.rds")
explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Experiment_M_8_n_train_1000_n_test_1000_rho_0_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_true.rds")

{

  # Compute the absolute difference between v(S; x) and f(x) for all x and S
  T = abs(explanation$internal$output$dt_vS[,-"id_combination"] -
            explanation$internal$output$dt_vS[rep(.N, .N), -"id_combination"])

  # Remove the empty and grand coalitions
  T = T[-c(1, .N)]

  # Compute the MAD and its inverse. A coalition with low MAD (high MAD^-1) contains important features.
  # Compute the MAD and its inverse. A coalition with low MAD contains important features, for the smaller coalition
  # sizes. While the MAD is high for large coalition sizes where you miss an important feature.
  MAD = rowMeans(T) # * explanation$internal$objects$X[-c(1, .N), n_features]
  MAD_inverse = 1/(MAD)

  # Since MAD is always lower for larger coalitions (more features, easier to model f(x)), we also make a version
  # where we scale MAD with the number of features in the coalition, i.e., |S|.
  MAD_scaled = rowMeans(T) * explanation$internal$objects$X[-c(1, .N), n_features]
  MAD_scaled_inverse = 1/(MAD_scaled)

  # Extract the shapley kernel weights k(M, |S|)
  shapley_kernel_weight = explanation$internal$objects$X[-c(1, .N), shapley_weight]
  shapley_kernel_weight = shapley_kernel_weight / sum(shapley_kernel_weight)

  # Compute k(M, |S|) / MAD
  MAD_weights = MAD_inverse * shapley_kernel_weight
  MAD_weights = MAD_weights / sum(MAD_weights)

  # Pair the MAD_weights with the complement since we will sample pairwise
  MAD_weights_paired = (MAD_weights + rev(MAD_weights)) / 2

  # Now the same with the scaled version
  MAD_scaled_weights = MAD_scaled_inverse * shapley_kernel_weight
  MAD_scaled_weights = MAD_scaled_weights / sum(MAD_scaled_weights)
  MAD_scaled_weights_paired = (MAD_scaled_weights + rev(MAD_scaled_weights)) / 2

  # Plot the weights
  matplot(seq(2, length(MAD_weights) + 1),
          cbind(shapley_kernel_weight,
                MAD_weights,
                MAD_weights_paired,
                MAD_scaled_weights,
                MAD_scaled_weights_paired
                ),
          pch = 16,
          xlab = "Coalition index", ylab = "Normalized weight",
          log = "y")
  legend("top", legend = c("Kernel", "MAD", "MAD paired", "MAD_scaled", "MAD_scaled paired"),
         col = 1:5, pch = 16, bty = "n")


  m = 8
  n_features <- seq(ceiling((m - 1)/2))
  n <- sapply(n_features, choose, n = m)
  n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
  n_cumsum = (cumsum(n) + 2)
  n_cumsum = n_cumsum[-length(n_cumsum)]


  n_cumsum = cumsum(n) + 1.5
  abline(v = n_cumsum, lty = 2, col = "gray50")



}

{

  plot(seq(2, length(MAD_weights) + 1), MAD)
  abline(v = n_cumsum, lty = 2, col = "gray50")


  plot(seq(2, length(MAD_weights) + 1), MAD)
  abline(v = c(1.5, 1.5 + cumsum(sapply(1:(m-1), choose, n = m))), lty = 2, col = "red")



  m = 8
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  n_cumsum_ext = c(0, cumsum(n))

  value_in_each_coalition_size = lapply(seq(length(n_cumsum_ext) - 1), function(idx) {
    MAD[seq(n_cumsum_ext[idx] + 1, n_cumsum_ext[idx + 1])]
  })
  value_in_each_coalition_size




  all_coal_sizes = seq(ceiling((m - 1)/2))
  all_paired_coal_sizes = seq(floor((m - 1)/2))
  res_order = sapply(all_coal_sizes, function(size) {
    if (size %in% all_paired_coal_sizes) {
      order(value_in_each_coalition_size[[size]] - rev(value_in_each_coalition_size[[m - size]]), decreasing = FALSE)
    } else {
      order(value_in_each_coalition_size[[size]], decreasing = FALSE)
    }
  })

  add_index = cumsum(sapply(res_order, length))
  for (idx in seq(2, length(res_order))) {
    res_order[[idx]] = res_order[[idx]] + add_index[idx - 1]
  }

  res_order_two = c()
  for (idx in seq(floor((m - 1)/2))) {
    res_order_two = c(res_order_two, c(rbind(res_order[[idx]], 2^m - 1 - res_order[[idx]])))
  }
  if (m %% 2 == 0) {
    res_order_two = c(res_order_two, rbind(res_order[[ceiling((m - 1)/2)]]))
  }
  res_order_two = c(1, 2^m, res_order_two + 1) # add one due to empty set

  # explanation$internal$objects$S[res_order[[4]] + 1,]



  plot(res_order_two)
  plot(sort(res_order_two))
  sort(res_order_two)
  paired_order = unlist(res_order)

  paired_order


  pilot_estimates_coal_order(explanation = explanation, strategies = "MAD")

}
