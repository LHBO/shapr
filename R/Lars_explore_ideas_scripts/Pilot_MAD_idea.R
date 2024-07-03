# In this file, we look at the Mean Absolute Difference (MAD) between the conditional expectation and the prediction
# and consider using these as weights for importance sampling when sampling the coalitions in aproximated SV.

#
library(data.table)

# Load some true explanation objects to see how the
explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Experiment_M_5_n_train_1000_n_test_250_rho_0.7_betas_2_10_0.25_-3_-1_1.5_true.rds")
explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_rds_saves/Paper3_Experiment_M_7_n_train_1000_n_test_250_rho_0.5_betas_0_1_1_1_1_1_1_1_true.rds")
explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Samp_VS_kernel_Xgboost_M_10_n_train_1000_n_test_1000_rho_0.2_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_true.rds")


# Compute the absolute difference between v(S; x) and f(x) for all x and S
T = abs(explanation$internal$output$dt_vS[,-"id_combination"] -
          explanation$internal$output$dt_vS[rep(.N, .N), -"id_combination"])

# Remove the empty and grand coalitions
T = T[-c(1, .N)]

# Compute the MAD and its inverse. A coalition with low MAD (high MAD^-1) contains important features
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
        cbind(shapley_kernel_weight, MAD_weights, MAD_weights_paired, MAD_scaled_weights, MAD_scaled_weights_paired),
        pch = 16,
        xlab = "Coalition index", ylab = "Normalized weight")
legend("top", legend = c("Kernel", "MAD", "MAD paired", "MAD_scaled", "MAD_scaled paired"),
       col = 1:5, pch = 16, bty = "n")


