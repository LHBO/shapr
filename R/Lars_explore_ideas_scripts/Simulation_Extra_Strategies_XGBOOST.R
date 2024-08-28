# Libraries -------------------------------------------------------------------------------------------------------
library(shapr)
library(data.table)

# New code --------------------------------------------------------------------------------------------------------
shapley_reweighting <- function(X, reweight = "on_N") {
  # Updates the shapley weights in X based on the reweighting strategy BY REFERENCE

  if (reweight == "on_N") {
    X[-c(1,.N), shapley_weight := mean(shapley_weight), by = N]
  } else if (reweight == "on_coal_size") {
    X[-c(1,.N), shapley_weight := mean(shapley_weight), by = n_features]
  } else if (reweight == "on_all") {
    m <- X[.N, n_features]
    X[-c(1,.N), shapley_weight := shapr:::shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m = 10^6)/sum_shapley_weights(m)]
  } else if (reweight == "on_N_sum") {
    X[, shapley_weight := sum(shapley_weight), by = N]
  } else if (reweight == "on_all_cond") {
    X[, shapley_weight := as.numeric(shapley_weight)]
    m <- X[.N, n_features]
    K <- X[-c(1,.N), sum(shapley_weight)]
    X[-c(1,.N), shapley_weight := shapr:::shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m = 10^6)/sum_shapley_weights(m)]
    X[-c(1,.N), cond := 1-(1-shapley_weight)^K]
    X[-c(1,.N), shapley_weight := shapley_weight/cond]
  } else if (reweight == "on_all_cond_paired") {
    X[, shapley_weight := as.numeric(shapley_weight)]
    m <- X[.N, n_features]
    K <- X[-c(1,.N), sum(shapley_weight)]
    X[-c(1,.N), shapley_weight := shapr:::shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m = 10^6)/sum_shapley_weights(m)]
    X[-c(1,.N), cond := 1-(1-2*shapley_weight)^(K/2)]
    X[-c(1,.N), shapley_weight := 2*shapley_weight/cond]
  } else if (reweight == "comb") {
    # Very ad-hoc: Half on_coal_size and half of on_all
    X[, shapley_weight1 := mean(shapley_weight), by = n_features]
    X[-c(1,.N), shapley_weight1 := shapley_weight1/sum(shapley_weight1)]
    m <- X[.N, n_features]
    X[-c(1,.N), shapley_weight2 := shapr:::shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m = 10^6)/sum_shapley_weights(m)]
    X[-c(1,.N), shapley_weight2 := shapley_weight2/sum(shapley_weight2)]
    X[-c(1,.N), shapley_weight := (shapley_weight1+shapley_weight2)/2]
  }
  # strategy= "none" or something else do nothing
  return(NULL)
}

#' @keywords internal
sum_shapley_weights <- function(m){
  coal_samp_vec <- seq(m - 1)
  n <- sapply(coal_samp_vec, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, coal_samp_vec) * n
  return(sum(w))
}



# Code starts -----------------------------------------------------------------------------------------------------
# Code starts here ------------------------------------------------------------------------------------------------
# Get the name of the computer we are working on
hostname = R.utils::System$getHostname()
cat(sprintf("We are working on '%s'.\n", R.utils::System$getHostname()))

# Check if we are working on an UiO computer or not and define the correct folder based on system
if (hostname == "Larss-MacBook-Pro.local" || Sys.info()[[7]] == "larsolsen") {
  # Where the files are stored
  folder = "/Users/larsolsen/PhD/Paper3/shapr"
  folder_save = "/Users/larsolsen/PhD/Paper3/Paper3_save_location"
  UiO = FALSE
} else if (grepl("hpc.uio.no", hostname)) {
  # TBA
  folder = ""
  UiO = TRUE
} else if (grepl("uio.no", hostname)) {
  folder = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr"
  folder_save = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location"
  UiO = TRUE
} else {
  stop("We do not recongize the system at which the code is run (not Lars's MAC, HPC, nor UiO).")
}

# module load R/4.2.1-foss-2022a
# cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts
# Rscript samp_freq_plots.R 15


args = commandArgs(trailingOnly = TRUE)

rhos = as.character(args[1])
if (grepl(",", rhos)) {
  rhos = as.numeric(unlist(strsplit(rhos, ",")))
} else {
  rhos = as.numeric(rhos)
}

repetitions = 50
# rhos = c(0, 0.2, 0.5, 0.9)
new_strategies = c("on_all_cond", "on_all_cond_paired")
new_strategies = c("on_all_cond_paired")

versions = c("largest_weights_random", "unique_paired")

weight_versions = c("analytical", "non_analytical", "mean_L", "mean_ps")
weight_versions = c("mean_L", "mean_ps")

resave = FALSE

folder = folder_save # = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location"

# Get the weights
dt_new_weights_analytical = readRDS(file.path(folder, paste0("Analytical_prop_M_", 10, "_res.rds")))
dt_new_weights_sequence = readRDS(file.path(folder, paste0("Sequence_length_M_10_combined.rds")))
dt_new_weights_sequence = dcast(dt_new_weights_sequence[type == "mean"], M + N_S + Size ~ version, value.var = "Ps_tilde")


# Iterate over the dependencies
rho = 0
for (rho in rhos) {
  true = readRDS(paste0("/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_", rho, "_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_true.rds"))
  precomputed_vS = true$internal$output # Extract only the precomputed_vS list

  # Iterate over the repetitions
  repetition = 1
  for (repetition in seq(repetitions)) {
    file_name = paste0("/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_", rho, "_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_estimated_repetition_", repetition ,".rds")
    file = readRDS(file_name)

    version = "unique_paired"
    for (version in versions) {
      if (resave && !is.null(file[[1]][[1]][[1]]$internal$parameters$precomputed_vS)) {
        # Resave the file but without the precomputed v(S) values
        cat(sprintf("Resaving: Using memory efficient version: %s \U2192 ",
                    format(object.size(file), units = "auto")))
        for (met in names(file)) {
          if (met == "True_vs_Pilot_Order") next
          for (rep in names(file[[met]])) {
            for (comb in names(file[[met]][[rep]])) {
              file[[met]][[rep]][[comb]]$internal$parameters$precomputed_vS = NULL
            }
          }
        }
        cat(sprintf("%s.\n", format(object.size(file), units = "auto")))
        saveRDS(file, file_name )
      }


      # Get the relevant files
      #file_relevant = file$unique_paired$repetition_1
      file_relevant = file[[version]]$repetition_1

      file_relevant_names = names(file_relevant)
      n_combinations = as.integer(sub(".*_", "", names(file_relevant)))

      # Iterate over the strategies
      strategy = new_strategies[1]
      for (strategy in new_strategies) {
        for (weight_version in weight_versions) {
          strategy_name = paste(strategy, version, weight_version, sep = "_")

          # Get the strategy specific save name
          save_name = paste0("/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_", rho, "_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_estimated_repetition_", repetition ,"_",  strategy_name, ".rds")

          # List to store the results for each new strategy
          save_list = list()
          save_list[[strategy_name]] = list(repetition_1 = list())

          # Iterate over the different number of coalitions
          n_comb_now_idx = 20
          for (n_comb_now_idx in seq_along(n_combinations)) {
            message(sprintf("version = %s \t weight_version = %s \t rho = %.1f \t repetition = %d \t strategy = %s \t n_comb = %d",
                            version, weight_version, rho, repetition, strategy, n_combinations[n_comb_now_idx]))

            if (n_combinations[n_comb_now_idx] == 2) {
              # Only empty and grand coalition so just copy
              save_list[[strategy_name]][[1]][[file_relevant_names[n_comb_now_idx]]] = file_relevant$n_combinations_2
            } else {
              #
              file_now = file_relevant[[n_comb_now_idx]]

              # # FJERN
              # file_now = rrr$unique_paired$repetition_1$n_combinations_20

              this_X = data.table::copy(file_now$only_save$X)

              # # FJERN
              # this_X[-c(1,.N), sum(shapley_weight)]
              # shapley_reweighting(this_X, reweight = "on_all_cond")
              # this_X[-c(1,.N), sum(shapley_weight)]
              # this_X[-c(1,.N), shapley_weight := shapley_weight / sum(shapley_weight)]
              # this_X[-c(1,.N), sum(shapley_weight)]
              # this_X
              #
              #
              # shapley_reweighting(this_X, reweight = "on_all_cond_paired")
              # this_X[-c(1,.N), sum(shapley_weight)]
              # this_X[-c(1,.N), shapley_weight := shapley_weight / sum(shapley_weight)]
              # this_X[-c(1,.N), sum(shapley_weight)]
              # this_X


              if (version == "largest_weights_random") { # Set weights to 1 as the coals are not sampled with replacement
                this_X[-c(1,.N), shapley_weight := 1.0]
              }

              # Re weight the shapely weights
              shapley_reweighting(this_X, reweight = strategy)

              if (weight_version == "analytical") {
                dt_new_weights = copy(dt_new_weights_analytical)

                # Find the weights of the combination closest to n_combinations
                n_comb_use = dt_new_weights$n_combinations[which.min(abs(dt_new_weights$n_combinations - n_combinations[n_comb_now_idx]))]
                dt_new_weights_now = dt_new_weights[n_combinations == n_comb_use]

                # Update the weights with the provided weights for each coalition size
                this_X[dt_new_weights_now, on = "n_features", shapley_weight := get("mean")]
              } else if (weight_version == "mean_L") {

                # Get the weights
                dt_new_weights = copy(dt_new_weights_sequence)

                # Find the weights of the combination closest to n_combinations
                n_comb_use = dt_new_weights$N_S[which.min(abs(dt_new_weights$N_S - n_combinations[n_comb_now_idx]))]
                dt_new_weights_now = dt_new_weights[N_S == n_comb_use]

                dt_new_weights_now <- rbind(dt_new_weights_now, dt_new_weights_now[(.N - ifelse(n_row %% 2 == 1, 1, 0)):1])
                dt_new_weights_now[, Size := seq(.N)]
                setnames(dt_new_weights_now, "Size", "n_features")

                # Update the weights with the provided weights for each coalition size
                this_X[dt_new_weights_now, on = "n_features", shapley_weight := get("mean L")]

              } else if (weight_version == "mean_ps") {

                # Get the weights
                dt_new_weights = copy(dt_new_weights_sequence)

                # Find the weights of the combination closest to n_combinations
                n_comb_use = dt_new_weights$N_S[which.min(abs(dt_new_weights$N_S - n_combinations[n_comb_now_idx]))]
                dt_new_weights_now = dt_new_weights[N_S == n_comb_use]
                dt_new_weights_now = dcast(dt_new_weights_now[type == "mean"], M + N_S + Size ~ version, value.var = "Ps_tilde")
                dt_new_weights_now


                dt_new_weights_now <- rbind(dt_new_weights_now, dt_new_weights_now[(.N - ifelse(n_row %% 2 == 1, 1, 0)):1])
                dt_new_weights_now[, Size := seq(.N)]
                setnames(dt_new_weights_now, "Size", "n_features")

                # Update the weights with the provided weights for each coalition size
                this_X[dt_new_weights_now, on = "n_features", shapley_weight := get("mean ps")]

              } else {
                stop("Unknown weight_version")
              }

              # Get the weight matrix
              this_W <- shapr:::weight_matrix(X = this_X, normalize_W_weights = TRUE)

              # Get the number of features
              m = this_X[.N, n_features]

              ## Get feature matrix ---------
              S_sampled <- shapr:::feature_matrix_cpp(features = this_X[["features"]], m = m)

              # Create the S matrix if we had used all combinations
              S_all = shapr:::feature_matrix_cpp(
                features = unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE), m = m
              )
              S_all_list = as.list(seq(nrow(S_all)))
              names(S_all_list) = apply(S_all, 1, paste, collapse = "")

              # Get a mapping from the indices of the current set of combinations/coalitions to the indices
              # in the version where we use all 2^M combinations/coalitions.
              current_combination_idx_in_all_combinations = as.numeric(S_all_list[apply(S_sampled, 1, paste, collapse = "")])

              # Use the pre-computed v(S) data provided by the user
              processed_vS_list = precomputed_vS

              # Extract only the relevant rows (combinations) from the
              processed_vS_list$dt_vS = processed_vS_list$dt_vS[current_combination_idx_in_all_combinations,]
              processed_vS_list$dt_vS[, id_combination := .I]

              # Compute the new Shapley values
              shap_names <- true$internal$parameters$feature_names
              kshap <- t(this_W %*% as.matrix(processed_vS_list$dt_vS[, -"id_combination"]))
              dt_kshap <- data.table::as.data.table(kshap)
              colnames(dt_kshap) <- c("none", shap_names)
              dt_shapley = dt_kshap

              # Store the results
              save_list[[strategy_name]][[1]][[file_relevant_names[n_comb_now_idx]]] = list(shapley_values = dt_shapley,
                                                                                            MSEv = NULL,
                                                                                            only_save = list(X = this_X,
                                                                                                             W = this_W,
                                                                                                             S = S_sampled))
            }
          }

          # Save to disk
          saveRDS(object = save_list, file = save_name)
        }
      }
    }
  }
}


