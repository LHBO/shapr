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

compute_MAE_MSE_fast = function(mat_1, mat_2, evaluation_criterion = c("MSE", "MAE")) {
  evaluation_criterion = match.arg(evaluation_criterion)
  if (evaluation_criterion == "MSE") mean((mat_1[,-1] - mat_2[,-1])^2) else mean(abs(mat_1[,-1] - mat_2[,-1]))
}


# mat_1 = matrix(rnorm(25), 5, 5)
# mat_2 = mat_1 + 2
# compute_MAE_MSE_fast(mat_1, mat_2, evaluation_criterion = c("MAE", "MSE"))

# Code starts -----------------------------------------------------------------------------------------------------
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



repetitions = 100
new_strategies = c("on_all_cond", "on_all_cond_paired")
new_strategies = c("on_all_cond_paired")
versions = c("largest_weights_random", "unique_paired")
weight_versions = c("non_analytical", "mean_L", "mean_ps")
weight_versions = "non_analytical"
weight_versions = c("analytical", "non_analytical", "mean_L", "mean_ps")

resave = FALSE

args = commandArgs(trailingOnly = TRUE)
version_cmd = as.character(args[1])
if (!is.null(version_cmd) && version_cmd != "NULL") versions = version_cmd

print(versions)


message("Reading TRUE")
true = readRDS(file.path(folder_save, "Wine_data_sep_rf.rds"))
precomputed_vS = true$internal$output # Extract only the precomputed_vS list
true_SV = as.matrix(true$shapley_values)
message("DONE Reading TRUE")

# Get the weights
message("Reading dt weights")
dt_new_weights_analytical = readRDS(file.path(folder_save, paste0("Analytical_prop_M_11_res.rds")))
dt_new_weights_sequence = readRDS(file.path(folder_save, paste0("Sequence_length_M_11_combined.rds")))
dt_new_weights_sequence = dcast(dt_new_weights_sequence[type == "mean"], M + N_S + Size ~ version, value.var = "Ps_tilde")
message("DONE Reading dt weights")



for (version in versions) {
  if (version == "unique_paired") {
    file_name = file.path(folder_save, "NEW_Wine_data_res_unique_paired.rds")
  } else if (version == "largest_weights_random") {
    file_name = file.path(folder_save, "NEW_Wine_data_res_largest_weights_random.rds")
  } else {
    stop("Unknown version")
  }

  message("Reading FILE")
  file = readRDS(file_name)
  message("DONE Reading FILE")

  n_combinations_names = names(file$res)
  n_combinations = as.integer(sub(".*_", "", n_combinations_names))
  rep_names = names(file$res$n_combinations_2)
  repetitions_seq = as.integer(sub(".*_", "", rep_names))



  # Iterate over the strategies
  strategy = new_strategies[1]
  for (strategy in new_strategies) {
    weight_version = weight_versions[1]
    for (weight_version in weight_versions) {
      strategy_name = paste(strategy, version, weight_version, sep = "_")

      # Get the strategy specific save name
      save_name = file.path(folder_save, paste0("NEW_Wine_data_res_", strategy_name, ".rds"))
      save_name_dt = file.path(folder_save, paste0("NEW_Wine_data_res_dt_only_", strategy_name, ".rds"))

      # List to store the results for each new strategy
      save_list = list(res = list())

      # data table to store the MAE
      res_dt = data.table(Strategy = character(), n_combinations = numeric(), repetition = integer(), MAE = numeric())

      # Iterate over the repetitions
      repetition = repetitions_seq[1]
      for (repetition in repetitions_seq) {
        rep_now = rep_names[repetition]

        # Iterate over the different number of coalitions
        n_comb_now_idx = 20
        for (n_comb_now_idx in seq_along(n_combinations)) {
          n_comb_now = n_combinations_names[n_comb_now_idx]
          if (n_comb_now_idx %% round(length(n_combinations) / 10) == 0) {
            message(sprintf("version = %s \t weight_version = %s \t repetition = %d \t strategy = %s \t n_comb = %d",
                            version, weight_version, repetition, strategy, n_combinations[n_comb_now_idx]))
          }

          if (n_combinations[n_comb_now_idx] == 2) {
            # Only empty and grand coalition so just copy
            save_list$res[[n_comb_now]][[rep_now]] = file$res$n_combinations_2[[rep_now]]

          } else {
            #

            file_now = file$res[[n_comb_now]][[rep_now]]


            # Get the X data table
            this_X = data.table::copy(file_now$internal$objects$X)
            this_X[, shapley_weight := as.numeric(shapley_weight)]


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
            } else if (weight_version %in% c("mean_L", "mean_ps")) {

              # Get the weights
              dt_new_weights = copy(dt_new_weights_sequence)

              # Find the weights of the combination closest to n_combinations
              n_comb_use = dt_new_weights$N_S[which.min(abs(dt_new_weights$N_S - n_combinations[n_comb_now_idx]))]
              dt_new_weights_now = dt_new_weights[N_S == n_comb_use]

              dt_new_weights_now <- rbind(dt_new_weights_now, dt_new_weights_now[(.N - ifelse(.N %% 2 == 1, 0, 1)):1])
              dt_new_weights_now[, Size := seq(.N)]
              setnames(dt_new_weights_now, "Size", "n_features")

              # Update the weights with the provided weights for each coalition size
              this_X[dt_new_weights_now, on = "n_features", shapley_weight := get(gsub("_", " ", weight_version))]

            } else {
              # Non-analytical
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
            save_list$res[[n_comb_now]][[rep_now]] = list(shapley_values = dt_shapley,
                                                          internal = list(objects = list(X = this_X,
                                                                                         W = this_W,
                                                                                         S = S_sampled)),
                                                          pred_explain = NULL,
                                                          MSEv = NULL,
                                                          timing = NULL)

            # Compute the MAE and add it to the data table
            res_dt = rbind(
              res_dt,
              data.table(
                Strategy = strategy_name,
                n_combinations = n_combinations[n_comb_now_idx],
                repetition = repetition,
                MAE = compute_MAE_MSE_fast(as.matrix(dt_kshap), true_SV, "MAE")
              )
            )
            print(nrow(res_dt))
          }
        } # End combinations
      } # End repetitions

      # Order the rows
      setorderv(res_dt, c("n_combinations", "repetition"))
      saveRDS(object = res_dt, file = save_name_dt)
      save_list$res_dt = res_dt

      # Save to disk
      saveRDS(object = save_list, file = save_name)
    } # Weight_version
  } # Strategy
} # Version




# Fix to create the dt only files
if (FALSE) {
  new_strategies = c("on_all_cond_paired")
  versions = c("largest_weights_random", "unique_paired")
  weight_versions = c("analytical", "non_analytical", "mean_L", "mean_ps")
  folder_save = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location"

  for (version in versions) {
    for (strategy in new_strategies) {
      for (weight_version in weight_versions) {
        strategy_name = paste(strategy, version, weight_version, sep = "_")
        save_name = file.path(folder_save, paste0("NEW_Wine_data_res_", strategy_name, ".rds"))
        save_name_dt = file.path(folder_save, paste0("NEW_Wine_data_res_dt_only_", strategy_name, ".rds"))
        if (!file.exists(save_name)) next
        message(sprintf("version = %s \t weight_version = %s \t strategy = %s", version, weight_version, strategy))
        file = readRDS(save_name)
        saveRDS(object = file$res_dt, file = save_name_dt)
      }
    }
  }
}


# Fix from MSE to MAE
if (FALSE) {
  path_source = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location"
  true = readRDS(file.path(path_source, "Wine_data_sep_rf.rds"))
  precomputed_vS = true$internal$output # Extract only the precomputed_vS list
  true_SV = as.matrix(true$shapley_values)

  sampling_methods = c("largest_weights_random",
                       "largest_weights_random_new_weights_empirical",
                       "MAD",
                       "MAD_new_weights_empirical",
                       "paired_coalitions_weights_direct_equal_weights_new_weights_gompertz",
                       "unique_paired_new_weights_gompertz",
                       "paired_coalitions_new_weights_gompertz",
                       "unique_paired_new_weights_empirical",
                       "paired_coalitions_new_weights_empirical",
                       "paired_coalitions_weights_direct_equal_weights_new_weights_empirical",
                       "unique",
                       "unique_paired",
                       # "unique_paired_equal_weights",
                       "unique_paired_SW",
                       "paired_coalitions",
                       "paired_coalitions_weights_direct_equal_weights",
                       "largest_weights",
                       "largest_weights_combination_size",
                       "largest_weights_new_weights_empirical",
                       "largest_weights_combination_size_new_weights_empirical")

  sampling_method = sampling_methods[1]
  for (sampling_method in sampling_methods) {
    message("reading")
    if (!file.exists(file.path(path_source, paste0("NEW_Wine_data_res_", sampling_method, ".rds")))) next
    file = readRDS(file.path(path_source, paste0("NEW_Wine_data_res_", sampling_method, ".rds")))
    n_combinations_vec = as.integer(sub(".*_", "", names(file$res)))
    B = length(file$res$n_combinations_2)

    res_dt = NULL


    n_combinations = n_combinations_vec[1]
    for (n_combinations in n_combinations_vec) {

      seed = 1
      for (seed in seq(B)) {
        cat(paste0("Strategy = ", sampling_method, ", n_combinations = ", n_combinations, ", seed = ", seed, ".\n"))

        # Compute the MAE
        SV_now = file$res[[paste0("n_combinations_", n_combinations)]][[paste0("repetition_", seed)]]$shapley_values


        # update the results dt
        res_dt = rbind(res_dt, data.table(Strategy = sampling_method,
                                          n_combinations = n_combinations,
                                          repetition = seed,
                                          MAE = compute_MAE_MSE_fast(true_SV,
                                                                     as.matrix(SV_now),
                                                                     evaluation_criterion = "MAE")))
      }
    }
    # Order the rows
    setorderv(res_dt, c("n_combinations", "repetition"))
    file$res_dt = res_dt

    # Save to disk
    # Do not want to save the full objects
    # saveRDS(object = file, file = file.path(path_source, paste0("NEW_Wine_data_res_", sampling_method, ".rds")))
    saveRDS(object = res_dt, file = file.path(path_source, paste0("NEW_Wine_data_res_dt_only_", sampling_method, ".rds")))
  }






}

