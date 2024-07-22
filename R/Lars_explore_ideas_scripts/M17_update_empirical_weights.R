library(data.table)

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

compute_MAE_MSE_fast = function(mat_1, mat_2, evaluation_criterion = c("MSE", "MAE")) {
  evaluation_criterion = match.arg(evaluation_criterion)
  if (evaluation_criterion == "MSE") mean((mat_1[,-1] - mat_2[,-1])^2) else mean(abs(mat_1[,-1] - mat_2[,-1]))
}


# The parameters of the expirment
M = 17
n_train = 1000
n_test = 500
rho_equi = FALSE
name_prefix = ""

# M = 10
# n_train = 1000
# n_test = 1000
# rho_equi = TRUE
# name_prefix = "Gompertz_Xgboost"


betas = c(2, 10, 0.25, -3, -1, 1.5, -0.5, 10, 1.25, 1.5, -2, 3, -1, -5, 4, -10, 2, 5, -0.5, -1, -2)
betas = betas[seq(M+1)]
use_pilot_estimates_regression = FALSE
max_repetitions = 50


# Load the new weights
new_empirical_weights = readRDS(file.path(folder_save, paste0("Samp_prop_and_gompertz_M_", M, ".rds")))

# The values of rho that we have to fix
rhos = c(0, 0.2, 0.5, 0.9)

# Iterate over the rhos
rho_idx = 2
for (rho_idx in seq(length(rhos))) {
    rho = rhos[rho_idx]

    # Make file names
    if (name_prefix == "") {
      file_name = paste("M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "equi", rho_equi,
                        "betas", paste(as.character(betas), collapse = "_"), sep = "_")
    } else {
      file_name = paste(name_prefix, "M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "equi", rho_equi,
                        "betas", paste(as.character(betas), collapse = "_"), sep = "_")
    }

    if (use_pilot_estimates_regression) {
      file_name_updated = paste(file_name, "pilot", strsplit(pilot_approach_regression, "_")[[1]][2],
                               sub(".*::([^\\(]+)\\(.*", "\\1",  pilot_regression_model), sep = "_")
    } else {
      file_name_updated = file_name
    }
    save_file_name_setup = file.path(folder_save, paste0(file_name, "_model.rds"))
    save_file_name_true = file.path(folder_save, paste0(file_name, "_true.rds"))

    #  Find the relevant files in the folder and their repetition numbers/indices
    files_in_dir = list.files(folder_save)
    relevant_files_in_dir = files_in_dir[grepl(paste0(file_name_updated, "_estimated_repetition_"), files_in_dir)]
    relevant_files_in_dir = relevant_files_in_dir[!grepl("tmp", relevant_files_in_dir)] # remove any tmp files
    if (length(relevant_files_in_dir) == 0) {
      stop(paste0("Cannot find any files for the provided paremeters. ",
                  "Looking for file name structures '", file_name, "'."))
    }
    relevant_repetitions =
      sort(as.integer(sapply(strsplit(unlist(strsplit(relevant_files_in_dir, '.rds')), '\\_'), tail, 1)))
    relevant_repetitions = relevant_repetitions[seq(min(max_repetitions, length(relevant_repetitions)))]
    if (!is.null(max_repetitions) && max_repetitions > length(relevant_repetitions)) {
      message(paste0("The parameter `max_repetitions` (", max_repetitions, ") is larger than the number of available ",
                     "repetitions (", length(relevant_repetitions), "). Use all available repetitions.\n"))
    }


    # Load the setup file, i.e., the model and data
    setup = readRDS(save_file_name_setup)

    # Load the true explanations
    true_explanation = readRDS(save_file_name_true)

    # Iterate over the repetitions
    repetition_idx = 1
    relevant_repetitions = 7
    for (repetition_idx in seq_along(relevant_repetitions)) {

      # Get the current repetition
      repetition = relevant_repetitions[repetition_idx]

      # Small printout to the user
      cat(sprintf("Working on rho = %g (%d of %d) and repetition = %d (%d of %d).\n",
                  rho, rho_idx, length(rhos), repetition, repetition_idx, length(relevant_repetitions)))

      # Create the save file name
      save_file_name_rep = file.path(folder_save, paste0(file_name_updated, "_estimated_repetition_", repetition, ".rds"))

      # Load the rds file
      current_repetition_results = readRDS(save_file_name_rep)


      # Time to update the empirical weights for the approaches that used them and recompute the Shapley values and save the new values

      # Get the sampling methods
      current_sampling_methods = names(current_repetition_results)
      current_sampling_methods = current_sampling_methods[current_sampling_methods != "True_vs_Pilot_Order"]

      # Get the sampling methods with the empirical weights
      relevant_sampling_methods = current_sampling_methods[grepl("_new_weights_empirical", current_sampling_methods)]

      # Iterate over them
      sampling_method = relevant_sampling_methods[1]
      for (sampling_method in relevant_sampling_methods) {

        # Get the results for the current sampling method
        current_repetition_results_now = current_repetition_results[[sampling_method]]$repetition_1

        # We skip n_combinations = 2
        i = 2
        for (i in seq(2, length(current_repetition_results_now))) {
          n_comb_now = as.integer(strsplit(names(current_repetition_results_now)[i], "n_combinations_")[[1]][2])
          if (n_comb_now == 2^M) next

          # Current results
          current_n_comb = current_repetition_results_now[[i]]
          X = copy(current_n_comb$only_save$X)

          # Find the weights of the combination closest to n_combinations
          n_comb_use = new_empirical_weights$n_combinations[which.min(abs(new_empirical_weights$n_combinations - n_comb_now))]
          dt_new_weights_now = new_empirical_weights[n_combinations == n_comb_use]

          if (all(unique(X$shapley_weight)[-1] %in% dt_new_weights_now$empirical)) {
            print(n_comb_now)
            next # Already have the correct values
          }

          X[, shapley_weight := as.numeric(shapley_weight)]

          # Update the weights with the provided weights for each coalition size
          X[dt_new_weights_now, on = "n_features", shapley_weight := get("empirical")]



          # Get a mapping from the indices of the current set of combinations/coalitions to the indices
          # in the version where we use all 2^M combinations/coalitions.
          S_sampled = current_n_comb$only_save$S
          S_all = true_explanation$internal$objects$S
          S_all_list = as.list(seq(nrow(S_all)))
          names(S_all_list) = apply(S_all, 1, paste, collapse = "")
          current_combination_idx_in_all_combinations = as.numeric(S_all_list[apply(S_sampled, 1, paste, collapse = "")])




          # Use the pre-computed v(S) data provided by the user
          processed_vS_list = true_explanation$internal$output

          # Extract only the relevant rows (combinations) from the
          processed_vS_list$dt_vS = processed_vS_list$dt_vS[current_combination_idx_in_all_combinations,]

          #processed_vS_list$dt_vS[, id_combination_orginal := id_combination]
          processed_vS_list$dt_vS[, id_combination := .I]
          # print(processed_vS_list)


          # Compute the new W matrix
          W_updated <- shapr:::weight_matrix(X = X, normalize_W_weights = TRUE, is_groupwise = FALSE)

          # Compute the new Shapley values
          kshap <- t(W_updated %*% as.matrix(processed_vS_list$dt_vS[, -"id_combination"]))
          dt_kshap <- data.table::as.data.table(kshap)
          colnames(dt_kshap) <- c("none", true_explanation$internal$parameters$feature_names)

          # Update the objects
          current_n_comb$only_save$X = X
          current_n_comb$only_save$W = W_updated
          current_n_comb$shapley_values = dt_kshap


          # Look at the errors
          old_error = compute_MAE_MSE_fast(as.matrix(current_repetition_results[[sampling_method]]$repetition_1[[i]]$shapley_values),
                                           as.matrix(true_explanation$shapley_values),
                                           evaluation_criterion = "MAE")

          new_error = compute_MAE_MSE_fast(as.matrix(dt_kshap),
                                           as.matrix(true_explanation$shapley_values),
                                           evaluation_criterion = "MAE")

          print(c(old_error, new_error))



          # Update the list
          current_repetition_results[[sampling_method]]$repetition_1[[i]] = current_n_comb




        }
      }

      # Save the updated version of the results
      saveRDS(current_repetition_results, save_file_name_rep)


    }
}




# GARBADGE
#
#
#
# tmp = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_0_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_estimated_repetition_7.rds")
# true = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_0_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_true.rds")
# new_empirical_weights = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Samp_prop_and_gompertz_M_10.rds")
#
# sampling_methods = names(tmp)
# sampling_methods_updated = sampling_methods[sampling_methods != "True_vs_Pilot_Order"]
#
#
#
# tmp_updated = tmp[sampling_methods_updated]
#
# tmp_updat
#
# return_list = list()
#
# for (sampling_method in sampling_methods_updated) {
#   print(sampling_method)
#
#
#   if (!grepl("_new_weights_empirical", sampling_method)) next
#
#   res_sampling_method_now = tmp[[sampling_method]]$repetition_1
#
#   return_list[[sampling_method]] = list()
#   return_list[[sampling_method]][[1]] = res_sampling_method_now$n_combinations_2
#
#   i = 2
#   for (i in seq(2, length(res_sampling_method_now))) {
#     n_comb_now = as.integer(strsplit(names(res_sampling_method_now)[i], "n_combinations_")[[1]][2])
#
#     # Current results
#     current_n_comb = res_sampling_method_now[[i]]
#     X = copy(current_n_comb$only_save$X)
#
#
#
#     # Find the weights of the combination closest to n_combinations
#     n_comb_use = new_empirical_weights$n_combinations[which.min(abs(new_empirical_weights$n_combinations - n_comb_now))]
#     dt_new_weights_now = new_empirical_weights[n_combinations == n_comb_use]
#
#     X[, shapley_weight := as.numeric(shapley_weight)]
#
#     # Update the weights with the provided weights for each coalition size
#     X[dt_new_weights_now, on = "n_features", shapley_weight := get("empirical")]
#
#
#
#     # Get a mapping from the indices of the current set of combinations/coalitions to the indices
#     # in the version where we use all 2^M combinations/coalitions.
#     S_sampled = current_n_comb$only_save$S
#     S_all = true$internal$objects$S
#     S_all_list = as.list(seq(nrow(S_all)))
#     names(S_all_list) = apply(S_all, 1, paste, collapse = "")
#     current_combination_idx_in_all_combinations = as.numeric(S_all_list[apply(S_sampled, 1, paste, collapse = "")])
#
#
#
#
#     # Use the pre-computed v(S) data provided by the user
#     processed_vS_list = true$internal$output
#
#     # Extract only the relevant rows (combinations) from the
#     processed_vS_list$dt_vS = processed_vS_list$dt_vS[current_combination_idx_in_all_combinations,]
#
#     #processed_vS_list$dt_vS[, id_combination_orginal := id_combination]
#     processed_vS_list$dt_vS[, id_combination := .I]
#     # print(processed_vS_list)
#
#     p <- true$pred_explain
#
#     W_updated <- shapr:::weight_matrix(X = X, normalize_W_weights = TRUE, is_groupwise = FALSE)
#
#     kshap <- t(W_updated %*% as.matrix(processed_vS_list$dt_vS[, -"id_combination"]))
#     dt_kshap <- data.table::as.data.table(kshap)
#     colnames(dt_kshap) <- c("none", true$internal$parameters$feature_names)
#
#     current_n_comb$only_save$W = W_updated
#     current_n_comb$only_save$X = X
#     current_n_comb$shapley_values = dt_kshap
#     return_list[[sampling_method]][[i]] = current_n_comb
#
#
#
#   }
# }
#
# lapply(tmp_updated, function(x) {
#
#
# })
#
# x = tmp_updated$paired_coalitions_weights_direct_equal_weights_new_weights_gompertz
#
# x$repetition_1$n_combinations_1024$
