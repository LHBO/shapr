
# Libraries -------------------------------------------------------------------------------------------------------
library(data.table)
library(stringr)
library(shapr)
library(shapr)



# Functions -------------------------------------------------------------------------------------------------------


shapley_reweighting <- function(X, reweight = "on_N", K = NULL) {
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
    if (is.null(K)) K <- X[-c(1,.N), sum(shapley_weight)]
    X[-c(1,.N), shapley_weight := shapr:::shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m = 10^6)/sum_shapley_weights(m)]
    X[-c(1,.N), cond := 1-(1-shapley_weight)^K]
    X[-c(1,.N), shapley_weight := shapley_weight/cond]
  } else if (reweight == "on_all_cond_paired") {
    X[, shapley_weight := as.numeric(shapley_weight)]
    m <- X[.N, n_features]
    if (is.null(K)) K <- X[-c(1,.N), sum(shapley_weight)]
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



compute_SV_values = function(X_now, dt_all_coalitions, dt_vS, shap_names) {
  # Get the weight matrix
  W_now <- shapr:::weight_matrix(X = X_now, normalize_W_weights = TRUE)

  # Use the pre-computed v(S) data and only extract the relevant rows (combinations)
  dt_vS_now = as.matrix(dt_vS[X_now[,id_combination_full], -"id_combination"])

  # Compute the new Shapley values
  dt_kshap <- data.table::as.data.table(t(W_now %*% dt_vS_now))
  colnames(dt_kshap) <- c("none", shap_names)
  return(dt_kshap)
}

create_X_dt_unique_and_paired = function(m, presampled_coalitions, dt_all_coalitions, weight_zero_m = 10^6) {

  # Find weights for given number of features
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  # String version
  # Insert all sampled coalitions into a data table and find their frequencies
  dt_freq = data.table::data.table(features = presampled_coalitions)[, .(shapley_weight = .N), by = features]

  # Get the number of features in each coalition
  dt_freq[, n_features := stringr::str_count(features, ",") + 1]

  # Add the number of coalitions of each size
  dt_freq[, N := n[n_features]]
  dt_freq[, p := p[n_features]]

  # Get the id_combination if we had used all combinations
  dt_freq[, id_combination_full := dt_all_coalitions[dt_freq, id, on = "features"]]

  # Convert from string to list of integer vectors. stringr is faster than base and stringi
  dt_freq[, features := lapply(stringr::str_split(features, ','), as.integer)]

  # Add the empty and grand coalitions
  dt_freq = rbindlist(
    list(
      data.table(features = list(integer(0)), shapley_weight = weight_zero_m, n_features = 0L, N = 1L, p = NA, id_combination_full = 1),
      dt_freq,
      data.table(features = list(1:m), shapley_weight = weight_zero_m, n_features = as.integer(m), N = 1L,  p = NA, id_combination_full = 2^m)
    )
  )
  data.table::setorder(dt_freq, "id_combination_full")
  dt_freq[, id_combination := .I]
  data.table::setcolorder(dt_freq, c("id_combination", "id_combination_full", "features", "n_features", "N", "shapley_weight", "p"))

  # Optional to match the old setup
  dt_freq[, N := as.integer(N)]
  dt_freq[, shapley_weight := as.integer(shapley_weight)]
  dt_freq[, n_features := as.integer(n_features)]

  return(dt_freq)
}

create_X_dt_largest_random = function(m, presampled_coalitions, dt_all_coalitions, weight_zero_m = 10^6, weight = c("uniform", "shapley")) {
  weight = match.arg(weight)
  dt <- data.table::data.table(id_combination = seq(length(presampled_coalitions)))
  dt[, features_num := presampled_coalitions]
  dt[, features := sapply(features_num, function(x) paste(x, collapse = ","))]
  dt[, n_features := length(features_num[[1]]), id_combination]
  dt[, N := 1]
  dt[-c(1, .N), N := n[n_features]]
  dt[, N := as.integer(N)]
  if (weight == "uniform") {
    dt[, shapley_weight := 1]
  } else if (weight == "shapley") {
    dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]
  }
  dt[c(1, .N), shapley_weight := weight_zero_m]

  # Get the id_combination if we had used all combinations
  dt[, id_combination_full := dt_all_coalitions[dt, id, on = "features"]]
  data.table::setorder(dt, "id_combination_full")
  data.table::setcolorder(dt, c("id_combination", "id_combination_full", "features", "features_num", "n_features", "N", "shapley_weight"))
  dt[, features := NULL]
  data.table::setnames(dt, "features_num", "features")
  return(dt)
}




# Command line arguments ------------------------------------------------------------------------------------------
args = commandArgs(trailingOnly = TRUE)
rhos = c(0.0, 0.2, 0.5, 0.9)
rhos = as.character(args[1])
if (grepl(",", rhos)) {
  rhos = as.numeric(unlist(strsplit(rhos, ",")))
} else {
  rhos = as.numeric(rhos)
}

repetitions = as.character(args[2])
if (!(repetitions %in% c("NULL", "NA", "NaN"))) {
  if (grepl(",", repetitions)) {
    repetitions = as.numeric(unlist(strsplit(repetitions, ",")))
  } else {
    repetitions = unlist(strsplit(repetitions, ":"))
    if (length(repetitions) > 1) {
      repetitions = seq(as.numeric(repetitions[1]), as.numeric(repetitions[2]))
    } else {
      repetitions = as.numeric(repetitions)
    }
  }
} else {
  repetitions = seq(10)
}



# Rscript M_20_run_simulations.R 0.0 1:5
# Rscript M_20_run_simulations.R 0.0 6:10
# Rscript M_20_run_simulations.R 0.2 1:5
# Rscript M_20_run_simulations.R 0.2 6:10
# Rscript M_20_run_simulations.R 0.5 1:5
# Rscript M_20_run_simulations.R 0.5 6:10
# Rscript M_20_run_simulations.R 0.9 1:5
# Rscript M_20_run_simulations.R 0.9 6:10


# Workstation -----------------------------------------------------------------------------------------------------
# Get where we are working
hostname = R.utils::System$getHostname()
message(sprintf("We are working on '%s'.", R.utils::System$getHostname()))

# Check if we are working on an UiO computer or not and define the correct folder based on system
if (hostname == "Larss-MacBook-Pro.local" || Sys.info()[[7]] == "larsolsen") {
  folder = "/Users/larsolsen/PhD/Paper3/shapr"
  folder_save = "/Users/larsolsen/PhD/Paper3/Paper3_save_location"
  UiO = FALSE
} else if (grepl("hpc.uio.no", hostname)) {
  # To be added
  folder = ""
  UiO = TRUE
} else if (grepl("uio.no", hostname)) {
  folder = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr"
  folder_save = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location"
  UiO = TRUE
} else {
  stop("We do not recongize the system at which the code is run (not Lars's MAC, HPC, nor UiO).")
}




# Default parameters ----------------------------------------------------------------------------------------------
M = m = 20
n_train = 1000
n_test = 250
rho_equi = FALSE
betas = c(2, 10, 0.25, -3, -1, 1.5, -0.5, 10, 1.25, 1.5, -2, 3, -1, -5, 4, -10, 2, 5, -0.5, -1, -2)
weight_zero_m = 10^6

if (M == 20) {
  n_combinations_array = c(seq(10, 200, 10), 250, 500, 750, seq(1000, 9000, 1000), c(seq(10000, 1040000, 10000), seq(1040000, 1048000, 1000)))
  n_combinations_array = c(n_combinations_array, 1048500)
  #n_combinations_array = c(seq(10, 200, 10), 250, 500, 750, seq(1000, 9000, 1000), c(seq(250000, 1000000, 25000)))
  n_combinations_vec_extra <- sapply(seq(ceiling((M - 1)/2)), choose, n = M)
  n_combinations_vec_extra[seq(floor((M - 1)/2))] = 2*n_combinations_vec_extra[seq(floor((M - 1)/2))]
  n_combinations_vec_extra = cumsum(n_combinations_vec_extra[-length(n_combinations_vec_extra)]) + 2
  n_combinations_vec_extra = c(n_combinations_vec_extra - 50,
                               n_combinations_vec_extra - 10,
                               n_combinations_vec_extra,
                               n_combinations_vec_extra + 10,
                               n_combinations_vec_extra + 50)
  n_combinations_vec_extra = n_combinations_vec_extra[-length(n_combinations_vec_extra)]
  n_combinations_array = sort(unique(c(n_combinations_array, n_combinations_vec_extra)))
  n_combinations_array = n_combinations_array[n_combinations_array > 2]
  n_combinations_array = n_combinations_array[n_combinations_array <= 1048501]
}
length(n_combinations_array)
# n_combinations_array = c(10, 50, 100, 5000, 50000, 250000)
# n_combinations_array = c(10, 50, 100, 5000, 50000, 250000, 500000, 1000000, 1048000)

# Create list of all feature combinations
all_coalitions = unlist(lapply(0:M, utils::combn, x = M, simplify = FALSE), recursive = FALSE)
dt_all_coalitions = data.table(features = sapply(all_coalitions, function(x) paste(x, collapse = ",")))[, id := .I]


# dt_exact <- data.table::data.table(id_combination = seq(2^m))
# dt_exact[, features := unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)]
# dt_exact[, n_features := length(features[[1]]), id_combination]
# dt_exact[, N := .N, n_features]
# dt_exact[, shapley_weight := shapr:::shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]


dt_exact2 = dt_exact[sort(c(1, sample(999998, x = seq(2, 2^M-1)), .N)), ]
dt_exact2 = dt_exact[c(1, sample(999998, x = seq(2, 2^M-1)), .N), ]
dt_exact2[, id_combination_old := id_combination]
dt_exact2[, id_combination := .I]
dt_exact2

all_coalitions


# for (rho in c(0.2, 0.5, 0.9)) {
#   # Get the true value
#   file_name = paste("M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "equi", rho_equi,
#                     "betas", paste(as.character(betas), collapse = "_"), sep = "_")
#
#   # Get the model and the true Shapley values
#   save_file_name_setup = file.path(folder_save, paste0(file_name, "_model.rds"))
#   save_file_name_true = file.path(folder_save, paste0(file_name, "_true.rds"))
#
#   # Load the true explanations
#   true_explanations = readRDS(save_file_name_true)
#
#   # If we have duplicates, then we remove and save the file
#   if (!is.null(true_explanations$internal$parameters$precomputed_vS)) {
#     print(object.size(true_explanations), units = "auto")
#     true_explanations$internal$parameters$precomputed_vS = NULL
#     print(object.size(true_explanations), units = "auto")
#     saveRDS(true_explanations, save_file_name_true)
#   }
# }



# Get the weights
message("Loading expected sequence lengths")
dt_new_weights_sequence = readRDS(file.path(folder_save, paste0("Sequence_length_M_", m, "_combined.rds")))
dt_new_weights_sequence = dcast(dt_new_weights_sequence[type == "mean"], M + N_S + Size ~ version, value.var = "Ps_tilde")
# dt_sequence_length = readRDS(file.path(folder_save, paste0("Sequence_length_M_", m, ".rds")))
message("Done loading expected sequence lengths")


# Start the computations ------------------------------------------------------------------------------------------
rho_idx = 4
for (rho_idx in seq_along(rhos)) {

  # Get the current rho
  rho = rhos[rho_idx]

  # Get the true value
  file_name = paste("M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "equi", rho_equi,
                    "betas", paste(as.character(betas), collapse = "_"), sep = "_")


  # Get the model and the true Shapley values
  save_file_name_setup = file.path(folder_save, paste0(file_name, "_model.rds"))
  save_file_name_true = file.path(folder_save, paste0(file_name, "_true.rds"))

  # Load the true explanations
  true_explanations = readRDS(save_file_name_true)
  dt_vS = true_explanations$internal$output$dt_vS
  shap_names <- true_explanations$internal$parameters$feature_names
  dt_true_mat = as.matrix(true_explanations$shapley_values[,-1])

  # If we have duplicates, then we remove and save the file
  if (!is.null(true_explanations$internal$parameters$precomputed_vS)) {
    print(object.size(true_explanations), units = "auto")
    true_explanations$internal$parameters$precomputed_vS = NULL
    print(object.size(true_explanations), units = "auto")
    saveRDS(true_explanations, save_file_name_true)
  }



  # Iterate over the repetitions
  repetition_idx = 1
  for (repetition_idx in seq_along(repetitions)) {

    # Get the current repetition
    repetition = repetitions[repetition_idx]

    # Small printout to the user
    message(sprintf("Working on rho = %g (%d of %d) and repetition = %d (%d of %d).",
                    rho, rho_idx, length(rhos), repetition, repetition_idx, length(repetitions)))




    message("Loading presampled coalitions unique")
    presampled_coalitions_unique = file.path(folder_save, paste0("Unique_sampling_M_", M, "_repetition_", repetition, ".rds"))
    if (file.exists(presampled_coalitions_unique)) {
      presampled_coalitions_unique = readRDS(presampled_coalitions_unique)
    } else {
      presampled_coalitions_unique = NULL
    }
    message("Done loading presampled coalitions unique")

    message("Loading presampled coalitions paired")
    presampled_coalitions_paired = file.path(folder_save, paste0("Paired_sampling_M_", M, "_repetition_", repetition, ".rds"))
    if (file.exists(presampled_coalitions_paired)) {
      presampled_coalitions_paired = readRDS(presampled_coalitions_paired)
    } else {
      presampled_coalitions_paired = NULL
    }
    message("Done loading presampled coalitions paired")

    message("Loading presampled coalitions largest")
    presampled_coalitions_largest = file.path(folder_save, paste0("Largest_random_sampling_M_", M, "_repetition_", repetition, ".rds"))
    if (file.exists(presampled_coalitions_largest)) {
      presampled_coalitions_largest = readRDS(presampled_coalitions_largest)
    } else {
      presampled_coalitions_largest = NULL
    }
    message("Done loading presampled coalitions largest")

    # Data.table to store the results for this repetition
    MAE_dt = data.table("Rho" = rho,
                        "Repetition" = repetition,
                        "N_S" = n_combinations_array,
                        "Unique" = NaN,
                        "Paired" = NaN,
                        "Paired Average" = NaN,
                        "Paired Kernel" = NaN,
                        "Paired C-Kernel" = NaN,
                        "Paired CEL-Kernel" = NaN,
                        "Paired CEPS-Kernel" = NaN,
                        "Paired Imp C-Kernel" = NaN,
                        "Paired Imp CEL-Kernel" = NaN,
                        "Paired Imp CEPS-Kernel" = NaN)


    n_combination_idx = 4
    for (n_combination_idx in seq_along(n_combinations_array)) {
      #for (n_combination_idx in c(1, 10, 20)) {
      n_combination = n_combinations_array[n_combination_idx]

      # Small printout to the user
      message(sprintf("Working on rho = %g (%d of %d), repetition = %d (%d of %d), and n_combination = %d (%d of %d).",
                      rho, rho_idx, length(rhos), repetition, repetition_idx, length(repetitions), n_combination, n_combination_idx, length(n_combinations_array)))


      # {
      #   #Exact
      #
      #   dt_exact2
      #   data.table::setorder(dt_exact2, "n_features")
      #   dt_exact2[, id_combination := .I]
      #
      #
      #   dt_exact3 = copy(dt_exact2)
      #   data.table::setorder(dt_exact3, "id_combination_old")
      #   dt_exact3[, id_combination := .I]
      #
      #   # Compute the approximated Shapley values
      #   dt_kshap_exact =
      #     compute_SV_values(X_now = dt_exact3, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)
      #
      #   # Get the MAE between the approximated and full Shapley values
      #   mean(abs(dt_true_mat - as.matrix(dt_kshap_exact[,-1])))
      #
      #
      #
      #   # Create the S matrix if we had used all combinations
      #   S_all = shapr::feature_matrix_cpp(
      #     features = unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE),
      #     m = m
      #   )
      #   S_all_list = as.list(seq(nrow(S_all)))
      #   names(S_all_list) = apply(S_all, 1, paste, collapse = "")
      #
      #   ## Get feature matrix ---------
      #   S_sampled <- shapr:::feature_matrix_cpp(features = X_now[["features"]], m = m)
      #
      #   # Get a mapping from the indices of the current set of combinations/coalitions to the indices
      #   # in the version where we use all 2^M combinations/coalitions.
      #   # current_combination_idx_in_all_combinations =
      #   #   sapply(seq(nrow(S_sampled)), function(idx) which(apply(S_all, 1, function(x) identical(x, S_sampled[idx,]))))
      #   current_combination_idx_in_all_combinations = as.numeric(S_all_list[apply(S_sampled, 1, paste, collapse = "")])
      #
      #
      # }



      ## Unique ----------------------------------------------------------------------------------------------------------
      message("Unique")
      {

        # sampling_methods = "unique" #c("unique", "unique_SW", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_unif_V2")
        # Get the n_combinations coalitions to include
        presampled_coalitions =
          presampled_coalitions_unique$all_coalitions[seq(presampled_coalitions_unique$dt_N_S_and_L[N_S == n_combination, L])]

        # Get the X data.table
        X_now = create_X_dt_unique_and_paired(m = m, presampled_coalitions = presampled_coalitions, dt_all_coalitions = dt_all_coalitions)

        # Compute the approximated Shapley values
        dt_kshap_unique =
          compute_SV_values(X_now = X_now, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

        # Get the MAE between the approximated and full Shapley values
        MAE_dt[n_combination_idx, "Unique" := mean(abs(dt_true_mat - as.matrix(dt_kshap_unique[,-1])))]


      }

      ## Paired ----------------------------------------------------------------------------------------------------------
      # Paired, average, kernel, c-kernel, cel-kernel
      message("Paired")
      {

        # Get the n_combinations coalitions to include
        presampled_coalitions =
          presampled_coalitions_paired$all_coalitions[seq(presampled_coalitions_paired$dt_N_S_and_L[N_S == n_combination, L])]

        # Get the X data.table
        X_now = create_X_dt_unique_and_paired(m = m, presampled_coalitions = presampled_coalitions, dt_all_coalitions = dt_all_coalitions)

        { # Paired
          # Compute the approximated Shapley values
          dt_kshap_paired =
            compute_SV_values(X_now = X_now, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

          # Get the MAE between the approximated and full Shapley values
          MAE_dt[n_combination_idx, "Paired" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired[,-1])))]
        }


        { # Paired average
          X_now_copy = copy(X_now)
          X_now_copy[, shapley_weight := as.numeric(shapley_weight)]

          # Average the weights on the coalition sizes
          shapley_reweighting(X = X_now_copy, reweight = "on_N")

          # Compute the approximated Shapley values
          dt_kshap_paired_average =
            compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

          # Get the MAE between the approximated and full Shapley values
          MAE_dt[n_combination_idx, "Paired Average" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired_average[,-1])))]

        }

        { # Paired kernel
          X_now_copy = copy(X_now)

          # Insert the Shapley kernel weights
          X_now_copy[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m = weight_zero_m)]

          # Compute the approximated Shapley values
          dt_kshap_paired_kernel =
            compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

          # Get the MAE between the approximated and full Shapley values
          MAE_dt[n_combination_idx, "Paired Kernel" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired_kernel[,-1])))]

        }

        { # Paired C-Kernel
          X_now_copy = copy(X_now)

          # Insert the corrected Shapley kernel weights
          shapley_reweighting(X = X_now_copy, reweight = "on_all_cond_paired")

          # Compute the approximated Shapley values
          dt_kshap_paired_c_kernel =
            compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

          # Get the MAE between the approximated and full Shapley values
          MAE_dt[n_combination_idx, "Paired C-Kernel" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired_c_kernel[,-1])))]
        }


        { # Paired CEL-Kernel and Paired CEPS-Kernel
          X_now_copy = copy(X_now)
          X_now_copy[, shapley_weight := as.numeric(shapley_weight)]

          # Get the weights
          dt_new_weights = copy(dt_new_weights_sequence)

          # Find the weights of the combination closest to n_combinations
          n_comb_use = dt_new_weights$N_S[which.min(abs(dt_new_weights$N_S - n_combination))]
          dt_new_weights_now = dt_new_weights[N_S == n_comb_use]

          dt_new_weights_now <- rbind(dt_new_weights_now, dt_new_weights_now[(.N - ifelse(.N %% 2 == 1, 0, 1)):1])
          dt_new_weights_now[, Size := .I]
          setnames(dt_new_weights_now, "Size", "n_features")


          # EXPECTED E[L]
          # Update the weights with the provided weights for each coalition size
          X_now_copy[dt_new_weights_now, on = "n_features", shapley_weight := get(gsub("_", " ", "mean_L"))]

          # Compute the approximated Shapley values
          dt_kshap_paired_cel_kernel =
            compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

          # Get the MAE between the approximated and full Shapley values
          MAE_dt[n_combination_idx, "Paired CEL-Kernel" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired_cel_kernel[,-1])))]


          # EXPECTED E[P_S]
          # Update the weights with the provided weights for each coalition size
          X_now_copy[dt_new_weights_now, on = "n_features", shapley_weight := get(gsub("_", " ", "mean_ps"))]

          # Compute the approximated Shapley values
          dt_kshap_paired_ceps_kernel =
            compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

          # Get the MAE between the approximated and full Shapley values
          MAE_dt[n_combination_idx, "Paired CEPS-Kernel" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired_ceps_kernel[,-1])))]
        }
      }


      ## Paired Imp ------------------------------------------------------------------------------------------------------
      message("Paired Imp")
      {
        # Get the features to include
        presampled_coalitions = all_coalitions[sort(presampled_coalitions_largest[seq(n_combination)])]

        # Get the X data.table
        X_now = create_X_dt_largest_random(m = m, presampled_coalitions = presampled_coalitions, dt_all_coalitions = dt_all_coalitions)


        { # Paired imp c-kernel
          X_now_copy = copy(X_now)

          # Average the weights on the coalition sizes
          shapley_reweighting(X = X_now_copy, reweight = "on_all_cond_paired")

          # Compute the approximated Shapley values
          dt_kshap_paired_imp_c_kernel =
            compute_SV_values(X_now = X_now, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

          # Get the MAE between the approximated and full Shapley values
          MAE_dt[n_combination_idx, "Paired Imp C-Kernel" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired_imp_c_kernel[,-1])))]

        }

        { # Paired imp cel-kernel and paired imp ceps-kernel
          X_now_copy = copy(X_now)
          X_now_copy[, shapley_weight := as.numeric(shapley_weight)]

          # Get the weights
          dt_new_weights = copy(dt_new_weights_sequence)

          # Find the weights of the combination closest to n_combinations
          n_comb_use = dt_new_weights$N_S[which.min(abs(dt_new_weights$N_S - n_combination))]
          dt_new_weights_now = dt_new_weights[N_S == n_comb_use]

          dt_new_weights_now <- rbind(dt_new_weights_now, dt_new_weights_now[(.N - ifelse(.N %% 2 == 1, 0, 1)):1])
          dt_new_weights_now[, Size := .I]
          setnames(dt_new_weights_now, "Size", "n_features")


          # EXPECTED E[L]
          # Update the weights with the provided weights for each coalition size
          X_now_copy[dt_new_weights_now, on = "n_features", shapley_weight := get(gsub("_", " ", "mean_L"))]

          # Compute the approximated Shapley values
          dt_kshap_paired_imp_cel_kernel =
            compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

          # Get the MAE between the approximated and full Shapley values
          MAE_dt[n_combination_idx, "Paired Imp CEL-Kernel" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired_imp_cel_kernel[,-1])))]


          # EXPECTED E[P_S]
          # Update the weights with the provided weights for each coalition size
          X_now_copy[dt_new_weights_now, on = "n_features", shapley_weight := get(gsub("_", " ", "mean_ps"))]

          # Compute the approximated Shapley values
          dt_kshap_paired_imp_ceps_kernel =
            compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

          # Get the MAE between the approximated and full Shapley values
          MAE_dt[n_combination_idx, "Paired Imp CEPS-Kernel" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired_imp_ceps_kernel[,-1])))]


        }

      }

      print(MAE_dt[n_combination_idx,])

      # Save the results
      if (n_combination_idx %% floor((length(n_combinations_array)/10)) == 0) {
        saveRDS(MAE_dt, file.path(folder_save, paste0(file_name, "_MAE_", repetition, "_tmp.rds")))
      }

    } # End n_combinations
    print(MAE_dt)

    # Melt the data.table
    MAE_dt_long = melt(MAE_dt, id.vars = c("Rho", "Repetition", "N_S"), variable.name = "Strategy", value.name = "MAE")
    # library(ggplot2)
    # ggplot(MAE_dt_long, aes(x = N_S, y = MAE, col = Strategy)) +
    #   geom_line(linewidth = 0.65) +
    #   scale_y_log10(
    #     breaks = scales::trans_breaks("log10", function(x) 10^x),
    #     labels = scales::trans_format("log10", scales::math_format(10^.x))
    #   )

    # Save the results and remove tmp file
    saveRDS(MAE_dt_long, file.path(folder_save, paste0(file_name, "_MAE_", repetition, ".rds")))
    if (file.exists(file.path(folder_save, paste0(file_name, "_MAE_", repetition, "_tmp.rds")))) {
      file.remove(file.path(folder_save, paste0(file_name, "_MAE_", repetition, "_tmp.rds")))
    }

  } # End repetition
} # End rho


