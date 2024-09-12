
# Libraries -------------------------------------------------------------------------------------------------------
library(data.table)
library(stringr)
library(shapr)
library(ggplot2)



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
  n <- sapply(seq(m - 1), choose, n = m)
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
    dt[, shapley_weight := shapr:::shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]
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

rho_equi = TRUE
rho_equi = as.logical(args[2])

repetitions = as.character(args[3])
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



#Rscript M_20_run_simulations.R 0.0 6:7 ixion
#Rscript M_20_run_simulations.R 0.0 8:10 Diktys

# Rscript M_20_run_simulations.R 0.0 6:10 labbu
# Rscript M_20_run_simulations.R 0.0 11:17 nam1
# Rscript M_20_run_simulations.R 0.0 18:25 nam1


# Rscript M_20_run_simulations.R 0.0 24,25,23
# Rscript M_20_run_simulations.R 0.0 17,16

# FERDIG
# Rscript M_20_run_simulations.R 0.2 1:10 diktys
# Rscript M_20_run_simulations.R 0.2 11:20 metis
# Rscript M_20_run_simulations.R 0.2 16:20 carpo
# Rscript M_20_run_simulations.R 0.2 25:21 aload

# FERDIG
# Rscript M_20_run_simulations.R 0.5 1:10 ixion
# Rscript M_20_run_simulations.R 0.5 11:20 nyx (snart ferdig)
# Rscript M_20_run_simulations.R 0.5 21:25 sumeru

# FERDIG
# Rscript M_20_run_simulations.R 0.9 2:5 sumeru
# Rscript M_20_run_simulations.R 0.9 6:10 tsenahale
# Rscript M_20_run_simulations.R 0.9 11:15 mawu
# Rscript M_20_run_simulations.R 0.9 16:20 beleili
# Rscript M_20_run_simulations.R 0.9 21:25 bastet

# Rscript M_20_run_simulations.R 0.9 TRUE 26:50 sumeru



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


# dt_exact2 = dt_exact[sort(c(1, sample(999998, x = seq(2, 2^M-1)), .N)), ]
# dt_exact2 = dt_exact[c(1, sample(999998, x = seq(2, 2^M-1)), .N), ]
# dt_exact2[, id_combination_old := id_combination]
# dt_exact2[, id_combination := .I]
# dt_exact2



# for (rho in c(0.2, 0.5)) {
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
  message("Loading true Shapley values")
  true_explanations = readRDS(save_file_name_true)
  dt_vS = true_explanations$internal$output$dt_vS
  shap_names <- true_explanations$internal$parameters$feature_names
  dt_true_mat = as.matrix(true_explanations$shapley_values[,-1])
  message("Done loading true Shapley values")

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
                        #"Paired CEPS-Kernel" = NaN,
                        "Paired Imp C-Kernel" = NaN,
                        "Paired Imp CEL-Kernel" = NaN,
                        #"Paired Imp CEPS-Kernel" = NaN
                        )


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
          X_now_copy[, shapley_weight := shapr:::shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m = weight_zero_m)]

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
          dt_new_weights_now <- rbind(dt_new_weights_now, dt_new_weights_now[(.N - ifelse(M[1] %% 2 == 1, 0, 1)):1])
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


          # # EXPECTED E[P_S]
          # # Update the weights with the provided weights for each coalition size
          # X_now_copy[dt_new_weights_now, on = "n_features", shapley_weight := get(gsub("_", " ", "mean_ps"))]
          #
          # # Compute the approximated Shapley values
          # dt_kshap_paired_ceps_kernel =
          #   compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)
          #
          # # Get the MAE between the approximated and full Shapley values
          # MAE_dt[n_combination_idx, "Paired CEPS-Kernel" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired_ceps_kernel[,-1])))]
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
          dt_new_weights_now <- rbind(dt_new_weights_now, dt_new_weights_now[(.N - ifelse(M[1] %% 2 == 1, 0, 1)):1])
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


          # # EXPECTED E[P_S]
          # # Update the weights with the provided weights for each coalition size
          # X_now_copy[dt_new_weights_now, on = "n_features", shapley_weight := get(gsub("_", " ", "mean_ps"))]
          #
          # # Compute the approximated Shapley values
          # dt_kshap_paired_imp_ceps_kernel =
          #   compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)
          #
          # # Get the MAE between the approximated and full Shapley values
          # MAE_dt[n_combination_idx, "Paired Imp CEPS-Kernel" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired_imp_ceps_kernel[,-1])))]


        }

      }

      print(MAE_dt[n_combination_idx,])

      # Save the results
      if (n_combination_idx %% floor((length(n_combinations_array)/10)) == 0) {
        saveRDS(MAE_dt, file.path(folder_save, paste0(file_name, "_MAE_repetition_", repetition, "_tmp.rds")))
      }

    } # End n_combinations
    print(MAE_dt)

    # Save ------------------------------------------------------------------------------------------------------------
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
    saveRDS(MAE_dt_long, file.path(folder_save, paste0(file_name, "_MAE_repetition_", repetition, ".rds")))
    if (file.exists(file.path(folder_save, paste0(file_name, "_MAE_repetition_", repetition, "_tmp.rds")))) {
      file.remove(file.path(folder_save, paste0(file_name, "_MAE_repetition_", repetition, "_tmp.rds")))
    }

  } # End repetition
} # End rho




if (FALSE) {
  # Plots -----------------------------------------------------------------------------------------------------------
  library(ggplot2)
  library(ggpubr)
  library(data.table)
  folder_save = "/Users/larsolsen/PhD/Paper3/Paper3_save_location"
  rhos = c(0.0, 0.2, 0.5, 0.9)

  ## Histogram -------------------------------------------------------------------------------------------------------
  M_20_data = lapply(rhos, function(rho) {
    # Each file is 2GB
    message(rho)
    readRDS(file.path(folder_save, paste0("M_20_n_train_1000_n_test_250_rho_", rho, "_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_-0.5_-1_-2_true.rds")))
  })
  names(M_20_data) = rhos

  # Absolute difference between the phi_0 and f(x) (for each rho)
  sapply(names(M_20_data), function(x) {
    round(mean(abs(M_20_data[[x]]$pred_explain - M_20_data[[x]]$internal$parameters$prediction_zero)), 2)
  })

  # Absolute difference between the phi_0 and f(x) and then divided by M (for each rho)
  sapply(names(M_20_data), function(x) {
    round(mean(abs(M_20_data[[x]]$pred_explain - M_20_data[[x]]$internal$parameters$prediction_zero)) / 20, 2)
  })

  # Look at the mean absolute Shapley values for each feature and rho
  sapply(names(M_20_data), function(x) {
    round(colMeans(abs(M_20_data[[x]]$shapley_values)), 2)
  })

  # Look at the standard deviation of the absolute Shapley values for each feature and rho
  sapply(names(M_20_data), function(x) {
    round(apply(as.matrix(abs(M_20_data[[x]]$shapley_values)), 2, sd), 2)
  })

  # Look at the max of the absolute Shapley values for each feature and rho
  sapply(names(M_20_data), function(x) {
    round(apply(as.matrix(abs(M_20_data[[x]]$shapley_values)), 2, max), 2)
  })

  # Get the phi_0 values for each rho
  phi0s = data.table(rho = names(M_20_data), phi0 = sapply(names(M_20_data), function(x) M_20_data[[x]]$internal$parameters$prediction_zero))
  phi0s_text = data.table(
    label = paste0("phi[0]=='", format(round(phi0s$phi0, 2), drop0Trailing = FALSE), "'"), # Add "'" so that i can parse the string but keep trailing zeros
    rho   = phi0s$rho,
    x     = round(phi0s$phi0, 2),
    y     = rep(33.25, 4)
  )

  # Extract the explicands for each rho
  explicands = rbindlist(lapply(names(M_20_data),
                                function(x) data.table(rho = x, id_explicand = seq(length(M_20_data[[x]]$pred_explain)), prediction = M_20_data[[x]]$pred_explain)))

  # Create the histogram
  fig_hist = ggplot(explicands, aes(x = prediction)) +
    geom_histogram(binwidth = 6, color="black", fill = "grey") +
    facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), ncol = 4) +
    geom_vline(data = phi0s, mapping = aes(xintercept = phi0), color = "black", linewidth = 1, linetype = "dashed") +
    geom_text(data = phi0s_text, mapping = aes(x = x, y = y, label = label), parse = TRUE, size = 6, color = "black", hjust = -0.695, vjust = 0.05) +
    labs(color = "Strategy:", fill = "Strategy:", x = expression(f(bold(x)*"*")), y = "Count") +
    #lims(y = c(0, 90)) +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          axis.title.y = element_text(size = rel(1.7)),# margin = margin(r = 25)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.4)))

  # Save the histogram
  ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_20_fig_hist_V1.png"),
         plot =  fig_hist,
         width = 14,
         height = 3.5,
         scale = 1,
         dpi = 350)


  ## MAE -------------------------------------------------------------------------------------------------------------
  # What correlation matrix to use
  # rho_equi = TRUE
  rho_equi = FALSE
  M = m = 20
  n_train = 1000
  n_test = 250
  betas = c(2, 10, 0.25, -3, -1, 1.5, -0.5, 10, 1.25, 1.5, -2, 3, -1, -5, 4, -10, 2, 5, -0.5, -1, -2)

  # Compute the vertical dashed lines
  n_features <- seq(ceiling((m - 1)/2))
  n <- sapply(n_features, choose, n = m)
  n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
  n_cumsum = (cumsum(n) + 2) + 0.5

  # Number of repetitions
  n_repetitions = 25

  # List the strategies to create the MAE plot for
  strat_MAE = c("Unique",
                "Paired",
                "Paired Average",
                "Paired Kernel",
                "Paired C-Kernel",
                "Paired CEL-Kernel",
                #"Paired CEPS-Kernel",
                "Paired Imp C-Kernel",
                "Paired Imp CEL-Kernel"
                #"Paired Imp CEPS-Kernel"
  )

  # Strategies in the relative difference plot
  strat_rel_diff = c("Paired Average", "Paired C-Kernel", "Paired CEL-Kernel")
  strat_rel_diff_reference = c("Paired C-Kernel")

  # Load the MAE data
  res = data.table::rbindlist(
    lapply(c(0.0, 0.2, 0.5, 0.9), function(rho) {
      data.table::rbindlist(
        lapply(seq(n_repetitions), function(repetition) {
          file_name = paste("M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "equi", rho_equi,
                            "betas", paste(as.character(betas), collapse = "_"), sep = "_")
          file_name = file.path(folder_save, "M_20_MAE", paste0(file_name, "_MAE_repetition_", repetition, ".rds"))
          if (!file.exists(file_name)) return(NULL)
          print(file_name)
          readRDS(file_name)
        }))
    }))

  # Values to skip
  N_S_skip = NULL
  res = res[!N_S %in% N_S_skip]

  # Compute the mean, lower, and upper MAE
  res_MAE = res[Strategy %in% strat_MAE, .(MAE_mean = mean(MAE),
                                           MAE_lower = quantile(MAE, 0.025),
                                           MAE_upper = quantile(MAE, 0.975)),
                by = c("Rho", "N_S", "Strategy")]


  M_20_fig_MAE =
    ggplot(res_MAE, aes(x = N_S, y = MAE_mean, col = Strategy, fill = Strategy)) +
    facet_wrap( . ~ Rho, labeller = label_bquote(cols = rho ==.(Rho)), scales = "free_y") +
    geom_vline(xintercept = n_cumsum, col = "gray50", linetype = "dashed", linewidth = 0.4) +
    geom_ribbon(aes(ymin = MAE_lower, ymax = MAE_upper), alpha = 0.4, linewidth = 0.0) +
    geom_line(linewidth = 0.65) +
    scale_x_continuous(labels = scales::label_number()) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    theme(legend.position = 'bottom') +
    guides(col = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
    labs(color = "Strategy:", fill = "Strategy:", linetype = "Strategy:",
         x = expression(N[S]),
         y = bquote(bar(MAE)[50]*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.8)),
          legend.text = element_text(size = rel(1.8)),
          axis.title = element_text(size = rel(1.7)),
          axis.text = element_text(size = rel(1.5)))
    # scale_color_manual(values = colors) +
    # scale_fill_manual(values = colors)
    #coord_cartesian(ylim = c(10^(-4.1), 10^(-0.7)))
  M_20_fig_MAE

  ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_20_fig_MAE_V1.png"),
         plot = M_20_fig_MAE,
         width = 14,
         height = 9,
         scale = 0.85,
         dpi = 350)

  ## Relative diff ---------------------------------------------------------------------------------------------------
  # Compute the relative error for each n_combination and repetition, and then compute mean, lower and upper values
  res_rel_diff = copy(res[Strategy %in% strat_rel_diff])
  res_rel_diff[, rel_error := (MAE - MAE[Strategy == strat_rel_diff_reference]) / MAE[Strategy == strat_rel_diff_reference], by = list(Rho, N_S, Repetition)]
  res_rel_diff_errors = res_rel_diff[, .(rel_error_mean = mean(rel_error),
                                         rel_error_lower = quantile(rel_error, 0.025),
                                         rel_error_upper = quantile(rel_error, 0.975)),
                                     by = c("Rho", "N_S", "Strategy")]

  ggplot(res_rel_diff_errors, aes(x = N_S, y = rel_error_mean, color = Strategy, fill = Strategy)) +
    facet_wrap( . ~ Rho, labeller = label_bquote(cols = rho ==.(Rho)), scales = "free_y") +
    geom_ribbon(aes(ymin = rel_error_lower, ymax = rel_error_upper), alpha = 0.4, linewidth = 0.1) +
    geom_line(linewidth = 0.65) +
    coord_cartesian(ylim = c(-0.1, 0.2))


  # Compute the bootstrap for relative difference for the MAE_mean
  boot_repetitions = 100

  res_rel_diff_boot = data.table::rbindlist(
    lapply(seq(boot_repetitions),
           function(b, res_rel_diff, strat_rel_diff_reference) {
             message(b)

             # Set seed for reproducibility
             set.seed(b)

             # Get the bootstrap data table
             res_rel_diff_boot_now = res_rel_diff[, .SD[sample(n_repetitions, replace = TRUE)], by = .(Rho, N_S, Strategy)]

             # Compute the average MAE
             res_rel_diff_boot_now = res_rel_diff_boot_now[, .(MAE_mean = mean(MAE)), by = .(Rho, N_S, Strategy)]

             # Compute the relative difference
             res_rel_diff_boot_now[, rel_error := (MAE_mean - MAE_mean[Strategy == strat_rel_diff_reference]) / MAE_mean[Strategy == strat_rel_diff_reference], by = list(Rho, N_S)]
           },
           res_rel_diff = res_rel_diff, strat_rel_diff_reference = strat_rel_diff_reference),
    use.names = TRUE, idcol = "id_boot")

  # Compute the relative errors for the
  res_rel_diff_boot_errors = res_rel_diff_boot[, .(rel_error_mean = mean(rel_error),
                                                   rel_error_lower = quantile(rel_error, 0.025),
                                                   rel_error_upper = quantile(rel_error, 0.975)),
                                               by = .(Rho, N_S, Strategy)]


  # Plot the results
  M_20_fig_relative = ggplot(res_rel_diff_boot_errors, aes(x = N_S, y = rel_error_mean, color = Strategy, fill = Strategy)) +
    facet_wrap( . ~ Rho, labeller = label_bquote(cols = rho ==.(Rho)), scales = "free_y") +
    coord_cartesian(ylim = c(-0.125, 0.25)) +
    geom_ribbon(data = res_rel_diff_errors, aes(ymin = rel_error_lower, ymax = rel_error_upper), alpha = 0.15, linewidth = 0.4, linetype = "dashed") +
    geom_ribbon(aes(ymin = rel_error_lower, ymax = rel_error_upper), alpha = 0.6, linewidth = 0.1) +
    geom_line(linewidth = 1.1) +
    labs(y = latex2exp::TeX(r'($\frac{\bar{MAE}_{Strategy} - \bar{MAE}_{Paired~C-Kernel}}{\bar{MAE}_{Paired~C-Kernel}}$)')) +
    theme(legend.position = 'bottom') +
    guides(col = guide_legend(nrow = 1, theme = theme(legend.byrow = FALSE)),
           fill = guide_legend(nrow = 1, theme = theme(legend.byrow = FALSE)),
           linetype = "none") +
    labs(color = "Strategy:",
         fill = "Strategy:",
         x = expression(N[S])) +
    scale_x_continuous(labels = scales::label_number()) +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.9)),
          legend.text = element_text(size = rel(1.9)),
          axis.title = element_text(size = rel(1.7)),
          axis.text = element_text(size = rel(1.5)))

  M_20_fig_relative
  ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_20_fig_relative_V1.png"),
         plot = M_20__fig_relative,
         width = 14.2,
         height = 7,
         scale = 1,
         dpi = 350)







  # Rel -------------------------------------------------------------------------------------------------------------




  # Compute the average
  res2 = res[, .(MAE = mean(MAE), lower = quantile(MAE, 0.025), upper = quantile(MAE, 0.975)), by = c("Rho", "N_S", "Strategy")]


  n_features <- seq(ceiling((m - 1)/2))
  n <- sapply(n_features, choose, n = m)
  n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
  n_cumsum = (cumsum(n) + 2) + 0.5

  Strat_now = c("Unique",
                "Paired",
                "Paired Average",
                "Paired Kernel",
                "Paired C-Kernel",
                "Paired CEL-Kernel",
                #"Paired CEPS-Kernel",
                "Paired Imp C-Kernel",
                "Paired Imp CEL-Kernel"
                #"Paired Imp CEPS-Kernel"
  )

  res3 = res2[Strategy %in% Strat_now, ]
  fig_M_20_MAE =
    ggplot(res3, aes(x = N_S, y = MAE, col = Strategy, fill = Strategy)) +
    geom_vline(xintercept = n_cumsum, col = "gray50", linetype = "dashed", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, linewidth = 0.0) +
    geom_line(linewidth = 0.65) +
    scale_x_continuous(labels = scales::label_number()) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    facet_wrap( . ~ Rho, labeller = label_bquote(cols = rho ==.(Rho)), scales = "free_y") +
    theme(legend.position = 'bottom') +
    guides(col = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
    labs(color = "Strategy:", fill = "Strategy:", linetype = "Strategy:",
         x = expression(N[S]),
         y = bquote(bar(MAE)[25]*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
    theme(strip.text = element_text(size = rel(1.6)),
          legend.title = element_text(size = rel(1.7)),
          legend.text = element_text(size = rel(1.7)),
          axis.title.x = element_text(size = rel(1.6)),
          axis.title.y = element_text(size = rel(2)),
          axis.text.x = element_text(size = rel(1.75)),
          axis.text.y = element_text(size = rel(1.75))) +
    scale_color_hue() + #added as we want ordered
    scale_fill_hue()




  MAE_dt_long2 = copy(res3)
  data.table::setnames(MAE_dt_long2, old = c("Rho", "Strategy", "MAE", "N_S"), new = c("rho", "sampling", "mean", "n_combinations"))
  fig_M_20_rel =
    relative_difference(dt = MAE_dt_long2,
                      m = 20,
                      strat_ref = "Paired C-Kernel",
                      strat_other = c("Unique",
                                      "Paired",
                                      "Paired Average",
                                      "Paired Kernel",
                                      "Paired C-Kernel",
                                      "Paired CEL-Kernel",
                                      #"Paired CEPS-Kernel",
                                      "Paired Imp C-Kernel",
                                      "Paired Imp CEL-Kernel"
                                      #"Paired Imp CEPS-Kernel"
                      ),
                      include_coal_size_lines = TRUE,
                      y_limits = c(-0.9, 5.1),
                      y_breaks = c(-1, -0.4, -0.1, 0, 0.1, 0.4, 1, 2, 4)
  )

  fig_M_20_rel_reg_scale =
    relative_difference(dt = MAE_dt_long2,
                        m = 20,
                        strat_ref = "Paired C-Kernel",
                        strat_other = c(#"Unique",
                                        #"Paired",
                                        "Paired Average",
                                        #"Paired Kernel",
                                        "Paired C-Kernel",
                                        "Paired CEL-Kernel"
                                        #"Paired CEPS-Kernel",
                                        #"Paired Imp C-Kernel",
                                        #"Paired Imp CEL-Kernel"
                                        #"Paired Imp CEPS-Kernel"
                        ),
                        include_coal_size_lines = FALSE,
                        y_limits = c(-0.025, 0.05),
                        scale = FALSE,
                        hue_indices = c(3,5,6),
                        hue_length = 8,
                        ncol = 2,
                        scales = "fixed"
    ) + scale_y_continuous(labels = scales::percent_format(accuracy = 1))


  library("ggpubr")
  fig_M_20_comb = ggarrange(fig_M_20_MAE, fig_M_20_rel,
                       labels = c("A", "B"),
                       ncol = 1, nrow = 2,
                       align = "hv",
                       common.legend = TRUE, legend = "bottom",
                       font.label = list(size = 25, color = "black"))

  ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_20_MAE_Relative_Diff_V4.png"),
         plot = fig_M_20_comb,
         width = 14.2,
         height = 18,
         scale = 0.85,
         dpi = 350)


  fig_M_20_comb = ggarrange(fig_M_20_MAE, fig_M_20_rel,
                            labels = c("B", "C"),
                            ncol = 1, nrow = 2,
                            align = "hv",
                            common.legend = TRUE, legend = "bottom",
                            font.label = list(size = 25, color = "black"))


  fig_M_20_comb_V2 = ggarrange(fig_hist, fig_M_20_comb, labels = c("A", "B", "C"),
            ncol = 1,
            heights = c(1,4.25),
            common.legend = TRUE, legend = "bottom",
            font.label = list(size = 25, color = "black"))
  ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_20_MAE_Relative_Diff_V11.png"),
         plot = fig_M_20_comb_V2,
         width = 14.2,
         height = 19.25,
         scale = 0.85,
         dpi = 350)


}
