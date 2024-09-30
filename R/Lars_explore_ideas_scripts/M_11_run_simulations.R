
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
create_X_dt_KernelSHAP = function(m, presampled_coalitions, prefixed_coalitions,
                                  dt_all_coalitions, weight_zero_m = 10^6, version_scaled = TRUE) {
  # regular, average, c-kernel, cel-kernel

  # Find weights for given number of features
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  # Weight the different coalition sizes (KernelSHAP version)
  num_subset_sizes = as.integer(ceiling((M - 1) / 2))
  num_paired_subset_sizes = as.integer(floor((M - 1) / 2))
  weight_vector = sapply(seq(num_subset_sizes), function(i) (M - 1.0) / (i * (M - i)))
  weight_vector[seq(num_paired_subset_sizes)] = 2*weight_vector[seq(num_paired_subset_sizes)]
  weight_vector = weight_vector / sum(weight_vector)

  # Get the sampled coalitions for which we have to compute the frequencies
  if (!is.null(prefixed_coalitions)) {
    presampled_coal_wo_prefixed_coal = presampled_coalitions[-seq(nrow(prefixed_coalitions))]
  } else {
    presampled_coal_wo_prefixed_coal = presampled_coalitions
  }

  # String version
  # Insert all sampled coalitions into a data table and find their frequencies
  dt_freq = data.table::data.table(features = presampled_coal_wo_prefixed_coal)[, .(shapley_weight = .N), by = features]

  # Fix the weights according to the technique in KernelSHAP
  if (version_scaled) {
    if (is.null(prefixed_coalitions)) {
      num_full_subsets = 0
      weight_left = sum(weight_vector)
    } else {
      num_full_subsets = length(prefixed_coalitions[.N - 1, features][[1]]) # This relies on the list version
      weight_left = sum(weight_vector[-seq(num_full_subsets)])
    }
    dt_freq[, shapley_weight := shapley_weight * weight_left / sum(shapley_weight)]
  }

  # Convert the list column to a comma-separated string for each row
  if (!is.null(prefixed_coalitions)) {
    prefixed_coalitions[, features := sapply(features, function(x) paste(unlist(x), collapse = ","))]
    setnames(prefixed_coalitions, "w", "shapley_weight")
  }

  # Put together with the prefixed samples
  dt_freq = rbind(prefixed_coalitions, dt_freq)

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
  # dt_freq

  # Optional to match the old setup
  dt_freq[, N := as.integer(N)]
  dt_freq[, n_features := as.integer(n_features)]

  # plot(dt_freq[-c(1, .N), id_combination_full], dt_freq[-c(1, .N), shapley_weight])
  #
  # dt_freq[, sum(shapley_weight), by = N][-1]
  return(dt_freq)
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

relative_difference_wine_V2 = function(dt,
                                       m,
                                       strat_ref,
                                       strat_other = NULL,
                                       y_breaks = waiver(),
                                       y_limits = NULL,
                                       scale = TRUE,
                                       legend_n_row = 2,
                                       include_coal_size_lines = FALSE,
                                       hue_indices = NULL,
                                       hue_length = NULL,
                                       y_lab_frac = TRUE,
                                       skip_N_S = NULL) {
  if (xor(is.null(hue_indices), is.null(hue_length))) stop("Both `hue_indices` and `hue_length` must be provided.")
  if (!is.null(hue_indices) && !is.null(hue_length)) {
    hues = seq(15, 375, length = hue_length + 1)
    colors = grDevices::hcl(h = hues, l = 65, c = 100)[1:hue_length][hue_indices]
  }

  library(latex2exp)
  library(ggallin)
  library(data.table)

  # Get all the names from the data table
  if (is.null(strat_other)) strat_other = levels(dt$sampling)

  # Extract the needed columns
  dt = dt[, c("sampling", "n_combinations", "mean", "lower", "upper")]

  # Only even n_combinations
  dt = dt[n_combinations %% 2 == 0]

  # Only get the wanted strategies
  dt = dt[sampling %in% strat_other,]

  # Compute the relative error for each n_combination
  dt[, rel_error := (mean - mean[sampling == strat_ref]) / mean[sampling == strat_ref], by = list(n_combinations)]
  dt[, rel_error_upper := (upper - upper[sampling == strat_ref]) / upper[sampling == strat_ref], by = list(n_combinations)]
  dt[, rel_error_lower := (lower - lower[sampling == strat_ref]) / lower[sampling == strat_ref], by = list(n_combinations)]

  # Convert sampling to a ordered factor
  dt = dt[, sampling := factor(sampling, levels = strat_other, ordered = TRUE)]

  #
  n_features <- seq(ceiling((m - 1)/2))
  n <- sapply(n_features, choose, n = m)
  n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
  n_cumsum = (cumsum(n) + 2) + 0.5

  # Remove some N_s values
  if (!is.null(skip_N_S)) dt = dt[!n_combinations %in% skip_N_S,]


  # plot(res2[N_S <= 2044 & !N_S %in% skip_N_S & Strategy %in% c("Paired CEL-Kernel"), rel_error_avg],
  #      dt[n_combinations <= 2044 & sampling == "Paired CEL-Kernel", rel_error])
  # abline(a = 0, b = 1, col = "red")
  #
  # ggplot(res2[N_S <= 2044 & Strategy %in% c("Paired Average", "Paired C-Kernel", "Paired CEL-Kernel"),],
  #        aes(x = N_S, y = rel_error_avg, color = Strategy, fill = Strategy)) +
  #   geom_ribbon(aes(ymin = rel_error_lower, ymax = rel_error_upper), alpha = 0.4, linewidth = 0.1) +
  #   geom_hline(yintercept = 0, col = "gray") +
  #   geom_line(linewidth = 0.65) +
  #   coord_cartesian(ylim = c(-0.1, 0.2))



  # Make the plot
  fig = ggplot(dt, aes(x = n_combinations, y = rel_error, col = sampling, fill = sampling)) +
    {if (include_coal_size_lines) geom_vline(xintercept = n_cumsum, col = "gray50", linetype = "dashed", linewidth = 0.4)} +
    geom_hline(yintercept = 0, col = "gray") +
    geom_line(linewidth = 0.65) +
    geom_ribbon(aes(ymin = rel_error_lower, ymax = rel_error_upper), alpha = 0.4, linewidth = 0.1) +
    {if (scale) scale_y_log10(trans = ggallin:::ssqrt_trans, breaks = y_breaks)} +
    {if (!scale) scale_y_continuous(breaks = y_breaks)} +
    coord_cartesian(ylim = y_limits) +
    scale_x_continuous(labels = scales::label_number()) +
    # scale_y_continuous(limits = c(-1, 2.5)) +
    theme(legend.position = 'bottom') +
    guides(col = guide_legend(nrow = legend_n_row, theme = theme(legend.byrow = FALSE)),
           fill = guide_legend(nrow = legend_n_row, theme = theme(legend.byrow = FALSE)),
           linetype = "none") +
    labs(color = "Strategy:",
         fill = "Strategy:",
         # y = "Relative difference",
         x = expression(N[S])) +
    { if (y_lab_frac) {
      labs(y = latex2exp::TeX(r'($\frac{\bar{MAE}_{Strategy} - \bar{MAE}_{Paired~C-Kernel}}{\bar{MAE}_{Paired~C-Kernel}}$)'))
    } else {
      labs(y = latex2exp::TeX(r'($(MAE_{Strategy} - MAE_{Paired~C-Kernel}) / MAE_{Paired~C-Kernel}$)'))
    }} +
    theme(strip.text = element_text(size = rel(1.6)),
          legend.title = element_text(size = rel(1.37)),
          legend.text = element_text(size = rel(1.37)),
          axis.title.y = element_text(size = rel(1.25)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.65)),
          axis.text.y = element_text(size = rel(1.65))) +
    {if (!is.null(hue_length)) scale_color_manual(values = colors)} +
    {if (!is.null(hue_length)) scale_fill_manual(values = colors)} +
    {if (is.null(hue_length)) scale_color_hue()} + #added as we want ordered}
    {if (is.null(hue_length)) scale_fill_hue()} #added as we want ordered}
  #fig

  return(fig)

  # The relative difference on signed square root scale.
}



# Command line arguments ------------------------------------------------------------------------------------------
args = commandArgs(trailingOnly = TRUE)

repetitions = as.character(args[1])
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

# Rscript M_11_run_simulations.R 1:250 labbu
# Rscript M_11_run_simulations.R 251:500

# Rscript M_11_run_simulations.R 1:100
# Rscript M_11_run_simulations.R 101:200
# Rscript M_11_run_simulations.R 201:300
# Rscript M_11_run_simulations.R 301:400
# Rscript M_11_run_simulations.R 401:500


# Rscript M_11_run_simulations.R 100:51
# Rscript M_11_run_simulations.R 200:151
# Rscript M_11_run_simulations.R 300:251
# Rscript M_11_run_simulations.R 400:351
# Rscript M_11_run_simulations.R 500:451

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

# Workstation -----------------------------------------------------------------------------------------------------
# Get where we are working
hostname = R.utils::System$getHostname()
message(sprintf("We are working on '%s'.", R.utils::System$getHostname()))

# Check if we are working on an UiO computer or not and define the correct folder based on system
if (hostname == "Larss-MacBook-Pro.local" || Sys.info()[[7]] == "larsolsen") {
  folder = "/Users/larsolsen/PhD/Paper3/shapr"
  folder_save = "/Users/larsolsen/PhD/Paper3/Paper3_save_location"
  folder_save_KernelSHAP = "/Users/larsolsen/PhD/Paper3/Paper3_save_location"
  folder_save_KernelSHAP_paired = "/Users/larsolsen/PhD/Paper3/Paper3_save_location_paired"
  UiO = FALSE
} else if (grepl("hpc.uio.no", hostname)) {
  # To be added
  folder = ""
  UiO = TRUE
} else if (grepl("uio.no", hostname)) {
  folder = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr"
  folder_save = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location"
  folder_save_2 = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location_2"
  folder_save_KernelSHAP = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location_KernelSHAP"
  folder_save_KernelSHAP_paired = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location_KernelSHAP_paired"
  UiO = TRUE
} else {
  stop("We do not recongize the system at which the code is run (not Lars's MAC, HPC, nor UiO).")
}




# Default parameters ----------------------------------------------------------------------------------------------
M = m = 11
n_train = 1000
n_test = 1000
betas = c(2, 10, 0.25, -3, -1, 1.5, -0.5, 10, 1.25, 1.5, -2, 3, -1, -5, 4, -10, 2, 5, -0.5, -1, -2)[seq(M+1)]
weight_zero_m = 10^6
n_combinations_array = seq(4, 2^M-2, 2)
verbose_now = FALSE
only_KernelSHAP = TRUE

# Create list of all feature combinations
all_coalitions = unlist(lapply(0:M, utils::combn, x = M, simplify = FALSE), recursive = FALSE)
dt_all_coalitions = data.table(features = sapply(all_coalitions, function(x) paste(x, collapse = ",")))[, id := .I]




# Get the weights
message("Loading expected sequence lengths")
dt_new_weights_sequence = readRDS(file.path(folder_save, paste0("Sequence_length_M_", m, "_combined.rds")))
dt_new_weights_sequence = dcast(dt_new_weights_sequence[type == "mean"], M + N_S + Size ~ version, value.var = "Ps_tilde")
# dt_sequence_length = readRDS(file.path(folder_save, paste0("Sequence_length_M_", m, ".rds")))
message("Done loading expected sequence lengths")


# Start the computations ------------------------------------------------------------------------------------------

# Make a filename
file_name = paste0("Wine_data_set_M_", M)

# Load the true explanations
message("Loading true Shapley values")
true_explanations = readRDS(file.path(folder_save, paste0("Wine_data_sep_rf", ".rds")))
dt_vS = true_explanations$internal$output$dt_vS
shap_names <- true_explanations$internal$parameters$feature_names
dt_true_mat = as.matrix(true_explanations$shapley_values[,-1])
message("Done loading true Shapley values")

# Iterate over the repetitions
repetition_idx = 1
for (repetition_idx in seq_along(repetitions)) {

  # Get the current repetition
  repetition = repetitions[repetition_idx]

  # Small printout to the user
  message(sprintf("Working on repetition = %d (%d of %d).", repetition, repetition_idx, length(repetitions)))


  if (!only_KernelSHAP) {
    message("Loading presampled coalitions unique")
    presampled_coalitions_unique = file.path(folder_save_2, paste0("Unique_sampling_M_", M, "_repetition_", repetition, ".rds"))
    if (file.exists(presampled_coalitions_unique)) {
      presampled_coalitions_unique = readRDS(presampled_coalitions_unique)
    } else {
      presampled_coalitions_unique = NULL
    }
    message("Done loading presampled coalitions unique")

    message("Loading presampled coalitions paired")
    presampled_coalitions_paired = file.path(folder_save_2, paste0("Paired_sampling_M_", M, "_repetition_", repetition, ".rds"))
    if (file.exists(presampled_coalitions_paired)) {
      presampled_coalitions_paired = readRDS(presampled_coalitions_paired)
    } else {
      presampled_coalitions_paired = NULL
    }
    message("Done loading presampled coalitions paired")

    message("Loading presampled coalitions largest")
    presampled_coalitions_largest = file.path(folder_save_2, paste0("Largest_random_sampling_M_", M, "_repetition_", repetition, ".rds"))
    if (file.exists(presampled_coalitions_largest)) {
      presampled_coalitions_largest = readRDS(presampled_coalitions_largest)
    } else {
      presampled_coalitions_largest = NULL
    }
    message("Done loading presampled coalitions largest")
  }

  message("Loading presampled coalitions KernelSHAP")
  presampled_coalitions_KernelSHAP = file.path(folder_save_KernelSHAP, paste0("KernelSHAP_sampling_M_", M, "_repetition_", repetition, ".rds"))
  if (file.exists(presampled_coalitions_KernelSHAP)) {
    presampled_coalitions_KernelSHAP = readRDS(presampled_coalitions_KernelSHAP)
  } else {
    presampled_coalitions_KernelSHAP = NULL
    stop("`presampled_coalitions_KernelSHAP` does not exist")
  }
  # print(presampled_coalitions_KernelSHAP)
  message("Done loading presampled coalitions KernelSHAP")

  message("Loading presampled coalitions KernelSHAP_paired")
  presampled_coalitions_KernelSHAP_paired = file.path(folder_save_KernelSHAP_paired, paste0("KernelSHAP_sampling_paired_M_", M, "_repetition_", repetition, ".rds"))
  if (file.exists(presampled_coalitions_KernelSHAP_paired)) {
    presampled_coalitions_KernelSHAP_paired = readRDS(presampled_coalitions_KernelSHAP_paired)
  } else {
    presampled_coalitions_KernelSHAP_paired = NULL
    stop("`presampled_coalitions_KernelSHAP_paired` does not exist")
  }
  # print(presampled_coalitions_KernelSHAP)
  message("Done loading presampled coalitions KernelSHAP_paired")


  # Data.table to store the results for this repetition
  MAE_dt = data.table("Repetition" = repetition,
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
                      "Paired Imp CEPS-Kernel" = NaN,
                      "KernelSHAP" = NaN,
                      "KernelSHAP Average" = NaN,
                      #"KernelSHAP C-Kernel" = NaN,
                      "KernelSHAP CEL-Kernel" = NaN,
                      "Paired KernelSHAP" = NaN,
                      "Paired KernelSHAP Average" = NaN,
                      #"Paired KernelSHAP C-Kernel" = NaN,
                      "Paired KernelSHAP CEL-Kernel" = NaN)
  if (only_KernelSHAP) {
    MAE_dt = data.table("Repetition" = repetition,
                        "N_S" = n_combinations_array,
                        "KernelSHAP" = NaN,
                        "KernelSHAP Average" = NaN,
                        #"KernelSHAP C-Kernel" = NaN,
                        "KernelSHAP CEL-Kernel" = NaN,
                        "Paired KernelSHAP" = NaN,
                        "Paired KernelSHAP Average" = NaN,
                        #"Paired KernelSHAP C-Kernel" = NaN,
                        "Paired KernelSHAP CEL-Kernel" = NaN)
  }




  n_combination_idx = 50
  for (n_combination_idx in seq_along(n_combinations_array)) {
    #for (n_combination_idx in c(1, 10, 20)) {
    n_combination = n_combinations_array[n_combination_idx]

    # Small printout to the user
    message(sprintf("Working on repetition = %d (%d of %d) and n_combination = %d (%d of %d).",
                    repetition, repetition_idx, length(repetitions),
                    n_combination, n_combination_idx, length(n_combinations_array)))


    ## Unique ----------------------------------------------------------------------------------------------------------
    if (verbose_now) message("Unique")
    if (!only_KernelSHAP) {

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
    if (verbose_now) message("Paired")
    if (!only_KernelSHAP) {

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
    if (verbose_now) message("Paired Imp")
    if (!only_KernelSHAP) {
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

    ## KernelSHAP ------------------------------------------------------------------------------------------------------
    # Paired, average, kernel, c-kernel, cel-kernel
    if (verbose_now) message("KernelSHAP")
    {

      # Figure out which list to look at
      dt_id = presampled_coalitions_KernelSHAP$look_up$dt_n_comb_needed_sample[N_S == n_combination, dt_id]

      # Get the n_combinations coalitions to include
      to_this_index = presampled_coalitions_KernelSHAP$samples[[dt_id]]$dt_N_S_and_L_small[N_S == n_combination, L]
      presampled_coalitions = copy(presampled_coalitions_KernelSHAP$samples[[dt_id]]$all_coalitions_small[seq(to_this_index)])
      prefixed_coalitions = copy(presampled_coalitions_KernelSHAP$samples[[dt_id]]$dt_res)

      # Get the X data.table
      X_now = create_X_dt_KernelSHAP(m = m,
                                     presampled_coalitions = presampled_coalitions,
                                     prefixed_coalitions = copy(prefixed_coalitions),
                                     dt_all_coalitions = dt_all_coalitions,
                                     version_scaled = TRUE)

      # # Get the X data.table
      # X_now_int = create_X_dt_KernelSHAP(m = m,
      #                                presampled_coalitions = presampled_coalitions,
      #                                prefixed_coalitions = copy(prefixed_coalitions),
      #                                dt_all_coalitions = dt_all_coalitions,
      #                                version_scaled = FALSE)

      # "KernelSHAP" = NaN,
      # "KernelSHAP Average" = NaN,
      # "KernelSHAP C-Kernel" = NaN,
      # "KernelSHAP CEL-Kernel" = NaN

      # Default version
      {
        # Compute the approximated Shapley values
        dt_KernelSHAP =
          compute_SV_values(X_now = X_now, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

        # Get the MAE between the approximated and full Shapley values
        MAE_dt[n_combination_idx, "KernelSHAP" := mean(abs(dt_true_mat - as.matrix(dt_KernelSHAP[,-1])))]
      }

      # Average
      {
        X_now_copy = copy(X_now)
        #plot(X_now_copy[-c(1, .N), id_combination_full], X_now_copy[-c(1, .N), shapley_weight])
        X_now_copy[, shapley_weight := as.numeric(shapley_weight)]

        # Average the weights on the coalition sizes
        shapley_reweighting(X = X_now_copy, reweight = "on_N")
        #plot(X_now_copy[-c(1, .N), id_combination_full], X_now_copy[-c(1, .N), shapley_weight])

        # Compute the approximated Shapley values
        dt_kernelSHAP_average =
          compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

        # Get the MAE between the approximated and full Shapley values
        MAE_dt[n_combination_idx, "KernelSHAP Average" := mean(abs(dt_true_mat - as.matrix(dt_kernelSHAP_average[,-1])))]
      }

      # # C-kernel
      # {
      #   X_now_copy = copy(X_now)
      #
      #   # Insert the corrected Shapley kernel weights
      #   shapley_reweighting(X = X_now_copy, reweight = "on_all_cond_paired")
      #
      #   # Compute the approximated Shapley values
      #   dt_kshap_paired_c_kernel =
      #     compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)
      #
      #   # Get the MAE between the approximated and full Shapley values
      #   MAE_dt[n_combination_idx, "Paired C-Kernel" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired_c_kernel[,-1])))]
      # }

      # CEL-kernel
      {
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
        dt_KernelSHAP_cel_kernel =
          compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

        # Get the MAE between the approximated and full Shapley values
        MAE_dt[n_combination_idx, "KernelSHAP CEL-Kernel" := mean(abs(dt_true_mat - as.matrix(dt_KernelSHAP_cel_kernel[,-1])))]
      }
    }

    ## KernelSHAP_paired ------------------------------------------------------------------------------------------------------
    # Paired, average, kernel, c-kernel, cel-kernel
    if (verbose_now) message("KernelSHAP_paired")
    {

      # Figure out which list to look at
      dt_id = presampled_coalitions_KernelSHAP_paired$look_up$dt_n_comb_needed_sample[N_S == n_combination, dt_id]

      # Get the n_combinations coalitions to include
      to_this_index = presampled_coalitions_KernelSHAP_paired$samples[[dt_id]]$dt_N_S_and_L_small[N_S == n_combination, L]
      presampled_coalitions = copy(presampled_coalitions_KernelSHAP_paired$samples[[dt_id]]$all_coalitions_small[seq(to_this_index)])
      prefixed_coalitions = copy(presampled_coalitions_KernelSHAP_paired$samples[[dt_id]]$dt_res)

      # Get the X data.table
      X_now = create_X_dt_KernelSHAP(m = m,
                                     presampled_coalitions = presampled_coalitions,
                                     prefixed_coalitions = copy(prefixed_coalitions),
                                     dt_all_coalitions = dt_all_coalitions,
                                     version_scaled = TRUE)

      # # Get the X data.table
      # X_now_int = create_X_dt_KernelSHAP(m = m,
      #                                presampled_coalitions = presampled_coalitions,
      #                                prefixed_coalitions = copy(prefixed_coalitions),
      #                                dt_all_coalitions = dt_all_coalitions,
      #                                version_scaled = FALSE)

      # "KernelSHAP" = NaN,
      # "KernelSHAP Average" = NaN,
      # "KernelSHAP C-Kernel" = NaN,
      # "KernelSHAP CEL-Kernel" = NaN

      # Default version
      {
        # Compute the approximated Shapley values
        dt_KernelSHAP =
          compute_SV_values(X_now = X_now, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

        # Get the MAE between the approximated and full Shapley values
        MAE_dt[n_combination_idx, "Paired KernelSHAP" := mean(abs(dt_true_mat - as.matrix(dt_KernelSHAP[,-1])))]
      }

      # Average
      {
        X_now_copy = copy(X_now)
        #plot(X_now_copy[-c(1, .N), id_combination_full], X_now_copy[-c(1, .N), shapley_weight])
        X_now_copy[, shapley_weight := as.numeric(shapley_weight)]

        # Average the weights on the coalition sizes
        shapley_reweighting(X = X_now_copy, reweight = "on_N")
        #plot(X_now_copy[-c(1, .N), id_combination_full], X_now_copy[-c(1, .N), shapley_weight])

        # Compute the approximated Shapley values
        dt_kernelSHAP_average =
          compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

        # Get the MAE between the approximated and full Shapley values
        MAE_dt[n_combination_idx, "Paired KernelSHAP Average" := mean(abs(dt_true_mat - as.matrix(dt_kernelSHAP_average[,-1])))]
      }

      # # C-kernel
      # {
      #   X_now_copy = copy(X_now)
      #
      #   # Insert the corrected Shapley kernel weights
      #   shapley_reweighting(X = X_now_copy, reweight = "on_all_cond_paired")
      #
      #   # Compute the approximated Shapley values
      #   dt_kshap_paired_c_kernel =
      #     compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)
      #
      #   # Get the MAE between the approximated and full Shapley values
      #   MAE_dt[n_combination_idx, "Paired C-Kernel" := mean(abs(dt_true_mat - as.matrix(dt_kshap_paired_c_kernel[,-1])))]
      # }

      # CEL-kernel
      {
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
        dt_KernelSHAP_cel_kernel =
          compute_SV_values(X_now = X_now_copy, dt_all_coalitions = dt_all_coalitions, dt_vS = dt_vS, shap_names = shap_names)

        # Get the MAE between the approximated and full Shapley values
        MAE_dt[n_combination_idx, "Paired KernelSHAP CEL-Kernel" := mean(abs(dt_true_mat - as.matrix(dt_KernelSHAP_cel_kernel[,-1])))]
      }
    }


    # Remove print
    if (verbose_now) print(MAE_dt[n_combination_idx,])


    # Save the results
    if (n_combination_idx %% floor((length(n_combinations_array)/10)) == 0) {
      if (only_KernelSHAP) {
        saveRDS(MAE_dt, file.path(folder_save_KernelSHAP_paired, paste0(file_name, "_MAE_repetition_", repetition, "_KernelSHAP_tmp.rds")))
      } else {
        saveRDS(MAE_dt, file.path(folder_save_KernelSHAP_paired, paste0(file_name, "_MAE_repetition_", repetition, "_tmp.rds")))
      }
    }
  } # End n_combinations
  print(MAE_dt)

  # Save ------------------------------------------------------------------------------------------------------------
  # Melt the data.table
  MAE_dt_long = melt(MAE_dt, id.vars = c("Repetition", "N_S"), variable.name = "Strategy", value.name = "MAE")
  # library(ggplot2)
  # ggplot(MAE_dt_long, aes(x = N_S, y = MAE, col = Strategy)) +
  #   geom_line(linewidth = 0.65) +
  #   scale_y_log10(
  #     breaks = scales::trans_breaks("log10", function(x) 10^x),
  #     labels = scales::trans_format("log10", scales::math_format(10^.x))
  #   )

  # Save the results and remove tmp file
  if (only_KernelSHAP) {
    saveRDS(MAE_dt_long, file.path(folder_save_KernelSHAP_paired, paste0(file_name, "_MAE_repetition_", repetition, "_KernelSHAP.rds")))
    if (file.exists(file.path(folder_save_KernelSHAP_paired, paste0(file_name, "_MAE_repetition_", repetition, "_KernelSHAP_tmp.rds")))) {
      file.remove(file.path(folder_save_KernelSHAP_paired, paste0(file_name, "_MAE_repetition_", repetition, "_KernelSHAP_tmp.rds")))
    }

  } else {
    saveRDS(MAE_dt_long, file.path(folder_save_KernelSHAP_paired, paste0(file_name, "_MAE_repetition_", repetition, ".rds")))
    if (file.exists(file.path(folder_save_KernelSHAP_paired, paste0(file_name, "_MAE_repetition_", repetition, "_tmp.rds")))) {
      file.remove(file.path(folder_save_KernelSHAP_paired, paste0(file_name, "_MAE_repetition_", repetition, "_tmp.rds")))
    }
  }

} # End repetition





if (FALSE) {
  # Plots -----------------------------------------------------------------------------------------------------------
  library(ggplot2)
  library(ggpubr)
  library(data.table)
  folder_save = "/Users/larsolsen/PhD/Paper3/Paper3_save_location"
  M = m = 11

  ## Histogram -------------------------------------------------------------------------------------------------------

  true_explanations = readRDS(file.path(folder_save, paste0("Wine_data_sep_rf", ".rds")))
  Wine_fig_hist = ggplot(data = data.table(pred = true_explanations$pred_explain), aes(x = pred)) +
    geom_histogram(color="black", fill = "grey") +
    geom_vline(aes(xintercept = true_explanations$shapley_values$none[1]),
               color = "black", linewidth = 1, linetype = "dashed") +
    annotate('text', x = 6.5, y = 12.72, #x = 6.15, y = 12.72,
             label = "phi[0]==5.65",
             parse = TRUE,
             size = 8,
             color = "black") +
    labs(color = "Strategy:", fill = "Strategy:", x = expression(f(bold(x)*"*")), y = "Count") +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.7)),
          legend.text = element_text(size = rel(1.7)),
          axis.title = element_text(size = rel(1.7)),
          axis.text = element_text(size = rel(1.5)))
    Wine_fig_hist




  ## MAE -------------------------------------------------------------------------------------------------------------
  n_repetitions = 500
  M = m = 11

  # List the strategies to create the MAE plot for
  strat_MAE = c("Unique",
                "Paired",
                "Paired Average",
                "Paired Kernel",
                "Paired C-Kernel",
                "Paired CEL-Kernel",
                #"Paired CEPS-Kernel",
                "Paired Imp C-Kernel",
                "Paired Imp CEL-Kernel",
                #"Paired Imp CEPS-Kernel",
                "KernelSHAP",
                "KernelSHAP Average",
                "KernelSHAP CEL-Kernel"
  )

  strat_MAE_final = c("Unique",
                      "Paired",
                      "Paired Average",
                      "Paired Kernel",
                      "Paired C-Kernel",
                      "Paired CEL-Kernel",
                      #"Paired CEPS-Kernel",
                      "Paired Imp C-Kernel",
                      "Paired Imp CEL-Kernel"
                      #"Paired Imp CEPS-Kernel",
  )

  # Strategies in the relative difference plot
  strat_rel_diff = c("Paired Average", "Paired C-Kernel", "Paired CEL-Kernel", "Paired CEPS-Kernel")
  strat_rel_diff = c("Paired Average", "Paired C-Kernel", "Paired CEL-Kernel")
  strat_rel_diff_reference = c("Paired C-Kernel")

  # Compute the vertical dashed lines
  n_features <- seq(ceiling((m - 1)/2))
  n <- sapply(n_features, choose, n = m)
  n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
  n_cumsum = (cumsum(n) + 2) + 0.5

  # Load the MAE data
  res =
    data.table::rbindlist(
      lapply(seq(n_repetitions), function(repetition) {
        message(repetition)
        file_name_org = paste0("Wine_data_set_M_", M)
        file_name_1 = file.path(folder_save, "Wine_MAE", paste0(file_name_org, "_MAE_repetition_", repetition, ".rds"))
        file_name_2 = file.path(folder_save, "Wine_MAE", paste0(file_name_org, "_MAE_repetition_", repetition, "_KernelSHAP.rds"))
        if (!file.exists(file_name_1)) return(NULL)
        if (!file.exists(file_name_2)) return(NULL)
        # print(file_name)
        return(rbind(readRDS(file_name_1), readRDS(file_name_2)))
      }))

  # Ensure the same number of repetitions
  n_repetitions = res[, max(Repetition)]
  res = res[Repetition <= n_repetitions]

  # Values to skip
  N_S_skip = c(16,18, 20, 22, 24, 26, 36, 2046)
  res = res[!N_S %in% N_S_skip]

  # Compute the mean, lower, and upper MAE
  res_MAE = res[Strategy %in% strat_MAE, .(MAE_mean = mean(MAE),
                                           MAE_lower = quantile(MAE, 0.025),
                                           MAE_upper = quantile(MAE, 0.975)),
                by = c("N_S", "Strategy")]

  Wine_fig_MAE =
    ggplot(res_MAE[Strategy %in% strat_MAE_final], aes(x = N_S, y = MAE_mean, col = Strategy, fill = Strategy)) +
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
         y = bquote(bar(MAE)[500]*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.9)),
          legend.text = element_text(size = rel(1.9)),
          axis.title = element_text(size = rel(1.7)),
          axis.text = element_text(size = rel(1.5))) +
    # scale_color_manual(values = colors) +
    # scale_fill_manual(values = colors) +
    coord_cartesian(ylim = c(10^(-4.1), 10^(-0.7)))

  Wine_fig_MAE
  Wine_fig_hist


  ## KernelSHAP ------------------------------------------------------------------------------------------------------
  strat_MAE_small = c(
    "Unique",
    "Paired",
    # "Paired Average",
    "Paired Kernel",
    "Paired C-Kernel",
    "KernelSHAP",
    "KernelSHAP Average",
    "KernelSHAP CEL-Kernel",
    "Paired Imp CEL-Kernel"
  )

  res_MAE_now = res_MAE[!(N_S %in% c(2038, 2040, 2042, 2044) & Strategy == "KernelSHAP CEL-Kernel")]
  res_MAE_now = res_MAE_now[!(N_S %in% c(2042, 2044) & Strategy == "KernelSHAP Average")]
  Wine_fig_MAE_KernelSHAP =
    ggplot(res_MAE_now[Strategy %in% strat_MAE_small,],
           aes(x = N_S, y = MAE_mean, col = Strategy, fill = Strategy)) +
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
         y = bquote(bar(MAE)[500]*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.9)),
          legend.text = element_text(size = rel(1.9)),
          axis.title = element_text(size = rel(1.7)),
          axis.text = element_text(size = rel(1.5))) +
    # scale_color_manual(values = colors) +
    # scale_fill_manual(values = colors) +
    coord_cartesian(ylim = c(10^(-4.1), 10^(-0.7)))

  ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Wine_fig_MAE_KernelSHAP_ribbon.png"),
         plot = Wine_fig_MAE_KernelSHAP,
         width = 14.5,
         height = 8,
         scale = 1,
         dpi = 350)



  ## Hist + MAE ------------------------------------------------------------------------------------------------------
  Wine_fig_MAE_hist = ggpubr::ggarrange(Wine_fig_MAE,  Wine_fig_hist,
                                        labels = c("A", "B"),
                                        nrow = 1,
                                        ncol = 2,
                                        widths = c(3, 1),
                                        common.legend = TRUE,
                                        legend = "bottom",
                                        font.label = list(size = 25, color = "black"))
  Wine_fig_MAE_hist
  ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Wine_fig_MAE_hist_V_FINAL.png"),
         plot = Wine_fig_MAE_hist,
         width = 14.2,
         height = 8,
         scale = 1,
         dpi = 350)





  ## Relative diff ---------------------------------------------------------------------------------------------------
  # Compute the relative error for each n_combination and repetition, and then compute mean, lower and upper values
  res_rel_diff = copy(res[Strategy %in% strat_rel_diff])
  res_rel_diff[, rel_error := (MAE - MAE[Strategy == strat_rel_diff_reference]) / MAE[Strategy == strat_rel_diff_reference], by = list(N_S, Repetition)]
  res_rel_diff_errors = res_rel_diff[, .(rel_error_mean = mean(rel_error),
                                         rel_error_lower = quantile(rel_error, 0.025),
                                         rel_error_upper = quantile(rel_error, 0.975)),
                                     by = c("N_S", "Strategy")]

  # ggplot(res_rel_diff_errors[N_S <= 2040],
  #        aes(x = N_S, y = rel_error_mean, color = Strategy, fill = Strategy)) +
  #   geom_ribbon(aes(ymin = rel_error_lower, ymax = rel_error_upper), alpha = 0.4, linewidth = 0.1) +
  #   geom_line(linewidth = 0.65) +
  #   coord_cartesian(ylim = c(-0.1, 0.2))


  # Compute the bootstrap for relative difference for the MAE_mean
  boot_repetitions = 250

  res_rel_diff_boot = data.table::rbindlist(
    lapply(seq(boot_repetitions),
           function(b, res_rel_diff, strat_rel_diff_reference) {
             message(b)

             # Set seed for reproducibility
             set.seed(b)

             # Get the bootstrap data table
             res_rel_diff_boot_now = res_rel_diff[, .SD[sample(n_repetitions, replace = TRUE)], by = .(N_S, Strategy)]

             # Compute the average MAE
             res_rel_diff_boot_now = res_rel_diff_boot_now[, .(MAE_mean = mean(MAE)), by = .(N_S, Strategy)]

             # Compute the relative difference
             res_rel_diff_boot_now[, rel_error := (MAE_mean - MAE_mean[Strategy == strat_rel_diff_reference]) / MAE_mean[Strategy == strat_rel_diff_reference], by = N_S]
           },
           res_rel_diff = res_rel_diff, strat_rel_diff_reference = strat_rel_diff_reference),
    use.names = TRUE, idcol = "id_boot")

  # Compute the relative errors for the
  res_rel_diff_boot_errors = res_rel_diff_boot[, .(rel_error_mean = mean(rel_error),
                                                   rel_error_lower = quantile(rel_error, 0.025),
                                                   rel_error_upper = quantile(rel_error, 0.975)),
                                               by = c("N_S", "Strategy")]

  # Plot the results
  Wine_fig_relative = ggplot(res_rel_diff_boot_errors[Strategy %in% strat_rel_diff], aes(x = N_S, y = rel_error_mean, color = Strategy, fill = Strategy)) +
    coord_cartesian(ylim = c(-0.125, 0.25)) +
    geom_ribbon(data = res_rel_diff_errors[Strategy %in% strat_rel_diff], aes(ymin = rel_error_lower, ymax = rel_error_upper), alpha = 0.15, linewidth = 0.4, linetype = "dashed") +
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

  Wine_fig_relative
  ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Wine_fig_relative_V_FINAL.png"),
         plot = Wine_fig_relative,
         width = 14.2,
         height = 7,
         scale = 1,
         dpi = 350)





  ## Shapley values --------------------------------------------------------------------------------------------------
  # Load the exact and approximated Shapley values
  folder_save = "/Users/larsolsen/PhD/Paper3/Paper3_save_location"
  true_explanations = readRDS(file.path(folder_save, paste0("Wine_data_sep_rf", ".rds")))
  approximated_explanations = readRDS(file.path(folder_save, paste0("NEW_Wine_data_res_on_all_cond_paired_unique_paired_non_analytical", ".rds")))

  # Get the six explicands with evenly spread out predictions
  n_explicands = 6
  index_explicands = order(true_explanations$pred_explain)[seq(1, true_explanations$internal$parameters$n_explain, length.out = n_explicands)]
  index_explicands = order(true_explanations$pred_explain)[seq(4, true_explanations$internal$parameters$n_explain, length.out = n_explicands)]
  index_explicands[4] = 26
  true_pred = true_explanations$pred_explain[index_explicands]
  names(true_pred) = paste0("p_hat1_", seq(n_explicands))
  true_sv = true_explanations$shapley_values[index_explicands]

  n_combinations_vec = c(20, 50, 100, 200, 500, 1000)

  #explanation_list = res$unique_paired_equal_weights
  approximated_explanations2 = lapply(approximated_explanations$res, function(x) x[[1]])
  approximated_explanations2 = approximated_explanations2[paste0("n_combinations_", n_combinations_vec)]
  names(approximated_explanations2) = paste0("n_combinations_", n_combinations_vec)
  explanation_list2 = c(approximated_explanations2, list(True = true_explanations))
  n_explicands = 6


  explanation_list3 = lapply(seq(length(explanation_list2)), function(i) {
    message(i)
    true_copy = copy(true_explanations)
    xx = copy(explanation_list2[[i]])
    true_copy$shapley_values = xx$shapley_values[index_explicands,]
    true_copy$pred_explain = true_pred
    true_copy$internal$data$x_explain = true_copy$internal$data$x_explain[index_explicands,]
    return(true_copy)
  })
  names(explanation_list3) = c(n_combinations_vec, 2^11)

  explanation_list3[[1]]$internal$paramters$is_groupwise = FALSE

  figure = plot_SV_several_approaches(explanation_list3, facet_scales = "free", index_explicands = c(1:6), digits = 2, )

  figure_now = figure + theme(legend.position = 'bottom') +
    guides(fill = guide_legend(title = expression(N[S]*":"), nrow = 1)) +
    labs(title = NULL)+
    theme(strip.text = element_text(size = rel(1.3)),
          legend.title = element_text(size = rel(1.75)),
          legend.text = element_text(size = rel(1.75)),
          axis.title = element_text(size = rel(1.6)),
          axis.text = element_text(size = rel(1.3)))


  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Wine_fig_shapley_values_v3.png",
         plot = figure_now,
         width = 14.2,
         height = 18.5,
         scale = 0.85,
         dpi = 350)

  ## E[L] vs E[pS] ---------------------------------------------------------------------------------------------------
  n_repetitions = 500
  M = m = 11

  ### MAE -----
  # List the strategies to create the MAE plot for
  # List the strategies to create the MAE plot for
  strat_MAE = c(#"Unique",
    # "Paired",
    # "Paired Average",
    # "Paired Kernel",
    "Paired C-Kernel",
    "Paired CEL-Kernel",
    "Paired CEPS-Kernel",
    # "Paired Imp C-Kernel",
    "Paired Imp CEL-Kernel",
    "Paired Imp CEPS-Kernel"
  )

  # Strategies in the relative difference plot
  strat_rel_diff = c("Paired Average", "Paired C-Kernel", "Paired CEL-Kernel", "Paired CEPS-Kernel")
  strat_rel_diff = c("Paired Average", "Paired C-Kernel", "Paired CEL-Kernel")
  strat_rel_diff_reference = c("Paired C-Kernel")

  # Compute the vertical dashed lines
  n_features <- seq(ceiling((m - 1)/2))
  n <- sapply(n_features, choose, n = m)
  n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
  n_cumsum = (cumsum(n) + 2) + 0.5

  # Load the MAE data
  res =
    data.table::rbindlist(
      lapply(seq(n_repetitions), function(repetition) {
        message(repetition)
        file_name = paste0("Wine_data_set_M_", M)
        file_name = file.path(folder_save, "Wine_MAE", paste0(file_name, "_MAE_repetition_", repetition, ".rds"))
        if (!file.exists(file_name)) return(NULL)
        # print(file_name)
        readRDS(file_name)
      }))

  # Values to skip
  N_S_skip = c(16,18, 20, 22, 24, 26, 36, 2046)
  res = res[!N_S %in%  N_S_skip]

  # Compute the mean, lower, and upper MAE
  res_MAE = res[Strategy %in% strat_MAE, .(MAE_mean = mean(MAE),
                                           MAE_lower = quantile(MAE, 0.025),
                                           MAE_upper = quantile(MAE, 0.975)),
                by = c("N_S", "Strategy")]

  Wine_fig_MAE =
    ggplot(res_MAE, aes(x = N_S, y = MAE_mean, col = Strategy, fill = Strategy)) +
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
         y = bquote(bar(MAE)[500]*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.9)),
          legend.text = element_text(size = rel(1.9)),
          axis.title = element_text(size = rel(1.7)),
          axis.text = element_text(size = rel(1.5))) +
    # scale_color_manual(values = colors) +
    # scale_fill_manual(values = colors) +
    coord_cartesian(ylim = c(10^(-4.1), 10^(-0.7)))

  Wine_fig_MAE


  ### REL ----
  # Compute the relative error for each n_combination and repetition, and then compute mean, lower and upper values
  strat_rel_diff = c("Paired Imp CEL-Kernel", "Paired Imp CEPS-Kernel")
  strat_rel_diff_reference = c("Paired Imp CEL-Kernel")

  strat_rel_diff = c("Paired CEL-Kernel", "Paired CEPS-Kernel")
  strat_rel_diff_reference = c("Paired CEL-Kernel")

  res_rel_diff = copy(res[Strategy %in% strat_rel_diff])
  res_rel_diff[, rel_error := (MAE - MAE[Strategy == strat_rel_diff_reference]) / MAE[Strategy == strat_rel_diff_reference], by = list(N_S, Repetition)]
  res_rel_diff_errors = res_rel_diff[, .(rel_error_mean = mean(rel_error),
                                         rel_error_lower = quantile(rel_error, 0.025),
                                         rel_error_upper = quantile(rel_error, 0.975)),
                                     by = c("N_S", "Strategy")]

  ggplot(res_rel_diff_errors[N_S <= 2040],
         aes(x = N_S, y = rel_error_mean, color = Strategy, fill = Strategy)) +
    geom_ribbon(aes(ymin = rel_error_lower, ymax = rel_error_upper), alpha = 0.4, linewidth = 0.1) +
    geom_line(linewidth = 0.65) +
    coord_cartesian(ylim = c(-0.0075, 0.0075))

  ggplot(res_rel_diff_errors[N_S <= 2040],
         aes(x = N_S, y = rel_error_mean, color = Strategy, fill = Strategy)) +
    geom_ribbon(aes(ymin = rel_error_lower, ymax = rel_error_upper), alpha = 0.4, linewidth = 0.1) +
    geom_line(linewidth = 0.65) +
    coord_cartesian(ylim = c(-0.002, 0.02))


  # Compute the bootstrap for relative difference for the MAE_mean
  boot_repetitions = 100

  res_rel_diff_boot = data.table::rbindlist(
    lapply(seq(boot_repetitions),
           function(b, res_rel_diff, strat_rel_diff_reference) {
             message(b)

             # Set seed for reproducibility
             set.seed(b)

             # Get the bootstrap data table
             res_rel_diff_boot_now = res_rel_diff[, .SD[sample(n_repetitions, replace = TRUE)], by = .(N_S, Strategy)]

             # Compute the average MAE
             res_rel_diff_boot_now = res_rel_diff_boot_now[, .(MAE_mean = mean(MAE)), by = .(N_S, Strategy)]

             # Compute the relative difference
             res_rel_diff_boot_now[, rel_error := (MAE_mean - MAE_mean[Strategy == strat_rel_diff_reference]) / MAE_mean[Strategy == strat_rel_diff_reference], by = N_S]
           },
           res_rel_diff = res_rel_diff, strat_rel_diff_reference = strat_rel_diff_reference),
    use.names = TRUE, idcol = "id_boot")

  # Compute the relative errors for the
  res_rel_diff_boot_errors = res_rel_diff_boot[, .(rel_error_mean = mean(rel_error),
                                                   rel_error_lower = quantile(rel_error, 0.025),
                                                   rel_error_upper = quantile(rel_error, 0.975)),
                                               by = c("N_S", "Strategy")]

  # Plot the results
  Wine_fig_relative = ggplot(res_rel_diff_boot_errors[Strategy %in% strat_rel_diff], aes(x = N_S, y = rel_error_mean, color = Strategy, fill = Strategy)) +
    # coord_cartesian(ylim = c(-0.03, 0.03)) + # FOR REGULAR VERSION
    coord_cartesian(ylim = c(-0.015, 0.015)) + # FOR IMP VERSION
    geom_ribbon(data = res_rel_diff_errors[Strategy %in% strat_rel_diff], aes(ymin = rel_error_lower, ymax = rel_error_upper), alpha = 0.15, linewidth = 0.4, linetype = "dashed") +
    geom_ribbon(aes(ymin = rel_error_lower, ymax = rel_error_upper), alpha = 0.6, linewidth = 0.1) +
    geom_line(linewidth = 1.1) +
    #labs(y = latex2exp::TeX(r'($\frac{\bar{MAE}_{Strategy} - \bar{MAE}_{Paired~CEL-Kernel}}{\bar{MAE}_{Paired~CEL-Kernel}}$)')) +
    labs(y = latex2exp::TeX(r'($\frac{\bar{MAE}_{Strategy} - \bar{MAE}_{Paired~CEL-Kernel}}{\bar{MAE}_{Paired~Imp~CEL-Kernel}}$)')) +
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

  Wine_fig_relative
  ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Wine_fig_M_relative_EL_vs_EPS_Imp.png"),
         plot = Wine_fig_relative,
         width = 14.2,
         height = 7,
         scale = 1,
         dpi = 350)


  ## Other things ----------------------------------------------------------------------------------------------------







  hue_length = 8
  hue_indices = c(3,5,6)
  colors = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC")
  colors = c("#00BFC4", "#CD9600", "#7CAE00", "#00BE67", "#F8766D", "#00A9FF", "#C77CFF", "#FF61CC")
  colors = c("#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#F8766D", "#00A9FF", "#C77CFF", "#FF61CC")
  colors_rel_diff = colors[hue_indices]

  if (xor(is.null(hue_indices), is.null(hue_length))) stop("Both `hue_indices` and `hue_length` must be provided.")
  if (!is.null(hue_indices) && !is.null(hue_length)) {
    seq(5, 375, length = hue_length + 1)
    hues = seq(15, 375, length = hue_length + 1)
    colors = grDevices::hcl(h = hues, l = 65, c = 100)[1:hue_length][hue_indices]
  }

  colors


  scales::show_col(grDevices::hcl(h = seq(15, 375, length = hue_length + 1), l = 65, c = 100))






  res

  strat_ref = "Paired C-Kernel"
  # Compute the relative error for each n_combination and repetition
  res[, rel_error := (MAE - MAE[Strategy == strat_ref]) / MAE[Strategy == strat_ref], by = list(N_S, Repetition)]
  # Compute the average
  res2 = res[, .(rel_error_avg = mean(rel_error),
                 rel_error_lower = quantile(rel_error, 0.025),
                 rel_error_upper = quantile(rel_error, 0.975)), by = c("N_S", "Strategy")]



  res_small = res[Strategy %in% c("Paired Average", "Paired C-Kernel", "Paired CEL-Kernel")]



  res_small[,.N, by = .(Strategy, N_S)]



  res_small[Strategy == "Paired Average" & N_S == 100, MAE]
  res_small[Strategy == "Paired Average" & N_S == 100]
  bootstrap_summary(res_small[Strategy == "Paired Average" & N_S == 100])
  data_subset = res_small[Strategy == "Paired Average" & N_S == 100]

  bootstrap_fn(res_small[Strategy == "Paired Average" & N_S == 100], indices = NULL)

  library(boot)


res_small = res_small[!N_S %in% c(18, 22, 36),]


  B = 100
  jjj = data.table::rbindlist(lapply(seq(B), function(b) {
    message(b)

    # Set seed for reproducibility
    set.seed(b)

    # Get the bootstrap data table
    res_small2 = res_small[, .SD[sample(n_repetitions, replace = TRUE)], by = .(N_S, Strategy)]

    # Compute the average MAE
    res_small2 = res_small2[, .(MAE_avg = mean(MAE)), by = c("N_S", "Strategy")]

    # Compute the relative difference
    res_small2[, rel_error := (MAE_avg - MAE_avg[Strategy == strat_ref]) / MAE_avg[Strategy == strat_ref], by = N_S]
    }
  ), use.names = TRUE, idcol = "id_boot")

  jjj2 = jjj[, .(rel_error_avg = mean(rel_error),
                 rel_error_lower = quantile(rel_error, 0.025),
                 rel_error_upper = quantile(rel_error, 0.975)),
             by = c("N_S", "Strategy")]

  ggplot(jjj2[N_S <= 2044 & !N_S %in% c(16,18, 20, 22, 24, 26, 36) & Strategy %in% c("Paired Average", "Paired C-Kernel", "Paired CEL-Kernel"),],
         aes(x = N_S, y = rel_error_avg, color = Strategy, fill = Strategy)) +
    geom_ribbon(aes(ymin = rel_error_lower, ymax = rel_error_upper), alpha = 0.4, linewidth = 0.1) +
    geom_hline(yintercept = 0, col = "gray") +
    #geom_line(linewidth = 0.65) +
    geom_line(data = res2[N_S <= 2044 & !N_S %in% c(18, 22, 36) & Strategy %in% c("Paired Average", "Paired C-Kernel", "Paired CEL-Kernel"),],
              aes(x = N_S, y = rel_error)) +
    coord_cartesian(ylim = c(-0.1, 0.2))



    res2[, rel_error := (MAE - MAE[Strategy == strat_ref]) / MAE[Strategy == strat_ref], by = N_S]





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





  res3 = res2[Strategy %in% Strat_now & N_S <= 2045, ]
  fig_M_11_MAE =
    ggplot(res3, aes(x = N_S, y = MAE, col = Strategy, fill = Strategy)) +
    geom_vline(xintercept = n_cumsum, col = "gray50", linetype = "dashed", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, linewidth = 0.0) +
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
         y = bquote(bar(MAE)[500]*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
    theme(strip.text = element_text(size = rel(1.6)),
          legend.title = element_text(size = rel(1.7)),
          legend.text = element_text(size = rel(1.7)),
          axis.title.x = element_text(size = rel(1.6)),
          axis.title.y = element_text(size = rel(2)),
          axis.text.x = element_text(size = rel(1.75)),
          axis.text.y = element_text(size = rel(1.75))) +
    scale_color_hue() + #added as we want ordered
    scale_fill_hue()
  fig_M_11_MAE



  MAE_dt_long2 = copy(res3)
  data.table::setnames(MAE_dt_long2, old = c("Strategy", "MAE", "N_S"), new = c("sampling", "mean", "n_combinations"))
  fig_M_11_rel =
    relative_difference_wine_V2(dt = MAE_dt_long2,
                                m = 11,
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

  fig_M_11_rel_reg_scale =
    relative_difference_wine_V2(dt = MAE_dt_long2,
                        m = 11,
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
                        y_limits = c(-0.1, 0.25),
                        scale = FALSE,
                        hue_indices = c(3,5,6),
                        hue_length = 8,
                        y_breaks = c(-0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5),
                        skip_N_S = c(18, 22, 36),
    ) # + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  fig_M_11_rel_reg_scale

  library("ggpubr")
  fig_M_11_comb = ggarrange(fig_M_11_MAE,
                            fig_M_11_rel_reg_scale,
                            labels = c("A", "B"),
                            ncol = 1, nrow = 2,
                            heights = c(1.5, 1),
                            align = "hv",
                            common.legend = TRUE, legend = "bottom",
                            font.label = list(size = 25, color = "black"))
  fig_M_11_comb

  ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_11_MAE_Relative_Diff_V4.png"),
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
