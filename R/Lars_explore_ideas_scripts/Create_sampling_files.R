#' Title
#'
#' @param m Integer. Number of features.
#' @param n_sample_scale Integer. We sample n_sample_scale * n_combinations per iteration.
#' Too large value means that we will generate too many coalitions and then delete them at the end.
#' @param n_combinations Integer. The max number of combinations we want to consider. Up to 2^m - 2
#'
#' @return
#' @export
#'
#' @examples
coalition_sampling_paired = function(m, n_combinations = 2^m - 2,  n_sample_scale = 5, return_coalitions = FALSE,
                                     seed = NULL, verbose = TRUE) {
  if (n_combinations > 2^m - 2) stop("n_combinations is larger than 2^m.")
  if (!is.null(seed)) set.seed(seed)

  # Find weights for given number of features
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  # List to store all the sampled coalitions
  all_coalitions = c()

  # Variable to keep track of the number of unique coalitions
  unique_coalitions = 0

  # Variable to keep track of the iteration number
  iteration = 1

  # Loop until we have enough unique samples
  while (unique_coalitions < n_combinations) {

    # Sample the coalition sizes
    n_features_sample <- sample(
      x = n_features,
      size = n_sample_scale*n_combinations,
      replace = TRUE,
      prob = p
    )

    # Sample the coalitions
    feature_sample <- shapr:::sample_features_cpp(m, n_features_sample)

    # Get the paired coalitions
    feature_sample_paired <- lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)

    # Merge the coalitions in alternating fashion as we do paired sampling (i.e., first is S and second is Sbar and so on)
    coalitions = c(rbind(feature_sample, feature_sample_paired))

    # Convert the coalitions to strings such that we can compare them
    coalitions = sapply(coalitions, paste, collapse = ",")

    # Add the new coalitions to the previously sampled coalitions
    all_coalitions = c(all_coalitions, coalitions)

    # Get the cumulative number of unique coalitions for each coalition in all_coalitions
    dt_cumsum = data.table(coalitions = all_coalitions, N_S = cumsum(!duplicated(all_coalitions)))[, L := .I]

    # Extract rows where the N_S value increases (i.e., where we sample a new unique coalition)
    dt_N_S_and_L <- dt_cumsum[N_S != shift(N_S, type = "lag", fill = 0)]

    # Get the number of unique coalitions
    unique_coalitions = dt_N_S_and_L[.N, N_S]

    # Message to user
    if (verbose) {
      message(paste0("Iteration ", iteration, ": N_S = ", unique_coalitions,
                     ", Sampled = ", n_sample_scale*n_combinations*iteration, "."))
    }

    # Update the iteration number
    iteration = iteration + 1
  }

  # Post processing: keep only the coalitions until n_combinations
  all_coalitions = all_coalitions[seq(dt_N_S_and_L[N_S == n_combinations, L])]
  if (length(unique(all_coalitions)) != n_combinations) stop("Not the right number of unique coalitions")

  # Return
  if (return_coalitions) {
    return(list(dt_N_S_and_L = dt_N_S_and_L, all_coalitions = all_coalitions))
  } else {
    return(dt_N_S_and_L)
  }
}

coalition_sampling_unique = function(m, n_combinations = 2^m - 2,  n_sample_scale = 5, return_coalitions = FALSE,
                                     seed = NULL, verbose = TRUE) {
  if (n_combinations > 2^m - 2) stop("n_combinations is larger than 2^m.")
  if (!is.null(seed)) set.seed(seed)

  # Find weights for given number of features
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  # List to store all the sampled coalitions
  all_coalitions = c()

  # Variable to keep track of the number of unique coalitions
  unique_coalitions = 0

  # Variable to keep track of the iteration number
  iteration = 1

  # Loop until we have enough unique samples
  while (unique_coalitions < n_combinations) {

    # Sample the coalition sizes
    n_features_sample <- sample(
      x = n_features,
      size = n_sample_scale*n_combinations,
      replace = TRUE,
      prob = p
    )

    # Sample the coalitions
    coalitions <- shapr:::sample_features_cpp(m, n_features_sample)

    # Convert the coalitions to strings such that we can compare them
    coalitions = sapply(coalitions, paste, collapse = ",")

    # Add the new coalitions to the previously sampled coalitions
    all_coalitions = c(all_coalitions, coalitions)

    # Get the cumulative number of unique coalitions for each coalition in all_coalitions
    dt_cumsum = data.table(coalitions = all_coalitions, N_S = cumsum(!duplicated(all_coalitions)))[, L := .I]

    # Extract rows where the N_S value increases (i.e., where we sample a new unique coalition)
    dt_N_S_and_L <- dt_cumsum[N_S != shift(N_S, type = "lag", fill = 0)]

    # Get the number of unique coalitions
    unique_coalitions = dt_N_S_and_L[.N, N_S]

    # Message to user
    if (verbose) {
      message(paste0("Iteration ", iteration, ": N_S = ", unique_coalitions,
                     ", Sampled = ", n_sample_scale*n_combinations*iteration, "."))
    }

    # Update the iteration number
    iteration = iteration + 1
  }

  # Post processing: keep only the coalitions until n_combinations
  all_coalitions = all_coalitions[seq(dt_N_S_and_L[N_S == n_combinations, L])]
  if (length(unique(all_coalitions)) != n_combinations) stop("Not the right number of unique coalitions")

  # Return
  if (return_coalitions) {
    return(list(dt_N_S_and_L = dt_N_S_and_L, all_coalitions = all_coalitions))
  } else {
    return(dt_N_S_and_L)
  }
}



#cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts
#module load R/4.2.1-foss-2022a




# Code starts -----------------------------------------------------------------------------------------------------
library(data.table)
args = commandArgs(trailingOnly = TRUE)
version = as.character(args[1])
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
  repetitions = NULL
}

m = 20
n_combinations = 1048500
n_sample_scale = 30

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


repetition = 1
for (repetition in seq(repetitions)) {
  # Generate the coalitions
  if (version == "paired") {
    tmp = coalition_sampling_paired(m = m,
                                    n_combinations = n_combinations,
                                    n_sample_scale = n_sample_scale,
                                    return_coalitions = TRUE,
                                    seed = repetition + 1)
    version_name = "Paired_sampling"
  } else {
    tmp = coalition_sampling_unique(m = m,
                                    n_combinations = n_combinations,
                                    n_sample_scale = n_sample_scale,
                                    return_coalitions = TRUE,
                                    seed = repetition + 1)
    version_name = "Unique_sampling"
  }

  # Print the size
  print(object.size(tmp$all_coalitions), units = "MB")

  # Save the file
  saveRDS(tmp, file.path(folder_save, paste0(version_name, "_M_", M, "_repetition_", repetition, ".rds")))

  # Convert to integers
  tmp$all_coalitions = lapply(stringr::str_split(tmp$all_coalitions, ','), as.integer)

  # Print the size
  print(object.size(tmp$all_coalitions), units = "MB")

  # Save the file
  saveRDS(tmp, file.path(folder_save, paste0(version_name, "_M_", M, "_repetition_", repetition, "_integers.rds")))
}





Rscript Create_sampling_files.R paired
