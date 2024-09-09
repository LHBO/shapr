coalition_sampling_largest_random = function(m,
                                             n_combinations = 2^m - 2,
                                             seed = NULL) {
  if (n_combinations > 2^m - 2) stop("n_combinations is larger than 2^m.")
  if (!is.null(seed)) set.seed(seed)

  # Add one if odd as we will do paired and then rather remove one coalition afterwards
  n_combinations_new = ifelse(n_combinations %% 2 == 1, n_combinations + 1, n_combinations)

  # Since we do paired version, we divide by two
  n_combinations_new = n_combinations_new / 2

  # Get all possible paired coalition sizes. Size 0 corresponds to empty/grand coalition.
  all_coal_sizes = seq(0, ceiling((m - 1)/2))
  all_paired_coal_sizes = seq(0, floor((m - 1)/2))
  all_unique_coal_size = setdiff(all_coal_sizes, all_paired_coal_sizes)

  # Get the number of paired coalitions of each size. Add 1 as we include coalition size 0.
  n_coal_sizes <- sapply(all_coal_sizes, choose, n = m)
  n_coal_sizes[all_unique_coal_size + 1] = n_coal_sizes[all_unique_coal_size + 1] / 2

  # Get the cumulative sum of the number of coalitions for each size
  coalition_sizes_cumsum = cumsum(n_coal_sizes)

  # Get the order to include the coalitions
  id_comb_include = c(1, unlist(lapply(seq(length(coalition_sizes_cumsum) - 1), function(i) sample(seq(coalition_sizes_cumsum[i] + 1, coalition_sizes_cumsum[i + 1])))))

  # Get the paired indices too
  id_comb_include = c(rbind(id_comb_include, 2^m + 1 - id_comb_include))

  # If odd, then remove the last sampled
  if (n_combinations %% 2 == 1) id_comb_include = id_comb_include[-length(id_comb_include)]

  return(id_comb_include)
}


args = commandArgs(trailingOnly = TRUE)
m = as.integer(args(1))
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
  folder_save = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location_2"
  UiO = TRUE
} else {
  stop("We do not recongize the system at which the code is run (not Lars's MAC, HPC, nor UiO).")
}


repetition = 1
for (repetition in repetitions) {
  message(paste0("Working on version 'largest random' and repetition ", repetition, "."))

  samples = coalition_sampling_largest_random(m = m, seed = repetition + 1)

  saveRDS(samples, file.path(folder_save, paste0("Largest_random_sampling_M_", m, "_repetition_", repetition, ".rds")))
}





