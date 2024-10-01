coalition_sampling_largest_SHAP = function(m,
                                           n_combinations = 2^m - 2,
                                           n_sample_scale = 3,
                                           return_coalitions = TRUE,
                                           seed = NULL,
                                           verbose = TRUE,
                                           always_pair_coalitions = TRUE) {
  if (n_combinations > 2^m - 2) stop("n_combinations is larger than 2^m.")
  if (!is.null(seed)) set.seed(seed)

  # Number of features
  M = m

  # weight the different coalition sizes
  num_subset_sizes = as.integer(ceiling((M - 1) / 2))
  num_paired_subset_sizes = as.integer(floor((M - 1) / 2))
  weight_vector = sapply(seq(num_subset_sizes), function(i) (M - 1.0) / (i * (M - i)))
  weight_vector[seq(num_paired_subset_sizes)] = 2*weight_vector[seq(num_paired_subset_sizes)]
  weight_vector = weight_vector / sum(weight_vector)


  # Create a data table with max number of coalitions before we include the smaller coalition size. Ensure even numbers
  n_coal_each_size = choose(M, seq(num_subset_sizes))
  n_coal_each_size[seq(num_paired_subset_sizes)] = 2*n_coal_each_size[seq(num_paired_subset_sizes)]
  n_coal_each_size = cumsum(n_coal_each_size)
  n_coal_each_size
  n_comb_needed = sapply(n_coal_each_size, function(x) ifelse(x %% 2 == 0, x - 2, x - 1))
  n_comb_needed[n_comb_needed >= n_combinations] = n_combinations
  n_comb_needed[length(n_comb_needed)] = n_combinations
  dt_n_comb_needed = data.table(dt_id = seq_along(n_comb_needed), N_S = n_comb_needed)
  dt_n_comb_needed[, N_S_fixed := fifelse(dt_id == 1, 0, 2 * sapply(dt_id, function(id) sum(choose(M, seq_len(id - 1)))))]
  dt_n_comb_needed


  # Create a look up table.
  # The idea now is that if for each value of N_S, we can get which result list to look at by looking at
  # `dt_n_comb_needed_sample[N_S == 916, dt_id]`.
  dt_n_comb_needed_sample = data.table(N_S = seq(2, n_combinations, 2),
                                       dt_id = sapply(seq(2, n_combinations, 2), function(x) which.max(n_comb_needed >= x)))


  id_now_idx = 1
  id_max = length(dt_n_comb_needed$dt_id)
  full_res = lapply(seq_along(dt_n_comb_needed$dt_id), function(id_now_idx) {
    id_now = dt_n_comb_needed$dt_id[id_now_idx]

    # Get the number of unique coalitions to sample
    N_S_now = dt_n_comb_needed[dt_id == id_now, N_S]

    # data table to store the coalitions that are pre-defined to be included with the corresponding normalized Shapley kernel weights
    dt_res = NULL

    # For all id_now larger than 1, we include all coalitions of sizes less than `id_now`
    if (id_now > 1) {
      subset_size = 1
      for (subset_size in seq(id_now - 1)) {
        feature_sample = unlist(lapply(subset_size, utils::combn, x = M, simplify = FALSE), recursive = FALSE)
        w = weight_vector[subset_size] / choose(M, subset_size)
        if (subset_size <= num_paired_subset_sizes) {
          # Add paired sampled and half the weight
          feature_sample = c(rbind(feature_sample, lapply(feature_sample, function(x, M) {seq(M)[-x]}, M = M)))
          w = w / 2
        }
        dt_res_now = data.table(features = feature_sample, w = w)
        dt_res = rbind(dt_res, dt_res_now)
      }
    }

    dt_res

    # Then we need to sample the remaining features
    # add random samples from what is left of the subset space
    nfixed_samples = ifelse(is.null(dt_res), 0, nrow(dt_res))
    samples_left = N_S_now - nfixed_samples

    num_full_subsets = id_now - 1

    if (num_full_subsets != num_subset_sizes) {
      # Get the normalized weights for the remaining coalition sizes
      remaining_weight_vector = copy(weight_vector)
      if (always_pair_coalitions) {
        remaining_weight_vector = remaining_weight_vector / 2 # because we draw two samples each below
      } else {
        remaining_weight_vector[seq(num_paired_subset_sizes)] = remaining_weight_vector[seq(num_paired_subset_sizes)] / 2 # because we draw two samples each below
      }
      if (num_full_subsets > 0) remaining_weight_vector = remaining_weight_vector[-seq(num_full_subsets)] # Remove the fully sampled coalition size
      remaining_weight_vector = remaining_weight_vector / sum(remaining_weight_vector)


      # List to store all the sampled coalitions
      all_coalitions = c()

      # Variable to keep track of the number of unique coalitions
      unique_coalitions = nfixed_samples

      # Variable to keep track of the iteration number
      iteration = 1

      # Loop until we have enough unique samples
      while (unique_coalitions < N_S_now) {

        # Sample the coalition sizes
        message(paste0("(", id_now, "/", id_max, ") ", "Getting the coalition sizes"))
        n_features_sample <- sample(
          x = length(remaining_weight_vector),
          size = n_sample_scale * samples_left,
          replace = TRUE,
          prob = remaining_weight_vector
        ) + num_full_subsets # Add the num_full_subsets to get the correct coal sizes

        # Sample the coalitions
        message(paste0("(", id_now, "/", id_max, ") ", "Getting coalitions"))
        feature_sample <- shapr:::sample_features_cpp(m, n_features_sample)

        # Get the paired coalitions
        message(paste0("(", id_now, "/", id_max, ") ", "Making the paired"))
        if (always_pair_coalitions) {
          # Get the paired coalitions
          feature_sample_paired <- lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)

          # Merge the coalitions in alternating fashion as we do paired sampling (i.e., first is S and second is Sbar and so on)
          coalitions = c(rbind(feature_sample, feature_sample_paired))

        } else {
          # In python SHAP, they do not pair the coalition of M/2 for M even.
          # This is strange as we then no longer can garante that both S and Sbar are sampled
          coalitions <- unlist(lapply(feature_sample, function(x, m) {
            if (length(x) == M / 2) {
              return(list(x))
            } else {
              return(list(x, seq(m)[-x]))
            }
          }, m = m), recursive = FALSE)
        }

        # Add the fixed
        if (nfixed_samples > 0) coalitions = c(dt_res$features, coalitions)

        # Convert the coalitions to strings such that we can compare them
        message(paste0("(", id_now, "/", id_max, ") ", "Converting to strings"))
        coalitions = sapply(coalitions, paste, collapse = ",")

        # Add the new coalitions to the previously sampled coalitions
        all_coalitions = c(all_coalitions, coalitions)

        # Get the cumulative number of unique coalitions for each coalition in all_coalitions
        message(paste0("(", id_now, "/", id_max, ") ", "Getting cumsum"))
        dt_cumsum = data.table(coalitions = all_coalitions, N_S = cumsum(!duplicated(all_coalitions)))[, L := .I]
        dt_cumsum

        # Extract rows where the N_S value increases (i.e., where we sample a new unique coalition)
        message(paste0("(", id_now, "/", id_max, ") ", "Getting shift"))
        dt_N_S_and_L <- dt_cumsum[N_S != shift(N_S, type = "lag", fill = 0)]

        # Get the number of unique coalitions
        unique_coalitions = dt_N_S_and_L[.N, N_S]

        # if (unique_coalitions < N_S_now) stop("Not enough sampels")

        # Message to user
        if (verbose) {
          message(paste0("Iteration ", iteration, ": N_S_now = ", N_S_now,
                         ": N_S = ", unique_coalitions, ", Sampled = ", nrow(dt_cumsum), "."))
        }

        # Update the iteration number
        iteration = iteration + 1
      }

      # Stop at next limit
      # dt_N_S_and_L[N_S == N_S_now]
      dt_N_S_and_L_small = dt_N_S_and_L[N_S <= N_S_now]
      all_coalitions_small = all_coalitions[seq(dt_N_S_and_L_small[.N, L])]
    }

    # Return
    if (return_coalitions) {
      return(list(dt_res = dt_res, dt_N_S_and_L_small = dt_N_S_and_L_small, all_coalitions_small = all_coalitions_small))
    } else {
      return(dt_N_S_and_L)
    }
  })


  final_list = list(look_up = list(dt_n_comb_needed = dt_n_comb_needed, dt_n_comb_needed_sample = dt_n_comb_needed_sample),
                    samples = full_res)

  #print(object.size(final_list), units = "MB")
  return(final_list)
}

# Code starts -----------------------------------------------------------------------------------------------------
library(data.table)
library(shapr)

args = commandArgs(trailingOnly = TRUE)
m = as.integer(args[1])
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
# always_pair_coalitions = as.logical(args[3])
always_pair_coalitions = TRUE
version_name = "KernelSHAP_Important_sampling"

# m = 20
# n_combinations = 1048500
# n_sample_scale = 35

n_combinations = 2^m - 2
n_sample_scale = 3

if (m == 20) n_combinations = 1048500

# Get where we are working
hostname = R.utils::System$getHostname()
message(sprintf("We are working on '%s'.", R.utils::System$getHostname()))

# Check if we are working on an UiO computer or not and define the correct folder based on system
if (hostname == "Larss-MacBook-Pro.local" || Sys.info()[[7]] == "larsolsen") {
  folder = "/Users/larsolsen/PhD/Paper3/shapr"
  folder_save = "/Users/larsolsen/PhD/Paper3/Paper3_save_location"
  folder_save_paired = "/Users/larsolsen/PhD/Paper3/Paper3_save_location_KernelSHAP_paired_imp"
  UiO = FALSE
} else if (grepl("hpc.uio.no", hostname)) {
  # To be added
  folder = ""
  UiO = TRUE
} else if (grepl("uio.no", hostname)) {
  folder = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr"
  folder_save = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location_KernelSHAP_imp"
  folder_save_paired = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location_KernelSHAP_paired_imp"
  UiO = TRUE
} else {
  stop("We do not recongize the system at which the code is run (not Lars's MAC, HPC, nor UiO).")
}


repetition = 1
for (repetition in repetitions) {
  message(paste0("Working on version '", version_name, "' and repetition ", repetition, "."))

  # Generate the coalitions
  tmp = coalition_sampling_largest_SHAP(m = m,
                                        n_combinations = n_combinations,
                                        n_sample_scale = n_sample_scale,
                                        return_coalitions = TRUE,
                                        seed = repetition + 1,
                                        always_pair_coalitions = always_pair_coalitions)

  # Print the size
  print(object.size(tmp), units = "MB")

  # Save the file
  if (always_pair_coalitions) {
    saveRDS(tmp, file.path(folder_save_paired, paste0(version_name, "_paired_M_", m, "_repetition_", repetition, ".rds")))
  } else {
    saveRDS(tmp, file.path(folder_save, paste0(version_name, "_M_", m, "_repetition_", repetition, ".rds")))
  }


  # # Convert to integers
  # system.time({tmp$all_coalitions2 = lapply(stringr::str_split(tmp$all_coalitions, ','), as.integer)})
  # tmp$all_coalitions = lapply(stringr::str_split(tmp$all_coalitions, ','), as.integer)
  #
  # # Print the size
  # print(object.size(tmp$all_coalitions), units = "MB")
  #
  # # Save the file
  # saveRDS(tmp, file.path(folder_save, paste0(version_name, "_M_", M, "_repetition_", repetition, "_integers.rds")))
}


# cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts
# module load R/4.2.1-foss-2022a

# Rscript Create_sampling_files_kernelSHAP.R 20 1:50 FALSE

# Rscript Create_sampling_files_kernelSHAP.R 20 50:1 TRUE LABBU
# Rscript Create_sampling_files_kernelSHAP.R 20 50:1 FALSE diktys
# Rscript Create_sampling_files_kernelSHAP.R 20 51:100 TRUE Sraosha
# Rscript Create_sampling_files_kernelSHAP.R 20 51:100 FALSE Sumeru
# Rscript Create_sampling_files_kernelSHAP.R 20 100:51 FALSE carpo
# Rscript Create_sampling_files_kernelSHAP.R 20 100:51 TRUE aload
# Rscript Create_sampling_files_kernelSHAP.R 20 101:150 TRUE nyx
# Rscript Create_sampling_files_kernelSHAP.R 20 101:150 FALSE metis
# Rscript Create_sampling_files_kernelSHAP.R 20 150:101 TRUE adroa
# Rscript Create_sampling_files_kernelSHAP.R 20 150:101 FALSE adonis


# final_list$look_up$dt_n_comb_needed
# final_list$look_up$dt_n_comb_needed_sample
#
# final_list$samples[[1]]


# # String version
# # Insert all sampled coalitions into a data table and find their frequencies
# dt_freq = data.table::data.table(features = tmp)[, .(shapley_weight = .N), by = features]
#
# # normalize the kernel weights for the random samples to equal the weight left after
# # the fixed enumerated samples have been already counted
# weight_left = sum(weight_vector[-seq(num_full_subsets)])
# KernelWeights[-seq(nfixed_samples)] = KernelWeights[-seq(nfixed_samples)] * weight_left / sum(KernelWeights[-seq(nfixed_samples)])
#
#
# dt_freq[, shapley_weight2 := shapley_weight * weight_left / sum(shapley_weight)]
#
# setorderv(dt_freq, cols = "shapley_weight", order=-1L)
# dt_freq

