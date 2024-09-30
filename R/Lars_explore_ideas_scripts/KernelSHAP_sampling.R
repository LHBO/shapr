# How kernelSHAP samples

M = 10
# weight the different coalition sizes
num_subset_sizes = as.integer(ceiling((M - 1) / 2))
num_paired_subset_sizes = as.integer(floor((M - 1) / 2))
weight_vector = sapply(seq(num_subset_sizes), function(i) (M - 1.0) / (i * (M - i)))
weight_vector[seq(num_paired_subset_sizes)] = 2*weight_vector[seq(num_paired_subset_sizes)]
weight_vector = weight_vector / sum(weight_vector)
subset_size = 1
num_samples_left = n_combinations
remaining_weight_vector = copy(weight_vector)
remaining_weight_vector = copy(weight_vector)
all_nsubsets = rep(0, length(num_subset_sizes))
max_coalitions_to_exclude_all_coal_in_size = NULL
min_coalitions_to_include_all_coal_in_size = NULL
subset_size = 1
for (subset_size in seq(num_subset_sizes)) {
  nsubsets = choose(M, subset_size)
  if (subset_size <= num_paired_subset_sizes) nsubsets = 2 * nsubsets
  nsubsets
  all_nsubsets[subset_size] = nsubsets
  # rescale what's left of the remaining weight vector to sum to 1
  #print((num_samples_left * remaining_weight_vector[subset_size] / nsubsets) >= 1.0 - 1e-8)
  min_coalitions_to_include_all_coal_in_size = c(
    min_coalitions_to_include_all_coal_in_size,
    ceiling((1.0 - 1e-8) * nsubsets / remaining_weight_vector[subset_size]))
  max_coalitions_to_exclude_all_coal_in_size = c(
    max_coalitions_to_exclude_all_coal_in_size,
    floor((1.0 - 1e-8) * nsubsets / remaining_weight_vector[subset_size]))
  print(ceiling((1.0 - 1e-8) * nsubsets / remaining_weight_vector[subset_size]))

  if (remaining_weight_vector[subset_size] < 1.0) {
    remaining_weight_vector = remaining_weight_vector / (1 - remaining_weight_vector[subset_size])
  }

  #print((num_samples_left * remaining_weight_vector[subset_size] / ) >= (1 - 1e-8) * nsubsets / remaining_weight_vector[subset_size])
}
subset_size = 1
for (subset_size in seq(num_subset_sizes)) {
  nsubsets = choose(M, subset_size)
  if (subset_size <= num_paired_subset_sizes) nsubsets = 2 * nsubsets
  nsubsets

  tmp = ceiling(nsubsets / remaining_weight_vector[subset_size])

  if (subset_size > 1) {
    tmp = tmp + 2 * sum(choose(M, seq(subset_size - 1)))
  }

  min_coalitions_to_include_all_coal_in_size = c(
    min_coalitions_to_include_all_coal_in_size,
    tmp)


  if (remaining_weight_vector[subset_size] < 1.0) {
    remaining_weight_vector = remaining_weight_vector / (1 - remaining_weight_vector[subset_size])
  }

  #print((num_samples_left * remaining_weight_vector[subset_size] / ) >= (1 - 1e-8) * nsubsets / remaining_weight_vector[subset_size])
}
min_coalitions_to_include_all_coal_in_size


final_limits = c(min_coalitions_to_include_all_coal_in_size [cumsum(min_coalitions_to_include_all_coal_in_size ) < 2^M], 2^M)


min_coalitions_to_include_all_coal_in_size
min_coalitions_to_include_all_coal_in_size = cumsum(min_coalitions_to_include_all_coal_in_size)
min_coalitions_to_include_all_coal_in_size
max_coalitions_to_exclude_all_coal_in_size = cumsum(max_coalitions_to_exclude_all_coal_in_size)
all_nsubsets
cumsum(all_nsubsets)

min_coalitions_to_include_all_coal_in_size



jj = data.table(n_combinations = seq(2, 2^M-2, 2), dt_id = sapply(seq(2, 2^M-2, 2), function(x) which.max(final_limits > x)))
jjj = jj[, .SD[.N], by = dt_id]



jjj

M = 10
n_combinations = 268 # 660
{


  # weight the different coalition sizes
  num_subset_sizes = as.integer(ceiling((M - 1) / 2))
  num_paired_subset_sizes = as.integer(floor((M - 1) / 2))
  weight_vector = sapply(seq(num_subset_sizes), function(i) (M - 1.0) / (i * (M - i)))
  weight_vector[seq(num_paired_subset_sizes)] = 2*weight_vector[seq(num_paired_subset_sizes)]
  weight_vector = weight_vector / sum(weight_vector)


  # fill out all the subset sizes we can completely enumerate
  # given nsamples*remaining_weight_vector[coalition_size]
  num_full_subsets = 0
  num_samples_left = n_combinations
  group_inds = seq(M)
  mask = rep(0, M)
  remaining_weight_vector = copy(weight_vector)

  coalition_idx = 1
  masks = matrix(0, ncol = M, nrow = n_combinations)
  KernelWeights = rep(0, n_combinations)
  #masks


  library(data.table)
  dt_res = NULL

  subset_size = 1
  for (subset_size in seq(num_subset_sizes)) {
    # determine how many subsets (and their complements) are of the current size
    nsubsets = choose(M, subset_size)
    if (subset_size <= num_paired_subset_sizes) nsubsets = 2 * nsubsets

    # see if we have enough samples to enumerate all subsets of this size
    # Based on the expected sampling. I.e., if a subset size contains 10 subsets.
    # but the prop of sampling this size is 0.5, then we need it to be at least 20 samples left.
    if ((num_samples_left * remaining_weight_vector[subset_size] / nsubsets) >= 1.0 - 1e-8) {

      num_full_subsets = num_full_subsets + 1
      num_samples_left = num_samples_left - nsubsets # Subtract total number of coalitions of size subset_size

      # rescale what's left of the remaining weight vector to sum to 1
      if (remaining_weight_vector[subset_size] < 1.0) {
        remaining_weight_vector = remaining_weight_vector / (1 - remaining_weight_vector[subset_size])
      }

      # add all the samples of the current subset size
      w = weight_vector[subset_size] / choose(M, subset_size)

      # Half if paired coalition
      if (subset_size <= num_paired_subset_sizes) w = w / 2

      # Generate all combinations taken subset_size at a time
      combinations <- combn(seq(M), subset_size)


      feature_sample = unlist(lapply(subset_size, utils::combn, x = M, simplify = FALSE), recursive = FALSE)
      if (subset_size <= num_paired_subset_sizes) feature_sample = c(rbind(feature_sample, lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)))
      dt_res_now = data.table(features = feature_sample)[, w := w]
      dt_res = rbind(dt_res, dt_res_now)

      inds_idx = 1
      for (inds_idx in seq(ncol(combinations))) {
        inds = combinations[, inds_idx]
        masks[coalition_idx, inds] = 1
        KernelWeights[coalition_idx] = w

        # Update the index
        coalition_idx = coalition_idx + 1

        # Check if adding paired version
        if (subset_size <= num_paired_subset_sizes) {
          masks[coalition_idx, ] = 1 - masks[coalition_idx - 1, ]
          KernelWeights[coalition_idx] = w

          # Update the index
          coalition_idx = coalition_idx + 1
        }
      }
    } else {
      print(subset_size)
      break
    }
  }
}
KernelWeights

masks
num_full_subsets

masks_dt = data.table(masks = apply(masks[seq(nfixed_samples),], 1, paste, collapse = ","))




nfixed_samples = nrow(dt_res)



# add random samples from what is left of the subset space
nfixed_samples = coalition_idx - 1
samples_left = n_combinations - nfixed_samples

if (num_full_subsets != num_subset_sizes) {
  remaining_weight_vector = copy(weight_vector)
  remaining_weight_vector[seq(num_paired_subset_sizes)] = remaining_weight_vector[seq(num_paired_subset_sizes)] / 2 # because we draw two samples each below
  remaining_weight_vector = remaining_weight_vector[-seq(num_full_subsets)] # Remove the fully sampled coalition size
  remaining_weight_vector = remaining_weight_vector / sum(remaining_weight_vector)

  # Get the sampled coalition sizes
  ind_set = sample(length(remaining_weight_vector),
                   size = 100 * samples_left,
                   prob = remaining_weight_vector,
                   replace = TRUE) # TODO this 10 is important as we stop when we run our of samples
  ind_set_pos = 1
  used_masks = data.table(id = integer(), mask = character())


  {
    #New version

    # Sample the coalition sizes
    message("Getting the coalition sizes")
    n_features_sample <- sample(
      x = length(remaining_weight_vector),
      size = 100 * samples_left,
      replace = TRUE,
      prob = remaining_weight_vector
    ) + num_full_subsets # Add the num_full_subsets to get the correct coal sizes

    # Sample the coalitions
    message("Getting coalitions")
    feature_sample <- shapr:::sample_features_cpp(m, n_features_sample)

    # Get the paired coalitions
    message("Making the paired")
    feature_sample_paired <- lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)

    # Merge the coalitions in alternating fashion as we do paired sampling (i.e., first is S and second is Sbar and so on)
    coalitions = c(rbind(feature_sample, feature_sample_paired))

    # Add the fixed
    coalitions = c(dt_res$features, coalitions)

    # Convert the coalitions to strings such that we can compare them
    message("Converting to strings")
    coalitions = sapply(coalitions, paste, collapse = ",")

    # Add the new coalitions to the previously sampled coalitions
    all_coalitions = NULL
    all_coalitions = c(all_coalitions, coalitions)

    # Get the cumulative number of unique coalitions for each coalition in all_coalitions
    message("Getting cumsum")
    dt_cumsum = data.table(coalitions = all_coalitions, N_S = cumsum(!duplicated(all_coalitions)))[, L := .I]
    dt_cumsum

    # Extract rows where the N_S value increases (i.e., where we sample a new unique coalition)
    message("Getting shift")
    dt_N_S_and_L <- dt_cumsum[N_S != shift(N_S, type = "lag", fill = 0)]

    # Get the number of unique coalitions
    unique_coalitions = dt_N_S_and_L[.N, N_S]

    # Stop at next limit
    dt_N_S_and_L[N_S == n_combinations]
    dt_N_S_and_L_small = dt_N_S_and_L[N_S <= n_combinations]


    all_coalitions[seq(dt_N_S_and_L_small[.N, L])]


    for (ind_now in ind_set) {
      subset_size = ind_now + num_full_subsets
      mask_now = rep(0, M)
      mask_now[sample(M, size = subset_size)] = 1
      mask_now_str = paste(mask_now, collapse = ",")



    }


  }


  while (samples_left > 0 && ind_set_pos <= length(ind_set)) {
    ind = ind_set[ind_set_pos]  # we call sample once to save time and then just read it here
    ind_set_pos = ind_set_pos + 1
    subset_size = ind + num_full_subsets

    mask_now = rep(0, M)
    mask_now[sample(M, size = subset_size)] = 1
    mask_now_str = paste(mask_now, collapse = ",")

    # Id update. If numeric, then it is a new sample
    id_update = used_masks[mask == mask_now_str, id]

    if (length(id_update) == 0) { # !mask_now_str %in% used_masks$mask (Now i only need to check for unique caol once)
      new_sample = TRUE
      used_masks = rbind(used_masks, data.table(id = coalition_idx, mask = mask_now_str))
      samples_left = samples_left - 1

      # Update masks and weights
      masks[coalition_idx, ] = mask_now
      KernelWeights[coalition_idx] = 1
      coalition_idx = coalition_idx + 1
    } else {
      new_sample = FALSE

      # Update sampling frequency
      KernelWeights[id_update] = KernelWeights[id_update] + 1
    }

    # add the compliment sample
    if (samples_left > 0 && subset_size <= num_paired_subset_sizes) {
      mask_now = 1 - mask_now
      mask_now_str = paste(mask_now, collapse = ",")

      # only add the sample if we have not seen it before, otherwise just
      # increment a previous sample's weight
      if (new_sample) {
        used_masks = rbind(used_masks, data.table(id = coalition_idx, mask = mask_now_str))
        samples_left = samples_left - 1

        # Update masks and weights
        masks[coalition_idx, ] = mask_now
        KernelWeights[coalition_idx] = 1
        coalition_idx = coalition_idx + 1

      } else {
        # we know the compliment sample is the next one after the original sample, so + 1
        KernelWeights[id_update + 1] = KernelWeights[id_update + 1] + 1
      }
    }
  }

  # normalize the kernel weights for the random samples to equal the weight left after
  # the fixed enumerated samples have been already counted
  weight_left = sum(weight_vector[-seq(num_full_subsets)])
  KernelWeights[-seq(nfixed_samples)] = KernelWeights[-seq(nfixed_samples)] * weight_left / sum(KernelWeights[-seq(nfixed_samples)])
}

masks
KernelWeights
num_full_subsets

apply(masks, 1, sum)





# Final version ----------
library(data.table)
library(shapr)

verbose = TRUE

# Number of features
M = m = 20

# weight the different coalition sizes
num_subset_sizes = as.integer(ceiling((M - 1) / 2))
num_paired_subset_sizes = as.integer(floor((M - 1) / 2))
weight_vector = sapply(seq(num_subset_sizes), function(i) (M - 1.0) / (i * (M - i)))
weight_vector[seq(num_paired_subset_sizes)] = 2*weight_vector[seq(num_paired_subset_sizes)]
weight_vector = weight_vector / sum(weight_vector)

# Variable that will store the normalized probability of sampling the remaining colaition sizes
remaining_weight_vector = copy(weight_vector)

# Array to store the number of combinations needed to include the different coalition sizes
n_comb_needed = NULL

# Find the number of combinations needed to include the different coalition sizes
subset_size = 1
for (subset_size in seq(num_subset_sizes)) {

  # Get the number of (paired) coalitions of this subset size
  nsubsets = choose(M, subset_size)
  if (subset_size <= num_paired_subset_sizes) nsubsets = 2 * nsubsets

  # Get the expected number of samples needed to sample nsubsets coalitions of size
  # `subset_size` using the normalized sampling probability
  n_comb_needed_now = ceiling(nsubsets / remaining_weight_vector[subset_size])

  # Add the number of coalitions of smaller sizes that are included
  if (subset_size > 1) n_comb_needed_now  = n_comb_needed_now + 2 * sum(choose(M, seq(subset_size - 1)))

  # Store the new values
  n_comb_needed = c(n_comb_needed, n_comb_needed_now)

  # Update the probability of the remaining colaition sizes such that they sum to 1
  if (remaining_weight_vector[subset_size] < 1.0) {
    remaining_weight_vector = remaining_weight_vector / (1 - remaining_weight_vector[subset_size])
  }
}

# Create a data table with max number of coalitions before we include the smaller coalition size. Ensure even numbers
n_comb_needed = sapply(n_comb_needed, function(x) ifelse(x %% 2 == 0, x - 2, x - 1))
dt_n_comb_needed = data.table(dt_id = seq_along(n_comb_needed), N_S = n_comb_needed)
dt_n_comb_needed[, N_S_fixed := fifelse(dt_id == 1, 0, 2 * sapply(dt_id, function(id) sum(choose(M, seq_len(id - 1)))))]
dt_n_comb_needed


# Create a look up table.
# The idea now is that if for each value of N_S, we can get which result list to look at by looking at
# `dt_n_comb_needed_sample[N_S == 916, dt_id]`.
dt_n_comb_needed_sample = data.table(N_S = seq(2, 2^M-4, 2), dt_id = sapply(seq(2, 2^M-4, 2), function(x) which.max(n_comb_needed >= x)))



id_now_idx = 2
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
    remaining_weight_vector[seq(num_paired_subset_sizes)] = remaining_weight_vector[seq(num_paired_subset_sizes)] / 2 # because we draw two samples each below
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
        size = 3 * samples_left,
        replace = TRUE,
        prob = remaining_weight_vector
      ) + num_full_subsets # Add the num_full_subsets to get the correct coal sizes

      # Sample the coalitions
      message(paste0("(", id_now, "/", id_max, ") ", "Getting coalitions"))
      feature_sample <- shapr:::sample_features_cpp(m, n_features_sample)

      # Get the paired coalitions
      message(paste0("(", id_now, "/", id_max, ") ", "Making the paired"))
      coalitions <- unlist(lapply(feature_sample, function(x, m) {
        if (length(x) == M / 2) {
          return(list(x))
        } else {
          return(list(x, seq(m)[-x]))
        }
      }, m = m), recursive = FALSE)
      #coalitions

      # if (M %% 2 != 0) {
      #   feature_sample_paired <- lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)
      # } else {
      #   feature_sample_lengths = lengths(feature_sample)
      #   feature_sample_to_pairs = feature_sample[feature_sample_lengths != M / 2]
      #   feature_sample_paired <- lapply(feature_sample_to_pairs, function(x, m) {seq(m)[-x]}, m = m)
      # }

      # # Merge the coalitions in alternating fashion as we do paired sampling (i.e., first is S and second is Sbar and so on)
      # coalitions = c(rbind(feature_sample, feature_sample_paired))

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
    dt_N_S_and_L[N_S == N_S_now]
    dt_N_S_and_L_small = dt_N_S_and_L[N_S <= N_S_now]
    all_coalitions_small = all_coalitions[seq(dt_N_S_and_L_small[.N, L])]
  }

  # length(unique(all_coalitions_small))
  #
  # tmp = all_coalitions_small[-seq(nrow(dt_res))]
  #
  # tmp

  return(list(dt_res = dt_res, dt_N_S_and_L_small = dt_N_S_and_L_small, all_coalitions_small = all_coalitions_small))

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


  #
  #     while (samples_left > 0 && ind_set_pos <= length(ind_set)) {
  #       ind = ind_set[ind_set_pos]  # we call sample once to save time and then just read it here
  #       ind_set_pos = ind_set_pos + 1
  #       subset_size = ind + num_full_subsets
  #
  #       mask_now = rep(0, M)
  #       mask_now[sample(M, size = subset_size)] = 1
  #       mask_now_str = paste(mask_now, collapse = ",")
  #
  #       # Id update. If numeric, then it is a new sample
  #       id_update = used_masks[mask == mask_now_str, id]
  #
  #       if (length(id_update) == 0) { # !mask_now_str %in% used_masks$mask (Now i only need to check for unique caol once)
  #         new_sample = TRUE
  #         used_masks = rbind(used_masks, data.table(id = coalition_idx, mask = mask_now_str))
  #         samples_left = samples_left - 1
  #
  #         # Update masks and weights
  #         masks[coalition_idx, ] = mask_now
  #         KernelWeights[coalition_idx] = 1
  #         coalition_idx = coalition_idx + 1
  #       } else {
  #         new_sample = FALSE
  #
  #         # Update sampling frequency
  #         KernelWeights[id_update] = KernelWeights[id_update] + 1
  #       }
  #
  #       # add the compliment sample
  #       if (samples_left > 0 && subset_size <= num_paired_subset_sizes) {
  #         mask_now = 1 - mask_now
  #         mask_now_str = paste(mask_now, collapse = ",")
  #
  #         # only add the sample if we have not seen it before, otherwise just
  #         # increment a previous sample's weight
  #         if (new_sample) {
  #           used_masks = rbind(used_masks, data.table(id = coalition_idx, mask = mask_now_str))
  #           samples_left = samples_left - 1
  #
  #           # Update masks and weights
  #           masks[coalition_idx, ] = mask_now
  #           KernelWeights[coalition_idx] = 1
  #           coalition_idx = coalition_idx + 1
  #
  #         } else {
  #           # we know the compliment sample is the next one after the original sample, so + 1
  #           KernelWeights[id_update + 1] = KernelWeights[id_update + 1] + 1
  #         }
  #       }
  #     }
  #
  #     # normalize the kernel weights for the random samples to equal the weight left after
  #     # the fixed enumerated samples have been already counted
  #     weight_left = sum(weight_vector[-seq(num_full_subsets)])
  #     KernelWeights[-seq(nfixed_samples)] = KernelWeights[-seq(nfixed_samples)] * weight_left / sum(KernelWeights[-seq(nfixed_samples)])
  #   }



})


final_list = list(look_up = list(dt_n_comb_needed = dt_n_comb_needed, dt_n_comb_needed_sample = dt_n_comb_needed_sample),
                  samples = full_res)
print(object.size(final_list), units = "MB")

final_list$look_up$dt_n_comb_needed
final_list$look_up$dt_n_comb_needed_sample

final_list$samples[[1]]







