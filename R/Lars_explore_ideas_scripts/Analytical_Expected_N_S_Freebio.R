# Libraries and functions  ----------------------------------------------------------------------------------------
## Libraries
#install.packages(c("future", "future.apply", "progressr", "data.table"))
library(gmp)
library(future)
library(future.apply)
library(progressr)
library(data.table)

find_combinations <- function(target, bins, bin_sizes = rep(target, bins)) {
  # print(paste0("target = ", target, " bins = ", bins, " bin_sizes = ", paste(bin_sizes, collapse = ", ")))
  #if (any(bin_sizes < 1) || sum(bin_sizes) < target) stop("Invalid bin sizes")
  #if (bin_sizes[1] > target - bins + 1) print("hdfj")

  #if (bins > target) stop("target must be larger or equal to the number of bins")
  if (bins == 1) {
    # Base case: Only one number left, which must be the target itself (if allowed)
    return(if (target <= bin_sizes[1]) matrix(target) else NULL)
  }

  # Initialize an empty list to store combinations
  combinations <- list()

  # Iterate over all possible values of the first element (0 to target)
  for (x in seq(0, min(bin_sizes[1], target))) {
    # Recursive call: find combinations of the remaining bins-1 elements
    sub_combinations <- find_combinations(target = target - x, bins = bins - 1, bin_sizes = bin_sizes[-1])

    # Combine the current element with each sub-combination and store them
    if (!is.null(sub_combinations)) combinations <- append(combinations, list(cbind(x, sub_combinations)))
  }

  # Combine all combinations into a single matrix
  ret_mat = do.call(rbind, combinations)
  if (!is.null(ret_mat)) colnames(ret_mat) = paste0("V", seq(ncol(ret_mat)))
  return(ret_mat)
}

expected_draws_fast = function(m,
                               N_s_vector = seq(2, 2^m - 2),
                               paired = TRUE,
                               n_workers = max(1, future::availableCores() - 2),
                               return_as_character = FALSE) {
  ### Internal functions needed to get the progressr updates
  # Function to compute the sum_{q} 1 / (1 - P_j)
  internal_compute_sum_Pj_values = function(q_values, n_size, probs, N_coal_all) {
    # Create progressr feedback every 20th N_s_values
    p <- progressr::progressor(steps = length(q_values) / 20)

    # Use future to parallelize the calls and make it into a bigq vector
    do.call(c, future.apply::future_lapply(q_values, function(q, n_size, probs, N_coal_all) {
      # To get the elapsed time
      t1 = Sys.time()

      # Get all the way to distribute the integer q into length(n_size) bins, where each bin has an upper limit of
      # the allowed values. I.e., each coalition size has a max number coalitions.
      # TODO: rewrite find_combinations such that it supports gmp, however, this is not needed for before m is huge,
      # and then we are not able to compute the exact values. So this is not important to fix.
      rel_combs = find_combinations(target = q, bins = length(n_size), bin_sizes = as.integer(n_size))

      # Needed to use do.call to make sure that comb_per_coal_size is of class bigz. Any other better ways=
      comb_per_coal_size = do.call(rbind, sapply(seq(nrow(rel_combs)), function(row_id) do.call(c, sapply(seq(ncol(rel_combs)), function(col_id) gmp::chooseZ(n_size[col_id], rel_combs[row_id, col_id])))))

      # Compute the number of combinations per coalition size
      comb_per_coal_size_prod = apply(comb_per_coal_size, 1, prod)

      # Check that we have the right number of combinations
      if (sum(comb_per_coal_size_prod) != gmp::chooseZ(N_coal_all, q)) {
        error("Difference between the sum of comb_per_coal_size_prod and gmp::chooseZ(N_coal_all, q) is nonezero")
      }

      # Get the values for each combinations
      prob_per_coal_size = do.call(c, sapply(seq(nrow(rel_combs)), function(row_id) {
        coeff = rel_combs[row_id,]
        coeff[coeff == 0] = NA
        1 / (1 - sum(coeff * probs, na.rm = TRUE))
      }))

      # Multiply with the number of duplicated of each combination
      sum_Pj_values_now = sum(prob_per_coal_size * comb_per_coal_size_prod)

      # Get the time difference
      time_diff = Sys.time() - t1

      # Give a printout to the user about the elapsed time
      message(paste0("q = ", q, "\t combinations = ", gmp::chooseZ(N_coal_all, q),
                     "\t unique weight combinations = ", nrow(rel_combs), "\t Elapsed time = ",
                     round(as.numeric(time_diff),2), " ", attr(time_diff, "units")))

      # Provide progress update every 20th step
      if (q %% 20 == 0) p(sprintf("Computing sum Pj (q = %g)", q))

      # Returned the calculated sum Pj values
      return(sum_Pj_values_now)
    }, n_size = n_size, probs = probs, N_coal_all = N_coal_all))
  }

  # Function to compute the expected number of draws
  internal_compute_N_draws = function(N_s_values, N_coal_all, sum_Pj_values) {
    # Create progressr feedback every 20th N_s_values
    p <- progressr::progressor(steps = length(N_s_values) / 20)

    # Use future to parallelize the calls and make it into a bigq vector
    do.call(c, future.apply::future_lapply(N_s_values, function(N_s_vector_idx,
                                                                N_coal_all,
                                                                sum_Pj_values) {
      # Get the current N_s value
      N_s_now = N_s_vector[N_s_vector_idx]

      # Set the default value to 1
      N_draws_now = gmp::as.bigz(1)

      # If N_s_now = 1, then the expected number of draws is one
      if (N_s_now > 1) {
        # Store the expected number of draws until we have sampled N_s_now unique samples
        N_draws_now = (-1)^(N_s_now - 1) * gmp::chooseZ(N_coal_all - 1, N_coal_all - N_s_now)

        # Iterate over the N_s values
        for (q in seq(N_s_now - 1)) {
          N_draws_now = N_draws_now +
            (-1)^(N_s_now - 1 - q) * gmp::chooseZ(N_coal_all - q - 1, N_coal_all - N_s_now) * sum_Pj_values[q]
        }
      }

      # Provide progress update every 20th step
      if (N_s_vector_idx %% 20 == 0) p(sprintf("Computing N_draws (N_s = %g)", N_s_vector_idx))

      # Returned the calculated number of expected draws
      return(N_draws_now)
    }, N_coal_all = N_coal_all, sum_Pj_values = sum_Pj_values))
  }

  ## Code
  # We use -2 due to empty and grand coalition and -1 as we do not do sampling when we use all coalitions
  # Remove all N_s values that are larger than the possible values
  N_s_vector = N_s_vector[N_s_vector <= 2^m  - 2]

  # Get the number of coalitions (2^M - 2)
  N_coal_all = 2^m - 2

  # Get all coalition sizes
  n_features <- seq(m - 1)

  # Get the number of coalitions of each coalition size
  n <- do.call(c, sapply(n_features, gmp::chooseZ, n = m))
  n_size = n # Duplicate which will be different in paired

  # Update variable if we are doing paired sampling
  if (paired) {
    # Use only the even values of N_s
    N_s_vector = N_s_vector[N_s_vector %% 2 == 0]

    # Divide by two as we are doing paired
    N_s_vector = N_s_vector / 2

    # Divide by two as we are doing paired
    N_coal_all = N_coal_all / 2

    # Get paired coalition sizes
    n_features = n_features[seq(ceiling((m-1)/2))]

    # Get the coalition sizes of the allowed sizes
    n = n[seq(ceiling((m-1)/2))]

    # Get the number of paired coalitions in each coalition size. Half the number when |S| = |Sbar|.
    n_size = c(n[seq(floor((m-1)/2))])
    if (m %% 2 == 0) n_size = c(n_size, n[ceiling((m-1)/2)] / 2)
  }

  # Get the probabilities for each coalition size (shapley kernel weights)
  probs = (m - 1) / (n * n_features * (m - n_features))

  # Normalize the probabilities for each coalition size (i.e., sum(probs * n_size) = 1)
  probs = probs / sum(probs * n_size)

  # Vector to store the expected number of draws
  N_draws = as.bigq(rep(1, length(N_s_vector)))

  # Vector to store the second sum of probabilities for each subset size of coalitions.
  sum_Pj_values = as.bigq(rep(NA, N_s_vector[length(N_s_vector)] - 1))

  # Iterate over the sizes and compute the probabilities
  if (N_s_vector[length(N_s_vector)] - 1 == 0) stop("Too low `N_s_vector` value. Only need one draw.")

  # Set up future and progressr (workers should not exceede the number of independent tasks)
  future::plan(multisession, workers = min(n_workers, N_s_vector[length(N_s_vector)] - 1))
  progressr::handlers('cli')

  # Compute the sum_{q} 1 / (1 - Pj) values
  t1 = Sys.time()
  progressr::with_progress({
    sum_Pj_values = internal_compute_sum_Pj_values(q_values = seq(N_s_vector[length(N_s_vector)] - 1),
                                                   n_size = n_size,
                                                   probs = probs,
                                                   N_coal_all = N_coal_all)
    })#, enable = TRUE) # To get feedback in non-interactive mode
  time_diff_Pj = Sys.time() - t1

  # Give a printout to the user about the elapsed time
  message(paste0("Elapsed time computing the sum_{q} 1 / (1 - Pj) values: ",
                 round(as.numeric(time_diff_Pj), 2), " ", attr(time_diff_Pj, "units")))

  # Then iterate over N_s values (i.e., the values for which we want the expected number of draws needed to obtain)
  t1 = Sys.time()
  progressr::with_progress({
    N_draws = internal_compute_N_draws(N_s_values = seq_along(N_s_vector),
                                       N_coal_all = N_coal_all,
                                       sum_Pj_values = sum_Pj_values)
  }) #, enable = TRUE) # To get feedback in non-interactive mode
  time_diff_Ns = Sys.time() - t1

  # Give a printout to the user about the elapsed time
  message(paste0("Elapsed time computing the extected number of draws: ",
                 round(as.numeric(time_diff_Ns), 2), " ", attr(time_diff_Ns, "units")))

  # Go back to sequential mode
  future::plan(sequential)

  # If paired, then we double the number of coalitions as each coalition is paired/represents two coalitions.
  if (paired) {
    N_draws = 2*N_draws
    N_s_vector = 2*N_s_vector
  }

  # Convert from bigq to character as ARM computers do not accept bigq in data.tables
  if (return_as_character) {
    N_draws = as.character(N_draws)
    sum_Pj_values = as.character(sum_Pj_values)
  }

  # Return the expected number of draws
  return(list(res_dt = data.table(N_s = N_s_vector, N_draws = N_draws),
              Pj_dt = data.table(q = seq(length(sum_Pj_values)), sum_Pj_values = sum_Pj_values),
              times = list(time_diff_Pj = time_diff_Pj, time_diff_Ns = time_diff_Ns)))
}



# Code ------------------------------------------------------------------------------------------------------------
folder_save = "/home/lholsen/Paper3/Paper3_save_location"

args = commandArgs(trailingOnly = TRUE)
n_workers = as.integer(args[1])

# The dimensions
m_seq = unlist(strsplit(as.character(args[2]), ","))
if (length(m_seq) > 1) {
  m_seq = unname(sapply(m_seq, function(i) as.numeric(i)))
} else {
  m_seq = as.numeric(m_seq)
}

N_s_vector_max = as.integer(args[3])

# The sampling strategies
versions = c("paired", "unique")

version = versions[1]
m = 5
for (version in versions) {
  for (m in m_seq) {
    message(paste0("\nWorking on version = '", version, "' and m = ", m, "."))

    N_s_vector = seq(2, 2^m - 2)
    N_s_vector = N_s_vector[N_s_vector <= N_s_vector_max]


    # Compute the expected number of draws
    t1 = Sys.time()
    analytical = expected_draws_fast(m = m,
                                     N_s_vector = N_s_vector,
                                     paired = ifelse(version == "paired", TRUE, FALSE),
                                     n_workers = min(future::availableCores() - 2, n_workers),
                                     return_as_character = TRUE)
    t2 = Sys.time()
    time_diff = t2 - t1
    print(time_diff)
    analytical$times[["time_total"]] = time_diff

    # Save the results
    saveRDS(analytical, file.path(folder_save, paste0("Sequence_length_M_", m, "_version_", version, "_analytical.rds")))

    # Convert the values to strings as we get some warnings from data.table()
    analytical_str = copy(analytical)
    analytical_str$res_dt[, N_draws := as.character(N_draws)]
    analytical_str$Pj_dt[, sum_Pj_values := as.character(sum_Pj_values)]
    saveRDS(analytical_str, file.path(folder_save, paste0("Sequence_length_M_", m, "_version_", version, "_analytical_str.rds")))
  }
}



if (FALSE) {
  m = 10

  analytical = readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, "_version_paired_analytical_str.rds"))
  simulated = readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, ".rds"))
  matplot(cbind(as.numeric(as.bigq(analytical$res_dt$N_draws)), simulated$dt_avg[N_S %% 2 == 0, L_avg]), type = "l", lwd = 2)
  matplot(as.numeric(as.bigq(analytical$res_dt$N_draws)) - simulated$dt_avg[N_S %% 2 == 0, L_avg], type = "l", lwd = 2)
}

