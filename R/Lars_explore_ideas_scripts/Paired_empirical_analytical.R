m = 10
n_features <- seq(m - 1)
n <- sapply(n_features, choose, n = m)
w <- shapley_weights(m = m, N = n, n_features) * n
p <- w / sum(w)
p

w2 = (m - 1) / (n_features * (m - n_features))
p2 <- w2 / sum(w2)
p2

p3 = c(2*p2[1], p2[2])
n3 = c(n[1], n[2] / 2)

p3
n3
rep

expected_draws_unique_wo_time = function(m, N_s_vector = seq(2, 2^m - 2)) {
  library(gmp)

  # Get all coalition sizes
  n_features <- seq(m - 1)

  # Get the number of coalitions of each coalition size
  n <- sapply(n_features, choose, n = m)

  # Get the probabilities for each coalition
  all_probs = rep(shapr:::shapley_weights(m = m, N = n, n_features), times = n)

  # Normalize the probabilities
  all_probs = all_probs / sum(all_probs)

  # Data table with the probabilities for each coalition size
  dt_probs = data.table(coal_size = n_features,
                        n = n)
  dt_probs[, prob := all_probs[cumsum(c(1, dt_probs[-.N, n]))]]

  # Get the number of coalitions (2^M -2 )
  N_coal_all = length(all_probs)

  # Vector to store the expected number of draws
  N_draws = as.bigq(rep(NA, length(N_s_vector)))

  # Vector to store the second sum of probabilities for each subset size of coalitions.
  sum_Pj_values = as.bigq(rep(NA, N_s_vector[length(N_s_vector)] - 1))

  # Iterate over the sizes and compute the probabilities
  for (q in seq(N_s_vector[length(N_s_vector)] - 1)) {
    message(paste0("q = ", q, "\t combinations = ", gmp::chooseZ(N_coal_all, q)))


    # # Get all combinations of q features among the values 1, 2,..., N_coal_all. (i.e., the possible draw sequences)
    # time_1 = system.time({combinations_q = utils::combn(x = N_coal_all, m = q, simplify = FALSE)})
    # message(paste0("Time combinations: ",  paste(names(time_1)[1:3], "=", round(time_1, 3)[1:3], collapse = ", ")))
    #
    # # Compute the probabilities. Use vapply with specified output type and length to obtain a slight
    # # performance boost by avoiding R’s internal type checking and allocation, which makes it faster.
    # time_2 = system.time({
    #   sum_Pj_values[q] = sum(vapply(combinations_q, function(combo) 1 / (1 - sum(all_probs[combo])), numeric(1)))})
    # message(paste0("Time probabilities: ",  paste(names(time_2)[1:3], "=", round(time_2, 3)[1:3], collapse = ", ")))

    # combinations_q
    # vapply(combinations_q, function(combo) 1 / (1 - sum(all_probs[combo])), numeric(1))
    # sum(vapply(combinations_q, function(combo) 1 / (1 - sum(all_probs[combo])), numeric(1)))
    # table(vapply(combinations_q, function(combo) 1 / (1 - sum(all_probs[combo])), numeric(1)))
    # length(combinations_q)
    # vapply(combinations_q, function(combo) 1 / (1 - sum(all_probs[combo])), numeric(1))


    # time_3 = system.time({
      rel_combs = find_combinations(target = q, bins = m-1, bin_sizes = n)
      message(paste0("q = ", q, "\t combinations = ", gmp::chooseZ(N_coal_all, q), "\t unique weight combinations = ", ncol(rel_combs)))


      #matrix.bigz(rel_combs, ncol = ncol(rel_combs), nrow = nrow(rel_combs))
      #
      # mode(rel_combs) <- "double"
      # nrow(rel_combs)
      #
      # kk = t(compositions(q, m-1))
      # nrow(kk)
      #
      #
      # jj = kk[apply(kk, 1, FUN = function(x) all(x <= n)),]
      # nrow(jj)
      # rel_combs = jj
      #
      # Determine the dimensions of the `rel_combs` matrix
      nrows <- nrow(rel_combs)
      ncols <- ncol(rel_combs)

      # Initialize an empty matrix to store the results
      comb_per_coal_size <- matrix.bigz(NA, nrow = nrows, ncol = ncols)

      # Use nested for loops to compute combinations and fill the result matrix
      # Use foor loops and not sapply as I need to use gmp classes the whole way
      for (row_id in seq(nrows)) {
        for (col_id in seq(ncols)) {
          # Compute the combination using gmp::chooseZ and store it in the result matrix
          comb_per_coal_size[row_id, col_id] <- gmp::chooseZ(n[col_id], rel_combs[row_id, col_id])
        }
      }

      # Compute the number of combinations per coalition size
      comb_per_coal_size_prod = apply(comb_per_coal_size, 1, prod)

      #
      # comb_per_coal_size
      #
      #
      # comb_per_coal_size = t(sapply(seq(nrow(rel_combs)), function(row_id) sapply(seq(ncol(rel_combs)), function(col_id) gmp::chooseZ(n[col_id], rel_combs[row_id, col_id]))))
      # comb_per_coal_size = t(sapply(seq(nrow(rel_combs)), function(row_id) sapply(seq(ncol(rel_combs)), function(col_id) choose(n[col_id], rel_combs[row_id, col_id]))))
      # comb_per_coal_size_prod = apply(comb_per_coal_size, 1, prod)
      # comb_per_coal_size_prod = apply(comb_per_coal_size, 1, function(x) prod(as.bigz(x)))
      #
      # class(matrix.bigz(comb_per_coal_size, ncol = ncol(comb_per_coal_size), nrow = nrow( comb_per_coal_size)))
      #

      #if (sum(comb_per_coal_size_prod) != choose(N_coal_all, q)) stop("incorrect lengths")
      # Check that we have the right number of combinations
      print(sum(comb_per_coal_size_prod) - gmp::chooseZ(N_coal_all, q))
      #as.double(gmp::chooseZ(N_coal_all, q) + 1:100)


      prob_per_coal_size = sapply(seq(nrow(rel_combs)), function(row_id) {
        coeff = rel_combs[row_id,]
        coeff[coeff == 0] = NA
        1 / (1 - sum(coeff * dt_probs$prob, na.rm = TRUE))
      })

      #print(sum_Pj_values[q])
      sum_Pj_values[q] = sum(prob_per_coal_size * as.bigq(comb_per_coal_size_prod))
      #print(sum_Pj_values[q])
    # })
    # message(paste0("Time probabilities: ",  paste(names(time_3)[1:3], "=", round(time_3, 3)[1:3], collapse = ", ")))

  }

  # Then iterate over N_s values (i.e., the values for which we want the expected number of draws needed to obtain)
  for (N_s_vector_idx in seq_along(N_s_vector)) {

    # Get the current N_s value
    N_s_now = N_s_vector[N_s_vector_idx]
    print(N_s_now)

    # If N_s_now = 1, then the expected number of draws is one
    if (N_s_now > 1) {
      # Store the expected number of draws until we have sampled N_s_now unique samples
      N_draws[N_s_vector_idx] = (-1)^(N_s_now - 1) * gmp::chooseZ(N_coal_all - 1, N_coal_all - N_s_now)

      for (q in seq(N_s_now - 1)) {
        N_draws[N_s_vector_idx] = N_draws[N_s_vector_idx] +
          (-1)^(N_s_now - 1 - q) * gmp::chooseZ(N_coal_all - q - 1, N_coal_all - N_s_now) * sum_Pj_values[q]
      }
    } else {
      N_draws[N_s_vector_idx] = 1
    }
  }

  # Return the expected number of draws
  return(list(N_draws = N_draws, sum_Pj_values = sum_Pj_values))
}


#' Computing the expected number of draws until getting N_s unique coalitions of m possible coalitions.
#'
#' @param m Positive integer. The total number of coalitions.
#' @param N_s_vector Vector of positive integers. The number of unique coalitions at which we want to estimate the expected number of draws.
expected_draws_unique = function(m, N_s_vector = seq(2, 2^m - 2)) {
  library(gmp)

  # We use -2 due to empty and grand coalition and -1 as we do not do sampling when we use all coalitions
  # Remove all N_s values that are larger than the possible values
  N_s_vector = N_s_vector[N_s_vector <= 2^m  - 2]

  # Get the number of coalitions (2^M - 2)
  N_coal_all = 2^m - 2

  # Get all coalition sizes
  n_features <- seq(m - 1)

  # Get the number of coalitions of each coalition size
  n <- do.call(c, sapply(n_features, gmp::chooseZ, n = m))

  # Get the probabilities for each coalition
  probs = shapr:::shapley_weights(m = m, N = n, n_features)

  # Data table with the normalized probabilities for each coalition size (i.e., sum(dt_probs$n * dt_probs$prob) = 1)
  # dt_probs = data.table(coal_size = n_features, n = n, prob = probs / sum(probs * n))
  probs = probs / sum(probs * n)

  # Vector to store the expected number of draws
  N_draws = as.bigq(rep(NA, length(N_s_vector)))

  # Vector to store the second sum of probabilities for each subset size of coalitions.
  sum_Pj_values = as.bigq(rep(NA, N_s_vector[length(N_s_vector)] - 1))

  # Iterate over the sizes and compute the probabilities
  for (q in seq(N_s_vector[length(N_s_vector)] - 1)) {
    time_3 = system.time({
      rel_combs = find_combinations(target = q, bins = m-1, bin_sizes = as.integer(n)) # TODO: rewrite function to support bigz
      message(paste0("q = ", q, "\t combinations = ", gmp::chooseZ(N_coal_all, q), "\t unique weight combinations = ", nrow(rel_combs)))

      # Needed to use do.call to make sure that comb_per_coal_size is of class bigz. Any other better ways=
      comb_per_coal_size = do.call(rbind, sapply(seq(nrow(rel_combs)), function(row_id) do.call(c, sapply(seq(ncol(rel_combs)), function(col_id) gmp::chooseZ(n[col_id], rel_combs[row_id, col_id])))))

      # Compute the number of combinations per coalition size
      comb_per_coal_size_prod = apply(comb_per_coal_size, 1, prod)

      # Check that we have the right number of combinations
      if (sum(comb_per_coal_size_prod) != gmp::chooseZ(N_coal_all, q)) {
        error("Difference between the sum of comb_per_coal_size_prod and gmp::chooseZ(N_coal_all, q) is nonezero")
      }

      prob_per_coal_size = do.call(c, sapply(seq(nrow(rel_combs)), function(row_id) {
        coeff = rel_combs[row_id,]
        coeff[coeff == 0] = NA
        1 / (1 - sum(coeff * probs, na.rm = TRUE))
      }))

      sum_Pj_values[q] = sum(prob_per_coal_size * comb_per_coal_size_prod)
      },
      gcFirst = FALSE)
    message(paste0("Time probabilities: ",  paste(names(time_3)[1:3], "=", round(time_3, 3)[1:3], collapse = ", ")))

  }

  # Then iterate over N_s values (i.e., the values for which we want the expected number of draws needed to obtain)
  for (N_s_vector_idx in seq_along(N_s_vector)) {

    # Get the current N_s value
    N_s_now = N_s_vector[N_s_vector_idx]
    # print(N_s_now)

    # If N_s_now = 1, then the expected number of draws is one
    if (N_s_now > 1) {
      # Store the expected number of draws until we have sampled N_s_now unique samples
      N_draws[N_s_vector_idx] = (-1)^(N_s_now - 1) * gmp::chooseZ(N_coal_all - 1, N_coal_all - N_s_now)

      for (q in seq(N_s_now - 1)) {
        N_draws[N_s_vector_idx] = N_draws[N_s_vector_idx] +
          (-1)^(N_s_now - 1 - q) * gmp::chooseZ(N_coal_all - q - 1, N_coal_all - N_s_now) * sum_Pj_values[q]
      }
    } else {
      N_draws[N_s_vector_idx] = 1
    }
  }

  # Return the expected number of draws
  return(list(N_draws = N_draws, sum_Pj_values = sum_Pj_values))
}

pp4 = expected_draws_unique(m = 4)
pp2 = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_4.rds")
matplot(cbind(pp4$N_draws, pp2$dt_avg$L_avg[-1]), type = "l", lwd = 2)

450 883 717 216 034 179
162 246 633 998 500 042 073 712 630

ppp = expected_draws_unique_wo_time(m = 5)
pp = expected_draws_unique(m = 6)
expected_draws_unique(m = 5, N_s_vector = 2:9)
plot(pp$N_draws[1:14])


qq = expected_draws(m = 5, N_s_vector = 2:8, paired = FALSE)
qqq = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_5.rds")
matplot(cbind(ppp$N_draws, pp$N_draws, qq$res_dt$N_draws, qqq$dt_avg$L_avg[-1]), type = "l", lwd = 2)

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

expected_draws_fast = function(m, N_s_vector = seq(2, 2^m - 2), paired = TRUE, n_workers = 4) {
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

  ## Libraries
  library(gmp)
  library(future)
  library(future.apply)
  library(progressr)
  library(data.table)
  install.packages(c("future", "future.apply", "progressr", "data.table"))

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

    # Get the coalition sizes of the alowed sizes
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

  # Set up future and progressr
  future::plan(multisession, workers = n_workers)
  progressr::handlers('cli')

  # Compute the sum_{q} 1 / (1 - Pj) values
  t1 = Sys.time()
  progressr::with_progress({
    sum_Pj_values = internal_compute_sum_Pj_values(q_values = seq(N_s_vector[length(N_s_vector)] - 1),
                                                   n_size = n_size,
                                                   probs = probs,
                                                   N_coal_all = N_coal_all)})
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
  })
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

  # Return the expected number of draws
  return(list(res_dt = data.table(N_s = N_s_vector, N_draws = N_draws),
              Pj_dt = data.table(q = seq(length(sum_Pj_values)), sum_Pj_values = sum_Pj_values)))
}

m = 9
for (m in 4:7) {

  time_7 = Sys.time()
  analytical = expected_draws_fast(m = m)
  time_8 = Sys.time()
  print(time_8 - time_7)
  simulated = readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, ".rds"))
  matplot(cbind(analytical$res_dt$N_draws, simulated$dt_avg[N_S %% 2 == 0, L_avg]), type = "l", lwd = 2)
  matplot(analytical$res_dt$N_draws - simulated$dt_avg[N_S %% 2 == 0, L_avg], type = "l", lwd = 2)

  analytical_str = copy(analytical)
  analytical_str$res_dt[, N_draws := as.character(N_draws)]
  analytical_str$Pj_dt[, sum_Pj_values := as.character(sum_Pj_values)]
  saveRDS(analytical, paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, "_analytical.rds"))
  saveRDS(analytical_str, paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, "_analytical_str.rds"))


}
m = 8



expected_draws_fast_OLD = function(m, N_s_vector = seq(2, 2^m - 2), paired = TRUE) {
  ### Internal functions needed to get the progressr updates

  # Function to compute the sum_{q} 1 / (1 - P_j)
  internal_compute_sum_Pj_values = function(q_values, n_size, probs) {
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


  library(gmp)
  library(future)
  library(future.apply)
  library(progressr)
  library(data.table)

  # We use -2 due to empty and grand coalition and -1 as we do not do sampling when we use all coalitions
  # Remove all N_s values that are larger than the possible values
  N_s_vector = N_s_vector[N_s_vector <= 2^m  - 2]

  # Get the number of coalitions (2^M - 2)
  N_coal_all = 2^m - 2

  # Get all coalition sizes
  n_features <- seq(m - 1)

  # Get the number of coalitions of each coalition size
  n <- do.call(c, sapply(n_features, gmp::chooseZ, n = m))
  #n <- sapply(n_features, choose, n = m)
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

    # Get the coalition sizes of the alowed sizes
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

  # time_1 = Sys.time()
  # for (q in seq(N_s_vector[length(N_s_vector)] - 1)) {
  #   time_3 = system.time({
  #     rel_combs = find_combinations(target = q, bins = length(n_size), bin_sizes = as.integer(n_size))
  #
  #     message(paste0("q = ", q, "\t combinations = ", gmp::chooseZ(N_coal_all, q), "\t unique weight combinations = ", nrow(rel_combs)))
  #
  #     # Needed to use do.call to make sure that comb_per_coal_size is of class bigz. Any other better ways=
  #     comb_per_coal_size = do.call(rbind, sapply(seq(nrow(rel_combs)), function(row_id) do.call(c, sapply(seq(ncol(rel_combs)), function(col_id) gmp::chooseZ(n_size[col_id], rel_combs[row_id, col_id])))))
  #
  #     # Compute the number of combinations per coalition size
  #     comb_per_coal_size_prod = apply(comb_per_coal_size, 1, prod)
  #
  #     # Check that we have the right number of combinations
  #     if (sum(comb_per_coal_size_prod) != gmp::chooseZ(N_coal_all, q)) {
  #       error("Difference between the sum of comb_per_coal_size_prod and gmp::chooseZ(N_coal_all, q) is nonezero")
  #     }
  #
  #     prob_per_coal_size = do.call(c, sapply(seq(nrow(rel_combs)), function(row_id) {
  #       coeff = rel_combs[row_id,]
  #       coeff[coeff == 0] = NA
  #       1 / (1 - sum(coeff * probs, na.rm = TRUE))
  #     }))
  #
  #     sum_Pj_values[q] = sum(prob_per_coal_size * comb_per_coal_size_prod)
  #   },
  #   gcFirst = FALSE)
  #   message(paste0("Time probabilities: ",  paste(names(time_3)[1:3], "=", round(time_3, 3)[1:3], collapse = ", ")))
  # }
  # time_2 = Sys.time()
  # time_2 - time_1
  #
  #
  # time_3 = Sys.time()
  # sum_Pj_values_lapply = do.call(c, lapply(seq(N_s_vector[length(N_s_vector)] - 1), function (q) {
  #   time_3 = system.time({
  #     rel_combs = find_combinations(target = q, bins = length(n_size), bin_sizes = as.integer(n_size))
  #
  #     message(paste0("q = ", q, "\t combinations = ", gmp::chooseZ(N_coal_all, q), "\t unique weight combinations = ", nrow(rel_combs)))
  #
  #     # Needed to use do.call to make sure that comb_per_coal_size is of class bigz. Any other better ways=
  #     comb_per_coal_size = do.call(rbind, sapply(seq(nrow(rel_combs)), function(row_id) do.call(c, sapply(seq(ncol(rel_combs)), function(col_id) gmp::chooseZ(n_size[col_id], rel_combs[row_id, col_id])))))
  #
  #     # Compute the number of combinations per coalition size
  #     comb_per_coal_size_prod = apply(comb_per_coal_size, 1, prod)
  #
  #     # Check that we have the right number of combinations
  #     if (sum(comb_per_coal_size_prod) != gmp::chooseZ(N_coal_all, q)) {
  #       error("Difference between the sum of comb_per_coal_size_prod and gmp::chooseZ(N_coal_all, q) is nonezero")
  #     }
  #
  #     prob_per_coal_size = do.call(c, sapply(seq(nrow(rel_combs)), function(row_id) {
  #       coeff = rel_combs[row_id,]
  #       coeff[coeff == 0] = NA
  #       1 / (1 - sum(coeff * probs, na.rm = TRUE))
  #     }))
  #
  #     sum_Pj_values_now = sum(prob_per_coal_size * comb_per_coal_size_prod)
  #   },
  #   gcFirst = FALSE)
  #   message(paste0("Time probabilities: ",  paste(names(time_3)[1:3], "=", round(time_3, 3)[1:3], collapse = ", ")))
  #   return(sum_Pj_values_now)
  # }))
  # time_4 = Sys.time()
  # time_4 - time_3
  #
  #
  #
  # library(future)
  # library(future.apply)
  # library(progressr)

  # future::plan(multisession, workers = 4)
  # time_5 = Sys.time()
  # sum_Pj_values_future_lapply = do.call(c, future.apply::future_lapply(seq(N_s_vector[length(N_s_vector)] - 1), function (q) {
  #   time_3 = system.time({
  #     rel_combs = find_combinations(target = q, bins = length(n_size), bin_sizes = as.integer(n_size))
  #
  #     message(paste0("q = ", q, "\t combinations = ", gmp::chooseZ(N_coal_all, q), "\t unique weight combinations = ", nrow(rel_combs)))
  #
  #     # Needed to use do.call to make sure that comb_per_coal_size is of class bigz. Any other better ways=
  #     comb_per_coal_size = do.call(rbind, sapply(seq(nrow(rel_combs)), function(row_id) do.call(c, sapply(seq(ncol(rel_combs)), function(col_id) gmp::chooseZ(n_size[col_id], rel_combs[row_id, col_id])))))
  #
  #     # Compute the number of combinations per coalition size
  #     comb_per_coal_size_prod = apply(comb_per_coal_size, 1, prod)
  #
  #     # Check that we have the right number of combinations
  #     if (sum(comb_per_coal_size_prod) != gmp::chooseZ(N_coal_all, q)) {
  #       error("Difference between the sum of comb_per_coal_size_prod and gmp::chooseZ(N_coal_all, q) is nonezero")
  #     }
  #
  #     prob_per_coal_size = do.call(c, sapply(seq(nrow(rel_combs)), function(row_id) {
  #       coeff = rel_combs[row_id,]
  #       coeff[coeff == 0] = NA
  #       1 / (1 - sum(coeff * probs, na.rm = TRUE))
  #     }))
  #
  #     sum_Pj_values_now = sum(prob_per_coal_size * comb_per_coal_size_prod)
  #   },
  #   gcFirst = FALSE)
  #   message(paste0("Time probabilities: ",  paste(names(time_3)[1:3], "=", round(time_3, 3)[1:3], collapse = ", ")))
  #   return(sum_Pj_values_now)
  # }))
  # time_6 = Sys.time()
  # time_6 - time_5



  # Enable the progression handler
  progressr::handlers('cli')
  future::plan(multisession, workers = 7)
  time_7 = Sys.time()
  progressr::with_progress({
    sum_Pj_values = internal_compute_sum_Pj_values(q_values = seq(N_s_vector[length(N_s_vector)] - 1),
                                                   n_size = n_size,
                                                   probs = probs,
                                                   N_coal_all = N_coal_all)
  })
  time_8 = Sys.time()
  print(time_8 - time_7)
  future::plan(sequential)


  # Then iterate over N_s values (i.e., the values for which we want the expected number of draws needed to obtain)
  message("Computing N_draws ...")
  t1 = Sys.time()
  for (N_s_vector_idx in seq_along(N_s_vector)) {

    # Get the current N_s value
    N_s_now = N_s_vector[N_s_vector_idx]
    # print(N_s_now)

    # If N_s_now = 1, then the expected number of draws is one (the initiated value)
    if (N_s_now > 1) {
      # Store the expected number of draws until we have sampled N_s_now unique samples
      N_draws[N_s_vector_idx] = (-1)^(N_s_now - 1) * gmp::chooseZ(N_coal_all - 1, N_coal_all - N_s_now)

      for (q in seq(N_s_now - 1)) {
        N_draws[N_s_vector_idx] = N_draws[N_s_vector_idx] +
          (-1)^(N_s_now - 1 - q) * gmp::chooseZ(N_coal_all - q - 1, N_coal_all - N_s_now) * sum_Pj_values[q]
      }
    }
  }
  t2 = Sys.time()
  t2 - t1




  future::plan(multisession, workers = 7)
  t1 = Sys.time()
  progressr::with_progress({
    N_draws2 = internal_compute_N_draws(N_s_values = seq_along(N_s_vector),
                                        N_coal_all = N_coal_all,
                                        sum_Pj_values = sum_Pj_values)
  })
  t2 = Sys.time()
  t2 - t1
  future::plan(sequential)

  # If paired, then we double the number of coalitions as each coalition is paired/represents two coalitions.
  if (paired) {
    N_draws = 2*N_draws
    N_s_vector = 2*N_s_vector
  }

  # Return the expected number of draws
  return(list(res_dt = data.table(N_s = N_s_vector, N_draws = N_draws),
              Pj_dt = data.table(q = seq(length(sum_Pj_values)), sum_Pj_values = sum_Pj_values)))
}

expected_draws = function(m, N_s_vector = seq(2, 2^m - 2), paired = TRUE) {
  # We use -2 due to empty and grand coalition and -1 as we do not do sampling when we use all coalitions
  # Remove all N_s values that are larger than the possible values
  N_s_vector = N_s_vector[N_s_vector <= 2^m  - 2]

  # Get all coalition sizes
  n_features <- seq(m - 1)

  # Get the number of coalitions of each coalition size
  n <- sapply(n_features, choose, n = m)

  # Get the probabilities for each coalition
  all_probs = rep(shapr:::shapley_weights(m = m, N = n, n_features), times = n)

  if (paired) {
    # Use only the even values of N_s
    N_s_vector = N_s_vector[N_s_vector %% 2 == 0]

    # Divide by two as we are doing paired
    N_s_vector = N_s_vector / 2

    # Consider only the first half of the coalitions as they are paired.
    # Each coalition should then have twice the prob of being samples, but we normalize them afterwards, so skip it.
    all_probs = all_probs[seq(length(all_probs) / 2)]
  }

  # Normalize the probabilities
  all_probs = all_probs / sum(all_probs)

  # Get the number of coalitions (2^M -2 )
  N_coal_all = length(all_probs)

  # Vector to store the expected number of draws
  N_draws = rep(NA, length(N_s_vector))

  # Vector to store the second sum of probabilities for each subset size of coalitions.
  sum_Pj_values = rep(NA, N_s_vector[length(N_s_vector)] - 1)

  # Iterate over the sizes and compute the probabilities
  for (q in seq(N_s_vector[length(N_s_vector)] - 1)) {
    message(paste0("q = ", q, "\t combinations = ", choose(N_coal_all, q)))

    # Get all combinations of q features among the values 1, 2,..., N_coal_all. (i.e., the possible draw sequences)
    time_1 = system.time({combinations_q = utils::combn(x = N_coal_all, m = q, simplify = FALSE)})
    message(paste0("Time comb.: ",  paste(names(time_1)[1:3], "=", round(time_1, 3)[1:3], collapse = ", ")))

    # Compute the probabilities. Use vapply with specified output type and length to obtain a slight
    # performance boost by avoiding R’s internal type checking and allocation, which makes it faster.
    time_2 = system.time({
      sum_Pj_values[q] = sum(vapply(combinations_q, function(combo) 1 / (1 - sum(all_probs[combo])), numeric(1)))})
    message(paste0("Time prob.: ",  paste(names(time_2)[1:3], "=", round(time_2, 3)[1:3], collapse = ", ")))
  }

  # Then iterate over N_s values (i.e., the values for which we want the expected number of draws needed to obtain)
  for (N_s_vector_idx in seq_along(N_s_vector)) {

    # Get the current N_s value
    N_s_now = N_s_vector[N_s_vector_idx]

    # If N_s_now = 1, then the expected number of draws is one
    if (N_s_now > 1) {
      # Store the expected number of draws until we have sampled N_s_now unique samples
      N_draws[N_s_vector_idx] = (-1)^(N_s_now - 1) * choose(N_coal_all - 1, N_coal_all - N_s_now)
      for (q in seq(N_s_now - 1)) {
        N_draws[N_s_vector_idx] = N_draws[N_s_vector_idx] +
          (-1)^(N_s_now - 1 - q) * choose(N_coal_all - q - 1, N_coal_all - N_s_now) * sum_Pj_values[q]
      }
    } else {
      N_draws[N_s_vector_idx] = 1
    }
  }

  # If paired, then we double the number of coalitions as each coalition is paired/represents two coalitions.
  if (paired) {
    N_draws = 2*N_draws
    N_s_vector = 2*N_s_vector
  }

  # Return the expected number of draws
  return(list(res_dt = data.table(N_s = N_s_vector, N_draws = N_draws),
              Pj_dt = data.table(q = seq(length(sum_Pj_values)), sum_Pj_values = sum_Pj_values)))
}


res_paired_5$res_dt
res_sim_paired_5$dt_avg



res_unique_5 = expected_draws(m = 5, N_s_vector = 2:30, paired = FALSE)
res_paired_5 = expected_draws(m = 5, N_s_vector = 2:30, paired = TRUE)
res_unique_5$res_dt
res_paired_5$res_dt


matplot(cbind(res_paired_5$res_dt$N_draws,
              res_sim_paired_5$dt_avg[N_S %% 2 == 0, L_avg]
),
type = "l")

res_paired_6 = expected_draws(m = 6, N_s_vector = 2:32, paired = TRUE)


# Create the data table
res_paired_6 <- data.table(
  N_s = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60),
  N_draws = c(2.000000, 4.119938, 6.371885, 8.769402, 11.327704, 14.063815, 16.996714, 20.147459, 23.539304, 27.197798,
              31.151148, 35.429770, 40.066049, 45.125534, 50.465099, 56.973966, 61.847723, 72.796355, 73.530909, 92.650583,
              89.672424, 112.328455, 115.114276, 134.209501, 147.617630, 166.664404, 189.393038, 217.619256, 256.347015, 315.420018)
)
res_sim_paired_6 = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_6.rds")



matplot(cbind(res_paired_6$N_draws,
              res_sim_paired_6$dt_avg[-.N][N_S %% 2 == 0, L_avg]),
        type = "l")


system.time({
  #all_probs = c(0.090909, 0.090909, 0.090909, 0.090909, 0.045455, 0.045455, 0.045455, 0.045455, 0.045455, 0.045455, 0.090909, 0.090909, 0.090909, 0.090909)
  length_alphabet = length(all_probs)

  j = 10 # A value between 1 and length_alphabet

  # Get all combinations of 1, 2, ..., j-1 features among the values 1,2,...,length_alphabet.
  combinations = lapply(seq(j-1), utils::combn, x = length_alphabet, simplify = TRUE)

  # Res stores the expected samples until we have sampled j unique samples
  res = (-1)^(j - 1) * choose(length_alphabet - 1, length_alphabet - j)
  for (q in seq(j - 1)) {
    Pj_values = sapply(seq(ncol(combinations[[q]])), function(id_col) sum(all_probs[combinations[[q]][,id_col]]))
    sum_Pj_values = sum(1 / (1 - Pj_values))
    res = res + (-1)^(j - 1 - q) * choose(length_alphabet - q - 1, length_alphabet - j) * sum_Pj_values
  }
  res
})


system.time({
  # Precompute combinations only once
  # all_probs = c(0.090909, 0.090909, 0.090909, 0.090909, 0.045455, 0.045455, 0.045455, 0.045455, 0.045455, 0.045455, 0.090909, 0.090909, 0.090909, 0.090909)


  m = 5
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  all_probs = rep(shapley_weights(m = m, N = n, n_features), times = n)
  all_probs = all_probs / sum(all_probs)
  all_probs

  length_alphabet = length(all_probs)
  length_alphabet = length(all_probs)
  j = 8 # A value between 1 and length_alphabet

  # Get all combinations for the required values of j-1 features
  combinations = lapply(seq(j-1), function(q) utils::combn(length_alphabet, q, simplify = FALSE))

  # Calculate initial value for res
  res = (-1)^(j - 1) * choose(length_alphabet - 1, length_alphabet - j)

  # Vectorized computation over all combinations
  for (q in seq(j - 1)) {
    print(q)
    # Use vapply with specified output type and length to obtain a slight performance boost by avoiding R’s internal type checking and allocation, which makes it faster.
    sum_Pj_values = sum(vapply(combinations[[q]], function(combo) 1 / (1 - sum(all_probs[combo])), numeric(1)))
    res = res + (-1)^(j - 1 - q) * choose(length_alphabet - q - 1, length_alphabet - j) * sum_Pj_values
  }

  # Print the result
  res
})

(all_probs * res) / sum(all_probs * res)


system.time({
  # Precompute combinations only once
  all_probs = c(0.090909, 0.090909, 0.090909, 0.090909, 0.045455, 0.045455, 0.045455)

  m = 5
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  all_probs = rep(shapley_weights(m = m, N = n, n_features), times = n)
  all_probs = all_probs[seq(length(all_probs) / 2)]
  all_probs = 2*all_probs
  all_probs = all_probs / sum(all_probs)
  all_probs

  length_alphabet = length(all_probs)
  j = 8/2 # A value between 1 and length_alphabet (remember that it is paired)

  # Get all combinations for the required values of j-1 features
  combinations = lapply(seq(j-1), function(q) utils::combn(length_alphabet, q, simplify = FALSE))

  # Calculate initial value for res
  res = (-1)^(j - 1) * choose(length_alphabet - 1, length_alphabet - j)

  # Vectorized computation over all combinations
  for (q in seq(j - 1)) {
    # Use vapply with specifed output type and length to obtain a slight performance boost by avoiding R’s internal type checking and allocation, which makes it faster.
    sum_Pj_values = sum(vapply(combinations[[q]], function(combo) 1 / (1 - sum(all_probs[combo])), numeric(1)))
    res = res + (-1)^(j - 1 - q) * choose(length_alphabet - q - 1, length_alphabet - j) * sum_Pj_values
  }

  res
  # Print the result
  res2 = res * 2
})
res2

(res2 * p) / res2







# Unique ----------------------------------------------------------------------------------------------------------
m = 5
n_features <- seq(m - 1)
n <- sapply(n_features, choose, n = m)
w <- shapley_weights(m = m, N = n, n_features) * n
p <- w / sum(w)
n_combinations = 8 + 2 # Remember to add two for the empty and grand coalitions
B = 10000
results_unique = rep(NA, B)
for (b in seq(B)) {
  #print(b)
  feature_sample_all <- list()
  unique_samples <- 0
  while (unique_samples < n_combinations - 2) {
    # Sample number of chosen features ----------
    n_features_sample <- sample(
      x = n_features,
      size = n_combinations - unique_samples - 2, # Sample -2 as we add zero and m samples below
      replace = TRUE,
      prob = p
    )

    # Sample specific set of features -------
    feature_sample <- sample_features_cpp(m, n_features_sample)
    feature_sample_all <- c(feature_sample_all, feature_sample)
    unique_samples <- length(unique(feature_sample_all))
  }
  results_unique[b] = length(feature_sample_all)
}
mean(results_unique)


# Paired ----------------------------------------------------------------------------------------------------------
n_combinations = 28 + 2
B = 50000
results_paired = rep(NA, B)
n_features <- seq(m - 1)
n <- sapply(n_features, choose, n = m)
w <- shapley_weights(m = m, N = n, n_features) * n
p <- w / sum(w)
p
for (b in seq(B)) {
  #iters = 0
  feature_sample_all <- list()
  unique_samples <- 0
  while (unique_samples < n_combinations - 2) {

    n_features_sample <- sample(
      x = n_features,
      size = (n_combinations - unique_samples - 2)/2, # Sample -2 as we add zero and m samples below. Divide by two due to paired sampling
      replace = TRUE,
      prob = p
    )

    feature_sample <- sample_features_cpp(m, n_features_sample)
    feature_sample_paired <- lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)
    feature_sample_all <- c(feature_sample_all, feature_sample, feature_sample_paired)
    unique_samples <- length(unique(feature_sample_all))
    #iters = iters + 1
    #print(c(iters, unique_samples))
    #if (iters %% 250 == 0) message(paste0("Iter: ", iters, "\t Samples: ", length(feature_sample_all) ,"\t Unique samples:", unique_samples))
  }
  results_paired[b] = length(feature_sample_all)


}
mean(results_paired)
sd(results_paired) / sqrt(B)



# Compare ---------------------------------------------------------------------------------------------------------
qq = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Samp_prop_M_10_res.rds")
qq[n_combinations == 4]








# Tester nye ting -----------------------------------------------------------------------------------------------------------
library(data.table)
p = c(2,10,2,2,1,1,1)
N_s = 5
combs = utils::combn(length(p), N_s, simplify = FALSE)
dt = data.table::rbindlist(lapply(combs, function(x) {
  tb = table(p[x])
  dt = data.table::data.table(t(as.vector(tb)))
  data.table::setnames(dt, names(tb))
}), fill = TRUE)
dt[is.na(dt)] = 0
dt
unique(dt)







m = 5

# Get all coalition sizes
n_features <- seq(m - 1)

# Get the number of coalitions of each coalition size
n <- sapply(n_features, choose, n = m)

# Get the probabilities for each coalition
all_probs = rep(shapr:::shapley_weights(m = m, N = n, n_features), times = n)

# Consider only the first half of the coalitions as they are paired.
# Each coalition should then have twice the prob of being samples, but we normalize them afterwards, so skip it.
all_probs = all_probs[seq(length(all_probs) / 2)]



N_s = 10
all_probs[1] = 10
combs = utils::combn(length(all_probs), N_s, simplify = FALSE)
dt = data.table::rbindlist(lapply(combs, function(x) {
  tb = table(all_probs[x])
  dt = data.table::data.table(t(as.vector(tb)))
  data.table::setnames(dt, names(tb))
}), fill = TRUE)
dt[is.na(dt)] = 0
dt_unique = unique(dt)
dt
dt_unique


n2 = n[seq(ceiling((m - 1)/2))]
n2[1] = n2[1] - 1
sapply(n2, function(x) min(c(N_s, x)))
min(c(N_s, n2))


# Use only the even values of N_s
N_s_vector = N_s_vector[N_s_vector %% 2 == 0]

# Divide by two as we are doing paired
N_s_vector = N_s_vector / 2



# The alphabet of possible letter
alphabet = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")

# The probabilities of sampling each letter
prob = c(0.3, 0.175, 0.15, 0.125, 0.1, 0.065, 0.04, 0.025, 0.015, 0.005)

# Number of repetitions
B = 50000

# 3D to store the results
freq = array(NA, dim = c(length(alphabet), B, length(alphabet)))

# Iterate over the word lengths and the repetitions
for (word_length in seq(length(alphabet))) {
  print(word_length)
  for (b in seq(B)) {

    # Number of unique letters in the sampled "word"
    unique = 0
    word = c()

    # Sample new letters until the number of unique letters in the word equals the word_length
    while (unique < word_length) {
      new_letters = sample(x = alphabet, size = word_length - unique, replace = TRUE, prob = prob)
      word = c(word, new_letters)
      unique = length(unique(word))
    }

    # Compute the frequencies of each letter in the word
    freq[word_length, b, ] = sapply(alphabet, function(letter) mean(word == letter))
  }
}

# Compute the mean over the repetitions for each word length
expected_freq = sapply(seq(length(alphabet)), function(word_length) apply(freq[word_length,,], 2, mean))

# Plot the results
library(ggplot2)
library(data.table)
dt = data.table(expected_freq)
setnames(dt, paste(seq(length(alphabet))))
dt[, `:=` (letter = alphabet) ]
dt = melt(dt, id = "letter", variable.name = "word_length_unique", value.name = "expected_frequence")
ggplot(data = dt, aes(x = word_length_unique, y = expected_frequence, group = letter, col = letter)) +
  geom_line() +
  labs(x = "Unique letters in word", y = "Expected frequency of letter in word", col = "Letter")

