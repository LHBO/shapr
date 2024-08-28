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


expected_draws_unique = function(m, N_s_vector = seq(2, 2^m - 2)) {
  # library(gmp)

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
  N_draws = rep(NA, length(N_s_vector))

  # Vector to store the second sum of probabilities for each subset size of coalitions.
  sum_Pj_values = rep(NA, N_s_vector[length(N_s_vector)] - 1)

  # Iterate over the sizes and compute the probabilities
  for (q in seq(N_s_vector[length(N_s_vector)] - 1)) {
    message(paste0("q = ", q, "\t combinations = ", choose(N_coal_all, q)))

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


    time_3 = system.time({
      rel_combs = find_combinations(target = q, parts = m-1, part_sizes = n)
      mode(rel_combs) <- "double"
      nrow(rel_combs)

      kk = t(compositions(q, m-1))
      nrow(kk)


      jj = kk[apply(kk, 1, FUN = function(x) all(x <= n)),]
      nrow(jj)
      rel_combs = jj

      comb_per_coal_size = t(sapply(seq(nrow(rel_combs)), function(row_id) sapply(seq(ncol(rel_combs)), function(col_id) gmp::chooseZ(n[col_id], rel_combs[row_id, col_id]))))
      comb_per_coal_size_prod = apply(comb_per_coal_size, 1, prod)
      #if (sum(comb_per_coal_size_prod) != choose(N_coal_all, q)) stop("incorrect lengths")
      print(sum(comb_per_coal_size_prod) - gmp::chooseZ(N_coal_all, q))
      as.double(gmp::chooseZ(N_coal_all, q) + 1:100)


      prob_per_coal_size = sapply(seq(nrow(rel_combs)), function(row_id) {
        coeff = rel_combs[row_id,]
        coeff[coeff == 0] = NA
        1 / (1 - sum(coeff * dt_probs$prob, na.rm = TRUE))
      })

      #print(sum_Pj_values[q])
      sum_Pj_values[q] = sum(prob_per_coal_size * comb_per_coal_size_prod)
      #print(sum_Pj_values[q])
      })
    message(paste0("Time probabilities: ",  paste(names(time_3)[1:3], "=", round(time_3, 3)[1:3], collapse = ", ")))

  }

  # Then iterate over N_s values (i.e., the values for which we want the expected number of draws needed to obtain)
  for (N_s_vector_idx in seq_along(N_s_vector)) {

    # Get the current N_s value
    N_s_now = N_s_vector[N_s_vector_idx]
    print(N_s_now)

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

  # Return the expected number of draws
  return(list(N_draws = N_draws, sum_Pj_values = sum_Pj_values))
}

pp = expected_draws_unique(m = 5)
pp = expected_draws_unique(m = 6)
expected_draws_unique(m = 5, N_s_vector = 2:9)
plot(pp$N_draws[1:14])


expected_draws = function(m, N_s_vector = seq(2, 2^m - 2 - 1), paired = TRUE) {
  # We use -2 due to empty and grand coalition and -1 as we do not do sampling when we use all coalitions
  # Remove all N_s values that are larger than the possible values
  N_s_vector = N_s_vector[N_s_vector < 2^m - 2 - 1]

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

res_unique_5 = expected_draws(m = 5, N_s_vector = 2:8, paired = FALSE)
res_paired_5 = expected_draws(m = 5, N_s_vector = 2:32, paired = TRUE)
res_unique_5$res_dt
res_paired_5$res_dt

res_paired_6 = expected_draws(m = 6, N_s_vector = 2:32, paired = TRUE)


2.0465 3.1434 4.2954 5.5076 6.7859 8.1368 9.5680
2.0000 4.1970 6.6289 9.3447

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

