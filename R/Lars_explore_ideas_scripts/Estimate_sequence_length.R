# In this file we compute empirical mean estimates of the length of a sequence until the sequence have N_S unique
# coalitions. Be careful if you want empty and grand included. We exclude them since we do not sample them in our
# sequence

# Functions -------------------------------------------------------------------------------------------------------
library(shapr)
library(data.table)
library(ggplot2)


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
# coalition_sampling_paired = function(m, n_combinations = 2^m - 2,  n_sample_scale = 5, return_coalitions = FALSE,
#                                      seed = NULL, verbose = TRUE) {
#   if (n_combinations > 2^m - 2) stop("n_combinations is larger than 2^m.")
#   if (!is.null(seed)) set.seed(seed)
#
#   # Find weights for given number of features
#   n_features <- seq(m - 1)
#   n <- sapply(n_features, choose, n = m)
#   w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
#   p <- w / sum(w)
#
#   # List to store all the sampled coalitions
#   all_coalitions = c()
#
#   # Variable to keep track of the number of unique coalitions
#   unique_coalitions = 0
#
#   # Variable to keep track of the iteration number
#   iteration = 1
#
#   # Loop until we have enough unique samples
#   while (unique_coalitions < n_combinations) {
#
#     # Sample the coalition sizes
#     n_features_sample <- sample(
#       x = n_features,
#       size = n_sample_scale*n_combinations,
#       replace = TRUE,
#       prob = p
#     )
#
#     # Sample the coalitions
#     feature_sample <- shapr:::sample_features_cpp(m, n_features_sample)
#
#     # Get the paired coalitions
#     feature_sample_paired <- lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)
#
#     # Merge the coalitions in alternating fashion as we do paired sampling (i.e., first is S and second is Sbar and so on)
#     coalitions = c(rbind(feature_sample, feature_sample_paired))
#
#     # Convert the coalitions to strings such that we can compare them
#     coalitions = sapply(coalitions, paste, collapse = ",")
#
#     # Add the new coalitions to the previously sampled coalitions
#     all_coalitions = c(all_coalitions, coalitions)
#
#     # Get the cumulative number of unique coalitions for each coalition in all_coalitions
#     dt_cumsum = data.table(coalitions = all_coalitions, N_S = cumsum(!duplicated(all_coalitions)))[, L := .I]
#
#     # Extract rows where the N_S value increases (i.e., where we sample a new unique coalition)
#     dt_N_S_and_L <- dt_cumsum[N_S != shift(N_S, type = "lag", fill = 0)]
#
#     # Get the number of unique coalitions
#     unique_coalitions = dt_N_S_and_L[.N, N_S]
#
#     # Message to user
#     if (verbose) {
#       message(paste0("Iteration ", iteration, ": N_S = ", unique_coalitions,
#                      ", Sampled = ", n_sample_scale*n_combinations*iteration, "."))
#     }
#
#     # Update the iteration number
#     iteration = iteration + 1
#   }
#
#   # Post processing: keep only the coalitions until n_combinations
#   all_coalitions = all_coalitions[seq(dt_N_S_and_L[N_S == n_combinations, L])]
#   if (length(unique(all_coalitions)) != n_combinations) stop("Not the right number of unique coalitions")
#
#   # Return
#   if (return_coalitions) {
#     return(list(dt_N_S_and_L = dt_N_S_and_L, all_coalitions = all_coalitions))
#   } else {
#     return(dt_N_S_and_L)
#   }
# }

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
    message("Getting the coalition sizes")
    n_features_sample <- sample(
      x = n_features,
      size = n_sample_scale*n_combinations,
      replace = TRUE,
      prob = p
    )

    # Sample the coalitions
    message("Getting coalitions")
    feature_sample <- shapr:::sample_features_cpp(m, n_features_sample)

    # Get the paired coalitions
    message("Making the paired")
    feature_sample_paired <- lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)

    # Merge the coalitions in alternating fashion as we do paired sampling (i.e., first is S and second is Sbar and so on)
    coalitions = c(rbind(feature_sample, feature_sample_paired))

    # Convert the coalitions to strings such that we can compare them
    message("Converting to strings")
    coalitions = sapply(coalitions, paste, collapse = ",")

    # Add the new coalitions to the previously sampled coalitions
    all_coalitions = c(all_coalitions, coalitions)

    # Get the cumulative number of unique coalitions for each coalition in all_coalitions
    message("Getting cumsum")
    dt_cumsum = data.table(coalitions = all_coalitions, N_S = cumsum(!duplicated(all_coalitions)))[, L := .I]

    # Extract rows where the N_S value increases (i.e., where we sample a new unique coalition)
    message("Getting shift")
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


repeated_coalition_sampling = function(m, repetitions, n_combinations = 2^m - 2, n_sample_scale = 5, verbose = TRUE, verbose_extra = NULL) {
  if (m == 20) n_combinations = 1048500
  dt = data.table::rbindlist(
    lapply(repetitions, function(repetition) {
      if (verbose){
        string = paste0("Working on repetition ", repetition, " of ", length(repetitions), ".")
        if (!is.null(verbose_extra)) string = paste(verbose_extra, string)
        message(string)
      }
      coalition_sampling_paired(m = m,
                                n_combinations = n_combinations,
                                n_sample_scale = n_sample_scale,
                                seed = repetition,
                                verbose = verbose)[,-"coalitions"]}
    ), use.names = TRUE, idcol = "Repetition")
  dt[, Repetition := repetitions[Repetition]]
  return(dt)
}

sum_shapley_weights <- function(m){
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  return(sum(w))
}


#' Title
#'
#' Get the normalised ps values for each coalition size or coalition
#'
#' @param m_seq vector of positive integers
#'
#' @return
#' @export
#'
#' @examples
get_exact_ps_values = function(m_seq, M_as_factor = TRUE, normalize = c("coalition_size", "coalition")) {
  normalize = match.arg(normalize)
  tmp_list = lapply(m_seq, function(m) {
    tmp = shapr:::shapley_weights(m = m,
                                  N = sapply(seq(m - 1), choose, n = m),
                                  n_components = seq(m - 1))
    if (normalize == "coalition_size") tmp = tmp/sum(tmp)
    if (normalize == "coalition") tmp = tmp/sum_shapley_weights(m)
    data.table(N_S = 2^m,
               Size = factor(seq(ceiling((m-1)/2))),
               weight = tmp[seq(1, ceiling((m-1)/2))])
  })
  names(tmp_list) = m_seq
  tmp_list = data.table::rbindlist(tmp_list, idcol = "M")
  if (M_as_factor) tmp_list[, M := factor(M, levels = m_seq, labels = paste0("M = ", m_seq))]
  return(tmp_list)
}


#' Convert from normalized weights for the coalition size to the normalized weights on coalition level
#'
#' @param weights
#' @param m
#'
#' @return
#' @export
#'
#' @examples
#' m = 10
#' aa = get_exact_ps_values(m, normalize = "coalition")
#' print(aa)
#' coalition_size_scalars = c(rep(2, floor((m-1)/2)), if(m %% 2 == 0) 1)
#' coalition_size_combinations = choose(m, seq(floor(m/2)))
#' sum(aa[, weight] * coalition_size_scalars * coalition_size_combinations)
#'
#' bb = get_exact_ps_values(m, normalize = "coalition_size")
#' print(bb)
#' sum(bb[, weight] * coalition_size_scalars)
#'
#' all.equal(correct_normalization(weights = bb[, weight], m = m), aa[, weight])
correct_normalization = function(weights, m) {
  coalition_size_scalars = c(rep(2, floor((m-1)/2)), if(m %% 2 == 0) 1)
  coalition_size_combinations = choose(m, seq(floor(m/2)))
  weights / sum(weights * coalition_size_scalars * coalition_size_combinations)
}


m = 20
aa = get_exact_ps_values(m, normalize = "coalition")
print(aa)
coalition_size_scalars = c(rep(2, floor((m-1)/2)), if(m %% 2 == 0) 1)
coalition_size_combinations = choose(m, seq(floor(m/2)))
sum(aa[, weight] * coalition_size_scalars * coalition_size_combinations)

bb = get_exact_ps_values(m, normalize = "coalition_size")
print(bb)
sum(bb[, weight] * coalition_size_scalars)

all.equal(correct_normalization(weights = bb[, weight], m = m), aa[, weight])



# dt_combined[M == "M = 10" & type == "upper" & version == "mean ps" & N_S == 104, correct_normalization(Ps_tilde, m = 10)]


#' Title
#'
#' It takes to long to plot all N_S values. This function reduces the number of N_S values
#'
#' @param dt_combined data.table
#' @param m_seq_reduce vector of integers. The dimension where we want to reduce the number of N_S values
#' @param length.want integer. How many N_S values we want
#'
#' @return
#' @export
#'
#' @examples
dt_combined_short_fun = function(dt_combined, m_seq_reduce, length.want = 1000) {
  for (m in m_seq_reduce) {
    message(paste0("Reducing M = ", m, "."))
    length.inc = round(dt_combined[M %in% m,][.N, N_S] / length.want) +
      round(dt_combined[M %in% m,][.N, N_S] / length.want) %% 2
    keep_row_idx = unique(c(seq(2, dt_combined[M %in% m,][.N, N_S], length.inc), dt_combined[M %in% m,][.N, N_S]))
    dt_combined = dt_combined[dt_combined[,xor(!M %in% m, M %in% m & N_S %in% keep_row_idx)],]
  }
  return(dt_combined)
}


# Code starts here ------------------------------------------------------------------------------------------------
# Get the name of the computer we are working on
hostname = R.utils::System$getHostname()
cat(sprintf("We are working on '%s'.\n", R.utils::System$getHostname()))

# Check if we are working on an UiO computer or not and define the correct folder based on system
if (hostname == "Larss-MacBook-Pro.local" || Sys.info()[[7]] == "larsolsen") {
  # Where the files are stored
  folder = "/Users/larsolsen/PhD/Paper3/shapr"
  folder_save = "/Users/larsolsen/PhD/Paper3/Paper3_save_location"
  UiO = FALSE
} else if (grepl("hpc.uio.no", hostname)) {
  # TBA
  folder_save = "/home/lholsen/Paper3/Paper3_save_location"
  UiO = TRUE
} else if (grepl("uio.no", hostname)) {
  folder = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr"
  folder_save = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location"
  UiO = TRUE
} else {
  stop("We do not recongize the system at which the code is run (not Lars's MAC, HPC, nor UiO).")
}

# module load R/4.2.1-foss-2022a
# cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts
# Rscript samp_freq_plots.R 15


args = commandArgs(trailingOnly = TRUE)

m_vec = as.character(args[1])
if (grepl(",", m_vec)) {
  m_vec = as.numeric(unlist(strsplit(m_vec, ",")))
} else {
  m_vec = unlist(strsplit(m_vec, ":"))
  if (length(m_vec) > 1) {
    m_vec = seq(as.numeric(m_vec[1]), as.numeric(m_vec[2]))
  } else {
    m_vec = as.numeric(m_vec)
  }
}
repetitions = as.character(args[2]) # 1:250
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
n_sample_scale = as.numeric(args[3]) # 10


# New version
repetitions_start = repetitions[1]
repetitions_end = repetitions[length(repetitions)]

# Rscript Estimate_sequence_length.R 20 1:50 40

# Rscript Estimate_sequence_length.R 20 1:25 40
# Rscript Estimate_sequence_length.R 20 26:50 40
# Rscript Estimate_sequence_length.R 20 51:75 40
# Rscript Estimate_sequence_length.R 20 76:100 40
#
# Rscript Estimate_sequence_length.R 20 101:125 40
# Rscript Estimate_sequence_length.R 20 126:250 40
# Rscript Estimate_sequence_length.R 20 151:275 40
# Rscript Estimate_sequence_length.R 20 176:200 40
#
# Rscript Estimate_sequence_length.R 20 201:225 40
# Rscript Estimate_sequence_length.R 20 226:250 40
# Rscript Estimate_sequence_length.R 20 251:275 40
# Rscript Estimate_sequence_length.R 20 276:300 40
#
# Rscript Estimate_sequence_length.R 20 301:325 40
# Rscript Estimate_sequence_length.R 20 326:350 40
# Rscript Estimate_sequence_length.R 20 351:375 40
# Rscript Estimate_sequence_length.R 20 376:400 40
#
# Rscript Estimate_sequence_length.R 20 401:425 40
# Rscript Estimate_sequence_length.R 20 426:450 40
# Rscript Estimate_sequence_length.R 20 451:475 40
# Rscript Estimate_sequence_length.R 20 476:500 40
#
# Rscript Estimate_sequence_length.R 20 501:525 40
# Rscript Estimate_sequence_length.R 20 526:550 40
# Rscript Estimate_sequence_length.R 20 551:575 40
# Rscript Estimate_sequence_length.R 20 576:600 40
# #
# Rscript Estimate_sequence_length.R 20 601:625 40
# Rscript Estimate_sequence_length.R 20 626:650 40
# Rscript Estimate_sequence_length.R 20 651:675 40
# Rscript Estimate_sequence_length.R 20 676:700 40
#
# Rscript Estimate_sequence_length.R 20 701:725 40
# Rscript Estimate_sequence_length.R 20 726:750 40
# Rscript Estimate_sequence_length.R 20 751:775 40
# Rscript Estimate_sequence_length.R 20 776:800 40
#
# Rscript Estimate_sequence_length.R 20 801:825 40
# Rscript Estimate_sequence_length.R 20 826:850 40
# Rscript Estimate_sequence_length.R 20 851:875 40
# Rscript Estimate_sequence_length.R 20 876:900 40
#
# Rscript Estimate_sequence_length.R 20 901:925 40
# Rscript Estimate_sequence_length.R 20 926:950 40
# Rscript Estimate_sequence_length.R 20 951:975 40
# Rscript Estimate_sequence_length.R 20 976:1000 40
#
# module load R/4.2.2-foss-2022b
# cd Paper3


# Iterate over the number
for (m in m_vec) {
  dt = repeated_coalition_sampling(m = m, repetitions = repetitions, n_sample_scale = n_sample_scale, verbose_extra = paste0("M = ", m, "."))
  dt_avg = dt[, list(L_avg = mean(L)), by = N_S]
  saveRDS(list(dt = dt, dt_avg = dt_avg), file.path(folder_save, paste0("Sequence_length_M_", m, "_rep_", repetitions_start, "_to_", repetitions_end, ".rds")))
}

stop("DONE")



#
if (FALSE) {
  plot(dt_avg[,N_S], dt_avg[,L_avg])
  pp = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_10.rds")
  dt = copy(pp$dt)
  dt_avg = copy(pp$dt_avg)
  dt = dt[Repetition <= 100,]

  dt[, Repetition := as.factor(Repetition)]

  library(ggplot2)
  ggplot(dt, aes(x = N_S, y = L, colour = Repetition)) +
    geom_line()


  dt_avg

  m = 10

  # Find weights for given number of features
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  sum(shapr:::shapley_weights(m = m, N = n, n_features) / sum_shapley_weights(m) * n)

  sum_shapley_weights(m)

  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n


  ps = shapr:::shapley_weights(m = m, N = n, n_features) / sum_shapley_weights(m)

  ps

  L_max = 10000
  dt_L = data.table(L = seq(L_max),
                    t(sapply(seq(L_max), function(L) {
                      pstilde = 2*ps / (1 - (1 - 2*ps)^(L/2))
                      pstilde = pstilde / sum(pstilde)
                      pstilde
                    })))
  setnames(dt_L, old = paste0("V", n_features), paste0("Coal_size_", n_features))
  dt_L
  ps/sum(ps)


  # Jeg vil bruke mean L for hver N_S size.


  dt_L_avg = data.table(N_S = dt_avg$N_S,
                        t(sapply(dt_avg$L_avg, function(L) {
                          pstilde = 2*ps / (1 - (1 - 2*ps)^(L/2))
                          pstilde = pstilde / sum(pstilde)
                          pstilde
                        })))
  setnames(dt_L_avg, old = paste0("V", n_features), new = paste(n_features))

  dt_L_avg

  dt_L_avg_long = melt.data.table(dt_L_avg,
                                  id.vars = "N_S",
                                  value.name = "Ps_tilde",
                                  variable.name = "Size",
                                  variable.factor = FALSE)
  dt_L_avg_long[, Size := as.integer(Size)]
  dt_L_avg_long

  dt_L_avg_long_lower = dt_L_avg_long[Size <= ceiling((m - 1) / 2)]
  dt_L_avg_long_lower[, Size := as.factor(Size)]

  ggplot(dt_L_avg_long_lower, aes(x = N_S, y = Ps_tilde, colour = Size)) +
    geom_line()




  dt

  dt[N_S == 100, L]
  dt_ps_tilde_mean = data.table(N_S = seq(dt[, max(N_S)]),
                                t(sapply(seq(dt[, max(N_S)]), function(N_S_now) {
                                  colMeans(t(sapply(dt[N_S == N_S_now, L], function(L_now) {
                                    pstilde = 2*ps / (1 - (1 - 2*ps)^(L_now/2))
                                    pstilde = pstilde / sum(pstilde)
                                    pstilde
                                  })))
                                })))
  setnames(dt_ps_tilde_mean, old = paste0("V", n_features), new = paste(n_features))

  dt_ps_tilde_mean

  dt_ps_tilde_mean_long = melt.data.table(dt_ps_tilde_mean,
                                          id.vars = "N_S",
                                          value.name = "Ps_tilde",
                                          variable.name = "Size",
                                          variable.factor = FALSE)
  dt_ps_tilde_mean_long[, Size := as.integer(Size)]
  dt_ps_tilde_mean_long

  dt_ps_tilde_mean_long_lower = dt_ps_tilde_mean_long[Size <= ceiling((m - 1) / 2)]
  dt_ps_tilde_mean_long_lower[, Size := as.factor(Size)]

  ggplot(dt_ps_tilde_mean_long_lower, aes(x = N_S, y = Ps_tilde, colour = Size)) +
    geom_line()


  dt_combined_ps = rbind(copy(dt_ps_tilde_mean_long_lower)[, version := "mean ps_tilde"],
                         copy(dt_L_avg_long_lower)[, version := "mean L"])
  dt_combined_ps[, version := as.factor(version)]
  dt_combined_ps[, linewidth := ifelse(version == "mean ps_tilde", 1, 1.1)]


  ggplot(dt_combined_ps, aes(x = N_S, y = Ps_tilde, colour = Size)) +
    geom_line(aes(linetype = version)) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    scale_x_continuous(labels = scales::label_number()) +
    scale_linetype_manual(values=c("twodash", "dotted"))

  dt_combined_ps[N_S == 100, ]
  dt_combined_ps[, c("N_S", "Ps_tilde")]


  dt_diff = dcast(dt_combined_ps, N_S + Size ~ version, value.var = "Ps_tilde")[, diff := `mean ps_tilde` - `mean L`]
  dt_diff

  ggplot(dt_diff, aes(x = N_S, y = diff, colour = Size)) +
    geom_line() +
    scale_x_continuous(labels = scales::label_number())


}

if (FALSE) {
  ## Plot L avg ------------------------------------------------------------------------------------------------------
  # Make figure of the L values (lengths of sequence of coalitions)
  library(data.table)
  library(ggplot2)
  m_seq = 7:20

  # Load data and compute mean, lower, and upper quantiles.
  dt_extended = data.table(M = numeric(), N_S = integer(), L_avg = numeric(), lower = numeric(), median = numeric(), upper = numeric())
  m_idx = 1
  for (m_idx in seq_along(m_seq)) {
    m = m_seq[m_idx]
    message(paste0("Working on m = ", m, " (", m_idx, " of ", length(m_seq)  ,")."))
    #if (file.exists(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, "_L_mean_upper_lower.rds"))) next

    # Read in the file
    file = readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, ".rds"))
    dt = copy(file$dt)
    dt_avg = copy(file$dt_avg)

    dt_extended_now = dt[, as.list(c(L_avg = mean(L), quantile(L, c(0.025, 0.5, 0.975)))), by = N_S]
    setnames(dt_extended_now, c("2.5%", "50%", "97.5%"), c("lower", "median", "upper"))
    dt_extended_now = dt_extended_now[N_S %% 2 == 0, ][, M := m]
    setcolorder(dt_extended_now, "M")

    dt_extended = rbind(dt_extended, dt_extended_now)

    # Save
    saveRDS(dt_extended_now, paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, "_L_mean_upper_lower.rds"))
  }

  # Read the file
  dt_extended2 = data.table::rbindlist(
    lapply(m_seq, function(m) readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, "_L_mean_upper_lower.rds")))
  )
  dt_extended = copy(dt_extended2)

  # Read the file
  dt_analytical2 = data.table::rbindlist(
    lapply(7:10, function(m) {
      dt_tmp = readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, "_version_paired_analytical.rds"))$res_dt
      dt_tmp[, N_draws := gmp::asNumeric(gmp::as.bigq(N_draws))]
      return(dt_tmp)
      }), use.names = TRUE, idcol = "M"
  )
  setnames(dt_analytical2, "N_s", "N_S")
  dt_analytical2[, N_S := as.integer(N_S)]
  dt_analytical2[, M := paste0("M = ", M + 6)]
  dt_analytical2[, M := factor(M)]
  dt_analytical = copy(dt_analytical2)

  # Remove extra N_S
  dt_extended = dt_combined_short_fun(dt_extended, m_seq_reduce = 15:20)
  dt_extended[M %in% 15:20, .N, by = M]
  dt_extended[, M := factor(M, levels = m_seq, labels = paste0("M = ", m_seq))]


  # Make the plot
  fig_L = ggplot(dt_extended, aes(x = N_S, y = L_avg)) +
    geom_line() +
    geom_line(data =  dt_analytical, aes(x = N_S, y = N_draws),  col = "red", linetype = "dashed", linewidth = 0.75) +
    geom_ribbon(aes(x = N_S, ymin = lower, ymax = upper), alpha = 0.4, linewidth = 0.1) +
    scale_x_continuous(labels = scales::label_number()) +
    facet_wrap("M ~ .", ncol = 2, scales = "free") +
    labs(x = expression(N[S]), y = bquote(E*"[L] and"~bar(L)~"with 95% confidence bands for L")) +
    theme(legend.position="bottom", legend.box = "horizontal") +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.35))) +
    scale_y_continuous(labels = scales::label_number())
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    )
  fig_L

  # Save the figure
  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Expected_L_v5_reg.png",
         plot = fig_L,
         width = 14.2,
         height = 18,
         scale = 0.85,
         dpi = 350)

  # Largest values
  dt_extended_largest = dt_extended[,.SD[.N], by = M][,c("M", "N_S", "L_avg")]
  dt_extended_largest[, ratio := L_avg / shift(L_avg, type = "lag")]
  dt_extended_largest
  dt_extended_largest[, mean(ratio, na.rm = TRUE)]


  # Difference
  dt_extended[, `:=` ("new_lower" = lower - L_avg, "new_upper" = upper - L_avg, "new_L_avg" = L_avg - L_avg)]
  dt_extended
  dt_analytical3 = merge(dt_analytical, dt_extended, by = c("M", "N_S"), sort = FALSE, all.y = TRUE)
  dt_analytical3[, new_N_draws := N_draws - L_avg]

  dt_analytical4 = dt_analytical3[M %in% paste0("M = ", 7:10),]
  dt_analytical4[, M := factor(M, labels = paste0("M = ", 7:10), levels = paste0("M = ", 7:10))]

  library(ggallin)
  fig_L_diff = ggplot(dt_analytical4, aes(x = N_S, y = new_L_avg)) +
    geom_line() +
    geom_line(aes(x = N_S, y = new_N_draws),  col = "red", linetype = "dashed", linewidth = 0.75) +
    geom_ribbon(aes(x = N_S, ymin = new_lower, ymax = new_upper), alpha = 0.4, linewidth = 0.1) +
    scale_x_continuous(labels = scales::label_number()) +
    facet_wrap("M ~ .", ncol = 4, scales = "free") +
    labs(x = expression(N[S]), y = bquote("Difference between"~bar(L)~"and"~E*"[L] and quantiles")) +
    theme(legend.position="bottom", legend.box = "horizontal") +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.4)),
          legend.text = element_text(size = rel(1.4)),
          axis.title = element_text(size = rel(1.3)),
          axis.text = element_text(size = rel(1.3))) +
    scale_y_continuous(labels = scales::label_number())
    # scale_y_continuous(trans = pseudolog10_trans,
    #                    labels = scales::label_number())
    # scale_y_log10(
    #   breaks = scales::trans_breaks("log10", function(x) 10^x),
    #   labels = scales::trans_format("log10", scales::math_format(10^.x))
    # )
  fig_L_diff

  # Save the figure
  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Expected_L_diff_Some.png",
         plot = fig_L_diff,
         width = 16,
         height = 5,
         scale = 0.85,
         dpi = 350)




  # Plot the denominator

  # List to store the final values
  m_seq = 3:20
  tmp_list_unnormalized = lapply(m_seq, function(m) {
    n = sapply(seq(m - 1), choose, n = m)
    tmp = shapr:::shapley_weights(m = m,
                                  N = n,
                                  n_components = seq(m - 1))
    tmp = tmp / sum(tmp * n)

    data.table(N_S = 2^m,
               Size = factor(seq(ceiling((m-1)/2))),
               weight = tmp[seq(1, ceiling((m-1)/2))])
  })
  names(tmp_list_unnormalized) = m_seq
  tmp_list_unnormalized = data.table::rbindlist(tmp_list_unnormalized, idcol = "M")
  tmp_list_unnormalized[, M := as.numeric(M)]
  tmp_list_unnormalized[, weight2 := 1-weight]


  ggplot(data = tmp_list_unnormalized, aes(x = M, y = weight, col = Size)) +
    geom_line() +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    )

  ggplot(data = tmp_list_unnormalized, aes(x = M, y = weight2, col = Size)) +
    geom_line() +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    )


  tmp_list_unnormalized2 = copy(tmp_list_unnormalized)
  tmp_list_unnormalized2[, M := factor(M, levels = m_seq, labels = paste0("M = ", m_seq))]
  tmp_list_unnormalized2


  dt1 = dt_extended[, c("M", "N_S", "L_avg")]
  dt2 = tmp_list_unnormalized2[, c("M", "Size", "weight", "weight2")]
  dt3 = merge(dt1,
              dt2,
              by = "M",
              all.x = TRUE,
              allow.cartesian = TRUE)
  dt3[, weight3 := weight2^(L_avg/2)]
  dt3[, weight4 := 1 - weight3]
  dt3[, weight5 := 2*weight/weight4]
  dt3

  dt3[, weight_imp2 := weight2^(N_S/2)]
  dt3[, weight_imp3 := 1 - weight_imp2]
  dt3[, weight_imp4 := 2*weight/weight_imp3]


  dt3[M == "M = 10" & N_S == 1022, ]

  $weight5



  # Plot (1-pS)^E[L] ------------------------------------------------------------------------------------------------
  fig_diff_exp = ggplot(dt3, aes(x = N_S, y = weight3, col = Size)) +
    geom_line() +
    facet_wrap("M ~ .", ncol = 2, scales = "free") +
    labs(y = expression((1 - p[S])^{"E[L]"}), x = expression(N[S])) +
    theme(legend.position="bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
           color = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1)) +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.4))) +
    scale_x_continuous(labels = scales::label_number()) +
    scale_y_continuous(labels = scales::label_number())
  fig_diff_exp

  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/One_minus_PS_exp_expected_L_v2.png",
         plot = fig_diff_exp,
         width = 14.2,
         height = 20,
         scale = 0.85,
         dpi = 350)


  # Plot 1-(1-pS)^E[L] ------------------------------------------------------------------------------------------------
  fig_diff_exp_one_minus = ggplot(dt3, aes(x = N_S, y = weight4, col = Size)) +
    geom_line() +
    facet_wrap("M ~ .", ncol = 2, scales = "free") +
    labs(y = expression(1 - (1 - p[S])^{"E[L]"}), x = expression(N[S])) +
    theme(legend.position="bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
           color = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1)) +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.4))) +
    scale_x_continuous(labels = scales::label_number()) +
    scale_y_continuous(labels = scales::label_number())
  fig_diff_exp_one_minus

  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/One_minus_One_minus_PS_exp_expected_L_v2.png",
         plot = fig_diff_exp_one_minus,
         width = 14.2,
         height = 20,
         scale = 0.85,
         dpi = 350)


  fig_diff_exp_one_minus_N_S = ggplot(dt3, aes(x = N_S, y = weight_imp3, col = Size)) +
    geom_line() +
    facet_wrap("M ~ .", ncol = 2, scales = "free_x") +
    labs(y = expression(1 - (1 - p[S])^{N[S]}), x = expression(N[S])) +
    theme(legend.position="bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
           color = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1)) +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.4))) +
    scale_x_continuous(labels = scales::label_number()) +
    scale_y_continuous(labels = scales::label_number())
  fig_diff_exp_one_minus_N_S

}


if (FALSE) {

  ## Create the files ------------------------------------------------------------------------------------------------
  m_seq = 7:20

  normalize_version = "coalition"
  normalize_version = "coalition_size"

  dt_avg_L = data.table(M = numeric(), N_S = numeric(), type = character(), Size = numeric(), Ps_tilde = numeric())
  dt_avg_ps = data.table(M = numeric(), N_S = numeric(), type = character(), Size = numeric(), Ps_tilde = numeric())
  dt_combined = data.table(M = numeric(), N_S = numeric(), type = character(), Size = numeric(), Ps_tilde = numeric(), version = factor())
  dt_diff = data.table(M = numeric(), N_S = numeric(), Size = numeric(), "mean L" = numeric(), "mean ps" = numeric(), diff = numeric())

  m_idx = 1
  for (m_idx in seq_along(m_seq)) {
    m = m_seq[m_idx]
    message(paste0("Working on m = ", m, " (", m_idx, " of ", length(m_seq)  ,")."))
    #if (m <= 17) next

    # Create the ps values
    n_features <- seq(m - 1)
    n <- choose(n = m, k = seq(m-1))
    ps = shapr:::shapley_weights(m = m, N = n, n_features) / sum_shapley_weights(m)

    # Read in the file
    file = readRDS(file.path(folder_save, paste0("Sequence_length_M_", m, ".rds")))
    dt = copy(file$dt)
    dt_avg = copy(file$dt_avg)
    print(object.size(dt), units = "GB")

    # values needed for normalizing


    ### Get the pstilde valued when using the average L
    dt_L_avg = data.table(N_S = dt_avg$N_S[dt_avg$N_S %% 2 == 0],
                          type = "mean",
                          t(sapply(dt_avg$L_avg[seq(2, length(dt_avg$L_avg), 2)], function(L) {
                            pstilde = 2*ps / (1 - (1 - 2*ps)^(L/2))
                            if (normalize_version == "coalition") pstilde = pstilde / sum(pstilde * n)
                            if (normalize_version == "coalition_size") pstilde = pstilde / sum(pstilde)
                            pstilde
                          })))
    setnames(dt_L_avg, old = paste0("V", n_features), new = paste(n_features))

    # Convert from wide to long
    dt_L_avg_long = melt.data.table(dt_L_avg, id.vars = c("N_S", "type"), value.name = "Ps_tilde", variable.name = "Size")

    # Extract only the lower coalition sizes with unique values
    dt_L_avg_long[, Size := as.integer(Size)]
    dt_L_avg_long_lower = dt_L_avg_long[Size <= ceiling((m - 1) / 2)]
    # dt_L_avg_long_lower[, Size := as.factor(Size)]

    # Add dimension, reorder, and add to the overall data.table
    dt_L_avg_long_lower[, M := m]
    setcolorder(dt_L_avg_long_lower, c("M"))
    dt_avg_L = rbind(dt_avg_L, dt_L_avg_long_lower)


    ### Get the average pstilde values
    dt_ps = rbindlist(lapply(seq(2, dt[, max(N_S)], 2), function(N_S_now) {
      message(N_S_now)
      if (N_S_now %% 10000 == 0) message(N_S_now)
      # Iterate over all values of L and compute the corresponding ps values
      ps_hat = t(sapply(dt[N_S == N_S_now, L], function(L_now) {
        pstilde = 2*ps / (1 - (1 - 2*ps)^(L_now/2))
        if (normalize_version == "coalition") pstilde = pstilde / sum(pstilde * n)
        if (normalize_version == "coalition_size") pstilde = pstilde / sum(pstilde)
        pstilde
      }))
      # Create a data table with the mean, 2.5% percentile, median, and 97.5% percentile
      data.table(type = c("mean", "lower", "median", "upper"),
                 rbind(colMeans(ps_hat), apply(ps_hat, 2, quantile, probs = c(0.025, 0.5, 0.975))))
    }),
    use.names = TRUE,
    idcol = "N_S")
    dt_ps[, N_S := rep(seq(2, dt[, max(N_S)], 2), each = 4)]
    setnames(dt_ps, old = paste0("V", n_features), new = paste(n_features))

    # old version without quantiles
    # dt_ps = data.table(N_S = seq(dt[, max(N_S)]),
    #                               t(sapply(seq(dt[, max(N_S)]), function(N_S_now) {
    #                                 colMeans(t(sapply(dt[N_S == N_S_now, L], function(L_now) {
    #                                   pstilde = 2*ps / (1 - (1 - 2*ps)^(L_now/2))
    #                                   pstilde = pstilde / sum(pstilde)
    #                                   pstilde
    #                                 })))
    #                               })))

    # Wide to long
    dt_ps_long = melt.data.table(dt_ps, id.vars = c("N_S", "type"), value.name = "Ps_tilde", variable.name = "Size")

    # Only get the lower coalition sizes
    dt_ps_long[, Size := as.integer(Size)]
    dt_ps_long_lower = dt_ps_long[Size <= ceiling((m - 1) / 2)]
    # dt_ps_long_lower[, Size := as.factor(Size)]

    # Add dimension, reorder, and add to the overall data.table
    dt_ps_long_lower[, M := m]
    setcolorder(dt_ps_long_lower, c("M"))
    dt_avg_ps = rbind(dt_avg_ps, dt_ps_long_lower)


    ### Combine
    dt_combined_ps = rbind(copy(dt_ps_long_lower)[, version := "mean ps"],
                           copy(dt_L_avg_long_lower)[, version := "mean L"])
    dt_combined_ps[, version := as.factor(version)]
    dt_combined = rbind(dt_combined, dt_combined_ps)

    ### Difference
    # dt_diff_now = dcast(dt_combined_ps, M + N_S + Size + type ~ version, value.var = "Ps_tilde")[, diff := `mean L` - `mean ps`]
    # dt_diff_now

    dt_diff_now = dcast(dt_combined_ps[type == "mean",-"type"], M + N_S + Size ~ version, value.var = "Ps_tilde")[, diff := `mean L` - `mean ps`]
    dt_diff = rbind(dt_diff, dt_diff_now)

    # Save to disk
    saveRDS(dt_combined_ps, paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, "_normalized_", normalize_version, "_combined.rds"))
    saveRDS(dt_diff_now, paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, "_normalized_", normalize_version, "_diff.rds"))
  }



  # Load the data files ---------------------------------------------------------------------------------------------
  m_seq = 7:20
  #m_seq = 19:20
  folder = "/Users/larsolsen/PhD/Paper3/Paper3_save_location"
  normalize_version = "coalition_size"
  normalize_version = "coalition"

  dt_combined3 =
    rbindlist(lapply(m_seq, function(m) readRDS(file.path(folder, paste0("Sequence_length_M_", m, "_normalized_", normalize_version, "_combined.rds")))))

  dt_diff3 =
    rbindlist(lapply(m_seq, function(m) readRDS(file.path(folder, paste0("Sequence_length_M_", m, "_normalized_", normalize_version, "_diff.rds")))))

  dt_combined = copy(dt_combined3)
  dt_diff = copy(dt_diff3)

  # Only want even N_S
  dt_combined[, Size := as.factor(Size)]
  dt_combined = dt_combined[N_S %% 2 == 0,]

  # Reduce
  m_seq_reduce = 16:20
  dt_combined[M %in% m_seq_reduce, .N, by = M]
  dt_combined = dt_combined_short_fun(dt_combined = dt_combined, m_seq_reduce = m_seq_reduce)
  dt_combined[M %in% m_seq_reduce, .N, by = M]

  # Convert
  dt_combined[, type := factor(type)]
  dt_combined[, M := factor(M, levels = m_seq, labels = paste0("M = ", m_seq))]
  dt_combined[, N_S := N_S + 2]

  # Create a data table containing the confidence bands
  data_ribbon = dcast(dt_combined[type %in% c("lower", "upper")], M + N_S + Size ~ type, value.var = "Ps_tilde")
  #data_ribbon[, M := factor(M, levels = m_seq, labels = paste0("M = ", m_seq))]

  # data table with the final ps values for each coal size
  dt_exact_ps = get_exact_ps_values(m_seq, normalize = normalize_version)






  # Plot pS values --------------------------------------------------------------------------------------------------
  # Make the plot
  fig_ps_bands = ggplot(data = dt_combined) +
    facet_wrap(M ~ ., ncol = 2, scales = "free") +
    geom_ribbon(data = data_ribbon,
                mapping = aes(x = N_S, ymin = lower, ymax = upper, colour = Size, fill = Size),
                alpha = 0.4, linewidth = 0.1) +
    geom_line(data = dt_combined[type == "mean" & version == "mean ps"],
              mapping = aes(x = N_S, y = Ps_tilde, colour = Size, group = Size, linetype = "dashed"), linewidth = 1) +
    geom_line(data = dt_combined[type == "mean" & version == "mean L"],
              mapping = aes(x = N_S, y = Ps_tilde, group = Size, linetype = "solid"),
              color = "black",
              linewidth = 0.4) +
    geom_point(dt_exact_ps,
               mapping = aes(x = N_S, y = weight, colour = Size),
               size = 2) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      # labels = scales::trans_format("log10", scales::math_format(10^.x))
      labels = scales::label_number()
    ) +
    guides(fill = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
           color = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
           linetype = guide_legend(title = "Version: ")) +
    labs(x = expression(N[S]), y = "Normalized weights") +
    scale_linetype_manual(values = c("solid", "dashed"), labels = c(expression(bar(p[S])(L)), expression(p[S](bar(L))))) +
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.4))) +
    scale_x_continuous(labels = scales::label_number())
  fig_ps_bands

  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Ps_tilde_confidence_bands_v9.png",
         plot = fig_ps_bands,
         width = 14.2,
         height = 20,
         scale = 0.85,
         dpi = 350)




  ## Paired Imp C-kernel ---------------------------------------------------------------------------------------------

  dt_cumsum = data.table::rbindlist(
    lapply(m_seq, function(m) {
      n <- sapply(seq(floor(m/2)), choose, n = m)
      n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
      n_cumsum = (cumsum(n) + 2) + 0.5
      #n_cumsum = n_cumsum[-length(n_cumsum)]
      data.table(M = paste0("M = ", m), n_cumsum = n_cumsum)
    })
  )
  dt_cumsum[, M := factor(M)]




  # data table with the final ps values for each coal size
  dt_exact_ps = get_exact_ps_values(m_seq, normalize = normalize_version)


  dt_paired_imp_c_kernel = data.table(
    rbindlist(
      lapply(m_seq, function(m) {
        message(m)
        n <- sapply(seq(floor(m/2)), choose, n = m)
        n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
        n_cumsum = (cumsum(n) + 2) + 0.5

        dt_n_cumsum = data.table(N_S = c(0, n_cumsum), Size = lapply(seq(length(n_cumsum) + 1), function(x) seq(x)))
        remove_non_added_sizes = TRUE
        data.table(rbindlist(
          lapply(unique(round(seq(2, 2^m, length.out = 1000))),
                 function(L, m, n_cumsum, remove_non_added_sizes) {
                   n_features <- seq(m - 1)
                   n <- choose(n = m, k = seq(m-1))
                   shapley_weight = shapr:::shapley_weights(m = m, N = n, n_features) / sum_shapley_weights(m)
                   #shapley_weight = shapley_weight/sum(shapley_weight)

                   dt_tmp = data.table(
                     M = paste0("M = ", m),
                     N_S = L,
                     Size = seq(m-1),
                     Ps_tilde = 2*shapley_weight / (1-(1-2*shapley_weight)^(L/2))
                   )

                   if (normalize_version == "coalition") dt_tmp[, Ps_tilde := Ps_tilde / sum(Ps_tilde * n)]
                   if (normalize_version == "coalition_size") dt_tmp[, Ps_tilde := Ps_tilde / sum(Ps_tilde)]
                   dt_tmp = dt_tmp[Size <= floor(m/2),]
                   dt_tmp

                    if (remove_non_added_sizes) {
                      dt_tmp = dt_tmp[dt_n_cumsum[which(L < dt_n_cumsum$N_S)[1] - 1, Size][[1]],]
                    }

                    return(dt_tmp)
                 }, m = m, n_cumsum = n_cumsum, remove_non_added_sizes = remove_non_added_sizes)))
      })))

    dt_paired_imp_c_kernel[, M := factor(M)]
    dt_paired_imp_c_kernel[, Size := factor(Size)]

    fig_ps_all_strategies = ggplot(data = dt_combined) +
      facet_wrap(M ~ ., ncol = 2, scales = "free") +
      geom_vline(data = dt_cumsum, aes(xintercept = n_cumsum, group = M), col = "gray60", linetype = "dashed", linewidth = 0.4) +
      geom_ribbon(data = data_ribbon,
                  mapping = aes(x = N_S, ymin = lower, ymax = upper, colour = Size, fill = Size),
                  alpha = 0.4, linewidth = 0.1) +
      geom_line(data = dt_combined[type == "mean" & version == "mean ps"],
                mapping = aes(x = N_S, y = Ps_tilde, colour = Size, group = Size, linetype = "dashed"), linewidth = 0.85) +
      geom_line(data = dt_combined[type == "mean" & version == "mean L"],
                mapping = aes(x = N_S, y = Ps_tilde, group = Size, linetype = "dotted"), color = "black", linewidth = 0.85) +
      geom_line(data = dt_paired_imp_c_kernel,
                mapping = aes(x = N_S, y = Ps_tilde, colour = Size, group = Size, linetype = "solid"), linewidth = 0.85) +
      geom_point(dt_exact_ps,
                 mapping = aes(x = N_S, y = weight, colour = Size),
                 size = 2.5) +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x)),
        #labels = scales::label_number()
      ) +
      guides(fill = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1, order = 1),
             color = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1, order = 1),
             linetype = guide_legend(title = "Version: ", order = 2)) +
      labs(x = expression(N[S]), y = "Normalized weights") +
      # scale_linetype_manual(values = c("solid", "dotted", "dashed"),
      #                       labels = c(expression(bar(p)["|"*S*"|"](L)),
      #                                  expression(p["|"*S*"|"](bar(L))),
      #                                  expression(p["|"*S*"|"](N[S]))),
      #                       guide = guide_legend(override.aes = list(c("solid", "solid", "solid"),
      #                                                                lwd = c(0.5,0.1,0.4)))) +
      scale_linetype_manual(values = c("solid", "dotted", "dashed"),
                            labels = c(expression(bar(p)[S](L)),
                                       expression(p[S](bar(L))),
                                       expression(p[S](N[S])))) +
      theme(legend.position = "bottom",
            legend.box = "horizontal",
            legend.justification = "right",
            strip.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.4))) +
      scale_x_continuous(labels = scales::label_number())
    fig_ps_all_strategies

    ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Ps_tilde_all_strategies_v6.png",
           plot = fig_ps_all_strategies,
           width = 14.2,
           height = 20,
           scale = 0.85,
           dpi = 350)




    only_these_M = c(10, 11, 20)
    only_these_M = paste0("M = ", only_these_M)
    fig_ps_all_strategies_some_M = ggplot(data = dt_combined[M %in% only_these_M, ]) +
      facet_wrap(M ~ ., ncol = length(only_these_M), scales = "free") +
      geom_vline(data = dt_cumsum[M %in% only_these_M, ], aes(xintercept = n_cumsum, group = M), col = "gray60", linetype = "dashed", linewidth = 0.4) +
      geom_ribbon(data = data_ribbon[M %in% only_these_M, ],
                  mapping = aes(x = N_S, ymin = lower, ymax = upper, colour = Size, fill = Size),
                  alpha = 0.4, linewidth = 0.1) +
      geom_line(data = dt_combined[type == "mean" & version == "mean ps"][M %in% only_these_M, ],
                mapping = aes(x = N_S, y = Ps_tilde, colour = Size, group = Size, linetype = "dashed"), linewidth = 0.85) +
      geom_line(data = dt_combined[type == "mean" & version == "mean L"][M %in% only_these_M, ],
                mapping = aes(x = N_S, y = Ps_tilde, group = Size, linetype = "dotted"), color = "black", linewidth = 0.85) +
      geom_line(data = dt_paired_imp_c_kernel[M %in% only_these_M, ],
                mapping = aes(x = N_S, y = Ps_tilde, colour = Size, group = Size, linetype = "solid"), linewidth = 0.85) +
      geom_point(dt_exact_ps[M %in% only_these_M, ],
                 mapping = aes(x = N_S, y = weight, colour = Size),
                 size = 2.5) +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
        #labels = scales::label_number()
      ) +
      guides(fill = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1, order = 1),
             color = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1, order = 1),
             linetype = guide_legend(title = "Version: ", order = 2, nrow = 1)) +
      labs(x = expression(N[S]), y = "Normalized weights") +

      # scale_linetype_manual(values = c("solid", "dotted", "dashed"),
      #                       labels = c(expression(bar(p)["|"*S*"|"](L)),
      #                                  expression(p["|"*S*"|"](bar(L))),
      #                                  expression(p["|"*S*"|"](N[S])))) +
      scale_linetype_manual(values = c("solid", "dotted", "dashed"),
                            labels = c(expression(bar(p)[S](L)),
                                       expression(p[S](bar(L))),
                                       expression(p[S](N[S])))) +
      theme(legend.position = "bottom",
            legend.box = "horizontal",
            plot.margin = margin(t = 5.5,  # Top margin
                                 r = 8,  # Right margin
                                 b = 5.5,  # Bottom margin
                                 l = 5.5),
            strip.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.85)),
            legend.text = element_text(size = rel(1.85)),
            axis.title = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.33))) +
      scale_x_continuous(labels = scales::label_number())
    fig_ps_all_strategies_some_M

    ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Ps_tilde_all_strategies_some_M_V4.png",
           plot = fig_ps_all_strategies_some_M,
           width = 16.85,
           height = 7,
           scale = 0.85,
           dpi = 350)


    fig_ps_all_strategies_some_M2 = fig_ps_all_strategies_some_M + theme(legend.key.size = unit(0.96, "cm"))

    ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Ps_tilde_all_strategies_some_M_V3.png",
           plot = fig_ps_all_strategies_some_M2,
           width = 16.85,
           height = 7,
           scale = 0.85,
           dpi = 350)




  }




  ## Only some M values ----------------------------------------------------------------------------------------------
  M_plot_only = c(10, 11, 17)
  M_plot_only = paste0("M = ", M_plot_only)
  fig_ps_bands_reduced = ggplot(data = dt_combined[M %in% M_plot_only, ]) +
    facet_wrap(M ~ .,
               ncol = length(M_plot_only),
               scales = "free_x") +
    geom_ribbon(data = data_ribbon[M %in% M_plot_only, ],
                mapping = aes(x = N_S, ymin = lower, ymax = upper, colour = Size, fill = Size),
                alpha = 0.4, linewidth = 0.1) +
    geom_line(data = dt_combined[type == "mean" & version == "mean ps"][M %in% M_plot_only, ],
              mapping = aes(x = N_S, y = Ps_tilde, colour = Size, group = Size, linetype = "dashed"), linewidth = 1) +
    geom_line(data = dt_combined[type == "mean" & version == "mean L"][M %in% M_plot_only, ],
              mapping = aes(x = N_S, y = Ps_tilde, group = Size, linetype = "solid"),
              color = "black",
              linewidth = 0.4) +
    geom_point(dt_exact_ps[M %in% M_plot_only, ],
               mapping = aes(x = N_S, y = weight, colour = Size),
               size = 2) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      # labels = scales::trans_format("log10", scales::math_format(10^.x))
      labels = scales::label_number()
    ) +
    guides(fill = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
           color = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
           linetype = guide_legend(title = "Version: ")) +
    labs(x = expression(N[S]), y = "Normalized weights") +
    scale_linetype_manual(values = c("solid", "dashed"), labels = c(expression(bar(p[S])(L)), expression(p[S](bar(L))))) +
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.4))) +
    scale_x_continuous(labels = scales::label_number())

  fig_ps_bands_reduced

  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Samp_freq_M_10_11_17_bands_V1.png",
         plot = fig_ps_bands_reduced,
         width = 14.2,
         height = 7,
         scale = 0.85,
         dpi = 350)







  # dt_combined[, M := factor(M, levels = m_seq, labels = paste0("M = ", m_seq))]
  # dt_combined[, N_S := N_S + 2]
  # fig_combined =
  #   ggplot(dt_combined, aes(x = N_S, y = Ps_tilde, colour = Size, linetype = )) +
  #   geom_line(aes(linetype = version))
  #   geom_point(tmp_list,
  #              mapping = aes(x = N_S, y = weight, colour = Size),
  #              size = 2) +
  #   scale_x_continuous(labels = scales::label_number()) +
  #   facet_wrap("M ~ .", ncol = 2, scales = "free") +
  #   scale_y_log10(
  #     breaks = scales::trans_breaks("log10", function(x) 10^x),
  #     labels = scales::trans_format("log10", scales::math_format(10^.x))
  #   ) +
  #   guides(fill = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
  #          color = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
  #          linetype = guide_legend(title = "Version: ")) +
  #   scale_linetype_manual(values=c("twodash", "dotted"),
  #                         labels = c(expression(p[S](bar(L))), expression(bar(p[S])(L)))) +
  #   labs(x = expression(N[S]), y = "Normalized weights") +
  #   theme(legend.position="bottom", legend.box = "horizontal") +
  #   theme(strip.text = element_text(size = rel(1.5)),
  #         legend.title = element_text(size = rel(1.5)),
  #         legend.text = element_text(size = rel(1.5)),
  #         axis.title = element_text(size = rel(1.5)),
  #         axis.text = element_text(size = rel(1.4)))
  #
  # ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Ps_tilde_V6.png",
  #        plot = fig_combined,
  #        width = 14.2,
  #        height = 20,
  #        scale = 0.85,
  #        dpi = 350)



  # Plot difference in pS values ------------------------------------------------------------------------------------
  # Plot of difference
  dt_diff = copy(dt_diff3)
  dt_diff
  dt_diff[, Size := as.factor(Size)]
  dt_diff = dt_diff[N_S %% 2 == 0,]
  dt_diff = dt_combined_short_fun(dt_diff, m_seq_reduce = 16:20)
  dt_diff[, M := factor(M, levels = m_seq, labels = paste0("M = ", m_seq))]
  dt_diff[, N_S := N_S + 2]

  dt_diff[M %in% levels(M)[1:6],]

  fig_diff = ggplot(dt_diff, aes(x = N_S, y = diff, colour = Size)) +
    geom_hline(yintercept = 0, col = "darkgray") +
    geom_line() +
    scale_x_continuous(labels = scales::label_number()) +
    #scale_y_continuous(labels = scales::label_number()) +
    facet_wrap("M ~ .", ncol = 2, scales = "free") +
    guides(fill = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
           color = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1)) +
    labs(x = expression(N[S]), y = expression(p[S](bar(L)) - bar(p[S])(L))) +
    theme(legend.position="bottom", legend.box = "horizontal") +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.4)))
  fig_diff

  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Ps_tilde_diff_V7_free.png",
         plot = fig_diff,
         width = 14.2,
         height = 20,
         scale = 0.85,
         dpi = 350)

}






if (FALSE) {
  ## Old plot --------------------------------------------------------------------------------------------------------
  # Here I just checked that the new way of sampling was correct by comparing the sequence length
  # that I obtained previously with the new one.
  vv = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.92_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_estimated_repetition_2.rds")

  L_values = sapply(vv$unique_paired$repetition_1[-c(1, length(vv$unique_paired$repetition_1))], function(x) {
    x$only_save$X[-c(1,.N), sum(shapley_weight)]
  })

  dt_extra = data.table(N_S = as.integer(sub(".*_", "", names(L_values))), L = L_values)

  ggplot(dt, aes(x = N_S, y = L)) +
    geom_line(aes(colour = Repetition)) +
    geom_point(data = dt_extra, mapping = aes(x = N_S, y = L), colour = "black") +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    scale_x_continuous(labels = scales::label_number())


  ggplot(dt_avg, aes(x = N_S, y = L_avg)) +
    geom_line() +
    geom_point(data = dt_extra, mapping = aes(x = N_S, y = L), colour = "black")
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
    scale_x_continuous(labels = scales::label_number())


  vv$unique_paired$repetition_1$n_combinations_6$only_save$X[-c(1,.N), sum(shapley_weight)]
}




















# Other versions --------------------------------------------------------------------------------------------------
if (FALSE) {

  m = 10

  # Find weights for given number of features
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  p


  n_combinations = 500


  feature_sample_all <- list()
  unique_samples <- 0
  feature_sample_all_unique = data.table(coalitions = character(), N = integer())

  iters = 0
  while (unique_samples < n_combinations - 2) {

    n_features_sample <- sample(
      x = n_features,
      size = (n_combinations - unique_samples - 2)/2, # Sample -2 as we add zero and m samples below. Divide by two due to paired sampling
      replace = TRUE,
      prob = p
    )

    feature_sample <- shapr:::sample_features_cpp(m, n_features_sample)
    feature_sample_paired <- lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)

    coalitions = c(feature_sample, feature_sample_paired)
    coalitions = sapply(coalitions, paste, collapse = ",")
    coalitions = data.table(table(coalitions))

    # Outer merge on column coalitions
    feature_sample_all_unique <-
      merge(feature_sample_all_unique, coalitions, by = "coalitions", all = TRUE, suffixes = c("_old", "_new"))

    # Add the number of sampled repetitions
    feature_sample_all_unique = feature_sample_all_unique[, .(coalitions, N = rowSums(cbind(N_old, N_new), na.rm = TRUE))]

    unique_samples = nrow(feature_sample_all_unique)

    iters = iters + 1


    #print(c(iters, unique_samples))
    #if (iters %% 250 == 0) message(paste0("Iter: ", iters, "\t Samples: ", length(feature_sample_all) ,"\t Unique samples:", unique_samples))
  }

  feature_sample_all_unique[, sum(N)]

  print(c(iters, unique_samples))









  n_combinations = 2^10


  feature_sample_all <- list()
  unique_samples <- 0
  feature_sample_all_unique = data.table(coalitions = character(), N = integer())

  dt_N_S_and_L = data.table(N_S = as.integer(seq(2, n_combinations - 2, 2)), L = as.integer(NA))

  iters = 0
  while (unique_samples < n_combinations - 2) {

    n_features_sample <- sample(
      x = n_features,
      size = 1,
      replace = TRUE,
      prob = p
    )

    feature_sample <- shapr:::sample_features_cpp(m, n_features_sample)
    feature_sample_paired <- lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)

    coalitions = c(feature_sample, feature_sample_paired)
    coalitions = sapply(coalitions, paste, collapse = ",")
    coalitions = data.table(table(coalitions))

    # Outer merge on column coalitions
    feature_sample_all_unique <-
      merge(feature_sample_all_unique, coalitions, by = "coalitions", all = TRUE, suffixes = c("_old", "_new"))

    # Add the number of sampled repetitions
    feature_sample_all_unique = feature_sample_all_unique[, .(coalitions, N = rowSums(cbind(N_old, N_new), na.rm = TRUE))]

    unique_samples_new = nrow(feature_sample_all_unique)

    if (unique_samples_new > unique_samples) {
      dt_N_S_and_L[N_S == unique_samples_new, L := feature_sample_all_unique[, sum(N)]]
    }

    unique_samples = unique_samples_new

    iters = iters + 1


    print(c(iters, unique_samples))
    #if (iters %% 250 == 0) message(paste0("Iter: ", iters, "\t Samples: ", length(feature_sample_all) ,"\t Unique samples:", unique_samples))
  }

  dt_N_S_and_L

  iters




}


