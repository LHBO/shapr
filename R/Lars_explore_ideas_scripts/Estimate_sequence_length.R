# In this file we compute empirical mean estimates of the length of a sequence until the sequence have N_S unique
# coalitions. Be careful if you want empty and grand included. We exclude them since we do not sample them in our
# sequence

# Functions -------------------------------------------------------------------------------------------------------
library(shapr)
library(data.table)


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
coalition_sampling = function(m, n_combinations = 2^m - 2,  n_sample_scale = 5, return_coalitions = FALSE,
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
    dt_cumsum = data.table(coalitions = all_coalitions, N_S = cumsum(!duplicated(all_coalitions)), L = .I)[, L := .I]

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

  # Post processing: keep only the coalitions until n_combinations - 2
  all_coalitions = all_coalitions[seq(dt_N_S_and_L[N_S == n_combinations - 2, L])]
  if (length(unique(all_coalitions)) != n_combinations - 2) stop("Not the right number of unique coalitions")

  # Return
  if (return_coalitions) {
    return(list(dt_N_S_and_L = dt_N_S_and_L, all_coalitions = all_coalitions))
  } else {
    return(dt_N_S_and_L)
  }
}


repeated_coalition_sampling = function(m, repetitions, n_combinations = 2^m - 2, n_sample_scale = 5, verbose = TRUE, verbose_extra = NULL) {
  dt = data.table::rbindlist(
    lapply(seq(repetitions), function(repetition) {
      if (verbose){
        string = paste0("Working on repetition ", repetition, " of ", repetitions ,".")
        if (!is.null(verbose_extra)) string = paste(verbose_extra, string)
        message(string)
      }
      coalition_sampling(m = m,
                         n_combinations = n_combinations,
                         n_sample_scale = n_sample_scale,
                         verbose = verbose)[,-"coalitions"]}
    ), use.names = TRUE,
    idcol = "Repetition"
  )
  return(dt)
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
  folder = ""
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


# Code starts here ------------------------------------------------------------------------------------------------
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
repetitions = as.integer(args[2]) # 250
n_sample_scale = as.numeric(args[3]) # 10

# Iterate over the number
for (m in m_vec) {
  dt = repeated_coalition_sampling(m = m, repetitions = repetitions, n_sample_scale = n_sample_scale,
                                   verbose_extra = paste0("M = ", m, "."))
  dt_avg = dt[, list(L_avg = mean(L)), by = N_S]
  saveRDS(list(dt = dt, dt_avg = dt_avg), file.path(folder_save, paste0("Sequence_length_M_", m, ".rds")))
}



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
}


if (FALSE) {
  #sammenlign
  vv = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.92_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_estimated_repetition_2.rds")

  L_values = sapply(vv$unique_paired$repetition_1[-c(1, length(vv$unique_paired$repetition_1))], function(x) {
    x$only_save$X[-c(1,.N), sum(shapley_weight)]
  })

  dt_extra = data.table(N_S = as.integer(sub(".*_", "", names(values))), L = L_values)

  ggplot(dt, aes(x = N_S, y = L)) +
    geom_line(aes(colour = Repetition)) +
    geom_point(data = dt_extra, mapping = aes(x = N_S, y = L), colour = "black")

  plot(values)

  ggplot(dt_avg, aes(x = N_S, y = L_avg)) +
    geom_line() +
    geom_point(data = dt_extra, mapping = aes(x = N_S, y = L), colour = "black")


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


