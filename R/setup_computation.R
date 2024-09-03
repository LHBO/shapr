#' Sets up everything for the Shapley values computation in [shapr::explain()]
#'
#' @inheritParams default_doc
#' @inheritParams explain
#' @inherit default_doc
#' @export
setup_computation <- function(internal, model, predict_model) {
  # model and predict_model are only needed for type AICc of approach empirical, otherwise ignored
  type <- internal$parameters$type

  # setup the Shapley framework
  internal <- if (type == "forecast") shapley_setup_forecast(internal) else shapley_setup(internal)

  # Setup for approach
  internal <- setup_approach(internal, model = model, predict_model = predict_model)

  return(internal)
}

#' @keywords internal
shapley_setup_forecast <- function(internal) {
  exact <- internal$parameters$exact
  n_features0 <- internal$parameters$n_features
  n_combinations <- internal$parameters$n_combinations
  is_groupwise <- internal$parameters$is_groupwise
  group_num <- internal$objects$group_num
  horizon <- internal$parameters$horizon
  feature_names <- internal$parameters$feature_names

  X_list <- W_list <- list()

  # Find columns/features to be included in each of the different horizons
  col_del_list <- list()
  col_del_list[[1]] <- numeric()
  if (horizon > 1) {
    k <- 2
    for (i in rev(seq_len(horizon)[-1])) {
      col_del_list[[k]] <- c(unlist(col_del_list[[k - 1]]), grep(paste0(".F", i), feature_names))
      k <- k + 1
    }
  }

  cols_per_horizon <- lapply(rev(col_del_list), function(x) if (length(x) > 0) feature_names[-x] else feature_names)

  horizon_features <- lapply(cols_per_horizon, function(x) which(internal$parameters$feature_names %in% x))

  # Apply feature_combination, weigth_matrix and feature_matrix_cpp to each of the different horizons
  for (i in seq_along(horizon_features)) {
    this_featcomb <- horizon_features[[i]]
    n_this_featcomb <- length(this_featcomb)

    this_group_num <- lapply(group_num, function(x) x[x %in% this_featcomb])

    X_list[[i]] <- feature_combinations(
      m = n_this_featcomb,
      exact = exact,
      n_combinations = n_combinations,
      weight_zero_m = 10^6,
      group_num = this_group_num
    )

    W_list[[i]] <- weight_matrix(
      X = X_list[[i]],
      normalize_W_weights = TRUE,
      is_groupwise = is_groupwise
    )
  }

  # Merge the feature combination data.table to single one to use for computing conditional expectations later on
  X <- rbindlist(X_list, idcol = "horizon")
  X[, N := NA]
  X[, shapley_weight := NA]
  data.table::setorderv(X, c("n_features", "horizon"), order = c(1, -1))
  X[, horizon_id_combination := id_combination]
  X[, id_combination := 0]
  X[!duplicated(features), id_combination := .I]
  X[, tmp_features := as.character(features)]
  X[, id_combination := max(id_combination), by = tmp_features]
  X[, tmp_features := NULL]

  # Extracts a data.table allowing mapping from X to X_list/W_list to be used in the compute_shapley function
  id_combination_mapper_dt <- X[, .(horizon, horizon_id_combination, id_combination)]

  X[, horizon := NULL]
  X[, horizon_id_combination := NULL]
  data.table::setorder(X, n_features)
  X <- X[!duplicated(id_combination)]

  W <- NULL # Included for consistency. Necessary weights are in W_list instead

  ## Get feature matrix ---------
  S <- feature_matrix_cpp(
    features = X[["features"]],
    m = n_features0
  )


  #### Updating parameters ####

  # Updating parameters$exact as done in feature_combinations
  if (!exact && n_combinations >= 2^n_features0) {
    internal$parameters$exact <- TRUE # Note that this is exact only if all horizons use the exact method.
  }

  internal$parameters$n_combinations <- nrow(S) # Updating this parameter in the end based on what is actually used.

  # This will be obsolete later
  internal$parameters$group_num <- NULL # TODO: Checking whether I could just do this processing where needed
  # instead of storing it

  internal$objects$X <- X
  internal$objects$W <- W
  internal$objects$S <- S
  internal$objects$S_batch <- create_S_batch_new(internal)

  internal$objects$id_combination_mapper_dt <- id_combination_mapper_dt
  internal$objects$cols_per_horizon <- cols_per_horizon
  internal$objects$W_list <- W_list
  internal$objects$X_list <- X_list


  return(internal)
}


#' @keywords internal
shapley_setup <- function(internal) {
  exact <- internal$parameters$exact
  n_features0 <- internal$parameters$n_features
  n_combinations <- internal$parameters$n_combinations
  is_groupwise <- internal$parameters$is_groupwise

  group_num <- internal$objects$group_num

  X <- feature_combinations(
    m = n_features0,
    exact = exact,
    n_combinations = n_combinations,
    weight_zero_m = 10^6,
    group_num = group_num,
    internal = internal
  )
  #print(copy(X))

  if (!is.null(internal$parameters$sort_combinations) && isTRUE(internal$parameters$sort_combinations)) {
    # 2024: Sort it as not_exact does not put the features in the same order as the order we get using exact
    # Sort it first based on length (number of features in coalition S) and then numerically.
    # Works up to coalitions size of 52 features.
    lengths = X[,.N, by = n_features]
    lengths_to_sort = lengths[N > 1, n_features]
    int_to_letters = c(letters, LETTERS)
    for (length_to_sort in lengths_to_sort) {
      lst_tmp = X[n_features == length_to_sort]$features
      lst_tmp = sapply(seq(length(lst_tmp)), function(i) paste0(int_to_letters[lst_tmp[[i]]], collapse = ""))
      X[n_features == length_to_sort] = X[n_features == length_to_sort][order(lst_tmp)]
    }
    X[, id_combination := seq(.N)]
  }

  # User has provided a data table containing the new weights for each coalition size
  dt_new_weights <- internal$parameters$dt_new_weights
  new_weights_string <- internal$parameters$new_weights_string
  if (!is.null(dt_new_weights)) {
    if (is.null(new_weights_string)) stop("`new_weights_string` most be provided.")
    # new_weights_string = "empirical"
    # new_weights_string = "gompertz"
    # print("hei")

    # Find the weights of the combination closest to n_combinations
    n_comb_use = dt_new_weights$n_combinations[which.min(abs(dt_new_weights$n_combinations - n_combinations))]
    dt_new_weights_now = dt_new_weights[n_combinations == n_comb_use]

    X[, shapley_weight := as.numeric(shapley_weight)]

    # Update the weights with the provided weights for each coalition size
    X[dt_new_weights_now, on = "n_features", shapley_weight := get(new_weights_string)]
    # X[dt_new_weights_now, on = "n_features", shapley_weight := 1]
    #
    # XX = merge(X,
    #       dt_new_weights_now,
    #       by = "n_features",
    #       all.x = TRUE)
    # XX[-c(1,.N), shapley_weight := empirical]
    #
    # dt_new_weights_now[, get(new_weights_string)]
  }
  #print(X)

  # Get weighted matrix ----------------
  #print("Time to compute W")
  #print(internal$parameters$n_combinations)
  W <- weight_matrix(
    X = X,
    normalize_W_weights = TRUE,
    is_groupwise = is_groupwise
  )
  #print("Done with computing W")
  # X_true = copy(X)
  # X_true[, shapley_weight := shapley_weight_2]
  # W2 <- weight_matrix(
  #   X = X,
  #   normalize_W_weights = FALSE,
  #   is_groupwise = is_groupwise
  # )
  # W_true <- weight_matrix(
  #   X = X_true,
  #   normalize_W_weights = TRUE,
  #   is_groupwise = is_groupwise
  # )
  # W2_true <- weight_matrix(
  #   X = X_true,
  #   normalize_W_weights = FALSE,
  #   is_groupwise = is_groupwise
  # )
  #
  # W_true[abs(W_true) < 0.00001] = 0
  # W2_true[abs(W2_true) < 0.00001] = 0
  #
  # W[abs(W) < 0.00001] = 0
  # W2[abs(W2) < 0.00001] = 0
  #
  # W[seq(m+1), 1:10]
  # W2[seq(m+1), 1:10]
  #
  # max(abs(W - W2))
  #
  # W_true[seq(m+1), 1:10]
  # W2_true[seq(m+1), 1:10]
  #
  # max(abs(W_true - W2_true))
  #
  # max(abs(W_true - W))
  #
  # W[seq(m+1), 1:10]
  # W2[seq(m+1), 1:10]
  # {
  # par(mfrow = c(3,1))
  # matplot(t(W), type = "l", lty = 1)
  # matplot(t(W_true), type = "l", lty = 1)
  # matplot(t(abs(W_true - W)), type = "l", lty = 1)
  # }

  ## Get feature matrix ---------
  S <- feature_matrix_cpp(
    features = X[["features"]],
    m = n_features0
  )

  # Add option to replace the W matrix where we compute the full W matrix and then extract the relevant columns
  if (!is.null(internal$parameters$replace_W) && isTRUE(internal$parameters$replace_W)) {
    #message("Replace the W matrix")
    #print("Replace the W matrix")
    W_all = weight_matrix(
      X = feature_exact(m = n_features0, weight_zero_m = 10^6),
      normalize_W_weights = TRUE,
      is_groupwise = is_groupwise
    )

    # Create the S matrix if we had used all combinations
    S_all = shapr::feature_matrix_cpp(
      features = unlist(lapply(0:n_features0, utils::combn, x = n_features0, simplify = FALSE), recursive = FALSE),
      m = n_features0
    )

    # Get a mapping from the indices of the current set of combinations/coalitions to the indices
    # in the version where we use all 2^M combinations/coalitions.
    current_combination_idx_in_all_combinations =
      sapply(seq(nrow(S)), function(idx) which(apply(S_all, 1, function(x) identical(x, S[idx,]))))
    W = W_all[,current_combination_idx_in_all_combinations]
  }

  #### Updating parameters ####

  # Updating parameters$exact as done in feature_combinations
  if (!exact && n_combinations >= 2^n_features0) internal$parameters$exact <- TRUE

  internal$parameters$n_combinations <- nrow(S) # Updating this parameter in the end based on what is actually used.

  # This will be obsolete later
  internal$parameters$group_num <- NULL # TODO: Checking whether I could just do this processing where needed
  # instead of storing it

  internal$objects$X <- X
  internal$objects$W <- W
  internal$objects$S <- S
  internal$objects$S_batch <- create_S_batch_new(internal)


  return(internal)
}

#' Define feature combinations, and fetch additional information about each unique combination
#'
#' @param m Positive integer. Total number of features.
#' @param exact Logical. If `TRUE` all `2^m` combinations are generated, otherwise a
#' subsample of the combinations is used.
#' @param n_combinations Positive integer. Note that if `exact = TRUE`,
#' `n_combinations` is ignored. However, if `m > 12` you'll need to add a positive integer
#' value for `n_combinations`.
#' @param weight_zero_m Numeric. The value to use as a replacement for infinite combination
#' weights when doing numerical operations.
#' @param group_num List. Contains vector of integers indicating the feature numbers for the
#' different groups.
#' @param internal List. Containing objects used internally in the package.
#'
#' @return A data.table that contains the following columns:
#' \describe{
#' \item{id_combination}{Positive integer. Represents a unique key for each combination. Note that the table
#' is sorted by `id_combination`, so that is always equal to `x[["id_combination"]] = 1:nrow(x)`.}
#' \item{features}{List. Each item of the list is an integer vector where `features[[i]]`
#' represents the indices of the features included in combination `i`. Note that all the items
#' are sorted such that `features[[i]] == sort(features[[i]])` is always true.}
#' \item{n_features}{Vector of positive integers. `n_features[i]` equals the number of features in combination
#' `i`, i.e. `n_features[i] = length(features[[i]])`.}.
#' \item{N}{Positive integer. The number of unique ways to sample `n_features[i]` features
#' from `m` different features, without replacement.}
#' }
#'
#' @export
#'
#' @author Nikolai Sellereite, Martin Jullum
#'
#' @examples
#' # All combinations
#' x <- feature_combinations(m = 3)
#' nrow(x) # Equals 2^3 = 8
#'
#' # Subsample of combinations
#' x <- feature_combinations(exact = FALSE, m = 10, n_combinations = 1e2)
feature_combinations <- function(m, exact = TRUE, n_combinations = 200, weight_zero_m = 10^6, group_num = NULL,
                                 internal = NULL) {
  m_group <- length(group_num) # The number of groups

  # Force user to use a natural number for n_combinations if m > 13
  if (m > 13 && is.null(n_combinations) && m_group == 0) {
    stop(
      paste0(
        "Due to computational complexity, we recommend setting n_combinations = 10 000\n",
        "if the number of features is larger than 13 for feature-wise Shapley values.\n",
        "Note that you can force the use of the exact method (i.e. n_combinations = NULL)\n",
        "by setting n_combinations equal to 2^m where m is the number of features.\n"
      )
    )
  }

  # Not supported for m > 30
  if (m > 30 && m_group == 0) {
    stop(
      paste0(
        "Currently we are not supporting cases where the number of features is greater than 30\n",
        "for feature-wise Shapley values.\n"
      )
    )
  }
  if (m_group > 30) {
    stop(
      paste0(
        "For computational reasons, we are currently not supporting group-wise Shapley values \n",
        "for more than 30 groups. Please reduce the number of groups.\n"
      )
    )
  }

  if (!exact) {
    if (m_group == 0) {
      # Switch to exact for feature-wise method
      if (n_combinations >= 2^m) {
        n_combinations <- 2^m
        exact <- TRUE
        message(
          paste0(
            "Success with message:\n",
            "n_combinations is larger than or equal to 2^m = ", 2^m, ". \n",
            "Using exact instead.\n"
          )
        )
      }
    } else {
      # Switch to exact for feature-wise method
      if (n_combinations >= (2^m_group)) {
        n_combinations <- 2^m_group
        exact <- TRUE
        message(
          paste0(
            "Success with message:\n",
            "n_combinations is larger than or equal to 2^group_num = ", 2^m_group, ". \n",
            "Using exact instead.\n"
          )
        )
      }
    }
  }

  if (m_group == 0) {
    # Here if feature-wise Shapley values
    if (exact) {
      dt <- feature_exact(m, weight_zero_m)
    } else {

      # If user has not specified, then we use the default one.
      if (is.null(internal$parameters$sampling_method)) internal$parameters$sampling_method = "unique"
      # print(internal$parameters$sampling_method)

      dt <- feature_not_exact(m = m,
                              n_combinations = n_combinations,
                              weight_zero_m = weight_zero_m,
                              sampling_method = internal$parameters$sampling_method,
                              sampling_method_full_name = internal$parameters$sampling_method_full_name,
                              pilot_estimates_vS = internal$parameters$pilot_estimates_vS,
                              specific_coalition_set = internal$parameters$specific_coalition_set,
                              specific_coalition_set_weights = internal$parameters$specific_coalition_set_weights)

      stopifnot(
        data.table::is.data.table(dt),
        !is.null(dt[["p"]])
      )
      p <- NULL # due to NSE notes in R CMD check
      dt[, p := NULL]
    }
  } else {
    # Here if group-wise Shapley values
    if (exact) {
      dt <- feature_group(group_num, weight_zero_m)
    } else {
      dt <- feature_group_not_exact(group_num, n_combinations, weight_zero_m)
      stopifnot(
        data.table::is.data.table(dt),
        !is.null(dt[["p"]])
      )
      p <- NULL # due to NSE notes in R CMD check
      dt[, p := NULL]
    }
  }
  return(dt)
}

#' @keywords internal
feature_exact <- function(m, weight_zero_m = 10^6) {
  dt <- data.table::data.table(id_combination = seq(2^m))
  combinations <- lapply(0:m, utils::combn, x = m, simplify = FALSE)
  dt[, features := unlist(combinations, recursive = FALSE)]
  dt[, n_features := length(features[[1]]), id_combination]
  dt[, N := .N, n_features]
  dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]

  return(dt)
}

#' Sample feature combinations when not using all possible combinations
#'
#' @param m Positive integer. Total number of features.
#' @param n_combinations Positive integer. Note that if `exact = TRUE`,
#' `n_combinations` is ignored. However, if `m > 12` you'll need to add a positive integer
#' value for `n_combinations`.
#' @param weight_zero_m Positive integer. Represents the Shapley weight for two special
#' cases, i.e. the case where you have either `0` or `m` features/feature groups.
#' @param sampling_method String. Specifying which of the coalition sampling methods to use.
#' @param pilot_estimates_vS LARS ADD DESCRIPTION
#' @param specific_coalition_set Integers. Array of integers of length `n_combinations`, where the first and last
#' entries are 1 and 2^m, respectively.
#' @param specific_coalition_set_weights Numerics. Array of numerics of length `n_combinations` where the i'th entry
#' represents the weight of the i'th coalitions specified in `specific_coalition_set`. If `NULL`, then the function
#' use the Shapley kernel weights. Note that the weights for the empty and full coalitions are determined by
#' `weight_zero_m`.
#'
#' @keywords internal
feature_not_exact <- function(m, n_combinations = 200, weight_zero_m = 10^6,
                              sampling_method = c("unique",
                                                  "unique_SW",
                                                  "unique_unif",
                                                  "unique_unif_V2",
                                                  "unique_equal_weights",
                                                  "unique_equal_weights_symmetric",
                                                  "unique_paired",
                                                  "unique_paired_unif",
                                                  "unique_paired_unif_V2",
                                                  "unique_paired_SW",
                                                  "unique_paired_equal_weights",
                                                  "unique_paired_equal_weights_1000",
                                                  "unique_paired_equal_weights_5000",
                                                  "unique_paired_equal_weights_10000",
                                                  "unique_paired_equal_weights_50000",
                                                  "unique_paired_equal_weights_100000",
                                                  "unique_paired_equal_weights_symmetric",
                                                  "non_unique",
                                                  "non_unique_SW",
                                                  "chronological_order_increasing",
                                                  "chronological_order_decreasing",
                                                  "largest_weights",
                                                  "largest_weights_random",
                                                  "largest_weights_combination_size",
                                                  "smallest_weights",
                                                  "smallest_weights_constant_SW",
                                                  "smallest_weights_combination_size",
                                                  "specific_coalition_set",
                                                  "pilot_estimates_paired",
                                                  "MAD"),
                              sampling_method_full_name = NULL,
                              pilot_estimates_vS = NULL, #
                              specific_coalition_set = NULL,
                              specific_coalition_set_weights = NULL) {

  # Update the sampling method
  sampling_method = gsub("_replace_W", "", sampling_method)
  sampling_method_full_name = gsub("_replace_W", "", sampling_method_full_name)

  sampling_method = strsplit(x = sampling_method, split = "_new_weights", fixed = TRUE)[[1]][1]
  sampling_method_full_name = strsplit(x = sampling_method_full_name, split = "_new_weights", fixed = TRUE)[[1]][1]

  # print("Inside feature_not_exact")
  # print(sampling_method)

  # Get the sampling method. The `unique` method is the default one in master.
  # The `non_unique` method is the version that was default in the first version in `shapr`.
  # Lars test some new ones.
  # The `chronological_order_increasing` version just takes the `n_combinations` first
  # combinations from internal
  # sampling_method <- match.arg(sampling_method)

  # Find weights for given number of features
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  # Check that user provided pilot estimates if the sampling method use pilot estimates.
  # TODO: note that in this version, the user need to manually provide the pilot estimates
  # when calling the `explain` function. In future versions, this should then be done internally in `shapr`.
  if (grepl("pilot", sampling_method) && is.null(pilot_estimates_vS)) {
    stop(paste0("User must provide `pilot_estimates_vS` as the `sampling_method` (",
                sampling_method, ") use pilot estimates."))
  }
  # print(sampling_method)
  # print(sampling_method_full_name)
  # #
  # message(sampling_method)
  # message(sampling_method_full_name)
  #
  # warning(sampling_method)
  # warning(sampling_method_full_name)

  if (sampling_method == "specific_coalition_set") {
    ## specific_coalition_set --------------------------------------------------------------------------------------------------
    # The user has specified a specific order of coalitions to add
    # print("Using the `specific_coalition_set` strategy.")


    # 20TH MAY, we add two new sampling methods that use the pilot estimates, but sample them instead, and use
    # the sampling frequency as the weights in stead of the true Shapley kernel weights.
    if (sampling_method_full_name %in% c("paired_coalitions_weights", "paired_coalitions_weights_equal_weights", "paired_coalitions_weights_direct", "paired_coalitions_weights_direct_equal_weights")) {
      # freq pilot ------------------------------------------------------------------------------------------------------

      # TODO. FIX THIS
      # explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.9_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_true.rds")
      # specific_coalition_set = pilot_estimates_coal_order(explanation)$paired_coalitions_weights
      # m = 10
      # n_combinations = 200
      # weight_zero_m = 10^6

      # Old code slow due to many calls to unique.
      # #iters = 0
      # feature_sample_all <- c()
      # unique_samples <- 0
      # while (unique_samples < n_combinations_pair - 1) {
      #   # Recall that n_combinations must be a even number. subtract one which represents the pair of the empty and grand coalitions
      #   features_sample = sample(seq(2, length(specific_coalition_set)), size = n_combinations_pair - unique_samples - 1, replace = TRUE,
      #                            prob = specific_coalition_set[-1])
      #   feature_sample_all = c( feature_sample_all, features_sample)
      #   unique_samples = length(unique(feature_sample_all))
      #   #iters = iters + 1
      #   #print(c(iters, unique_samples))
      # }

      # NEW faster version with fewer calls to unique.
      # Variable to keep track of the iteration number
      iteration = 1

      # How many times extra coalitions to sample each time (cheap to sample)
      n_sample_scale = 40

      # If print
      verbose_now = TRUE

      # Get the number of paired combinations we need to sample (n_combinations must be an even number)
      n_combinations_pair = n_combinations/2

      # List to store all the sampled coalitions
      feature_sample_all <- list()

      # Variable to keep track of the number of unique coalitions
      unique_samples <- 0

      # Loop until we have enough unique samples
      while (unique_samples < n_combinations_pair - 1) {

        # Recall that n_combinations must be a even number. subtract one which represents the pair of the empty and grand coalitions
        features_sample = sample(seq(2, length(specific_coalition_set)), # 1 is the empty/grand coalition pair
                                 size = n_sample_scale*n_combinations_pair, #n_combinations_pair - unique_samples - 1,
                                 replace = TRUE,
                                 prob = specific_coalition_set[-1])

        # Add the new coalitions to the previously sampled coalitions
        feature_sample_all = c(feature_sample_all, features_sample)

        # Get the cumulative number of unique coalitions for each coalition in feature_sample_all
        dt_cumsum = data.table(coalitions = feature_sample_all, N_S = cumsum(!duplicated(feature_sample_all)), L = .I)[, L := .I]

        # Extract rows where the N_S value increases (i.e., where we sample a new unique coalition)
        dt_N_S_and_L <- dt_cumsum[N_S != shift(N_S, type = "lag", fill = 0)]

        # Get the number of unique coalitions
        unique_samples = dt_N_S_and_L[.N, N_S]

        # Message to user
        if (verbose_now) {
          message(paste0("Iteration ", iteration, ": N_S = ", unique_samples,
                         ", Sampled = ", n_sample_scale*n_combinations_pair*iteration, "."))
        }

        # Update the iteration number
        iteration = iteration + 1
      }

      # Post processing: keep only the coalitions until n_combinations - 2
      feature_sample_all = feature_sample_all[seq(dt_N_S_and_L[N_S == n_combinations_pair - 1, L])]
      if (length(unique(feature_sample_all)) != n_combinations_pair - 1) stop("Not the right number of unique coalitions")

      # Convert to array such that the rest of the code works
      feature_sample_all = unlist(feature_sample_all)

      # Crete the sampling frequencies and mirror them
      sampling_freq = c(weight_zero_m, unname(table(feature_sample_all)))
      sampling_freq = c(sampling_freq, rev(sampling_freq))

      # sort(feature_sample_all)
      # table(feature_sample_all)

      # Get the unique combinations and add 1 (the empty comb), and then add the paired versions (complements)
      feature_sample_all_unique = c(1, sort(unique(feature_sample_all)))
      feature_sample_all_unique = c(feature_sample_all_unique, 2^m + 1 - rev(feature_sample_all_unique))

      # Create the data table. We create all 2^m combinations and then extract the ones we need.
      # TODO: this should be improved
      dt <- data.table::data.table(id_combination_org = seq(2^m))
      dt[, features := unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)]
      dt[, n_features := length(features[[1]]), id_combination_org]
      dt[, N := .N, n_features]
      dt[, shapley_weight_true := shapr:::shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]
      dt = dt[feature_sample_all_unique] # Extract only the relevant rows
      dt[, shapley_weight := sampling_freq]
      dt[, id_combination := .I]
      nms <- c("id_combination", "id_combination_org", "features", "n_features", "N", "shapley_weight", "shapley_weight_true")
      data.table::setcolorder(dt, nms)

      # # If we want to visualize the Shapley kernel weights
      # plot_mat = cbind(dt[-c(1, .N), shapley_weight] / sum(dt[-c(1, .N), shapley_weight]),
      #                  c(specific_coalition_set, rev(specific_coalition_set))[dt[-c(1, .N), id_combination_org]] / sum(c(specific_coalition_set, rev(specific_coalition_set))[dt[-c(1, .N), id_combination_org]]),
      #                  dt[-c(1, .N), shapley_weight_true] / sum(dt[-c(1, .N), shapley_weight_true]))
      # matplot(plot_mat, pch = 16)
      # legend("top", c("Sampling freq", "True prop", "True SKW"), pch = 16, col = 1:3)

      # If we are to use the
      if (sampling_method_full_name %in% c("paired_coalitions_weights_direct", "paired_coalitions_weights_direct_equal_weights")) {
        dt[, shapley_weight := c(specific_coalition_set, rev(specific_coalition_set))[dt[,id_combination_org]]]
        dt[c(1, .N), shapley_weight := weight_zero_m]
      }

      # Update the weights if we are to do the average weights for each coalition size
      if (sampling_method_full_name %in% c("paired_coalitions_weights_equal_weights", "paired_coalitions_weights_direct_equal_weights")) {
        dt[, shapley_weight := as.numeric(shapley_weight)]
        dt[, shapley_weight := mean(shapley_weight), by = n_features]

        # If we want to visualize the Shapley kernel weights
        # plot_mat = cbind(dt[-c(1, .N), shapley_weight] / sum(dt[-c(1, .N), shapley_weight]),
        #                  c(specific_coalition_set, rev(specific_coalition_set))[dt[-c(1, .N), id_combination_org]] / sum(c(specific_coalition_set, rev(specific_coalition_set))[dt[-c(1, .N), id_combination_org]]),
        #                  dt[-c(1, .N), shapley_weight_true] / sum(dt[-c(1, .N), shapley_weight_true]))
        # matplot(plot_mat, pch = 16)
        # legend("top", c("Sampling freq", "True prop", "True SKW"), pch = 16, col = 1:3)
      }

      #print(dt)


    } else {

      # Check that the user has specified `specific_coalition_set`.
      if (is.null(specific_coalition_set)) {
        stop(paste("Cannot use sampling method `specific_coalition_set` without also providing",
                   "the `specific_coalition_set` object to the `explain()` function."))
      }

      # Sort the coalitions in increasing order. This is okay as we are going to use all of them,
      # hence the order does not matter for the final Shapley values.
      specific_coalition_set = sort(specific_coalition_set)

      # Check that the coalition order is unique. I.e., that we do not add the same coalition multiple times
      if (length(unique(specific_coalition_set)) != length(specific_coalition_set)) {
        stop("There are repeated entries in `specific_coalition_set`.")
      }

      # Check that the number of user specified coalitions corresponds to `n_combinations`
      if (length(specific_coalition_set) != n_combinations) {
        stop(paste0("The number of entries in `specific_coalition_set` (", length(specific_coalition_set), ") must be ",
                    "the same as `n_combinations` (", n_combinations, ")."))
      }

      # The user has also specified corresponding weights
      if (!is.null(specific_coalition_set_weights)) {

        # Reorder the weights to match the sorted order of `specific_coalition_set`
        specific_coalition_set_weights = specific_coalition_set_weights[order(specific_coalition_set)]

        # Check that they are of equal length
        if (length(specific_coalition_set) != length(specific_coalition_set_weights)) {
          stop(paste0("The length of `specific_coalition_set` (", length(specific_coalition_set_weights), ") and ",
                      "`specific_coalition_set_weights` (", length(specific_coalition_set_weights) ,") differ."))
        }

        # Will use these weights and not the Shapley kernel weights
        Shapley_kernel_weights = FALSE
        message(paste("Use the entries in `specific_coalition_set_weights` instead of the Shapley kernel weights",
                      "in the computations of the Shapley values."))

      } else {
        # We use the Shapley kernel weights
        Shapley_kernel_weights = TRUE
      }

      # Check that the first and last elements are 1 and 2^M, respectively.
      if (any(!(c(1, 2^m) == specific_coalition_set[c(1, n_combinations)]))) {
        stop(paste("The first and last elements in `specific_coalition_set` must be `c(1, 2^M)`, respectively,",
                   "where M is the number of features."))
      }

      # TODO: we can be more clever here. Do not need to make all combinations and then extract only the relevant.
      # Based on the value of `n_combinations`, we do not need to use `0:m`, but look at `n` and then choose the
      # smallest `m'` such that we only need to use `0:m'`. Note that we need need to add the last one manually.
      # Mostly useful if `m` is large, i.e., 20+.
      dt <- data.table::data.table(id_combination = seq(n_combinations))
      dt[, features := unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE),
                              recursive = FALSE)[specific_coalition_set]]
      dt[, n_features := length(features[[1]]), id_combination]
      dt[, N := 1]
      dt[-c(1, .N), N := n[n_features]]
      dt[, N := as.integer(N)]
      dt[, shapley_weight :=
           if (Shapley_kernel_weights) shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)
         else specific_coalition_set_weights]
      dt[c(1, .N), shapley_weight := weight_zero_m]
    }

  } else if (sampling_method %in% c("unique", "unique_paired", "non_unique","unique_SW", "unique_paired_SW", "non_unique_SW",
                                    "unique_equal_weights", "unique_equal_weights_symmetric", "unique_unif_V2", "unique_paired_unif_V2",
                                    "unique_paired_equal_weights", "unique_paired_equal_weights_symmetric") ||
             grepl("unique_paired_equal_weights_", sampling_method)
  ) {
    # Check if any of "unique", "unique_paired", "non_unique", "unique_SW", "unique_paired_SW", "non_unique_SW"
    # This is the version that is in the Shapr master branch, except for `unique_paired`.

    feature_sample_all <- list() # List to store all the sampled coalitions
    unique_samples <- 0  # Variable to keep track of the number of unique coalitions

    if (sampling_method %in% c("unique", "unique_SW", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_unif_V2")) {
      # unique ----------------------------------------------------------------------------------------------------------

      # # OLD:
      # while (unique_samples < n_combinations - 2) {
      #   n_features_sample <- sample(
      #     x = n_features,
      #     size = n_combinations - unique_samples - 2, # Sample -2 as we add zero and m samples below
      #     replace = TRUE,
      #     prob = p
      #   )
      #
      #   # Sample specific set of features
      #   feature_sample <- sample_features_cpp(m, n_features_sample)
      #   feature_sample_all <- c(feature_sample_all, feature_sample)
      #   unique_samples <- length(unique(feature_sample_all))
      # }

      # NEW faster version with fewer calls to unique.
      # Variable to keep track of the iteration number
      iteration = 1

      # How many times extra coalitions to sample each time (cheap to sample)
      n_sample_scale = 40

      # If print
      verbose_now = TRUE

      # Loop until we have enough unique samples
      while (unique_samples < n_combinations - 2) {

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
        feature_sample_all = c(feature_sample_all, coalitions)

        # Get the cumulative number of unique coalitions for each coalition in feature_sample_all
        dt_cumsum = data.table(coalitions = feature_sample_all, N_S = cumsum(!duplicated(feature_sample_all)), L = .I)[, L := .I]

        # Extract rows where the N_S value increases (i.e., where we sample a new unique coalition)
        dt_N_S_and_L <- dt_cumsum[N_S != shift(N_S, type = "lag", fill = 0)]

        # Get the number of unique coalitions
        unique_samples = dt_N_S_and_L[.N, N_S]

        # Message to user
        if (verbose_now) {
          message(paste0("Iteration ", iteration, ": N_S = ", unique_samples,
                         ", Sampled = ", n_sample_scale*n_combinations*iteration, "."))
        }

        # Update the iteration number
        iteration = iteration + 1
      }

      # Post processing: keep only the coalitions until n_combinations - 2
      feature_sample_all = feature_sample_all[seq(dt_N_S_and_L[N_S == n_combinations - 2, L])]
      if (length(unique(feature_sample_all)) != n_combinations - 2) stop("Not the right number of unique coalitions")

      # Convert to version used below
      feature_sample_all = lapply(strsplit(feature_sample_all, ','), as.integer)


    } else if (sampling_method %in% c("unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "unique_paired_equal_weights_symmetric", "unique_paired_unif_V2") ||
               grepl("unique_paired_equal_weights_", sampling_method)) {
      # unique paired ---------------------------------------------------------------------------------------------------

      # Old version. Slow due to many calls to unique. Cheaper to creat a redundant number of coalitions and then only take the needed
      # iters = 0
      # while (unique_samples < n_combinations - 2) {
      #
      #   n_features_sample <- sample(
      #     x = n_features,
      #     size = (n_combinations - unique_samples - 2)/2, # Sample -2 as we add zero and m samples below. Divide by two due to paired sampling
      #     replace = TRUE,
      #     prob = p
      #   )
      #
      #   feature_sample <- sample_features_cpp(m, n_features_sample)
      #   feature_sample_paired <- lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)
      #   feature_sample_all <- c(feature_sample_all, feature_sample, feature_sample_paired)
      #   unique_samples <- length(unique(feature_sample_all))
      #   iters = iters + 1
      #   #print(c(iters, unique_samples))
      #   #if (iters %% 250 == 0) message(paste0("Iter: ", iters, "\t Samples: ", length(feature_sample_all) ,"\t Unique samples:", unique_samples))
      # }

      # NEW faster version with fewer calls to unique.
      # Variable to keep track of the iteration number
      iteration = 1

      # How many times extra coalitions to sample each time (cheap to sample)
      n_sample_scale = 40

      # If print
      verbose_now = TRUE

      # Loop until we have enough unique samples
      while (unique_samples < n_combinations - 2) {

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
        feature_sample_all = c(feature_sample_all, coalitions)

        # Get the cumulative number of unique coalitions for each coalition in feature_sample_all
        dt_cumsum = data.table(coalitions = feature_sample_all, N_S = cumsum(!duplicated(feature_sample_all)), L = .I)[, L := .I]

        # Extract rows where the N_S value increases (i.e., where we sample a new unique coalition)
        dt_N_S_and_L <- dt_cumsum[N_S != shift(N_S, type = "lag", fill = 0)]

        # Get the number of unique coalitions
        unique_samples = dt_N_S_and_L[.N, N_S]

        # Message to user
        if (verbose_now) {
          message(paste0("Iteration ", iteration, ": N_S = ", unique_samples,
                         ", Sampled = ", n_sample_scale*n_combinations*iteration, "."))
        }

        # Update the iteration number
        iteration = iteration + 1
      }

      # Post processing: keep only the coalitions until n_combinations - 2
      feature_sample_all = feature_sample_all[seq(dt_N_S_and_L[N_S == n_combinations - 2, L])]
      if (length(unique(feature_sample_all)) != n_combinations - 2) stop("Not the right number of unique coalitions")

      # Convert to version used below
      feature_sample_all = lapply(strsplit(feature_sample_all, ','), as.integer)





      # Add extra samples. Cannot due it for two as there are no samples coalitions. Only empty and grand
      if (sampling_method != "unique_paired_equal_weights_symmetric" && grepl("unique_paired_equal_weights_", sampling_method) && n_combinations > 2) {
        n_extra = as.integer(as.numeric(gsub(".*_(\\d+)$", "\\1", sampling_method)) / 2)

        #message(length(feature_sample_all))
        #print(length(feature_sample_all))

        feature_sample_unique = unique(feature_sample_all)
        unique_samples_new_counter = 0

        while (unique_samples_new_counter < n_extra) {
          n_features_sample_new <- sample(
            x = n_features,
            size = n_extra - unique_samples_new_counter,
            replace = TRUE,
            prob = p
          )

          # Sample specific set of features
          feature_sample_new <- sample_features_cpp(m, n_features_sample_new)

          # Only keep the combinations that were among the original n_combinations
          feature_sample_new = feature_sample_new[feature_sample_new %in% feature_sample_unique]
          feature_sample_new_paired <- lapply(feature_sample_new, function(x, m) {seq(m)[-x]}, m = m)

          feature_sample_all <- c(feature_sample_all, feature_sample_new, feature_sample_new_paired)
          unique_samples_new_counter <- unique_samples_new_counter + length(feature_sample_new)
        }
        #message(length(feature_sample_all))

      }

    } else {
      # non_unique ------------------------------------------------------------------------------------------------------
      n_features_sample <- sample(
        x = n_features,
        size = n_combinations - 2, # Sample -2 as we add zero and m samples below
        replace = TRUE,
        prob = p
      )
      feature_sample_all <- sample_features_cpp(m, n_features_sample)
    }
    # feature_sample_all



    # Add zero and m features
    feature_sample_all <- c(list(integer(0)), feature_sample_all, list(c(1:m)))
    X <- data.table(n_features = sapply(feature_sample_all, length))
    X[, n_features := as.integer(n_features)]

    # Get number of occurrences and duplicated rows
    is_duplicate <- NULL # due to NSE notes in R CMD check
    r <- helper_feature(m, feature_sample_all)
    X[, is_duplicate := r[["is_duplicate"]]]

    # When we sample combinations the Shapley weight is equal
    # to the frequency of the given combination
    X[, shapley_weight := r[["sample_frequence"]]]


    # Populate table and remove duplicated rows
    X[, features := feature_sample_all]
    if (any(X[["is_duplicate"]])) {
      X <- X[is_duplicate == FALSE]
    }
    X[, is_duplicate := NULL]
    data.table::setkeyv(X, "n_features")

    # Make feature list into character
    X[, features_tmp := sapply(features, paste, collapse = " ")]

    # Aggregate weights by how many samples of a combination we observe
    X <- X[, .(
      n_features = data.table::first(n_features),
      shapley_weight = sum(shapley_weight),
      features = features[1]
    ), features_tmp]

    X[, features_tmp := NULL]
    data.table::setorder(X, n_features)

    # Add shapley weight and number of combinations
    X[c(1, .N), shapley_weight := weight_zero_m]
    X[, N := 1]
    ind <- X[, .I[data.table::between(n_features, 1, m - 1)]]
    X[ind, p := p[n_features]]
    X[ind, N := n[n_features]]

    # Set column order and key table
    data.table::setkeyv(X, "n_features")
    X[, id_combination := .I]
    X[, N := as.integer(N)]
    nms <- c("id_combination", "features", "n_features", "N", "shapley_weight", "p")
    data.table::setcolorder(X, nms)

    dt = X

    # Overwrite the frequency Shapley kernel weights with the exact ones
    if (sampling_method %in% c("unique_SW", "unique_paired_SW", "non_unique_SW")) {
      dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]
    }

    if (sampling_method %in% c("unique_equal_weights", "unique_paired_equal_weights") || (grepl("unique_paired_equal_weights_", sampling_method) && sampling_method != "unique_paired_equal_weights_symmetric")) {
      # The idea is to weight each coalition size the same, as we know that in theory they are equal.
      # Hence, we set the Shapley weight to be the average sampling frequency for the coalition size.
      dt[, shapley_weight := as.numeric(shapley_weight)]
      dt[, shapley_weight := mean(shapley_weight), by = n_features]
    }

    if (sampling_method %in% c("unique_unif_V2", "unique_paired_unif_V2")) {
      dt[-c(1, .N), shapley_weight := 1]
    }

    if (sampling_method %in% c("unique_equal_weights_symmetric", "unique_paired_equal_weights_symmetric")) {
      # Same as above, but also using that the Shapley weights should be symmetric. I.e., coalitions with
      # j features and coalitions with m-j features should in theory have the same weight. We set it to be
      # the mean across both coalition sizes.
      dt[, shapley_weight := as.numeric(shapley_weight)]
      coalition_groups = unique(lapply(seq(0, ceiling(m/2)), function(i) sort(c(i, m-i))))
      for (cg_index in seq(length(coalition_groups))) {
        dt[n_features %in% coalition_groups[[cg_index]], shapley_weight:=mean(shapley_weight)]
      }
    }

    if (FALSE) {
      # TESTE convergence
      dt[, shapley_weight_true := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]

      dt[, shapley_weight := as.numeric(shapley_weight)]
      dt[, shapley_weight_mean := mean(shapley_weight), by = n_features]

      s1 = dt$shapley_weight[-c(1,nrow(dt))]
      s1 = s1 / sum(s1)
      s2 = dt$shapley_weight_mean[-c(1,nrow(dt))]
      s2 = s2 / sum(s2)
      s3 = dt$shapley_weight_true[-c(1,nrow(dt))]
      s3 = s3 / sum(s3)
      matplot(seq(2, n_combinations-1), cbind(s1, s2, s3), type = "s", lty = 1, lwd = 2)
      plot(s1, ylim = c(0, max(c(s1, s2))))
      plot(s2)
      points(s2, col = 2)
      points(s3, col = 3)

      # Lets add more repetitions
      unique_samples_new = 5000
      length(feature_sample_all)

      feature_sample_unique = unique(feature_sample_all)
      unique_samples_new_counter = 0

      while (unique_samples_new_counter < unique_samples_new) {
        n_features_sample_new <- sample(
          x = n_features,
          size = unique_samples_new - unique_samples_new_counter,
          replace = TRUE,
          prob = p
        )

        # Sample specific set of features
        feature_sample_new <- sample_features_cpp(m, n_features_sample_new)

        # Only keep the combinations that were among the original n_combinations
        feature_sample_new = feature_sample_new[feature_sample_new %in% feature_sample_unique]

        feature_sample_all <- c(feature_sample_all, feature_sample_new)
        unique_samples_new_counter <- unique_samples_new_counter + length(feature_sample_new)
      }

      length(feature_sample_all) # added unique_samples_new new samples

      r <- helper_feature(m, feature_sample_all)

      X2 <- data.table(n_features = sapply(feature_sample_all, length))
      X2[, n_features := as.integer(n_features)]

      # Get number of occurrences and duplicated rows
      is_duplicate <- NULL # due to NSE notes in R CMD check
      r <- helper_feature(m, feature_sample_all)
      X2[, is_duplicate := r[["is_duplicate"]]]

      # When we sample combinations the Shapley weight is equal
      # to the frequency of the given combination
      X2[, shapley_weight := r[["sample_frequence"]]]


      # Populate table and remove duplicated rows
      X2[, features := feature_sample_all]
      if (any(X2[["is_duplicate"]])) {
        X2 <- X2[is_duplicate == FALSE]
      }
      X2[, is_duplicate := NULL]
      data.table::setkeyv(X2, "n_features")

      # Make feature list into character
      X2[, features_tmp := sapply(features, paste, collapse = " ")]

      # Aggregate weights by how many samples of a combination we observe
      X2 <- X2[, .(
        n_features = data.table::first(n_features),
        shapley_weight = sum(shapley_weight),
        features = features[1]
      ), features_tmp]

      X2[, features_tmp := NULL]
      data.table::setorder(X2, n_features)

      # Add shapley weight and number of combinations
      X2[c(1, .N), shapley_weight := weight_zero_m]
      X2[, N := 1]
      ind <- X2[, .I[data.table::between(n_features, 1, m - 1)]]
      X2[ind, p := p[n_features]]
      X2[ind, N := n[n_features]]

      # Set column order and key table
      data.table::setkeyv(X2, "n_features")
      X2[, id_combination := .I]
      X2[, N := as.integer(N)]
      nms <- c("id_combination", "features", "n_features", "N", "shapley_weight", "p")
      data.table::setcolorder(X2, nms)

      dt[, shapley_weight_2 := as.numeric(X2$shapley_weight)]
      dt[, shapley_weight_2_mean := mean(shapley_weight_2), by = n_features]


      s4 = dt$shapley_weight_2[-c(1,nrow(dt))]
      s4 = s4 / sum(s4)
      s5 = dt$shapley_weight_2_mean[-c(1,nrow(dt))]
      s5 = s5 / sum(s5)
      #matplot(seq(2, n_combinations-1), cbind(s1, s2, s3, s4, s5), type = "s", lty = 1, lwd = 2)
      par(mfrow = c(2,1))
      {
        matplot(seq(2, n_combinations-1), cbind(s1, s3, s2), type = "s", lty = 1, lwd = 2)
        matplot(seq(2, n_combinations-1), cbind(s4, s3, s5), type = "s", lty = 1, lwd = 2)
      }
      {
        matplot(seq(2, n_combinations-1), cbind(s1 - s3, s2 - s3), type = "s", lty = 1, lwd = 2)
        matplot(seq(2, n_combinations-1), cbind(s4 - s3, s5 - s3), type = "s", lty = 1, lwd = 2)
      }


    }


  } else if (sampling_method  %in% c("unique_unif", "unique_paired_unif")) {
    # Unif_weights ----------------------------------------------------------------------------------------------------
    dt <- data.table::data.table(id_combination = seq(n_combinations))
    if (sampling_method == "unique_unif") {
      # Sample `n_combinations` combination indices. Sort to get in right order.
      samples = sort(sample(x = 2:(2^m-1), size = n_combinations - 2))
    } else {
      # Paired assure that we have an even number. SHOULD NEVER HAPPEN THOUGH
      n_combinations_now = if (n_combinations %% 2 == 1) n_combinations - 1 else n_combinations

      # Sample from first half and then add the paired/compliments.  Sort to get in right order.
      samples = sort(sample(x = 2:2^(m-1), size = (n_combinations_now - 2)/2))
      samples = c(samples, 2^m + 1 - rev(samples))
    }

    # Add empty and grand
    samples = c(1, samples, 2^m)

    dt[, features := unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)[samples]]
    dt[, n_features := length(features[[1]]), id_combination]
    dt[, N := 1]
    dt[-c(1, .N), N := n[n_features]]
    dt[, N := as.integer(N)]
    dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]

  } else if (sampling_method == "chronological_order_increasing") {
    # chronological_order_increasing ----------------------------------------------------------------------------------

    dt <- data.table::data.table(id_combination = seq(n_combinations))

    # TODO: we can be more clever here. Do not need to make all combinations and then extract only the relevant.
    # Based on the value of `n_combinations`, we do not need to use `0:m`, but look at `n` and then choose the
    # smallest `m'` such that we only need to use `0:m'`. Note that we need need to add the last one manually.
    # Mostly useful if `m` is large, i.e., 20+.
    dt[, features := unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)
       [c(seq(1, n_combinations-1), 2^m)]]
    dt[, n_features := length(features[[1]]), id_combination]
    dt[, N := 1]
    dt[-c(1, .N), N := n[n_features]]
    dt[, N := as.integer(N)]
    dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]


  } else if (sampling_method == "chronological_order_decreasing") {
    # chronological_order_decreasing ----------------------------------------------------------------------------------

    dt <- data.table::data.table(id_combination = seq(n_combinations))

    # We can do similar here as above.
    dt[, features := unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)
       [c(1, seq(2^m-n_combinations+2, 2^m))]]
    dt[, n_features := length(features[[1]]), id_combination]
    dt[, N := 1]
    dt[-c(1, .N), N := n[n_features]]
    dt[, N := as.integer(N)]
    dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]

  } else if (sampling_method == "largest_weights") {
    # largest_weights -------------------------------------------------------------------------------------------------

    # Create list of all feature combinations
    combinations = unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)

    # Create a list of the indices
    indices = seq(1, 2^m)

    # Alternate them such that we extract the smallest, then the largest, then the second smallest,
    # then the second largest and so on.
    alternating_indices = c(rbind(indices[1:2^(m-1)], rev(indices[-(1:2^(m-1))])))
    alternating_indices_relevant = alternating_indices[seq(n_combinations)]
    alternating_indices_relevant_sort = sort(alternating_indices_relevant)
    features = combinations[alternating_indices_relevant_sort]
    features

    dt <- data.table::data.table(id_combination = seq(n_combinations))
    dt[, features := features]
    dt[, n_features := length(features[[1]]), id_combination]
    dt[, N := 1]
    dt[-c(1, .N), N := n[n_features]]
    dt[, N := as.integer(N)]
    dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]


  } else if (sampling_method == "largest_weights_random") {
    # largest_weights random -------------------------------------------------------------------------------------------

    # for (n_combinations in n_combinations_vec) {
    #   print(n_combinations)
      # Add one if odd as we will do paired and then rather remove one coalition afterwards
      n_combinations_new = ifelse(n_combinations %% 2 == 1, n_combinations + 1, n_combinations)

      # Check if we are in the edge case that we are wanting 2^m-1 or 2^m coalitions
      if (n_combinations_new == 2^m) {
        id_comb_include = seq(n_combinations_new)

      } else {
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

        # Find out which coalition size we have to do the sampling in
        sample_in_this_coalition_size = which.max(coalition_sizes_cumsum > n_combinations_new)

        # Get the number of paired coalitions to sample
        n_sample_in_this_coalition_size = n_combinations_new - coalition_sizes_cumsum[sample_in_this_coalition_size - 1]

        # Get the indices of earlier coalition sizes
        id_comb_include = seq(coalition_sizes_cumsum[sample_in_this_coalition_size - 1])

        # Then sample the remaining coalitions
        id_comb_include_samp = sort(sample(seq(coalition_sizes_cumsum[sample_in_this_coalition_size - 1] + 1,
                                               coalition_sizes_cumsum[sample_in_this_coalition_size]),
                                           n_sample_in_this_coalition_size))
        id_comb_include = c(id_comb_include, id_comb_include_samp)

        # Get the paired indices too
        id_comb_include = c(id_comb_include, rev(2^m + 1 - id_comb_include))
      }

      if (n_combinations %% 2 == 1) id_comb_include = id_comb_include[-(ceiling(n_combinations/2) + 1)]

      # Create list of all feature combinations
      combinations = unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)

      # Get the features to include
      features = combinations[id_comb_include]

      dt <- data.table::data.table(id_combination = seq(n_combinations))
      dt[, features := features]
      dt[, n_features := length(features[[1]]), id_combination]
      dt[, N := 1]
      dt[-c(1, .N), N := n[n_features]]
      dt[, N := as.integer(N)]
      dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]
    #}


  } else if (sampling_method == "largest_weights_combination_size") {
    # largest_weights_combination_size --------------------------------------------------------------------------------

    # Create list of all feature combinations
    combinations = unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)

    alternating_coalition_size_indices_function = function(x) {
      lower_idx = 1
      upper_idx = length(x) - 1

      ret_arr = x[c(1, length(x))]

      while (lower_idx < upper_idx) {
        if (lower_idx + 1 == upper_idx) {
          ret_arr = c(ret_arr, seq(x[lower_idx] + 1, x[lower_idx+1]))
        } else {
          ret_arr = c(ret_arr, seq(x[lower_idx] + 1, x[lower_idx+1]))
          ret_arr = c(ret_arr, seq(x[upper_idx], x[upper_idx-1] + 1))
        }

        # Increase and lower the indices
        lower_idx = lower_idx + 1
        upper_idx = upper_idx - 1
      }
      return(ret_arr)
    }
    alternating_coalition_size_indices = alternating_coalition_size_indices_function(cumsum(c(1, n, 1)))
    alternating_coalition_size_indices_relevant = alternating_coalition_size_indices[seq(n_combinations)]
    alternating_coalition_size_indices_relevant_sort = sort(alternating_coalition_size_indices_relevant)
    features = combinations[alternating_coalition_size_indices_relevant_sort]

    dt <- data.table::data.table(id_combination = seq(n_combinations))
    dt[, features := features]
    dt[, n_features := length(features[[1]]), id_combination]
    dt[, N := 1]
    dt[-c(1, .N), N := n[n_features]]
    dt[, N := as.integer(N)]
    dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]


  } else if (sampling_method == "smallest_weights") {
    # smallest_weights ------------------------------------------------------------------------------------------------

    # Create list of all feature combinations
    combinations = unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)

    # Create a list of the indices
    indices = seq(2, 2^m-1)

    # Alternate them such that we extract the smallest, then the largest, then the second smallest,
    # then the second largest and so on.
    alternating_indices = c(1, 2^m, rev(c(rbind(indices[1:(2^(m-1)-1)], rev(indices[-(1:(2^(m-1)-1))])))))
    alternating_indices_relevant = alternating_indices[seq(n_combinations)]
    alternating_indices_relevant_sort = sort(alternating_indices_relevant)
    features = combinations[alternating_indices_relevant_sort]
    features

    dt <- data.table::data.table(id_combination = seq(n_combinations))
    dt[, features := features]
    dt[, n_features := length(features[[1]]), id_combination]
    dt[, N := 1]
    dt[-c(1, .N), N := n[n_features]]
    dt[, N := as.integer(N)]
    dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]
    dt



  } else if (sampling_method == "smallest_weights_constant_SW") {
    # smallest_weights ------------------------------------------------------------------------------------------------

    # Create list of all feature combinations
    combinations = unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)

    # Create a list of the indices
    indices = seq(2, 2^m-1)

    # Alternate them such that we extract the smallest, then the largest, then the second smallest,
    # then the second largest and so on.
    alternating_indices = c(1, 2^m, rev(c(rbind(indices[1:(2^(m-1)-1)], rev(indices[-(1:(2^(m-1)-1))])))))
    alternating_indices_relevant = alternating_indices[seq(n_combinations)]
    alternating_indices_relevant_sort = sort(alternating_indices_relevant)
    features = combinations[alternating_indices_relevant_sort]
    features

    dt <- data.table::data.table(id_combination = seq(n_combinations))
    dt[, features := features]
    dt[, n_features := length(features[[1]]), id_combination]
    dt[, N := 1]
    dt[-c(1, .N), N := n[n_features]]
    dt[, N := as.integer(N)]
    dt[, shapley_weight := 1]
    dt



  } else if (sampling_method == "smallest_weights_combination_size") {
    # smallest_weights_combination_size -------------------------------------------------------------------------------

    # Create list of all feature combinations
    combinations = unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)

    alternating_coalition_size_indices_function_rev = function(x) {
      lower_idx = 1
      upper_idx = length(x) - 1

      ret_arr = c()

      while (lower_idx < upper_idx) {
        if (lower_idx + 1 == upper_idx) {
          ret_arr = c(ret_arr, seq(x[lower_idx] + 1, x[lower_idx+1]))
        } else {
          ret_arr = c(ret_arr, seq(x[lower_idx] + 1, x[lower_idx+1]))
          ret_arr = c(ret_arr, seq(x[upper_idx], x[upper_idx-1] + 1))
        }

        # Increase and lower the indices
        lower_idx = lower_idx + 1
        upper_idx = upper_idx - 1
      }
      ret_arr = rev(ret_arr)
      ret_arr = c(x[c(1, length(x))], ret_arr)
      return(ret_arr)
    }
    alternating_coalition_size_indices = alternating_coalition_size_indices_function_rev(cumsum(c(1, n, 1)))
    alternating_coalition_size_indices_relevant = alternating_coalition_size_indices[seq(n_combinations)]
    alternating_coalition_size_indices_relevant_sort = sort(alternating_coalition_size_indices_relevant)
    features = combinations[alternating_coalition_size_indices_relevant_sort]


    dt <- data.table::data.table(id_combination = seq(n_combinations))
    dt[, features := features]
    dt[, n_features := length(features[[1]]), id_combination]
    dt[, N := 1]
    dt[-c(1, .N), N := n[n_features]]
    dt[, N := as.integer(N)]
    dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]

  } else if (grepl("pilot", sampling_method)) {

    stop("Pilot estimates are yet not implemented...")
    # We are doing pilot estimates

    # Here I should add setup stuff


    # Here we look at the different versions
    if (sampling_method == "pilot_estimates_paired") {

    }

  }

  # TODO: LARS sjekk at disse funker over. Se på hvordan Shapley MAE feilen utvikler seg med disse når vi øker `n_combinations`,
  # print(dt)
  if (is.null(dt$p)) dt[,p:=NA]

  return(dt)
}

#' Calculate Shapley weight
#'
#' @param m Positive integer. Total number of features/feature groups.
#' @param n_components Positive integer. Represents the number of features/feature groups you want to sample from
#' a feature space consisting of `m` unique features/feature groups. Note that ` 0 < = n_components <= m`.
#' @param N Positive integer. The number of unique combinations when sampling `n_components` features/feature
#' groups, without replacement, from a sample space consisting of `m` different features/feature groups.
#' @param weight_zero_m Positive integer. Represents the Shapley weight for two special
#' cases, i.e. the case where you have either `0` or `m` features/feature groups.
#'
#'
#' @return Numeric
#' @keywords internal
#'
#' @author Nikolai Sellereite
shapley_weights <- function(m, N, n_components, weight_zero_m = 10^6) {
  x <- (m - 1) / (N * n_components * (m - n_components))
  x[!is.finite(x)] <- weight_zero_m
  x
}


#' @keywords internal
helper_feature <- function(m, feature_sample) {
  x <- feature_matrix_cpp(feature_sample, m)
  dt <- data.table::data.table(x)
  cnms <- paste0("V", seq(m))
  data.table::setnames(dt, cnms)
  dt[, sample_frequence := as.integer(.N), by = cnms]
  dt[, is_duplicate := duplicated(dt)]
  dt[, (cnms) := NULL]

  return(dt)
}


#' Analogue to feature_exact, but for groups instead.
#'
#' @inheritParams shapley_weights
#' @param group_num List. Contains vector of integers indicating the feature numbers for the
#' different groups.
#'
#' @return data.table with all feature group combinations, shapley weights etc.
#'
#' @keywords internal
feature_group <- function(group_num, weight_zero_m = 10^6) {
  m <- length(group_num)
  dt <- data.table::data.table(id_combination = seq(2^m))
  combinations <- lapply(0:m, utils::combn, x = m, simplify = FALSE)

  dt[, groups := unlist(combinations, recursive = FALSE)]
  dt[, features := lapply(groups, FUN = group_fun, group_num = group_num)]
  dt[, n_groups := length(groups[[1]]), id_combination]
  dt[, n_features := length(features[[1]]), id_combination]
  dt[, N := .N, n_groups]
  dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_groups, weight_zero_m)]

  return(dt)
}

#' @keywords internal
group_fun <- function(x, group_num) {
  if (length(x) != 0) {
    unlist(group_num[x])
  } else {
    integer(0)
  }
}


#' Analogue to feature_not_exact, but for groups instead.
#'
#' Analogue to feature_not_exact, but for groups instead.
#'
#' @inheritParams shapley_weights
#' @inheritParams feature_group
#'
#' @return data.table with all feature group combinations, shapley weights etc.
#'
#' @keywords internal
feature_group_not_exact <- function(group_num, n_combinations = 200, weight_zero_m = 10^6) {
  # Find weights for given number of features ----------
  m <- length(group_num)
  n_groups <- seq(m - 1)
  n <- sapply(n_groups, choose, n = m)
  w <- shapley_weights(m = m, N = n, n_groups) * n
  p <- w / sum(w)

  # Sample number of chosen features ----------
  feature_sample_all <- list()
  unique_samples <- 0

  while (unique_samples < n_combinations - 2) {
    # Sample number of chosen features ----------
    n_features_sample <- sample(
      x = n_groups,
      size = n_combinations - unique_samples - 2, # Sample -2 as we add zero and m samples below
      replace = TRUE,
      prob = p
    )

    # Sample specific set of features -------
    feature_sample <- sample_features_cpp(m, n_features_sample)
    feature_sample_all <- c(feature_sample_all, feature_sample)
    unique_samples <- length(unique(feature_sample_all))
  }

  # Add zero and m features
  feature_sample_all <- c(list(integer(0)), feature_sample_all, list(c(1:m)))
  X <- data.table(n_groups = sapply(feature_sample_all, length))
  X[, n_groups := as.integer(n_groups)]


  # Get number of occurences and duplicated rows-------
  is_duplicate <- NULL # due to NSE notes in R CMD check
  r <- helper_feature(m, feature_sample_all)
  X[, is_duplicate := r[["is_duplicate"]]]

  # When we sample combinations the Shapley weight is equal
  # to the frequency of the given combination
  X[, shapley_weight := r[["sample_frequence"]]]

  # Populate table and remove duplicated rows -------
  X[, groups := feature_sample_all]
  if (any(X[["is_duplicate"]])) {
    X <- X[is_duplicate == FALSE]
  }
  X[, is_duplicate := NULL]

  # Make group list into character
  X[, groups_tmp := sapply(groups, paste, collapse = " ")]

  # Aggregate weights by how many samples of a combination we have
  X <- X[, .(
    n_groups = data.table::first(n_groups),
    shapley_weight = sum(shapley_weight),
    groups = groups[1]
  ), groups_tmp]

  X[, groups_tmp := NULL]
  data.table::setorder(X, n_groups)


  # Add shapley weight and number of combinations
  X[c(1, .N), shapley_weight := weight_zero_m]
  X[, N := 1]
  ind <- X[, .I[data.table::between(n_groups, 1, m - 1)]]
  X[ind, p := p[n_groups]]
  X[ind, N := n[n_groups]]

  # Adding feature info
  X[, features := lapply(groups, FUN = group_fun, group_num = group_num)]
  X[, n_features := sapply(X$features, length)]

  # Set column order and key table
  data.table::setkeyv(X, "n_groups")
  X[, id_combination := .I]
  X[, N := as.integer(N)]
  nms <- c("id_combination", "groups", "features", "n_groups", "n_features", "N", "shapley_weight", "p")
  data.table::setcolorder(X, nms)

  return(X)
}

#' Calculate weighted matrix
#'
#' @param X data.table
#' @param normalize_W_weights Logical. Whether to normalize the weights for the combinations to sum to 1 for
#' increased numerical stability before solving the WLS (weighted least squares). Applies to all combinations
#' except combination `1` and `2^m`.
#' @param is_groupwise Logical. Indicating whether group wise Shapley values are to be computed.
#'
#' @return Numeric matrix. See [weight_matrix_cpp()] for more information.
#' @keywords internal
#'
#' @author Nikolai Sellereite, Martin Jullum
weight_matrix <- function(X, normalize_W_weights = TRUE, is_groupwise = FALSE) {
  # Fetch weights
  w <- X[["shapley_weight"]]

  if (normalize_W_weights) {
    w[-c(1, length(w))] <- w[-c(1, length(w))] / sum(w[-c(1, length(w))])
  }

  if (!is_groupwise) {
    W <- weight_matrix_cpp(
      subsets = X[["features"]],
      m = X[.N][["n_features"]],
      n = X[, .N],
      w = w
    )
  } else {
    W <- weight_matrix_cpp(
      subsets = X[["groups"]],
      m = X[.N][["n_groups"]],
      n = X[, .N],
      w = w
    )
  }

  return(W)
}

#' @keywords internal
create_S_batch_new <- function(internal, seed = NULL) {
  n_features0 <- internal$parameters$n_features
  approach0 <- internal$parameters$approach
  n_combinations <- internal$parameters$n_combinations
  n_batches <- internal$parameters$n_batches

  X <- internal$objects$X

  if (!is.null(seed)) set.seed(seed)

  if (length(approach0) > 1) {
    X[!(n_features %in% c(0, n_features0)), approach := approach0[n_features]]

    # Finding the number of batches per approach
    batch_count_dt <- X[!is.na(approach), list(
      n_batches_per_approach =
        pmax(1, round(.N / (n_combinations - 2) * n_batches)),
      n_S_per_approach = .N
    ), by = approach]

    # Ensures that the number of batches corresponds to `n_batches`
    if (sum(batch_count_dt$n_batches_per_approach) != n_batches) {
      # Ensure that the number of batches is not larger than `n_batches`.
      # Remove one batch from the approach with the most batches.
      while (sum(batch_count_dt$n_batches_per_approach) > n_batches) {
        batch_count_dt[
          which.max(n_batches_per_approach),
          n_batches_per_approach := n_batches_per_approach - 1
        ]
      }

      # Ensure that the number of batches is not lower than `n_batches`.
      # Add one batch to the approach with most coalitions per batch
      while (sum(batch_count_dt$n_batches_per_approach) < n_batches) {
        batch_count_dt[
          which.max(n_S_per_approach / n_batches_per_approach),
          n_batches_per_approach := n_batches_per_approach + 1
        ]
      }
    }

    batch_count_dt[, n_leftover_first_batch := n_S_per_approach %% n_batches_per_approach]
    data.table::setorder(batch_count_dt, -n_leftover_first_batch)

    approach_vec <- batch_count_dt[, approach]
    n_batch_vec <- batch_count_dt[, n_batches_per_approach]

    # Randomize order before ordering spreading the batches on the different approaches as evenly as possible
    # with respect to shapley_weight
    X[, randomorder := sample(.N)]
    data.table::setorder(X, randomorder) # To avoid smaller id_combinations always proceeding large ones
    data.table::setorder(X, shapley_weight)

    batch_counter <- 0
    for (i in seq_along(approach_vec)) {
      X[approach == approach_vec[i], batch := ceiling(.I / .N * n_batch_vec[i]) + batch_counter]
      batch_counter <- X[approach == approach_vec[i], max(batch)]
    }
  } else {
    X[!(n_features %in% c(0, n_features0)), approach := approach0]

    # Spreading the batches
    X[, randomorder := sample(.N)]
    data.table::setorder(X, randomorder)
    data.table::setorder(X, shapley_weight)
    X[!(n_features %in% c(0, n_features0)), batch := ceiling(.I / .N * n_batches)]
  }

  # Assigning batch 1 (which always is the smallest) to the full prediction.
  X[, randomorder := NULL]
  X[id_combination == max(id_combination), batch := 1]
  setkey(X, id_combination)

  # Create a list of the batch splits
  S_groups <- split(X[id_combination != 1, id_combination], X[id_combination != 1, batch])

  return(S_groups)
}
