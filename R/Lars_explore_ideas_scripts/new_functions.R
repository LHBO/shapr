#' Compute MAE and MSE errors
#'
#' @description
#' Function computes the MSE and MAE error averaged over the features, averaged over
#' the observations, and averaged over both the features and observations.
#'
#' @param dt_true data.table. A data.table containing the true Shapley values.
#' @param dt_approx data.table. A data.table containing the estimated
#' Shapley values obtained from the [sharp::explain()] function.
#' @param include_none If we are to include `none`
#' (i.e., the phi0 which is computed independently of the approach).
#'
#' @return List of the different evaluation scores
#' @export
#' @author Lars Henry Berge Olsen
mean_absolute_and_squared_errors = function(dt_true, dt_approx, include_none = FALSE) {

  # Remove the 'none' column if we are not to include them
  if (!include_none) {
    dt_true = dt_true[,-1]
    dt_approx = dt_approx[,-1]
  }

  # Compute the MSE and MAE error averaged over the features
  mse_error_individual = apply((dt_true - dt_approx)^2, 1, mean)
  mae_error_individual = apply(abs(dt_true - dt_approx), 1, mean)

  # Compute the MSE and MAE error averaged over the observations
  mse_error_feature = apply((dt_true - dt_approx)^2, 2, mean)
  mae_error_feature = apply(abs(dt_true - dt_approx), 2, mean)

  # Compute the MSE and MAE error averaged both over the features and observations
  mse_error = mean(mse_error_individual)
  mae_error = mean(mae_error_individual)

  # # Compute the relative error
  # apply(abs((dt_true - dt_approx) / dt_true), 2, mean, trim = 0.01)
  # apply(abs((dt_true - dt_approx) / dt_true), 1, mean)

  # Return the results
  return(list(mse = mse_error,
              mae = mae_error,
              mse_individual = mse_error_individual,
              mae_individual = mae_error_individual,
              mse_feature = mse_error_feature,
              mae_feature = mae_error_feature))
}


#' Repeatedly explain the output of ML models with conditional Shapley values
#'
#' @description
#' This function repeatedly calls the [shapr::explain()] function with different seeds.
#'
#' @param model The model whose predictions we want to explain. Run [shapr::get_supported_models()] for a table of
#' which models `explain` supports natively. Unsupported models can still be explained by passing `predict_model` and
#' (optionally) `get_model_specs`, see details in [shapr::explain()] for more information.
#' @param x_explain A matrix or data.frame/data.table.
#' Contains the the features, whose predictions ought to be explained.
#' @param x_train Matrix or data.frame/data.table. Contains the data used to estimate the (conditional) distributions
#' for the features needed to properly estimate the conditional expectations in the Shapley formula.
#' @param approach Character vector of length `1` or `n_features`.
#' `n_features` equals the total number of features in the model. All elements should,
#' either be `"gaussian"`, `"copula"`, `"empirical"`, `"ctree"`, `"categorical"`, `"timeseries"`, or `"independence"`.
#' See details for more information.
#' @param prediction_zero Numeric. The prediction value for unseen data, i.e. an estimate of the expected prediction
#' without conditioning on any features. Typically we set this value equal to the mean of the response variable in our
#' training data, but other choices such as the mean of the predictions in the training data are also reasonable.
#' @param keep_samp_for_vS Logical. Indicates whether the samples used in the Monte Carlo estimation of v_S should be
#' returned (in `internal$output`).
#' @param n_samples Positive integer. Indicating the maximum number of samples to use in the Monte Carlo integration
#' for every conditional expectation.
#' @param n_batches Positive integer (or NULL).
#' Specifies how many batches the total number of feature combinations should be split into when calculating the
#' contribution function for each test observation.
#' The default value is NULL which uses a reasonable trade-off between RAM allocation and computation speed,
#' which depends on `approach` and `n_combinations`.
#' For models with many features, increasing the number of batches reduces the RAM allocation significantly.
#' This typically comes with a small increase in computation time.
#' @param n_repetitions Integer. The number of repetitions to do.
#' @param seed_start_value Integer. For reproducibility the user can set the seed. It will increment with one
#' in each repetition to ensure different results in the stochastic sampling procedures of the MC samples.
#' @param n_combinations_from Integer. The starting value of the sequence of values for `n_combinations`.
#' @param n_combinations_to Integer. The (maximal) end value of the sequence of values for `n_combinations`.
#' @param n_combinations_increment Integer. The increment of the sequence of values for `n_combinations`.
#' @param ... Arguments passed on to the different approaches.
#' @param sampling_methods String or vector of strings. The string, or strings, has/have to be valid samplig methods.
#' @param use_precomputed_vS Logical. If we are to only compute the v(S) once for each repetition and use them
#' for all coalition sampling schemes, as this saves a lot of time.
#' @param save_path String. A save path where to save the results after each repetition.
#'
#' @return List of objects of class `c("shapr", "list")`. Contains the following items:
#' \describe{
#'   \item{shapley_values}{data.table with the estimated Shapley values}
#'   \item{internal}{List with the different parameters, data and functions used internally}
#'   \item{pred_explain}{Numeric vector with the predictions for the explained observations.}
#' }
#' See [shapr::explain()] for more details.
#' @export
#'
#' @author Lars Henry Berge Olsen
repeated_explanations = function(model,
                                 x_explain,
                                 x_train,
                                 approach,
                                 prediction_zero,
                                 keep_samp_for_vS,
                                 n_samples,
                                 n_batches,
                                 sampling_methods = c("unique",
                                                      "unique_paired",
                                                      "non_unique",
                                                      "unique_SW",
                                                      "unique_paired_SW",
                                                      "non_unique_SW",
                                                      "chronological_order_increasing",
                                                      "chronological_order_decreasing",
                                                      "largest_weights",
                                                      "largest_weights_combination_size",
                                                      "smallest_weights",
                                                      "smallest_weights_combination_size"),
                                 n_repetitions = 10,
                                 use_precomputed_vS = TRUE,
                                 seed_start_value = 1,
                                 n_combinations_from = 2, #ncol(x_explain) + 1,
                                 n_combinations_to = 2^ncol(x_explain),
                                 n_combinations_increment = 1,
                                 n_combinations_array = NULL,
                                 save_path = NULL,
                                 ...) {

  # Set the design of the progress bar
  progressr::handlers("cli")

  # Check for valid sampling methods
  sampling_methods = match.arg(sampling_methods, several.ok = TRUE)

  # Create a list to store the results
  result_list = lapply(sampling_methods, function(x) list())
  names(result_list) = sampling_methods

  # Check if we are using the array of `n_combinations` or have to create it ourselves
  if (is.null(n_combinations_array)) {
    # Small message to the user
    message(paste("We use the parameters `n_combinations_from`, `n_combinations_to` and `n_combinations_increment`",
                   "to determine the used `n_combinations`, and not `n_combinations_array`."))

    # Get the values of `n_combinations` we are to consider
    sequence_n_combinations = unique(c(seq(n_combinations_from, n_combinations_to, n_combinations_increment),
                                       n_combinations_to))
  } else {
    # Small message to the user
    message(paste("We use the parameter `n_combinations_array` to determine the used `n_combinations`, and not the",
                   "parameters `n_combinations_from`, `n_combinations_to` and `n_combinations_increment`."))

    # We use the provided values
    sequence_n_combinations = n_combinations_array
  }

  # Check if we are in the special case of linear model and Gaussian approach
  # as we can then compute the `precomputed_vS` much faster and more precise.
  if (use_precomputed_vS && approach == "gaussian" && class(model) == "lm") {
    use_precomputed_vS_gaussian_lm = TRUE

    # Small message to the user
    message(paste0("We use the LM-Gaussian strategy, then `n_samples` (", n_samples,
                   ") is no longer applicable. It will rather technically be `Inf`."))

    # These variables are only needed if we do the LM-Gaussian strategy
    # Get the number of features, number of test observations, and their predicted response using the model
    M = ncol(x_explain)
    n_test = nrow(x_explain)
    response_test = predict(model, x_explain)

  } else {
    use_precomputed_vS_gaussian_lm = FALSE
  }

  # Extract the seed value
  seed = seed_start_value

  # Get the number of sampling methods and
  n_sampling_methods = length(sampling_methods)
  n_sequence_n_combinations = length(sequence_n_combinations)

  # Iterate over the number of iterations
  idx_rep = 1
  for (idx_rep in seq(n_repetitions)) {

    # If we are to use precomputed
    if (use_precomputed_vS) {

      # Check what kind of precomputed_vS we are going to use
      if (use_precomputed_vS_gaussian_lm) {
        # We are using the Gaussian approach or the predictive model is a linear model

        # Small message to user
        message(sprintf("Creating the `precomputed_vS` for repetition %d of %d using the LM-Gaussian strategy.",
                        idx_rep, n_repetitions))

        # We are going to call `shapr::explain()` once to set up the `shapr` object. To do this we do not need
        # to estimate the contribution functions accurately hence we could set n_samples = 1, but it is faster
        # to use a precomputed dt_vS list with just rubbish. We add the special cases for the empty and full set
        # but we let the other entries be 0.
        dt_vS = data.table(id_combination = rep(seq(2^M)))[, `:=` (paste0("p_hat1_", seq(n_test)), 0)]
        dt_vS[id_combination == 1, `:=` (names(dt_vS)[-1], prediction_zero)] # can be directly given as it is a scalar.
        dt_vS[id_combination == .N, `:=` (names(dt_vS)[-1], as.list(response_test))] # need to be a list as it is a vector.

        # Create the shapr object. The Shapley value output will be rubbish,
        # but we only need the object/list structure. Do not want any warnings.
        progressr::with_progress({
          explanations_tmp = suppressWarnings(suppressMessages(
            shapr::explain(
              model = model,
              x_explain = x_explain,
              x_train = x_train,
              approach = approach,
              prediction_zero = prediction_zero,
              keep_samp_for_vS = keep_samp_for_vS,
              exact = TRUE,
              # n_combinations = 2^ncol(x_explain), # Do not need it as we specify `exact = TRUE`.
              n_samples = 1,
              n_batches = n_batches,
              seed = seed,
              precomputed_vS = list(dt_vS = dt_vS),
              ...
            )))}, enable = TRUE)

        # Compute the precompute_vS using the LM-Gaussian strategy
        progressr::with_progress({
          precomputed_vS = explain_linear_model_Gaussian_data(
            explanation = explanations_tmp,
            linear_model = model,
            only_return_dt_vS_list = TRUE)
        }, enable = TRUE)

      } else {
        # We are NOT using the Gaussian approach or the predictive model is NOT a linear model.
        # We therefore use the default version

        # Small warning to the user
        if (ncol(x_explain) > 10) message("Computing `precomputed_vS` might take some time due to many featueres.\n")

        # Small message to user
        message(paste0("Creating the `precomputed_vS` for repetition ", idx_rep, " of ", n_repetitions,
                       " using the Monte Carlo Integration strategy."))

        # We set the number of MC samples to use to be the value provided by the user
        n_samples_used = n_samples

        # Do not want any warnings
        progressr::with_progress({
          precomputed_vS = suppressWarnings(suppressMessages(
            shapr::explain(
              model = model,
              x_explain = x_explain,
              x_train = x_train,
              approach = approach,
              prediction_zero = prediction_zero,
              keep_samp_for_vS = keep_samp_for_vS,
              n_combinations = 2^ncol(x_explain),
              n_samples = n_samples_used,
              n_batches = n_batches,
              seed = seed,
              ...
            )))$internal$output}, enable = TRUE)
      }
    } else {
      precomputed_vS = NULL
    }



    # Iterate over the sampling methods
    sampling_method_idx = 1
    for (sampling_method_idx in seq(n_sampling_methods)) {
      sampling_method = sampling_methods[sampling_method_idx]

      # Small printout to the user
      message(sprintf("Rep %d (%d of %d). Method: %s (%d of %d).",
                      idx_rep, idx_rep, n_repetitions,
                      sampling_method, sampling_method_idx, n_sampling_methods))

      # A string used in the result list
      idx_rep_str = paste0("repetition_", idx_rep)

      # Add a new empty sub sub list in the result list
      result_list[[sampling_method]][[idx_rep_str]] = list()

      # If we are using a paired method, then we skip interactions where n_combinations is odd
      if (grepl("paired", sampling_method)) {
        used_sequence_n_combinations = sequence_n_combinations[sequence_n_combinations %% 2 == 0]
      } else {
        used_sequence_n_combinations = sequence_n_combinations
      }

      # Get the number of different `n_combinations`
      n_combinations_total = length(used_sequence_n_combinations)

      # Create a temp function which computes the Shapley values for a specific number of coalitions
      compute_SV_function = function(n_combinations,
                                     n_combinations_to,
                                     model,
                                     x_explain,
                                     x_train,
                                     approach,
                                     prediction_zero,
                                     keep_samp_for_vS,
                                     n_samples,
                                     n_batches,
                                     seed,
                                     sampling_method,
                                     precomputed_vS,
                                     progress_bar,
                                     ...) {

        # Call the `shapr::explain` function with the provided parameters
        tmp_res = suppressMessages(suppressWarnings(
          shapr::explain(
            model = model,
            x_explain = x_explain,
            x_train = x_train,
            approach = approach,
            prediction_zero = prediction_zero,
            keep_samp_for_vS = keep_samp_for_vS,
            n_combinations = n_combinations,
            n_samples = n_samples,
            n_batches = min(n_combinations-1, n_batches),
            seed = seed,
            sampling_method = sampling_method,
            precomputed_vS = precomputed_vS,
            ...
          )))

        # Only want to save the extra stuff for the first object to save storage due to a lot of duplicates.
        if (n_combinations != used_sequence_n_combinations[1]) {
          tmp_res[["only_save"]] = tmp_res$internal$objects[c(2,3,4)]
          tmp_res$internal = NULL
          tmp_res$timing = NULL
          tmp_res$pred_explain = NULL
        }

        # EXTRACT PROGRESS iteration
        progress_bar

        # Update the progress bar
        progress_bar(message = sprintf("Rep: %d of %d. Method: %s (%d of %d). N_comb: %d of %d.",
                                       idx_rep, n_repetitions,
                                       sampling_method, sampling_method_idx, n_sampling_methods,
                                       n_combinations, n_combinations_to))

        # Return the results
        return(tmp_res)
      }

      # Have wrapped the future.apply::future_lapply inside this function to make the progressr work
      future_compute_SV_function = function(compute_SV_function,
                                            used_sequence_n_combinations,
                                            n_combinations_total,
                                            n_combinations_to,
                                            model,
                                            x_explain,
                                            x_train,
                                            approach,
                                            prediction_zero,
                                            keep_samp_for_vS,
                                            n_samples,
                                            n_batches,
                                            seed,
                                            sampling_method,
                                            precomputed_vS,
                                            ...) {

        # Create a progress bar
        progress_bar = progressr::progressor(steps = n_combinations_total)

        # Call the tmp_function for the different number of coalitions
        future.apply::future_lapply(
          X = as.list(used_sequence_n_combinations),
          FUN = suppressMessages(suppressWarnings(compute_SV_function)),
          n_combinations_to = n_combinations_to,
          model = model,
          x_explain = x_explain,
          x_train = x_train,
          approach = approach,
          prediction_zero = prediction_zero,
          keep_samp_for_vS = keep_samp_for_vS,
          n_samples = n_samples,
          n_batches = n_batches,
          seed = seed,
          sampling_method = sampling_method,
          precomputed_vS = precomputed_vS,
          progress_bar = progress_bar,
          future.seed = 1,
          future.scheduling = n_combinations_total,
          ...
        )
      }

      # Get the estimated Shapley values using the specified parameters
      result_list[[sampling_method]][[idx_rep_str]] = with_progress(
        future_compute_SV_function(compute_SV_function = compute_SV_function,
                                   used_sequence_n_combinations = used_sequence_n_combinations,
                                   n_combinations_total = n_combinations_total,
                                   n_combinations_to = n_combinations_to,
                                   model = model,
                                   x_explain = x_explain,
                                   x_train = x_train,
                                   approach = approach,
                                   prediction_zero = prediction_zero,
                                   keep_samp_for_vS = keep_samp_for_vS,
                                   n_samples = n_samples,
                                   n_batches = n_batches,
                                   seed = seed,
                                   sampling_method = sampling_method,
                                   precomputed_vS = precomputed_vS,
                                   ...),
        enable = TRUE)

      #
      #       # Ensure that we get a progress bar
      #       progressr::with_progress({
      #         # Iterate over the n_combinations sequence and compute the Shapley values
      #         result_list[[sampling_method]][[idx_rep_str]] = suppressMessages(suppressWarnings(future.apply::future_lapply(
      #           X = as.list(used_sequence_n_combinations),
      #           FUN = tmp_function,
      #           model = model,
      #           x_explain = x_explain,
      #           x_train = x_train,
      #           approach = approach,
      #           prediction_zero = prediction_zero,
      #           keep_samp_for_vS = keep_samp_for_vS,
      #           n_samples = n_samples,
      #           n_batches = n_batches,
      #           seed = seed,
      #           sampling_method = sampling_method,
      #           precomputed_vS = precomputed_vS,
      #           progress_bar = progress_bar,
      #           future.seed = 1,
      #           ...
      #         )))}, enable = TRUE)



      # Update the names
      names(result_list[[sampling_method]][[idx_rep_str]]) = paste0("n_combinations_", used_sequence_n_combinations)


      # # Old version where I used a for loop
      # n_combinations_idx = 1
      # for (n_combinations_idx in seq(n_sequence_n_combinations)) {
      #   n_combinations = sequence_n_combinations[n_combinations_idx]
      #
      #   # A string used in the result list
      #   n_combinations_str = paste0("n_combinations_", n_combinations)
      #
      #   # If we are using a paired method, then we skip interactions where n_combinations is odd
      #   if (grepl("paired", sampling_method) && n_combinations %% 2 == 1) next
      #
      #   # # Small printout to the user
      #   # cat(sprintf("Rep %d (%d of %d). Method: %s (%d of %d). N_comb %d (%d of %d).\n",
      #   #             idx_rep, idx_rep, n_repetitions,
      #   #             sampling_method, sampling_method_idx, n_sampling_methods,
      #   #             n_combinations, n_combinations_idx, n_sequence_n_combinations))
      #
      #   # preogressr::progressbar(message = sprintf("Rep %d (%d of %d). Method: %s (%d of %d). N_comb %d (%d of %d).\n",
      #   #                               idx_rep, idx_rep, n_repetitions,
      #   #                               sampling_method, sampling_method_idx, n_sampling_methods,
      #   #                               n_combinations, n_combinations_idx, n_sequence_n_combinations))
      #
      #   # Create the explanations and save the shapr objects to the results list
      #   result_list[[sampling_method]][[idx_rep_str]][[n_combinations_str]] = suppressMessages(explain(
      #     model = model,
      #     x_explain = x_explain,
      #     x_train = x_train,
      #     approach = approach,
      #     prediction_zero = prediction_zero,
      #     keep_samp_for_vS = keep_samp_for_vS,
      #     n_combinations = n_combinations,
      #     n_samples = n_samples,
      #     n_batches = min(n_combinations-1, n_batches),
      #     seed = seed,
      #     sampling_method = sampling_method,
      #     precomputed_vS = precomputed_vS,
      #     ...
      #   ))
      #
      #   if (n_combinations != sequence_n_combinations[1]) {
      #     result_list[[sampling_method]][[idx_rep_str]][[n_combinations_str]][["only_save"]] =
      #       result_list[[sampling_method]][[idx_rep_str]][[n_combinations_str]]$internal$objects[c(2,3,4)]
      #     result_list[[sampling_method]][[idx_rep_str]][[n_combinations_str]]$internal = NULL
      #     result_list[[sampling_method]][[idx_rep_str]][[n_combinations_str]]$timing = NULL
      #     result_list[[sampling_method]][[idx_rep_str]][[n_combinations_str]]$pred_explain = NULL
      #   }
      # }
    }

    # Update the seed value
    seed = seed + 1

    # Save the results if a save path has been provided
    if (!is.null(save_path)) {
      saveRDS(result_list, save_path)
    }
  }

  # return the results
  return(result_list)
}


#' Compute more precise Shapley values for linear model with the Gaussian approach
#'
#' @param explanation list. The returned object from the [shapr::explain()] function.
#' @param linear_model A fitted linear model using the [base::lm()] function.
#' @param only_return_dt_vS_list Boolean. If `TRUE`, then we skip computing the Shapley values
#' and rater directly return a list containing the estimated contribution function values.
#'
#' @return An `shapr` object. Same as the `explanation` input. Or a list containing the `vS_list`
#' if `only_return_dt_vS_list` is `TRUE`.
#' @export
#' @author Lars Henry Berge Olsen
explain_linear_model_Gaussian_data = function(explanation, linear_model, only_return_dt_vS_list = FALSE) {
  # Check that user has provided a correct explanation object
  if (!"shapr" %in% class(explanation)) {
    stop("The `explanation` parameter must be an object of class 'shapr'.")
  }

  # Check that the approach is Gaussian
  if (explanation$internal$parameters$approach != "gaussian") {
    stop("This function is only applicable when `approach = gaussian`.")
  }

  # Check that the model is linear
  if (!"lm" %in% class(linear_model)) {
    stop("The `linear_model` parameter must be an object of class 'lm'.")
  }

  # Create a progress bar
  progress_bar = progressr::progressor(explanation$internal$parameters$used_n_combinations - 2)

  # Create a data.table containing the conditional means and the corresponding given
  # feature values for all test observations and coalitions.
  dt = data.table::rbindlist(
    future.apply::future_lapply(
      seq(2, explanation$internal$parameters$used_n_combinations - 1), # Skip the empty and full coalitions
      function(S_ind, S, mu, cov_mat, x_explain, feature_names, progress_bar) {
        # This function computes the conditional mean of Xsbar | Xs = Xs_star and combine those
        # values with the

        # Get boolean representations if the features are in the S and the Sbar sets
        S_now = as.logical(S[S_ind,])
        Sbar_now = !as.logical(S[S_ind,])

        # Extract the mean values for the features in the two sets
        mu_S = mu[S_now]
        mu_Sbar = mu[Sbar_now]

        # Extract the relevant parts of the covariance matrix
        cov_mat_SbarS = cov_mat[Sbar_now, S_now, drop = FALSE]
        cov_mat_SS = cov_mat[S_now, S_now, drop = FALSE]
        cov_mat_SbarS_cov_mat_SS_inv = cov_mat_SbarS %*% solve(cov_mat_SS)

        # Extract the features we condition on
        x_S_star = x_explain[,..S_now]

        # Compute the conditional mean of Xsbar given Xs = Xs_star
        x_Sbar_mean_dt = data.table(t(mu_Sbar + cov_mat_SbarS_cov_mat_SS_inv %*% t(sweep(x_S_star, 2, mu_S, FUN = "-"))))

        # Update the progress bar
        progress_bar(amount = 1, message = "Estimating v(S) (LM-Gauss)")

        # Combine the conditional means with the conditional feature values
        return(cbind(id = seq(nrow(x_explain)),
                     data.table::copy(x_explain)[, (feature_names[Sbar_now]) := x_Sbar_mean_dt]))
      },
      S = explanation$internal$objects$S,
      mu = explanation$internal$parameters$gaussian.mu,
      cov_mat = explanation$internal$parameters$gaussian.cov_mat,
      x_explain = explanation$internal$data$x_explain,
      feature_names = explanation$internal$parameters$feature_names,
      progress_bar = progress_bar
    ),
    idcol = "id_combination")

  # We have to add one to id_combination as it started to count at one,
  # but we skipped the first id_combination in the computations above (empty set).
  dt[, id_combination := id_combination + 1]

  # Compute the estimated contribution functions
  dt[, "vS_hat" := (cbind(1, as.matrix(.SD)) %*% linear_model$coefficients),
     .SDcols = explanation$internal$parameters$feature_names]

  # We `dcast` the data.table to go from long to wide data.table,
  # where each row is coalition and and each column is an explicand.
  # Extract the results for the empty and grand coalition from the explanation object,
  # and then combine all of this together.
  # As the column names of these objects are different, we set `use.names = FALSE`.
  # Then we merge the data.tables without warning and checking that the column names matches.
  # REMOVED THIS ONE 13.10.23 as I earlier just set the whole dt_vS to zero.
  # dt_vS = rbind(
  #   explanation$internal$output$dt_vS[1],
  #   dcast(dt, id_combination ~ id, value.var = "vS_hat"), # Here we go from long to wide data table.
  #   explanation$internal$output$dt_vS[.N],
  #   use.names = FALSE
  # )
  dt_vS = rbind(
    data.table::data.table(id_combination = 1)[, `:=` (paste0("p_hat1_", seq(explanation$internal$parameters$n_explain)),
                                                       explanation$internal$parameters$prediction_zero)],
    data.table::dcast(dt, id_combination ~ id, value.var = "vS_hat"), # Here we go from long to wide data table.
    data.table::data.table(id_combination = explanation$internal$parameters$used_n_combinations)[, paste0("p_hat1_", seq(explanation$internal$parameters$n_explain)) :=
                                                                                                   as.list(shapr::predict_model(linear_model, explanation$internal$data$x_explain))],
    use.names = FALSE
  )

  # If we only are to return the computed contribution functions
  # This is useful when we are investigating the coalition sampling procedures
  # as we then do not to compute the Shapley values.
  if (only_return_dt_vS_list) return(list(dt_vS = dt_vS))

  # Compute the Shapley values again, but this time with the precomputed contribution functions
  progressr::with_progress({
    updated_explanation <- suppressWarnings(suppressMessages(
      shapr::explain(
        model = linear_model,
        x_explain = explanation$internal$data$x_explain,
        x_train = explanation$internal$data$x_train,
        approach = explanation$internal$parameters$approach,
        prediction_zero = explanation$internal$parameters$prediction_zero,
        keep_samp_for_vS = explanation$internal$parameters$keep_samp_for_vS,
        exact = explanation$internal$parameters$exact,
        n_combinations = explanation$internal$parameters$n_combinations, # Just to not get the message from shapr.
        n_samples = explanation$internal$parameters$n_samples,
        n_batches = explanation$internal$parameters$n_batches,
        gaussian.mu = explanation$internal$parameters$gaussian.mu,
        gaussian.cov_mat = explanation$internal$parameters$gaussian.cov_mat,
        seed = explanation$internal$parameters$seed,
        precomputed_vS = list(dt_vS = dt_vS) # We use dt_vS to compute the Shapley values
      )))}, enable = TRUE)

  # Set the number of samples to `Inf` and create a new boolean
  updated_explanation$internal$parameters$n_samples = Inf
  updated_explanation$internal$parameters$lm_gaussian_strategy = TRUE

  # Return the updated explanations
  return(updated_explanation)
}



#' Aggregate the results of repeated explanations and plot them
#'
#' @param repeated_explanations_list List. Output from the [shapr::repeated_explanations()] function.
#' @param true_explanations Shapr object.
#' Output from the [shapr::explain()] function containing the true Shapley values.
#' @param evaluation_criterion String. Either "MAE" or "MSE". Default is `MAE`.
#' @param level Numeric. The confidence level required. Default is `0.95`.
#' @param plot_figures Logic. If `TRUE`, then plot the figures.
#' @param return_figures Logic. If `TRUE`, then return the figures.
#' @param return_dt Logic. If `TRUE`, then return the data.tables containing the results.
#' @param ggplot_theme A [ggplot2::theme()] object to customize the non-data components of the plots:
#' i.e. titles, labels, fonts, background, gridlines, and legends. Themes can be used to give plots
#' a consistent customized look. Use the themes available in \code{\link[ggplot2:theme_bw]{ggplot2::ggtheme()}}.
#' if you would like to use a complete theme such as `theme_bw()`, `theme_minimal()`, and more.
#' @param brewer_palette String. Name of one of the color palettes from [RColorBrewer::RColorBrewer()].
#'  If `NULL`, then the function uses the default [ggplot2::ggplot()] color scheme.
#' The following palettes are available for use with these scales:
#' \describe{
#'    \item{Diverging}{BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral}
#'    \item{Qualitative}{Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3}
#'    \item{Sequential}{Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges,
#'      OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd}
#' }
#' @param brewer_direction Sets the order of colors in the scale. If 1, the default,
#' colors are as output by \code{\link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal()}}.
#' If -1, the order of colors is reversed.
#' @param flip_coordinates Boolean. Flip Cartesian coordinates so that the methods are on the y-axis.
#' This is primarily useful for converting geoms and statistics which display y conditional on x, to x conditional on y.
#' See [ggplot2::coord_flip()].
#' @param legend_position String or numeric vector `c(x,y)`. The allowed string values for the
#' argument `legend_position` are: `left`,`top`, `right`, `bottom`, and `none`. Note that, the argument
#' `legend_position` can be also a numeric vector `c(x,y)`. In this case it is possible to position
#' the legend inside the plotting area. `x` and `y` are the coordinates of the legend box.
#' Their values should be between `0` and `1`, where `c(0,0)` corresponds to the "bottom left"
#' and `c(1,1)` corresponds to the "top right" position.
#' @param index_combinations Integer vector. Which of the coalitions (combinations) to plot.
#' E.g. if you we only want combinations with even number of coalitions, we can set
#' `index_combinations = seq(4, 2^{n_features}, 2)`.
#' @param dt_CI Data.table. Returned data.table from earlier call to the function.
#' @param dt_long Data.table Returned data.table from earlier call to the function.
#' @param only_these_sampling_methods Array of strings. If we are only to use/plot these samping methods.
#'
#' @return Depends on the values of `return_figures` and `return_dt`.
#' @export
#'
#' @author Lars Henry Berge Olsen
aggregate_and_plot_results = function(repeated_explanations_list,
                                      true_explanations,
                                      index_combinations = NULL,
                                      evaluation_criterion = c("MAE", "MSE"),
                                      level = 0.95,
                                      plot_figures = FALSE,
                                      return_figures = TRUE,
                                      return_dt = TRUE,
                                      ggplot_theme = NULL,
                                      brewer_palette = NULL,
                                      brewer_direction = 1,
                                      flip_coordinates = FALSE,
                                      legend_position = NULL,
                                      scale_y_log10 = FALSE,
                                      scale_x_log10 = FALSE,
                                      dt_CI = NULL,
                                      dt_long = NULL,
                                      only_these_sampling_methods = NULL,
                                      n_workers = 1) {
  # Setup and checks ----------------------------------------------------------------------------
  # Check that ggplot2 is installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('data.table')")
  }
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('future')")
  }
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('future.apply')")
  }

  # Check if user only provided a single explanation and did not put it in a list
  if ("shapr" %in% class(repeated_explanations_list)) {
    # Put it in a list
    repeated_explanations_list <- list(repeated_explanations_list)
  }

  n_max_workers = future::availableCores()
  if (n_workers > n_max_workers) {
    warning(sprintf("Too many workers. Change from %d to %d (max available cores).",
                    n_workers, n_max_workers))
    n_workers = n_max_workers
  }

  if (n_workers > 1) {
    future::plan(multisession, workers = n_workers)
  } else {
    future::plan(sequential)
  }

  # Provide names for the sampling methods if not provided by the user
  if (is.null(names(repeated_explanations_list)) && !is.null(repeated_explanations_list)) {
    names(repeated_explanations_list) = paste("Sampling Method", seq(length(repeated_explanations_list)))
    message(paste0("The `repeated_explanations_list` was not a named list. Set the names to be: '",
                   paste(names(repeated_explanations_list), collapse = "', '"), "'."))
  }

  # Extract which evaluation criterion we are going to plot
  evaluation_criterion = match.arg(evaluation_criterion)

  # Get the number of repetitions
  n_repetitions = length(repeated_explanations_list[[1]])


  # Make data.tables ------------------------------------------------------------------------------------------------
  if (is.null(dt_CI)) {
    # Create list where each entry is a `n_coalitions` times `n_repetitions` matrix containing
    # the overall evaluation criterion (MAE or MSE) between the true Shapley values
    # (using all coalitions and a high value of `n_combinations`) and the repeated runs
    # (different seed values) with different sampling methods and number of used coalitions.
    results_list =
      future.apply::future_lapply(repeated_explanations_list, function (ith_method) {
        sapply(ith_method, function(ith_method_jth_repetition) {
          sapply(ith_method_jth_repetition, function(ith_method_jth_repetition_kth_coalition) {
            mean_absolute_and_squared_errors(
              true_explanations$shapley_values,
              ith_method_jth_repetition_kth_coalition$shapley_values)[[tolower(evaluation_criterion)]]
          })
        })
      })

    # For each method and `n_combination` value, compute the median and the quantile confidence interval
    # based on the user provided `level`. The default is a 95% confidence interval. Convert to a data.table.
    results_dt_with_missing_entries =
      data.table::rbindlist(
        future.apply::future_lapply(results_list, function(ith_method) {
          median_and_ci = apply(ith_method, 1, quantile, probs = c((1 - level)/2, 0.5, 1 - (1 - level)/2), na.rm = TRUE)
          tmp_dt = data.table::data.table(n_combinations =
                                            as.numeric(sapply(strsplit(rownames(ith_method), "_(?!.*_)", perl=TRUE), "[[", 2)),
                                          CI_lower = median_and_ci[1,],
                                          median = median_and_ci[2,],
                                          CI_upper = median_and_ci[3,],
                                          mean = apply(ith_method, 1, mean),
                                          min = apply(ith_method, 1, min),
                                          max = apply(ith_method, 1, max))
        }), idcol = "sampling")
    results_dt_with_missing_entries$sampling = factor(results_dt_with_missing_entries$sampling,
                                                      levels = names(repeated_explanations_list),
                                                      ordered = TRUE)

    # Remove the rows with missing entries
    results_dt = results_dt_with_missing_entries[!is.na(results_dt_with_missing_entries$median)]
  } else {
    results_dt = dt_CI
  }

  # Only keep the desired combinations
  if (!is.null(index_combinations)) {
    results_dt <- results_dt[n_combinations %in% index_combinations]
  }


  # We also compute some alternative aggregated versions of the data not needed to make the figure.
  # Create an alternative aggregated results data.table
  if (is.null(dt_long)) {
    result_dt_alternative =
      data.table::rbindlist(
        future.apply::future_lapply(results_list, function(ith_method) {
          data.table::data.table(n_combinations = as.numeric(sapply(strsplit(rownames(ith_method), "_(?!.*_)", perl=TRUE), "[[", 2)),
                                 ith_method)
        }), idcol = "sampling")


    # Convert the sampling column to a factor
    result_dt_alternative$sampling = factor(result_dt_alternative$sampling,
                                            levels = names(repeated_explanations_list),
                                            ordered = TRUE)

    # Remove rows with missing entries
    result_dt_alternative = result_dt_alternative[!is.na(result_dt_alternative$repetition_1)]

    # Change the column names
    data.table::setnames(result_dt_alternative, c(names(result_dt_alternative)[1:2], paste(seq(n_repetitions))))

    # Convert from a wide to long data.table
    result_dt_alternative_long = data.table::melt(data = result_dt_alternative,
                                                  id.vars = c("sampling", "n_combinations"),
                                                  variable.name = "repetition",
                                                  value.name = "evaluation_criterion")

  } else {
    result_dt_alternative_long = dt_long
  }

  future::plan(sequential)

  # Only keep the desired combinations
  if (!is.null(index_combinations)) {
    result_dt_alternative_long <- result_dt_alternative_long[n_combinations %in% index_combinations]
  }

  # Only use the specified sampling methods
  if (!is.null(only_these_sampling_methods)) {
    result_dt_alternative_long = result_dt_alternative_long[sampling %in% only_these_sampling_methods]
    results_dt = results_dt[sampling %in% only_these_sampling_methods]
  }

  # For the box plots to work we need the combinations to be a factor
  result_dt_alternative_long_combination_factor = copy(result_dt_alternative_long)
  result_dt_alternative_long_combination_factor$n_combinations =
    factor(result_dt_alternative_long_combination_factor$n_combinations)



  # Make the figures ------------------------------------------------------------------------------------------------
  # A list to store the figures
  figure_list = list()

  # Plot the results
  figure_list[["figure_CI"]] =
    ggplot2::ggplot(results_dt, ggplot2::aes(x = n_combinations, y = median, col = sampling)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = CI_lower, ymax = CI_upper, fill = sampling), alpha = 0.3) +
    ggplot2::labs(
      x = "Number of coalitions",
      y = paste0(evaluation_criterion, " (median + ", level*100,  "% CI)"),
      col = "Sampling method",
      fill = "Sampling method") +
    {if (!scale_y_log10) ggplot2::expand_limits(y = 0)} +
    {if (scale_y_log10) ggplot2::scale_y_log10()} +
    {if (scale_x_log10) ggplot2::scale_x_log10()} +
    {if (is.null(brewer_palette)) ggplot2::scale_fill_hue()} +
    {if (is.null(brewer_palette)) ggplot2::scale_color_hue()} +
    {if (!is.null(brewer_palette)) ggplot2::scale_fill_brewer(palette = brewer_palette,
                                                              direction = brewer_direction)} +
    {if (!is.null(brewer_palette)) ggplot2::scale_color_brewer(palette = brewer_palette,
                                                               direction = brewer_direction)} +
    {if (!is.null(ggplot_theme)) ggplot_theme} +
    {if (flip_coordinates) ggplot2::coord_flip()} +
    {if (!is.null(legend_position)) ggplot2::theme(legend.position = legend_position)}


  # linetype = c(as.numeric(results_dt$sampling) %% 3)
  # linetype[linetype == 0] = 3
  # results_dt[,linetype := linetype]

  figure_list[["figure_mean"]] =
    ggplot2::ggplot(results_dt, ggplot2::aes(x = n_combinations, y = mean, col = sampling)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Number of coalitions",
      y = paste(evaluation_criterion, "(mean)"),
      col = "Sampling method",
      fill = "Sampling method") +
    {if (!scale_y_log10) ggplot2::expand_limits(y = 0)} +
    {if (scale_y_log10) ggplot2::scale_y_log10()} +
    {if (scale_x_log10) ggplot2::scale_x_log10()} +
    {if (is.null(brewer_palette)) ggplot2::scale_color_hue()} +
    {if (!is.null(brewer_palette)) ggplot2::scale_color_brewer(palette = brewer_palette,
                                                               direction = brewer_direction)} +
    {if (!is.null(ggplot_theme)) ggplot_theme} +
    {if (flip_coordinates) ggplot2::coord_flip()} +
    {if (!is.null(legend_position)) ggplot2::theme(legend.position = legend_position)}

  figure_list[["figure_median"]] =
    ggplot2::ggplot(results_dt, ggplot2::aes(x = n_combinations, y = median, col = sampling)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Number of coalitions",
      y = paste(evaluation_criterion, "(median)"),
      col = "Sampling method",
      fill = "Sampling method") +
    {if (!scale_y_log10) ggplot2::expand_limits(y = 0)} +
    {if (scale_y_log10) ggplot2::scale_y_log10()} +
    {if (scale_x_log10) ggplot2::scale_x_log10()} +
    {if (is.null(brewer_palette)) ggplot2::scale_color_hue()} +
    {if (!is.null(brewer_palette)) ggplot2::scale_color_brewer(palette = brewer_palette,
                                                               direction = brewer_direction)} +
    {if (!is.null(ggplot_theme)) ggplot_theme} +
    {if (flip_coordinates) ggplot2::coord_flip()} +
    {if (!is.null(legend_position)) ggplot2::theme(legend.position = legend_position)}

  figure_list[["figure_lines"]] =
    ggplot2::ggplot(result_dt_alternative_long,
                    ggplot2::aes(x = n_combinations,
                                 y = evaluation_criterion,
                                 color = repetition,
                                 linetype = sampling,
                                 group = interaction(sampling, repetition))) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Number of coalitions",
      y = evaluation_criterion,
      col = "Repetition",
      linetype = "Sampling method") +
    {if (!scale_y_log10) ggplot2::expand_limits(y = 0)} +
    {if (scale_y_log10) ggplot2::scale_y_log10()} +
    {if (scale_x_log10) ggplot2::scale_x_log10()} +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(order = 1),
      color = ggplot2::guide_legend(order = 2)) +
    {if (is.null(brewer_palette)) ggplot2::scale_color_hue()} +
    {if (!is.null(brewer_palette)) ggplot2::scale_color_brewer(palette = brewer_palette,
                                                               direction = brewer_direction)} +
    {if (!is.null(ggplot_theme)) ggplot_theme} +
    {if (flip_coordinates) ggplot2::coord_flip()} +
    {if (!is.null(legend_position)) ggplot2::theme(legend.position = legend_position)}


  figure_list[["figure_boxplot"]] =
    ggplot2::ggplot(result_dt_alternative_long_combination_factor,
                    ggplot2::aes(x = n_combinations,
                                 y = evaluation_criterion,
                                 fill = sampling)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(
      x = "Number of coalitions",
      y = evaluation_criterion,
      fill = "Sampling method") +
    {if (!scale_y_log10) ggplot2::expand_limits(y = 0)} +
    {if (scale_y_log10) ggplot2::scale_y_log10()} +
    {if (scale_x_log10) ggplot2::scale_x_log10()} +
    {if (is.null(brewer_palette)) ggplot2::scale_fill_hue()} +
    {if (is.null(brewer_palette)) ggplot2::scale_color_hue()} +
    {if (!is.null(brewer_palette)) ggplot2::scale_fill_brewer(palette = brewer_palette,
                                                              direction = brewer_direction)} +
    {if (!is.null(brewer_palette)) ggplot2::scale_color_brewer(palette = brewer_palette,
                                                               direction = brewer_direction)} +
    {if (!is.null(ggplot_theme)) ggplot_theme} +
    {if (flip_coordinates) ggplot2::coord_flip()} +
    {if (!is.null(legend_position)) ggplot2::theme(legend.position = legend_position)}


  figure_list[["figure_lines_boxplot"]] =
    ggplot2::ggplot(result_dt_alternative_long_combination_factor,
                    ggplot2::aes(x = n_combinations,
                                 y = evaluation_criterion,
                                 fill = sampling)) +
    ggplot2::geom_line(ggplot2::aes(linetype = sampling,
                                    color = repetition,
                                    group = interaction(sampling, repetition))) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(
      x = "Number of coalitions",
      y = evaluation_criterion,
      col = "Repetition",
      fill = "Sampling method",
      linetype = "Sampling method") +
    {if (!scale_y_log10) ggplot2::expand_limits(y = 0)} +
    {if (scale_y_log10) ggplot2::scale_y_log10()} +
    {if (scale_x_log10) ggplot2::scale_x_log10()} +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1),
      linetype = ggplot2::guide_legend(order = 2),
      color = ggplot2::guide_legend(order = 3)) +
    {if (is.null(brewer_palette)) ggplot2::scale_fill_hue()} +
    {if (is.null(brewer_palette)) ggplot2::scale_color_hue()} +
    {if (!is.null(brewer_palette)) ggplot2::scale_fill_brewer(palette = brewer_palette,
                                                              direction = brewer_direction)} +
    {if (!is.null(brewer_palette)) ggplot2::scale_color_brewer(palette = brewer_palette,
                                                               direction = brewer_direction)} +
    {if (!is.null(ggplot_theme)) ggplot_theme} +
    {if (flip_coordinates) ggplot2::coord_flip()} +
    {if (!is.null(legend_position)) ggplot2::theme(legend.position = legend_position)}


  figure_list[["figure_boxplot_lines"]] =
    ggplot2::ggplot(result_dt_alternative_long_combination_factor,
                    ggplot2::aes(x = n_combinations,
                                 y = evaluation_criterion,
                                 fill = sampling)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_line(ggplot2::aes(linetype = sampling,
                                    color = repetition,
                                    group = interaction(sampling, repetition))) +
    ggplot2::labs(
      x = "Number of coalitions",
      y = evaluation_criterion,
      col = "Repetition",
      fill = "Sampling method",
      linetype = "Sampling method") +
    {if (!scale_y_log10) ggplot2::expand_limits(y = 0)} +
    {if (scale_y_log10) ggplot2::scale_y_log10()} +
    {if (scale_x_log10) ggplot2::scale_x_log10()} +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1),
      linetype = ggplot2::guide_legend(order = 2),
      color = ggplot2::guide_legend(order = 3)) +
    {if (is.null(brewer_palette)) ggplot2::scale_fill_hue()} +
    {if (is.null(brewer_palette)) ggplot2::scale_color_hue()} +
    {if (!is.null(brewer_palette)) ggplot2::scale_fill_brewer(palette = brewer_palette,
                                                              direction = brewer_direction)} +
    {if (!is.null(brewer_palette)) ggplot2::scale_color_brewer(palette = brewer_palette,
                                                               direction = brewer_direction)} +
    {if (!is.null(ggplot_theme)) ggplot_theme} +
    {if (flip_coordinates) ggplot2::coord_flip()} +
    {if (!is.null(legend_position)) ggplot2::theme(legend.position = legend_position)}


  # If plot figures, then plot the figures
  if (plot_figures) lapply(rev(figure_list), plot)

  # Return the results ---------------------------------------------------------------------------------------------
  # Check if we are to return figure or data.table
  if (return_figures || return_dt) {
    # Create a list to store the objects to return.
    return_list = list()

    # If return figure, than add the figure to the return list
    if (return_figures) return_list[["figures"]] = figure_list

    # If return data.table, than add the data.table to the return list
    if (return_dt) {
      return_list[["dt"]] = list("dt_CI" = results_dt,
                                 "dt_wide" = result_dt_alternative,
                                 "dt_long" = result_dt_alternative_long)
    }

    # If we are only returning one object we do not use a list.
    if (length(return_list) == 1) {
      return(return_list[[1]])
    } else {
      return(return_list)
    }
  }
}

