# Libraries -------------------------------------------------------------------------------------------------------
library(ggplot2)
library(data.table)
library(future)
library(future.apply)

# Functions -------------------------------------------------------------------------------------------------------
## File functions --------------------------------------------------------------------------------------------------
#' Function that extract the setup parameters from the file name
#'
#' @description
#' It looks for the patter "M_INT_n_train_INT_n_test_INT_rho_NUMERIC_betas_NUMERIC_ARRAY_dt_STRING\\.rds"
#'
#' @param input_string string. The filename of the file containing the results.
#'
#' @return List of the the extracted values
#' @export
extract_parameters_from_path <- function(input_string) {
  pattern <- "M_(\\d+)_n_train_(\\d+)_n_test_(\\d+)_rho_([0-9.]+)_betas_((?:-?\\d+(?:\\.\\d+)?_?)+)_dt_(MAE|MSE)\\.rds"
  match_result <- regexec(pattern, input_string)

  if (any(match_result[[1]] == -1)) stop("Pattern not found in the input string.")

  M <- as.numeric(regmatches(input_string, match_result)[[1]][2])
  n_train <- as.numeric(regmatches(input_string, match_result)[[1]][3])
  n_test <- as.numeric(regmatches(input_string, match_result)[[1]][4])
  rho <- as.numeric(regmatches(input_string, match_result)[[1]][5])
  betas_str <- regmatches(input_string, match_result)[[1]][6]
  betas <- as.numeric(strsplit(betas_str, "_")[[1]])
  evaluation_criterion <- regmatches(input_string, match_result)[[1]][7]

  return(list(M = M, n_train = n_train, n_test = n_test, rho = rho, betas_str = betas_str,
              betas = betas, evaluation_criterion = evaluation_criterion))
}

## Evaluation metrics ----------------------------------------------------------------------------------------------
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

#' Compute the MSE or MAE between to matrices
#'
#' @param dt_1 Matrix. A matrix containing the true Shapley values.
#' @param dt_2 Matrix. A matrix containing the estimated Shapley values
#' @param evaluation_criterion String. What evaluation criterion to use. Either "MSE" or "MAE".
#'
#' @return Numeric. The calculated evaluation criterion.
#' @export
compute_MAE_MSE_fast = function(mat_1, mat_2, evaluation_criterion = c("MSE", "MAE")) {
  evaluation_criterion = match.arg(evaluation_criterion)
  if (evaluation_criterion == "MSE") mean((mat_1[,-1] - mat_2[,-1])^2) else mean(abs(mat_1[,-1] - mat_2[,-1]))
}






## Shapley value functions ------------------------------------------------------------------------------------------

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
#' @param use_pilot_estimates_regression Boolean. If `TRUE`, then we compute the pilot estimate using regression.
#'   If `FALSE`, then we use the true contribution function values as the estimates and `pilot_approach_regression`
#'   and `pilot_regression_model` are ignored.
#' @param pilot_approach_regression String containing the regression approach (i.e., separate or surrogate)
#'   to use to compute the pilot estimates if `use_pilot_estimates_regression` is `TRUE.`
#' @param pilot_regression_model String containing the regression model to use to compute the pilot estimates if
#'  `use_pilot_estimates_regression` is `TRUE.`
#' @param n_combinations_array Array of integers containing the number of coalitions we should consider.
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
                                                      "unique_unif",
                                                      "unique_paired_V2",
                                                      "unique_SW",
                                                      "unique_equal_weights",
                                                      "unique_equal_weights_symmetric",
                                                      "unique_paired",
                                                      "unique_paired_unif",
                                                      "unique_paired_unif_V2",
                                                      "unique_paired_SW",
                                                      "unique_paired_equal_weights",
                                                      "unique_paired_equal_weights_1000",
                                                      "unique_paired_equal_weights_2500",
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
                                                      "largest_weights_combination_size",
                                                      "smallest_weights",
                                                      "smallest_weights_constant_SW",
                                                      "smallest_weights_combination_size",
                                                      "paired_coalitions",
                                                      "paired_coalitions_weights",
                                                      "paired_coalitions_weights_direct",
                                                      "paired_coalitions_weights_equal_weights",
                                                      "paired_coalitions_weights_direct_equal_weights",
                                                      "paired_coalitions_sub",
                                                      "paired_coalitions_scaled",
                                                      "paired_coalitions_avg",
                                                      "paired_coalitions_norm",
                                                      "single_mean_coalition_effect",
                                                      "single_median_coalition_effect",
                                                      "single_mean_ranking_over_each_test_obs",
                                                      "single_median_ranking_over_each_test_obs",
                                                      "pilot_estimates_paired",
                                                      "largest_weights_random",
                                                      "MAD"),
                                 n_repetitions = 10,
                                 use_precomputed_vS = TRUE,
                                 use_pilot_estimates_regression = TRUE,
                                 pilot_approach_regression = "regression_surrogate",
                                 pilot_regression_model = "parsnip::linear_reg()",
                                 seed_start_value = 1,
                                 n_combinations_from = 2, #ncol(x_explain) + 1,
                                 n_combinations_to = 2^ncol(x_explain),
                                 n_combinations_increment = 1,
                                 n_combinations_array = NULL,
                                 save_path = NULL,
                                 true_shapley_values_path = NULL,
                                 ...) {

  # Set the design of the progress bar
  progressr::handlers("cli")

  # print("In repeated_explanations")
  # print(sampling_methods)

  # Check for valid sampling methods
  # sampling_methods = match.arg(sampling_methods, several.ok = TRUE)

  if (!is.character(pilot_approach_regression) || !is.character(pilot_regression_model)) {
    stop("Both `pilot_approach_regression` and `pilot_regression_model` must be strings.")
  }

  # Create a list of the strategies that use pre-computed pilot-estimates
  specific_coalition_set_strategies = c("paired_coalitions",
                                        "paired_coalitions_weights",
                                        "paired_coalitions_weights_direct",
                                        "paired_coalitions_weights_equal_weights",
                                        "paired_coalitions_weights_direct_equal_weights",
                                        "paired_coalitions_sub",
                                        "paired_coalitions_scaled",
                                        "paired_coalitions_avg",
                                        "paired_coalitions_norm",
                                        "single_mean_coalition_effect",
                                        "single_median_coalition_effect",
                                        "single_mean_ranking_over_each_test_obs",
                                        "single_median_ranking_over_each_test_obs",
                                        "MAD")

  # Check if we are using the `specific_coalition_set` parameter in the `explain` function.
  # TODO. Here I should check that these are substrings of the sampling_methods, as the sampling
  # methods can have extended names with extra information so the name does not match.
  using_specific_coalition_set = any(sampling_methods %in% specific_coalition_set_strategies)

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

    # A string used in the result list
    idx_rep_str = paste0("repetition_", idx_rep)

    # If we are to use precomputed
    if (use_precomputed_vS) {

      # Check what kind of precomputed_vS we are going to use
      if (!is.null(true_shapley_values_path)) {
        message("Loadining the `precomputed_vS` from the true Shapley values.")
        explanation_precomputed_vS = readRDS(true_shapley_values_path)
        precomputed_vS = explanation_precomputed_vS$internal$output # Extract only the precomputed_vS list

      } else if (use_precomputed_vS_gaussian_lm) {
        # We are using the Gaussian approach and the predictive model is a linear model

        # Small message to user
        if (n_repetitions == 1) {
          message("Creating the `precomputed_vS` using the LM-Gaussian strategy.")
        } else {
          message(sprintf("Creating the `precomputed_vS` for repetition %d of %d using the LM-Gaussian strategy.",
                          idx_rep, n_repetitions))
        }

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
          explanations_tmp = suppressMessages(suppressWarnings(
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
            ))
        )},
          enable = TRUE)

        # Compute the explanation_precompute_vS using the LM-Gaussian strategy
        # Could save a tiny bit of time by setting this to `only_return_dt_vS_list = TRUE`,
        # but `pilot_estimates_paired_order` uses `explanation` as input.
        # TODO: Could rewrite that function quite easy.
        progressr::with_progress({
          explanation_precomputed_vS = explain_linear_model_Gaussian_data(
            explanation = explanations_tmp,
            linear_model = model,
            only_return_dt_vS_list = FALSE)
        }, enable = TRUE)

        # Extract only the precomputed_vS list
        precomputed_vS = explanation_precomputed_vS$internal$output

      } else {
        # We are NOT using the Gaussian approach or the predictive model is NOT a linear model.
        # We therefore use the default version

        # Small warning to the user
        if (ncol(x_explain) > 10) message("Computing `precomputed_vS` might take some time due to many featueres.")

        # Small message to user
        if (n_repetitions == 1) {
          message("Creating the `precomputed_vS` using the Monte Carlo Integration strategy.")
        } else {
          message(paste0("Creating the `precomputed_vS` for repetition ", idx_rep, " of ", n_repetitions,
                         " using the Monte Carlo Integration strategy."))
        }

        # We set the number of MC samples to use to be the value provided by the user
        n_samples_used = n_samples

        # Do not want any warnings
        progressr::with_progress({
          explanation_precomputed_vS = suppressMessages(suppressWarnings(
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
            ))
            )}, enable = TRUE)

        # Extract only the precomputed_vS list
        precomputed_vS = explanation_precomputed_vS$internal$output
      }
    } else {
      precomputed_vS = NULL

      # Stop if user has provided not suitable combination of parameters
      if (using_specific_coalition_set) {
        stop("To use `specific_coalition_set` in `explain`, the parameter `use_precomputed_vS` must be `TRUE`.")
      }
    }

    # Check if we need to compute the `specific_coalition_set`.
    if (using_specific_coalition_set) {
      message("Creating the pilot orders")

      specific_coalition_set_true = pilot_estimates_coal_order(explanation_precomputed_vS)

      # If we want to estimate the pilot estimates using
      if (use_pilot_estimates_regression) {

        message(paste0("Started to train the `", pilot_approach_regression, "` approach with `",
                       pilot_regression_model,"` regression model(s)."))

        progressr::with_progress({
          explanation_precomputed_vS =
            shapr::explain(
              model = model,
              x_explain = x_explain,
              x_train = x_train,
              prediction_zero = prediction_zero,
              keep_samp_for_vS = FALSE,
              n_combinations = 2^ncol(x_explain),
              n_batches = n_batches,
              seed = seed,
              approach = pilot_approach_regression,
              regression.model = pilot_regression_model,
              regression.tune_values = NULL,
              regression.vfold_cv_para = NULL,
              regression.recipe_func = NULL,
              regression.surrogate_n_comb = min(1000, 2^ncol(x_explain)-2),
              ...
            )}, enable = TRUE)

        message(paste0("Done with training the regression model(s)."))

        # Get the `specific_coalition_set`.
        #specific_coalition_set = pilot_estimates_paired_order(explanation_precomputed_vS, plot_figures = TRUE)
        specific_coalition_set = pilot_estimates_coal_order(explanation_precomputed_vS)
      } else {
        specific_coalition_set = specific_coalition_set_true
      }

      # TODO: REMOVE THIS PRINTOUT Compare the order using the true quantities and the regression method
      # print(Map(rbind, specific_coalition_set_true, specific_coalition_set))
      result_list[["True_vs_Pilot_Order"]][[idx_rep_str]] = Map(rbind, specific_coalition_set_true, specific_coalition_set)
      #print(result_list[["True_vs_Pilot_Order"]][[idx_rep_str]])
      #message(result_list[["True_vs_Pilot_Order"]][[idx_rep_str]])

      # TODO: we use the Shapley kernel weights now, but might change that in the future.
      # 2024-29-04 Changed to equal weights (1) and 10^6 for the empty and grand coalitions
      specific_coalition_set_weights = lapply(seq_along(specific_coalition_set), function(x) NULL)
      # specific_coalition_set_weights =
      #   lapply(seq_along(specific_coalition_set), function(x) c(10^6, 10^6, rep(1, 2^ncol(x_explain))))
      names(specific_coalition_set_weights) = names(specific_coalition_set)

      # Add the new pilot sampling methods weights
      specific_coalition_set$paired_coalitions_weights = specific_coalition_set$paired_coalitions_weights
      specific_coalition_set$paired_coalitions_weights_equal_weights = specific_coalition_set$paired_coalitions_weights
      specific_coalition_set$paired_coalitions_weights_direct = specific_coalition_set$paired_coalitions_weights
      specific_coalition_set$paired_coalitions_weights_direct_equal_weights = specific_coalition_set$paired_coalitions_weights

      specific_coalition_set_weights$paired_coalitions_weights = specific_coalition_set$paired_coalitions_weights
      specific_coalition_set_weights$paired_coalitions_weights_equal_weights = specific_coalition_set$paired_coalitions_weights
      specific_coalition_set_weights$paired_coalitions_weights_direct = specific_coalition_set$paired_coalitions_weights
      specific_coalition_set_weights$paired_coalitions_weights_direct_equal_weights = specific_coalition_set$paired_coalitions_weights

      #specific_coalition_set_weights
      #print(specific_coalition_set_weights)
    }

    # Iterate over the sampling methods
    sampling_method_idx = 1
    for (sampling_method_idx in seq(n_sampling_methods)) {
      sampling_method = sampling_methods[sampling_method_idx]
      replace_W = grepl("_replace_W", sampling_method, fixed = TRUE)
      sampling_method_updated = gsub("_replace_W", "", sampling_method)
      new_weights = grepl("_new_weights", sampling_method, fixed = TRUE)
      if (new_weights) {
        new_weights_string = tail(strsplit(sampling_method, "_")[[1]], 1)
        sampling_method_updated = gsub(paste0("_new_weights_", new_weights_string), "", sampling_method)

        if (R.utils::System$getHostname() == "Larss-MacBook-Pro.local" || Sys.info()[[7]] == "larsolsen") {
          file_name = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Samp_prop_and_gompertz_M_", ncol(x_explain), ".rds")
        } else {
          # kadingir
          file_name = paste0("/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location/Samp_prop_and_gompertz_M_", ncol(x_explain), ".rds")
        }
        if (!file.exists(file_name)) stop("There are no Samp_prop_and_gompertz file for this dimension.")
        dt_new_weights = readRDS(file_name)
      } else {
        new_weights_string = NULL
        dt_new_weights = NULL
        sampling_method_updated = gsub("_new_weights", "", sampling_method)
      }


      # Small printout to the user
      if (n_repetitions == 1) {
        message(sprintf("Method: %s (%d of %d).",
                        sampling_method, sampling_method_idx, n_sampling_methods))
      } else {
        message(sprintf("Rep %d (%d of %d). Method: %s (%d of %d).",
                        idx_rep, idx_rep, n_repetitions,
                        sampling_method, sampling_method_idx, n_sampling_methods))
      }

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

      # Get the estimated Shapley values using the specified parameters
      result_list[[sampling_method]][[idx_rep_str]] = with_progress(
        future_compute_SV_function(
          compute_SV_function = compute_SV_function,
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
          sampling_method = if (sampling_method_updated %in% specific_coalition_set_strategies) "specific_coalition_set" else sampling_method,
          sampling_method_full_name = sampling_method,
          sampling_method_idx = sampling_method_idx,
          n_sampling_methods = n_sampling_methods,
          precomputed_vS = precomputed_vS,
          specific_coalition_set =
            if (sampling_method_updated %in% specific_coalition_set_strategies) specific_coalition_set[[sampling_method_updated]] else NULL,
          specific_coalition_set_weights =
            if (sampling_method_updated %in% specific_coalition_set_strategies) specific_coalition_set_weights[[sampling_method_updated]] else NULL,
          replace_W = replace_W,
          new_weights_string = new_weights_string,
          dt_new_weights = dt_new_weights,
          n_repetitions = n_repetitions),
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
    if (!is.null(save_path)) saveRDS(result_list, save_path)
  }

  # return the results
  return(result_list)
}


#' Title
#' @description
#' # Create a temp function which computes the Shapley values for a specific number of coalitions
#'
#' @param n_combinations
#' @param n_combinations_to
#' @param used_sequence_n_combinations
#' @param model
#' @param x_explain
#' @param x_train
#' @param approach
#' @param prediction_zero
#' @param keep_samp_for_vS
#' @param n_samples
#' @param n_batches
#' @param seed
#' @param sampling_method
#' @param precomputed_vS
#' @param specific_coalition_set
#' @param specific_coalition_set_weights
#' @param progress_bar
#' @param ...
#'
#' @return
#'
#' @keywords internal
compute_SV_function = function(n_combinations,
                               n_combinations_to,
                               used_sequence_n_combinations,
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
                               sampling_method_full_name,
                               sampling_method_idx,
                               n_sampling_methods,
                               precomputed_vS,
                               specific_coalition_set,
                               specific_coalition_set_weights,
                               replace_W,
                               new_weights_string,
                               dt_new_weights,
                               n_repetitions,
                               progress_bar,
                               ...) {

  # print("In compute_SV_function.")
  # print(sampling_method)

  # If the sampling method is one of these, then we do not want to remove some of the coalitions
  # in the specific_coalition_set, as we need all of them.
  specific_coalition_set_strategies_sampling = c("paired_coalitions_weights",
                                                 "paired_coalitions_weights_direct",
                                                 "paired_coalitions_weights_equal_weights",
                                                 "paired_coalitions_weights_direct_equal_weights")

  sampling_method_full_name_updated = gsub("_replace_W", "", sampling_method_full_name)
  if (!is.null(new_weights_string)) {
    sampling_method_full_name_updated = gsub(paste0("_new_weights_", new_weights_string), "", sampling_method_full_name)
  }

  # Extract only the relevant coalitions from `specific_coalition_set` (not for method in specific_coalition_set_strategies_sampling)
  if (!is.null(specific_coalition_set) && !sampling_method_full_name_updated %in% specific_coalition_set_strategies_sampling) {
    specific_coalition_set = specific_coalition_set[seq(n_combinations)]
  }

  # Extract only the relevant coalitions from `specific_coalition_set_weights` (not for method in specific_coalition_set_strategies_sampling)
  if (!is.null(specific_coalition_set_weights) && !sampling_method_full_name_updated %in% specific_coalition_set_strategies_sampling) {
    specific_coalition_set_weights = specific_coalition_set_weights[seq(n_combinations)]
  }

  # Call the `shapr::explain` function with the provided parameters
  # print("hei")
  tmp_res = #suppressMessages(suppressWarnings(
    explain(
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
      sampling_method_full_name = sampling_method_full_name,
      precomputed_vS = precomputed_vS,
      specific_coalition_set = specific_coalition_set,
      specific_coalition_set_weights = specific_coalition_set_weights,
      new_weights_string = new_weights_string,
      dt_new_weights = dt_new_weights,
      replace_W = replace_W #, ...
    )
  #))
  # print("hei2")

  # Only want to save the extra stuff for the first object to save storage due to a lot of duplicates.
  if (n_combinations != used_sequence_n_combinations[1]) {
    tmp_res[["only_save"]] = tmp_res$internal$objects[c(2,3,4)]
    tmp_res$internal = NULL
    tmp_res$timing = NULL
    tmp_res$pred_explain = NULL
  }

  # Update the progress bar
  if (n_repetitions == 1) {
    progress_bar(message = sprintf("Method: %s (%d of %d). N_comb: %d of %d.",
                                   sampling_method, sampling_method_idx, n_sampling_methods,
                                   n_combinations, n_combinations_to))
  } else {
    progress_bar(message = sprintf("Rep: %d of %d. Method: %s (%d of %d). N_comb: %d of %d.",
                                   idx_rep, n_repetitions,
                                   sampling_method, sampling_method_idx, n_sampling_methods,
                                   n_combinations, n_combinations_to))
  }

  # Return the results
  return(tmp_res)
}


#' Title
#'
#' @description
#' Have wrapped the future.apply::future_lapply inside this function to make the progressr work
#'
#'
#' @param compute_SV_function
#' @param used_sequence_n_combinations
#' @param n_combinations_total
#' @param n_combinations_to
#' @param model
#' @param x_explain
#' @param x_train
#' @param approach
#' @param prediction_zero
#' @param keep_samp_for_vS
#' @param n_samples
#' @param n_batches
#' @param seed
#' @param sampling_method
#' @param precomputed_vS
#' @param specific_coalition_set
#' @param specific_coalition_set_weights
#' @param ...
#'
#' @return
#' @keywords internal
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
                                      sampling_method_full_name,
                                      sampling_method_idx,
                                      n_sampling_methods,
                                      precomputed_vS,
                                      specific_coalition_set,
                                      specific_coalition_set_weights,
                                      n_repetitions,
                                      replace_W,
                                      new_weights_string,
                                      dt_new_weights,
                                      ...) {

  # print("In future_compute_SV_function.")

  # Create a progress bar
  progress_bar = progressr::progressor(steps = n_combinations_total)

  # Call the tmp_function for the different number of coalitions
  future.apply::future_lapply(
    X = as.list(used_sequence_n_combinations),
    FUN = compute_SV_function, # suppressMessages(suppressWarnings(compute_SV_function)),
    used_sequence_n_combinations = used_sequence_n_combinations,
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
    sampling_method_full_name = sampling_method_full_name,
    sampling_method_idx = sampling_method_idx,
    n_sampling_methods = n_sampling_methods,
    precomputed_vS = precomputed_vS,
    specific_coalition_set = specific_coalition_set,
    specific_coalition_set_weights = specific_coalition_set_weights,
    replace_W = replace_W,
    new_weights_string = new_weights_string,
    dt_new_weights = dt_new_weights,
    n_repetitions = n_repetitions,
    progress_bar = progress_bar,
    future.seed = 1,
    future.scheduling = n_combinations_total,
    ...
  )
}





## Gaussian-LM functions-----------------------------------------------------------------------------------------------
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
    updated_explanation <- suppressMessages(suppressWarnings(
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














## Pilot estimates functions ---------------------------------------------------------------------------------------

#' Extract best coalition order with highest absolute importance
#'
#' @param explanation A [shapr] explanation object with all coalitions. Should be "true_explanations" in the beggining.
#' But later these will be the pilot estimates from e.g. a liner model.
#' @param plot_figures Boolean. If we are to plot figures.
#' @param strategies
#' @param always_empty_and_grand_coalitions_first
#'
#' @return array with the indices of the coalitions that we should add in decreasing importance order.
#' I.e., the first one is the most important.
#' @export
#'
#' @examples
pilot_estimates_coal_order = function(explanation,
                                      strategies = c("paired_coalitions",
                                                     "paired_coalitions_sub",
                                                     "paired_coalitions_scaled",
                                                     "paired_coalitions_avg",
                                                     "paired_coalitions_norm",
                                                     "paired_coalitions_weights",
                                                     "paired_coalitions_weights_direct",
                                                     "paired_coalitions_weights_equal_weights",
                                                     "paired_coalitions_weights_direct_equal_weights",
                                                     "single_mean_coalition_effect",
                                                     "single_median_coalition_effect",
                                                     "single_mean_ranking_over_each_test_obs",
                                                     "single_median_ranking_over_each_test_obs",
                                                     "MAD"),
                                      plot_figures = FALSE,
                                      always_empty_and_grand_coalitions_first = TRUE) {

  # explanation = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_rds_saves/Paper3_Experiment_M_5_n_train_1000_n_test_250_rho_0.5_betas_2_10_0.25_-3_-1_1.5_true.rds")

  # Check for valid strategies
  strategies = match.arg(strategies, several.ok = TRUE)

  # Extract the internal list from the explanation object
  internal = explanation$internal

  # Get the number of features
  M = internal$parameters$n_features

  if (2^M != internal$parameters$n_combinations) {
    stop("Currently we need the `explanation` to have `n_combinations` = 2^M, where M is the number of featuers.")
    # TODO: we can later remove this if we do not need this as we can rather return the features to use
    # internal$objects$X$features instead of using the index of these.
  }

  # We extract the W and S matrices
  W = explanation$internal$objects$W
  S = explanation$internal$objects$S

  # Extract the v(S) elements
  dt_vS = explanation$internal$output$dt_vS

  # Compute the R's. I.e., W * v(s), but without adding them together. So not a matrix product, but rather
  # an element wise multiplication. Do it for all test observations and store the results in a list.
  # So the list contains `N_explain` matrices of dimension `(M+1) x 2^M`
  R_matrix_list_all_individuals =
    lapply(seq(ncol(dt_vS[, -"id_combination"])),
           function (test_obs_idx) t(t(W)*dt_vS[, -"id_combination"][[test_obs_idx]]))
  print(object.size(R_matrix_list_all_individuals), units = "GB")

  # List to store the results to be returned
  return_list = list()
  if ("MAD" %in% strategies) {

    # Compute the absolute difference between v(S; x) and f(x) for all x and S
    T = abs(explanation$internal$output$dt_vS[,-"id_combination"] -
              explanation$internal$output$dt_vS[rep(.N, .N), -"id_combination"])

    # Remove the empty and grand coalitions
    T = T[-c(1, .N)]

    # Compute the MAD and its inverse. A coalition with low MAD contains important features, for the smaller coalition
    # sizes. While the MAD is high for large coalition sizes where you miss an important feature.
    MAD = rowMeans(T)

    # Compute the number of coalitions of each size
    n_features <- seq(M - 1)
    n <- sapply(n_features, choose, n = M)

    # Compute the cumulative sum of the number of features in the different coalition sizes
    n_cumsum = c(0, cumsum(n)) # Add 0 as a left bound for first coalition size and to make the lapply below work

    # Make a list where we the i'th entry is a vector containing the MAD values for coalitions of size i
    MAD_comb_size = lapply(seq(length(n_cumsum) - 1), function(idx) MAD[seq(n_cumsum[idx] + 1, n_cumsum[idx + 1])])

    # Get the possible coalition sizes that have a paired coalition size. So max{|S|, |Sbar|}.
    all_coal_sizes = seq(ceiling((M - 1)/2))
    all_paired_coal_sizes = seq(floor((M - 1)/2))

    # Index updates to use below. I.e., for size 2, we need to add M to the orders to get the overall order
    # sapply(all_coal_sizes, function(size) length(unlist(MAD_comb_size[seq(0, size - 1)])))
    update_indices = c(0, cumsum(sapply(MAD_comb_size[all_coal_sizes - 1], length)))

    # Get the order of most important coalitions for each size
    MAD_order_list = lapply(all_coal_sizes, function(size) {

      # Update the index so it respects the previous coalition sizes
      update_index = update_indices[size]

      if (size %in% all_paired_coal_sizes) {
        # Does have a paired version. Then we compute MAD(S) - MAD(Sbar).
        # An important S will have a low MAD(S) and high MAD(Sbar).
        # So overall, an important S yields a low MAD(S) - MAD(Sbar). So order them in increasing order.
        # Need to reverse the order to get the same order in both arrays.
        order_now = update_index + order(MAD_comb_size[[size]] - rev(MAD_comb_size[[M - size]]), decreasing = FALSE)

        # Add the paired version Sbar right after the S, so that they are kept together
        order_now = c(rbind(order_now, 2^M - 1 - order_now))
      } else {
        # No paired version, so order the coalitions in increasing order
        order_now = update_index + order(MAD_comb_size[[size]], decreasing = FALSE)
      }
      order_now
    })

    MAD_order = c(1, 2^M, unlist(MAD_order_list) + 1) # Include empty and grand comb, and add one due to empty set.

    # plot(seq(2, length(MAD) + 1), MAD[MAD_order[-c(1,2)] - 1])

    return_list$MAD = MAD_order


  }

  if (any(c("paired_coalitions", "paired_coalitions_sub", "paired_coalitions_scaled", "paired_coalitions_avg",
            "paired_coalitions_norm") %in% strategies)) {
    # We are doing the paired version
    # Alternate them such that we extract the smallest, then the largest, then the second smallest,
    # then the second largest and so on.
    alternating_indices = c(rbind(seq(1, 2^(M-1)), seq(2^M, 2^(M-1) + 1)))

    # Compute the mean standard weights for the different coalitions
    tmp = W[-1, alternating_indices]
    tmp2 = tmp[,seq(1, 2^M - 1, 2)] - tmp[,seq(2, 2^M, 2)]
    standard_weight = colMeans(abs(tmp2))
#
#     {
#       tmp = W[4, alternating_indices]
#       par(mfrow = c(2,2))
#       plot(tmp)
#       plot(abs(tmp))
#       tmp2 = tmp[seq(1, 2^M - 1, 2)] - tmp[seq(2, 2^M, 2)]
#       plot(tmp2)
#       plot(abs(tmp2))
#       par(mfrow = c(1,1))
#     }
#     {
#       par(mfrow = c(2,2))
#       tmp = W[-1, alternating_indices]
#       matplot(t(tmp), type = "b")
#       matplot(t(abs(tmp)), type = "b")
#       tmp2 = tmp[,seq(1, 2^M - 1, 2)] - tmp[,seq(2, 2^M, 2)]
#       matplot(t(tmp2))
#       matplot(t(abs(tmp2)))
#       par(mfrow = c(1,1))
#     }
#     {
#       par(mfrow = c(2,2))
#       tmp = W[-1, alternating_indices]
#       tmp2 = tmp[,seq(1, 2^M - 1, 2)] - tmp[,seq(2, 2^M, 2)]
#       standard_weight = colMeans(abs(tmp2))
#       plot(colMeans(abs(tmp2)))
#       plot(colMeans(abs(W[-1, alternating_indices])))
#       plot(colMeans(abs(W[-1,])))
#     }

    # Change the order of the coalitions such that we have S (odd indices) and then S_bar (even indices),
    # for all possible coalitions. Note that we add 1 as we exclude the phi0.
    # The list contains `M` matrices of dimension `2^M x N_explain`
    R_matrix_paired_order_list =
      lapply(seq(M), function (investigate_feature_number) {
        sapply(R_matrix_list_all_individuals,
               "[",
               investigate_feature_number + 1, )[alternating_indices,]
      })
    print("R_matrix_paired_order_list")

    # We compute the difference between the S and S_bar entries. Odd minus even indices.
    # The list contains `M` matrices of dimension `2^(M-1) x N_explain`
    R_matrix_paired_order_diff_list =
      lapply(seq_along(R_matrix_paired_order_list),
             function (feature_idx) {
               R_matrix_paired_order_list[[feature_idx]][seq(1, 2^M - 1, 2), ] -
                 R_matrix_paired_order_list[[feature_idx]][seq(2, 2^M, 2), ]
             })
    R_matrix_paired_order_list = NULL
    print("R_matrix_paired_order_diff_list")

    # R_matrix_paired_order_diff_list[[1]][1:5, 1:5]

    # Convert it to a data.table of dimension `(M * N_explain) x (2^(M-1) + 2)`.
    # The plus two is because of two id_cols (`id_feature` and `id`), while the remaining `2^(M-1)` columns
    # are the differences
    R_dt_paired_order_diff =
      data.table::rbindlist(
        lapply(seq_along(R_matrix_paired_order_diff_list),
               function (feature_idx) {
                 data.table::data.table(id = factor(seq(nrow(explanation$internal$data$x_explain))),
                                        D = t(R_matrix_paired_order_diff_list[[feature_idx]]))
               }),
        idcol = "id_feature",
        use.names = TRUE
      )
    R_matrix_paired_order_diff_list = NULL
    print("R_dt_paired_order_diff")

    # Change the column names
    data.table::setnames(R_dt_paired_order_diff, c("id_feature", "id", paste0("D", seq(2^(M-1)))))

    # Go from wide data.table to long data.table format
    R_dt_paired_order_diff_long = data.table::melt(R_dt_paired_order_diff,
                                                   id.vars = c("id_feature", "id"),
                                                   value.name = "Rij",
                                                   variable.name = "id_combination_diff",
                                                   variable.factor = TRUE)
    R_dt_paired_order_diff = NULL
    print("R_dt_paired_order_diff_long")

    # Compute the mean of the Rij's summed over all test observations, and the same when also using the absolute value
    # So, $\frac{1}{N_test}\sum_{j = 1}^N_test Rij$ and $\frac{1}{N_test}\sum_{j = 1}^N_test |Rij|$.
    R_dt = R_dt_paired_order_diff_long[, .(mean_Rij = mean(Rij),
                                           mean_abs_Rij = mean(abs(Rij))),
                                       by = list(id_combination_diff, id_feature)]

    # Add columns with the order of the mean_abs_Rij for each feature and each paired coalitions,
    # we also add the index of the coalitions that are included in the paired coalitions.
    R_dt[, `:=` (order_mean_abs_Rij = order(order(mean_abs_Rij, decreasing = TRUE), decreasing = FALSE),
                 id_combination_S = seq(1, 2^(M-1)),
                 id_combination_Sbar = seq(2^M, 2^(M-1) + 1)),
         by = id_feature]


    # library(latex2exp)
    # R_dt[, normalized_mean_abs_Rij := mean_abs_Rij / sum(mean_abs_Rij), by = list(id_feature)]
    # tmp_fig = ggplot2::ggplot(data = R_dt, ggplot2::aes(y = id_combination_diff, x = normalized_mean_abs_Rij)) +
    #   ggplot2::geom_bar(position = "dodge", stat = "identity") +
    #   ggplot2::facet_grid(cols = ggplot2::vars(id_feature), labeller = label_bquote(cols = X[.(id_feature)])) +
    #   ggplot2::labs(y = "Coalition difference index", x = TeX(r"( $\frac{1}{N_{explain}}\sum_{i = 1}^{N_{explain}} |T^{\[i\]}_{j,k}|$ )")) +
    #   ggplot2::scale_y_discrete(limits = rev, label = parse(text = paste("k[", seq(16,1), "]", sep = ""))) +
    #   ggplot2::theme(strip.text = ggplot2::element_text(size = ggplot2::rel(1.25)),
    #         legend.title = ggplot2::element_text(size = ggplot2::rel(1.4)),
    #         legend.text = ggplot2::element_text(size = ggplot2::rel(1.4)),
    #         axis.title = ggplot2::element_text(size = ggplot2::rel(1.25)),
    #         axis.text = ggplot2::element_text(size = ggplot2::rel(1.15)))
    # ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_rds_saves/Paper3_Experiment_M_5_n_train_1000_n_test_250_rho_0.5_betas_2_10_0.25_-3_-1_1.5_pilot.png",
    #        plot = tmp_fig,
    #        width = 14.2,
    #        height = 4.5,
    #        scale = 0.85,
    #        dpi = 350)


    # We aggregate the `mean_Rij` and `mean_abs_Rij` over the features, so we get a single
    # mean for each paired coalition. This is thus a value average over all features and test observations.
    R_dt_aggregated = R_dt[, lapply(.SD, mean),
                           .SDcols = c("mean_Rij", "mean_abs_Rij"),
                           by = id_combination_diff][, data.table::setnames(.SD,
                                                                            c("mean_Rij", "mean_abs_Rij"),
                                                                            c("mean_R", "mean_abs_R"))]

    # Add order values for the aggregated values. A low value means that it is how high importance.
    # I.e., if `mean_abs_R_ordered = 1` then this is the most important paired coalition.
    R_dt_aggregated[, `:=` (mean_abs_R_ordered = order(order(mean_abs_R, decreasing = TRUE), decreasing = FALSE),
                            mean_R_ordered = order(order(mean_R, decreasing = TRUE), decreasing = FALSE))]

    # 2024: Add the new orders
    R_dt_aggregated[, `:=` (paired_coalitions_sub = order(order(mean_abs_R - standard_weight, decreasing = TRUE), decreasing = FALSE),
                            paired_coalitions_scaled = order(order(mean_abs_R / standard_weight, decreasing = TRUE), decreasing = FALSE),
                            paired_coalitions_norm = order(order(abs(((mean_abs_R - mean(mean_abs_R))/sd(mean_abs_R)) - ((standard_weight - mean(standard_weight))/sd(standard_weight))), decreasing = TRUE), decreasing = FALSE)
    )]
    R_dt_aggregated[, paired_coalitions_avg := order(order(mean_abs_R_ordered + paired_coalitions_scaled))]

    # Merge together to only add the "id_combination_S" and "id_combination_Sbar" columns to the dt.
    R_dt_aggregated = R_dt_aggregated[unique(R_dt[,c("id_combination_diff", "id_combination_S", "id_combination_Sbar")]),
                                      on = "id_combination_diff"]

    # {
    #   mean_abs_R = R_dt_aggregated$mean_abs_R
    # dt_standard_pilot = data.table(id_combination_diff = rep(R_dt_aggregated$id_combination_diff),
    #                                weights = c(standard_weight / sum(standard_weight),
    #                                            mean_abs_R / sum(mean_abs_R)),
    #                                version = rep(c("Standard", "Pilot"), each = 16))
    # tmp_fig2 = ggplot2::ggplot(dt_standard_pilot, ggplot2::aes(y = id_combination_diff, x = weights, col = version, fill = version)) +
    #   ggplot2::geom_bar(position = "dodge", stat = "identity", width=0.75) +
    #   ggplot2::scale_y_discrete(limits = rev, label = parse(text = paste("k[", seq(16,1), "]", sep = ""))) +
    #   ggplot2::labs(color = "Version", fill = "Version", y = "Coalition difference index",
    #                 x = TeX(r"( $\frac{1}{N_{explain}}\sum_{i = 1}^{N_{explain}} \frac{1}{M}\sum_{i = 1}^{M} |T^{\[i\]}_{j,k}|$ )")) +
    #   ggplot2::theme(legend.position = c(0.79, 0.1923)) +
    #   ggplot2::theme(strip.text = ggplot2::element_text(size = ggplot2::rel(1.25)),
    #                  legend.title = ggplot2::element_text(size = ggplot2::rel(1.0)),
    #                  legend.text = ggplot2::element_text(size = ggplot2::rel(1.0)),
    #                  axis.title = ggplot2::element_text(size = ggplot2::rel(1.25)),
    #                  axis.text = ggplot2::element_text(size = ggplot2::rel(1.15))) +
    #   ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE), fill = ggplot2::guide_legend(reverse = TRUE)) +
    #   ggplot2::scale_fill_manual(values=c("grey35", "#999999")) +
    #   ggplot2::scale_color_manual(values=c("grey35", "#999999"))
    # fig_comb = gridExtra::grid.arrange(tmp_fig + ggplot2::scale_x_continuous(breaks = c(0.1, 0.2)), tmp_fig2, ncol = 2, widths = c(2,1))
    #
    #  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Experiment_M_5_n_train_1000_n_test_250_rho_0.5_betas_2_10_0.25_-3_-1_1.5_pilot_3.png",
    #         plot = fig_comb,
    #         width = 14.2,
    #         height = 4.5,
    #         scale = 0.85,
    #         dpi = 350)
    # }

    # {
    #   require(gridExtra)
    #   p1 = ggplot2::ggplot(data = R_dt_aggregated, ggplot2::aes(x = id_combination_diff, y = mean_abs_R / sum(mean_abs_R))) +
    #     ggplot2::geom_bar(position = "dodge", stat = "identity")
    #
    #   p2 = ggplot2::ggplot(data = R_dt_aggregated, ggplot2::aes(x = id_combination_diff, y = standard_weight)) +
    #     ggplot2::geom_bar(position = "dodge", stat = "identity")
    #
    #   gridExtra::grid.arrange(p2, p1, ncol = 1)
    #
    #
    #   p3 = ggplot2::ggplot(data = R_dt_aggregated, ggplot2::aes(x = id_combination_diff, y = mean_abs_R - standard_weight)) +
    #     ggplot2::geom_bar(position = "dodge", stat = "identity")
    #
    #   p4 = ggplot2::ggplot(data = R_dt_aggregated, ggplot2::aes(x = id_combination_diff, y = mean_abs_R/standard_weight)) +
    #     ggplot2::geom_bar(position = "dodge", stat = "identity")
    #
    #   p5 = ggplot2::ggplot(data = R_dt_aggregated, ggplot2::aes(x = id_combination_diff, y = abs(((mean_abs_R - mean(mean_abs_R))/sd(mean_abs_R)) - ((standard_weight - mean(standard_weight))/sd(standard_weight))))) +
    #     ggplot2::geom_bar(position = "dodge", stat = "identity")
    #
    #   gridExtra::grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
    # }

    # {
    #   # Standard weight is the weight before we apply the pilot estimates.
    #   pilot_weights = R_dt_aggregated$mean_abs_R[-1] / sum(R_dt_aggregated$mean_abs_R[-1])
    #   true_SKW = explanation$internal$objects$X$shapley_weight[seq(2, length(R_dt_aggregated$mean_abs_R))]
    #   true_SKW = true_SKW / sum(true_SKW)
    #   standard_weight_sc = standard_weight[-1] / sum(standard_weight[-1])
    #   matplot(cbind(true_SKW, standard_weight_sc, pilot_weights),
    #           pch = 16, ylab = "Weight / probability", xlab = "Coalition index",
    #           main = "Comapring the weights")
    #   legend("topright", c("Shapley Kernel", "Standard Weight in W", "Pilot"), title = "Weights", col = 1:3, pch = 16, bty = "n")
    # }


    # Add the results to the return list
    if ("paired_coalitions" %in% strategies) {
      # Extract which order we should add the paired coalitions based on the mean_abs_R score
      return_list$paired_coalitions = c(t(as.matrix(data.table::setorder(
        R_dt_aggregated[, c("mean_abs_R_ordered", "id_combination_S", "id_combination_Sbar")],
        mean_abs_R_ordered)[,-"mean_abs_R_ordered"])))
      return_list$paired_coalitions_weights = R_dt_aggregated$mean_abs_R
    }
    if ("paired_coalitions_sub" %in% strategies) {
      return_list$paired_coalitions_sub = c(t(as.matrix(data.table::setorder(
        R_dt_aggregated[, c("paired_coalitions_sub", "id_combination_S", "id_combination_Sbar")],
        paired_coalitions_sub)[,-"paired_coalitions_sub"])))
      # return_list$paired_coalitions_sub_weights = R_dt_aggregated$mean_abs_R - standard_weight
    }
    if ("paired_coalitions_scaled" %in% strategies) {
      return_list$paired_coalitions_scaled = c(t(as.matrix(data.table::setorder(
        R_dt_aggregated[, c("paired_coalitions_scaled", "id_combination_S", "id_combination_Sbar")],
        paired_coalitions_scaled)[,-"paired_coalitions_scaled"])))
    }
    if ("paired_coalitions_avg" %in% strategies) {
      return_list$paired_coalitions_avg = c(t(as.matrix(data.table::setorder(
        R_dt_aggregated[, c("paired_coalitions_avg", "id_combination_S", "id_combination_Sbar")],
        paired_coalitions_avg)[,-"paired_coalitions_avg"])))
    }
    if ("paired_coalitions_norm" %in% strategies) {
      return_list$paired_coalitions_norm = c(t(as.matrix(data.table::setorder(
        R_dt_aggregated[, c("paired_coalitions_norm", "id_combination_S", "id_combination_Sbar")],
        paired_coalitions_norm)[,-"paired_coalitions_norm"])))
    }
  }

  # Check if doing any of the single methods
  if (any(c("single_mean_coalition_effect",
            "single_median_coalition_effect",
            "single_mean_ranking_over_each_test_obs",
            "single_median_ranking_over_each_test_obs") %in% strategies)) {
    # We are at least doing one of these

    # Note that we add 1 as we exclude the phi0, and empty place after "," is intentional.
    R_matrix_list = lapply(seq(M), function (feature_idx) sapply(R_matrix_list_all_individuals, "[", feature_idx + 1, ))

    # Convert it to a data.table
    R_dt_tmp = data.table::rbindlist(
      lapply(seq(M), function (feature_idx)
        data.table::data.table(id = factor(seq(nrow(explanation$internal$data$x_explain))),
                               t(R_matrix_list[[feature_idx]]))
      ),
      idcol = "id_feature",
      use.names = TRUE
    )
    R_matrix_list = NULL

    # Change the column names
    data.table::setnames(R_dt_tmp, c("id_feature", "id", seq(2^M)))

    # Go from wide data.table to long data.table format
    R_dt_long = data.table::melt(R_dt_tmp,
                                 id.vars = c("id", "id_feature"),
                                 value.name = "Rij",
                                 variable.name = "id_combination",
                                 variable.factor = TRUE)
    R_dt_tmp = NULL

    # Reorder the columns and set the keys
    data.table::setcolorder(R_dt_long, c("id", "id_combination", "id_feature", "Rij"))
    data.table::setkeyv(R_dt_long, c("id", "id_combination", "id_feature"))

    # Compute the absolute value of Rij
    R_dt_long[, abs_Rij := abs(Rij)]

    # For each id and id_feature combination we compute the ordering of the absolute Rij coming from each coalition.
    # The ordering goes from 1 to 2^M, where a large a low value means that it has a high Rij value.
    # I.e., ordering 1 means that that coalition has the highest Rij value for that id and id_feature.
    R_dt_long[, `:=` (order_abs_Rij = order(order(abs_Rij, decreasing = TRUE), decreasing = FALSE)),
              by = list(id, id_feature)]

    # Then we average the abs_Rij scores over the features for each test observation and coalition.
    # So we go from a data.table of length `n_id * n_id_combination * n_id_features` to
    # `n_id * n_id_combination`.
    R_dt_long_agg_V1 = R_dt_long[, list(avg_abs_Rij = mean(abs_Rij)), by = list(id, id_combination)]

    # Compute the order of avg_abs_Rij for the different test observations.
    R_dt_long_agg_V1[, `:=` (order_avg_abs_Rij = order(order(avg_abs_Rij, decreasing = TRUE), decreasing = FALSE)),
                     by = list(id)]

    # Compute the mean ordering over all test observations
    single_mean_ranking_over_each_test_obs =
      as.integer(R_dt_long_agg_V1[, mean(order_avg_abs_Rij), by = id_combination][order(V1)]$"id_combination")
    single_median_ranking_over_each_test_obs =
      as.integer(R_dt_long_agg_V1[, median(order_avg_abs_Rij), by = id_combination][order(V1)]$"id_combination")

    # Then we average the abs_Rij scores over the features and test observations for each coalition.
    # So we go from a data.table of length `n_id * n_id_combination * n_id_features` to `n_id_combination`.
    R_dt_long_agg_V2 = R_dt_long[, list(mean_abs_Rij = mean(abs_Rij),
                                        median_abs_Rij = median(abs_Rij)),
                                 by = list(id_combination)]
    single_mean_coalition_effect = order(R_dt_long_agg_V2$mean_abs_Rij, decreasing = TRUE)
    single_median_coalition_effect = order(R_dt_long_agg_V2$median_abs_Rij, decreasing = TRUE)

    # Add the results to the return list
    if ("single_mean_coalition_effect" %in% strategies) {
      return_list$single_mean_coalition_effect = single_mean_coalition_effect
    }
    if ("single_median_coalition_effect" %in% strategies) {
      return_list$single_median_coalition_effect = single_median_coalition_effect
    }
    if ("single_mean_ranking_over_each_test_obs" %in% strategies) {
      return_list$single_mean_ranking_over_each_test_obs = single_mean_ranking_over_each_test_obs
    }
    if ("single_median_ranking_over_each_test_obs" %in% strategies) {
      return_list$single_median_ranking_over_each_test_obs = single_median_ranking_over_each_test_obs
    }

    # Some plots to look at the different orderings
    # matplot(t(data.table::dcast(R_dt_long_agg_V1,
    #                             id ~ id_combination,
    #                             value.var = "order_avg_abs_Rij")[,-"id"])[,1:10], type = "l", lty = 1)
    # points(single_mean_ranking_over_each_test_obs, seq(128), pch = 16, cex = 1.5, col = 1)
    # points(single_median_ranking_over_each_test_obs, seq(128), pch = 16, cex = 1.5, col = 2)
    # points(single_mean_coalition_effect, seq(128), pch = 17, cex = 1.5, col = 3)
    # points(single_median_coalition_effect, seq(128), pch = 17, cex = 1.5, col = 4)

    # dt_temp = data.table::melt(data.table::data.table(id = factor(seq(2^M)),
    #                             single_mean_coalition_effect = single_mean_coalition_effect,
    #                             single_median_coalition_effect = single_median_coalition_effect,
    #                             single_mean_ranking_over_each_test_obs = single_mean_ranking_over_each_test_obs,
    #                             single_median_ranking_over_each_test_obs = single_median_ranking_over_each_test_obs),
    #      id.vars = "id",
    #      measure.vars =
    #        c("single_mean_coalition_effect",
    #        "single_median_coalition_effect",
    #        "single_mean_ranking_over_each_test_obs",
    #        "single_median_ranking_over_each_test_obs"),
    #      value.name = "ranking",
    #      variable.name = "strategy",
    #      variable.factor = TRUE)
    #
    #   ggplot2::ggplot(dt_temp[as.integer(dt_temp$id) %in% c(1:100)],
    #                   ggplot2::aes(x = id, y = ranking, fill = strategy)) +
    #     ggplot2::geom_bar(position = "dodge", stat = "identity")
  }

  # Reorder the coalition orders such that the empty and grand coalitions are included as the first and second
  # coalitions. I.e., such that 1 and 2^M are the two first entries.
  if (always_empty_and_grand_coalitions_first) {
    return_list[names(return_list) != "paired_coalitions_weights"] =
      lapply(return_list[names(return_list) != "paired_coalitions_weights"],
             function(coalition_order) c(1, 2^M, coalition_order[!(coalition_order %in% c(1, 2^M))]))
  }

  # Plot some figures if requested by the user
  if (plot_figures) {
    dt_temp = data.table::melt(cbind(id = factor(seq(2^M)),
                                     data.table::as.data.table(return_list[names(return_list) != "paired_coalitions_weights"] )),
                               id.vars = "id",
                               value.name = "ranking",
                               variable.name = "strategy",
                               variable.factor = TRUE)

    ggplot2::ggplot(dt_temp[as.integer(dt_temp$id) %in% c(1:20)],
                    ggplot2::aes(x = id, y = ranking, fill = strategy)) +
      ggplot2::geom_bar(position = "dodge", stat = "identity")

    GGally::ggpairs(data.table::as.data.table(return_list)) +
      labs(x = "Ordering (lower means more important)",
           y = "Ordering (lower means more important)")

    # See at which location the entries in the vector is in the second vector.
    # We then see that the entries are more linked. I.e., even though the order is not exact,
    # the are somewhat in order. Mainly due to the magnitude of the different values for the different
    # coalition sizes
    plot(pmatch(return_list[[1]], return_list[[2]]))
  }

  # Return the order of paired coalitions should be added
  return(return_list)
}


#' Extract best paired coalition order
#'
#' @param explanation A [shapr] explanation object with all coalitions. Should be "true_explanations".
#' @param plot_figures Boolean. If we are to plot figures.
#'
#' @return array with the indices of the coalitions that we should add in decreasing importance order.
#' I.e., the first one is the most important.
#' @export
#'
#' @examples
# pilot_estimates_paired_order = function(explanation, plot_figures = FALSE) {
#
#   # Extract the internal list from the explanation object
#   internal = explanation$internal
#
#   # Get the number of features
#   M = internal$parameters$n_features
#
#   if (2^M != internal$parameters$n_combinations) {
#     stop("Currently we need the `explanation` to have `n_combinations` = 2^M, where M is the number of featuers.")
#     # TODO: we can later remove this if we do not need this as we can rather return the features to use
#     # internal$objects$X$features instead of using the index of these.
#   }
#
#   # We extract the W and S matrices
#   W = explanation$internal$objects$W
#   S = explanation$internal$objects$S
#
#   # Extract the v(S) elements
#   dt_vS = explanation$internal$output$dt_vS
#
#   # Compute the R's. I.e., W * v(s), but without adding them together.
#   # So not a matrix product, but rather an element wise multiplication.
#   # Do it for all test observations and store the results in a list.
#   R_matrix_list_all_individuals =
#     lapply(seq(ncol(dt_vS[, -"id_combination"])),
#            function (test_obs_idx) t(t(W)*dt_vS[, -"id_combination"][[test_obs_idx]]))
#
#   # Alternate them such that we extract the smallest, then the largest, then the second smallest,
#   # then the second largest and so on.
#   alternating_indices = c(rbind(seq(1, 2^(M-1)), seq(2^M, 2^(M-1) + 1)))
#
#   # Change the order of the coalitions such that we have S (odd indices) and
#   # then S_bar (even indices), for all possible coalitions.
#   # Note that we add 1 as we exclude the phi0.
#   R_matrix_paired_order_list =
#     lapply(seq(M), function (investigate_feature_number) {
#       sapply(R_matrix_list_all_individuals,
#              "[",
#              investigate_feature_number + 1, )[alternating_indices,]
#     })
#
#   # We compute the difference between the S and S_bar entries. Odd minus even indices.
#   R_matrix_paired_order_diff_list =
#     lapply(seq_along(R_matrix_paired_order_list),
#            function (feature_idx) {
#              R_matrix_paired_order_list[[feature_idx]][seq(1, 2^M,2), ] -
#                R_matrix_paired_order_list[[feature_idx]][seq(2, 2^M,2), ]
#            })
#
#   # Convert it to a data.table
#   R_dt_paired_order_diff =
#     data.table::rbindlist(
#       lapply(seq_along(R_matrix_paired_order_diff_list),
#              function (feature_idx) {
#                data.table::data.table(id = factor(seq(nrow(explanation$internal$data$x_explain))),
#                                       D = t(R_matrix_paired_order_diff_list[[feature_idx]]))
#              }),
#       idcol = "id_feature",
#       use.names = TRUE
#     )
#
#   # Change the column names
#   data.table::setnames(R_dt_paired_order_diff, c("id_feature", "id", paste0("D", seq(2^(M-1)))))
#
#   # Go from wide data.table to long data.table format
#   R_dt_paired_order_diff_long = data.table::melt(R_dt_paired_order_diff,
#                                                  id.vars = c("id_feature", "id"),
#                                                  value.name = "Rij",
#                                                  variable.name = "id_combination_diff",
#                                                  variable.factor = TRUE)
#
#   # Compute the mean of the Rij's summed over all test observations, and the same when also using the absolute value
#   # So, $\frac{1}{N_test}\sum_{j = 1}^N_test Rij$ and $\frac{1}{N_test}\sum_{j = 1}^N_test |Rij|$.
#   R_dt = R_dt_paired_order_diff_long[, .(mean_Rij = mean(Rij),
#                                          mean_abs_Rij = mean(abs(Rij))),
#                                      by = list(id_combination_diff, id_feature)]
#
#   # Add columns with the order of the mean_abs_Rij for each feature and each paired coalitions,
#   # we also add the index of the coalitions that are included in the paired coalitions.
#   R_dt[, `:=` (order_mean_abs_Rij = order(order(mean_abs_Rij, decreasing = TRUE), decreasing = FALSE),
#                id_combination_S = seq(1, 2^(M-1)),
#                id_combination_Sbar = seq(2^M, 2^(M-1) + 1)),
#        by = id_feature]
#
#
#   if (plot_figures) {
#     ggplot2::ggplot(data = R_dt, ggplot2::aes(x = id_combination_diff, y = mean_abs_Rij)) +
#       ggplot2::geom_bar(position = "dodge", stat = "identity") +
#       ggplot2::facet_grid(rows = ggplot2::vars(id_feature))
#   }
#
#   # We aggregate the `mean_Rij` and `mean_abs_Rij` over the features, so we get a single
#   # mean for each paired coalition. This is thus a value average over all features and test observations.
#   R_dt_aggregated = R_dt[, lapply(.SD, mean),
#                          .SDcols = c("mean_Rij", "mean_abs_Rij"),
#                          by = id_combination_diff][, data.table::setnames(.SD,
#                                                                           c("mean_Rij", "mean_abs_Rij"),
#                                                                           c("mean_R", "mean_abs_R"))]
#
#   # Add order values for the aggreagated values. A low value means that it is how high importance.
#   # I.e., if `mean_abs_R_ordered = 1` then this is the most important paired coalition.
#   R_dt_aggregated[, `:=` (mean_abs_R_ordered = order(order(mean_abs_R, decreasing = TRUE), decreasing = FALSE),
#                           mean_R_ordered = order(order(mean_R, decreasing = TRUE), decreasing = FALSE))]
#
#   # Merge together to only add the "id_combination_S" and "id_combination_Sbar" columns to the dt.
#   R_dt_aggregated = R_dt_aggregated[unique(R_dt[,c("id_combination_diff", "id_combination_S", "id_combination_Sbar")]),
#                                     on = "id_combination_diff"]
#
#   # Extract which order we should add the paired coalitions based on the mean_abs_R score
#   specific_coalition_set = c(t(as.matrix(data.table::setorder(
#     R_dt_aggregated[, c("mean_abs_R_ordered", "id_combination_S", "id_combination_Sbar")],
#     mean_abs_R_ordered)[,-"mean_abs_R_ordered"])))
#
#
#   # Return the order of paired coalitions should be added
#   return(specific_coalition_set)
# }


## Aggregation functions -------------------------------------------------------------------------------------------
#' Compute the aggregated results for a simulation
#'
#' @param M integer. The number of features
#' @param rhos Numeric array. The correlations.
#' @param n_train The number of training observations
#' @param n_test The number of test observations (explicands)
#' @param betas Numeric array. Vector containing the betas using in the setup.
#' @param folder_save String. Path to where the files are stored.
#' @param memory_efficient Boolean. If we are to remove all data except from the Shapley values from the objects.
#' @param max_repetitions Integer. If we only want to use a certain amount of repetitions.
#' @param save_results Boolean. If we should save the aggregated results in the `folder_save` folder.
#' @param evaluation_criterion String. If we are to compute the MSE or MAE between
#' the true and estimated Shapley values.
#' @param level Numeric. The level of the empirical CI.
#' @param n_workers Integer. Number of cores to compute the evaluation criterion results.
#' Only in the aggregation step. Recommend using 1 due to large objects having to be loaded in and out of memory.
#' So it is/can be slower with several cores.
#' @param objects_to_return String. What objects to return. One or several of "aggregated_results", "true_Shapley",
#' "repeated_Shapley".
#'
#' @return Depends on `objects_to_return`.
#' @export
combine_explanation_results = function(M,
                                       rhos,
                                       rho_equi,
                                       n_train,
                                       n_test,
                                       betas,
                                       folder_save,
                                       use_pilot_estimates_regression,
                                       pilot_approach_regression,
                                       pilot_regression_model,
                                       name_prefix = "",
                                       max_repetitions = NULL,
                                       memory_efficient = TRUE,
                                       save_results = TRUE,
                                       evaluation_criterion = c("MAE", "MSE"),
                                       level = 0.95,
                                       n_workers = 1,
                                       objects_to_return = "aggregated_results") {

  # Check some of the inputs
  if (length(betas) != M + 1) stop("Incorrect number of `betas` compared to `M`.")
  evaluation_criterion = match.arg(evaluation_criterion)
  objects_to_return = match.arg(arg = objects_to_return,
                                choices = c("aggregated_results",
                                            "true_Shapley",
                                            "repeated_Shapley"),
                                several.ok = TRUE)

  # Result lists. They can be quite large and take up many GBs of memory for large M
  true_explanations_list = list()
  repeated_explanations_list = list()
  aggregated_results_list = list()
  return_list = list()

  # Iterate over the rhos
  rho_idx = 1
  for (rho_idx in seq_along(rhos)) {
    if (rho_idx > 1) cat(sprintf("\n")) # White space from previous iteration

    # Get the current rho
    rho = rhos[rho_idx]

    # Update the result list
    repeated_explanations_list[[paste0("rho_", rho)]] = list()

    # Make file names
    if (name_prefix == "") {
      file_name = paste("M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "equi", rho_equi,
                        "betas", paste(as.character(betas), collapse = "_"), sep = "_")
    } else {
      file_name = paste(name_prefix, "M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "equi", rho_equi,
                        "betas", paste(as.character(betas), collapse = "_"), sep = "_")
    }

    if (use_pilot_estimates_regression) {
      file_name_updated = paste(file_name, "pilot", strsplit(pilot_approach_regression, "_")[[1]][2],
                               sub(".*::([^\\(]+)\\(.*", "\\1",  pilot_regression_model), sep = "_")
    } else {
      file_name_updated = file_name
    }
    save_file_name_setup = file.path(folder_save, paste0(file_name, "_model.rds"))
    save_file_name_true = file.path(folder_save, paste0(file_name, "_true.rds"))

    #  Find the relevant files in the folder and their repetition numbers/indices
    files_in_dir = list.files(folder_save)
    relevant_files_in_dir = files_in_dir[grepl(paste0(file_name_updated, "_estimated_repetition_"), files_in_dir)]
    relevant_files_in_dir = relevant_files_in_dir[!grepl("tmp", relevant_files_in_dir)] # remove any tmp files
    if (length(relevant_files_in_dir) == 0) {
      stop(paste0("Cannot find any files for the provided paremeters. ",
                  "Looking for file name structures '", file_name, "'."))
    }
    relevant_repetitions =
      sort(as.integer(sapply(strsplit(unlist(strsplit(relevant_files_in_dir, '.rds')), '\\_'), tail, 1)))
    relevant_repetitions = relevant_repetitions[seq(min(max_repetitions, length(relevant_repetitions)))]
    if (!is.null(max_repetitions) && max_repetitions > length(relevant_repetitions)) {
      message(paste0("The parameter `max_repetitions` (", max_repetitions, ") is larger than the number of available ",
                     "repetitions (", length(relevant_repetitions), "). Use all available repetitions.\n"))
    }

    # Load the setup file, i.e., the model and data
    setup = readRDS(save_file_name_setup)

    # Load the true explanations
    true_explanations_list[[paste0("rho_", rho)]] = readRDS(save_file_name_true)
    true_explanations_list[[paste0("rho_", rho)]]$internal$output$dt_samp_for_vS = NULL

    # Iterate over the repetitions
    repetition_idx = 1
    for (repetition_idx in seq_along(relevant_repetitions)) {

      # Get the current repetition
      repetition = relevant_repetitions[repetition_idx]

      # Small printout to the user
      cat(sprintf("Working on rho = %g (%d of %d) and repetition = %d (%d of %d).\n",
                  rho, rho_idx, length(rhos), repetition, repetition_idx, length(relevant_repetitions)))

      # Create the save file name
      save_file_name_rep = file.path(folder_save, paste0(file_name_updated, "_estimated_repetition_", repetition, ".rds"))

      # Load the rds file
      current_repetition_results = readRDS(save_file_name_rep)

      if (file.exists(file.path(folder_save, paste0(file_name_updated, "_estimated_repetition_", repetition, "on_all_cond.rds")))) {
        current_repetition_results_on_all_cond =
          readRDS(file.path(folder_save, paste0(file_name_updated, "_estimated_repetition_", repetition, "on_all_cond.rds")))
        current_repetition_results = c(current_repetition_results, current_repetition_results_on_all_cond)
      }

      if (file.exists(file.path(folder_save, paste0(file_name_updated, "_estimated_repetition_", repetition, "on_all_cond_paired.rds")))) {
        current_repetition_results_on_all_cond_paired =
          readRDS(file.path(folder_save, paste0(file_name_updated, "_estimated_repetition_", repetition, "on_all_cond_paired.rds")))
        current_repetition_results = c(current_repetition_results, current_repetition_results_on_all_cond_paired)
      }

      # We remove all non-essential stuff from the list
      if (memory_efficient) {
        cat(sprintf("Using memory efficient version: %s \U2192 ",
                    format(object.size(current_repetition_results), units = "auto")))

        for (met in names(current_repetition_results)) {
          if (met == "True_vs_Pilot_Order") next
          for (rep in names(current_repetition_results[[met]])) {
            for (comb in names(current_repetition_results[[met]][[rep]])) {
              tmp_res = current_repetition_results[[met]][[rep]][[comb]]
              tmp_res[["only_save"]] = NULL
              tmp_res$internal = NULL
              tmp_res$timing = NULL
              tmp_res$pred_explain = NULL
              current_repetition_results[[met]][[rep]][[comb]] = tmp_res
            }
          }
        }
        cat(sprintf("%s.\n", format(object.size(current_repetition_results), units = "auto")))
      }

      if (repetition_idx == 1) {
        repeated_explanations_list[[paste0("rho_", rho)]] = current_repetition_results
      } else {
        # Update the repetition names
        current_repetition_results = lapply(
          current_repetition_results, function(x) {
            names(x) = paste0("repetition_", repetition_idx)
            x})

        # Add the results to the list
        repeated_explanations_list[[paste0("rho_", rho)]] = modifyList(repeated_explanations_list[[paste0("rho_", rho)]],
                                                                       current_repetition_results)
      }
    }

    # Small printout to the user
    cat(sprintf("Aggregating the results.\n"))

    valid_methods = names(repeated_explanations_list[[rho_idx]])[names(repeated_explanations_list[[rho_idx]]) != "True_vs_Pilot_Order"]

    # Aggregate the results
    aggregated_results_list[[paste0("rho_", rho)]] =
      aggregate_results(repeated_explanations_list = repeated_explanations_list[[rho_idx]][valid_methods],
                        true_explanations = true_explanations_list[[rho_idx]],
                        evaluation_criterion = evaluation_criterion,
                        level = level,
                        n_workers = n_workers)

    if (save_results) {
      if (rho_idx == 1) return_list[["save_files"]] = list()
      return_list[["save_files"]][[paste0("rho_", rho)]] =
        file.path(folder_save, paste0(file_name_updated, "_dt_", evaluation_criterion, ".rds"))
      cat(sprintf("Saving the results.\n"))
      saveRDS(aggregated_results_list[[paste0("rho_", rho)]], return_list[["save_files"]][[paste0("rho_", rho)]])
    }
  }

  # Find out what to return
  if ("aggregated_results" %in% objects_to_return) return_list[["aggregated_results"]] = aggregated_results_list
  if ("true_Shapley" %in% objects_to_return) return_list[["true_Shapley"]] = true_explanations_list
  if ("repeated_Shapley" %in% objects_to_return) return_list[["repeated_Shapley"]] = repeated_explanations_list
  if (length(return_list) == 1) return_list = return_list[[1]]

  return(return_list)
}

#' Aggregate the results of repeated explanations
#'
#' @param repeated_explanations_list List. Output from the [shapr::repeated_explanations()] function.
#' @param true_explanations Shapr object.
#' Output from the [shapr::explain()] function containing the true Shapley values.
#' @param evaluation_criterion String. Either "MAE" or "MSE". Default is `MAE`.
#' @param level Numeric. The confidence level required. Default is `0.95`.
#' @param n_workers Integer. The number of cores to run the computations.
#'
#' @return A list of three [data.table::data.table()] containing the aggregated evaluation criterion results.
#' @export
aggregate_results = function(repeated_explanations_list,
                             true_explanations,
                             evaluation_criterion = c("MAE", "MSE"),
                             level = 0.95,
                             n_workers = 1) {
  ### Setup and checks
  # If user only provided a single explanation and did not put it in a list, then we put it into a list.
  if ("shapr" %in% class(repeated_explanations_list)) repeated_explanations_list <- list(repeated_explanations_list)

  # Check number of workers is less or equal to the number of available cores
  n_max_workers = future::availableCores()
  if (n_workers > n_max_workers) {
    warning(sprintf("Too many workers. Change from %d to %d (max available cores).", n_workers, n_max_workers))
    n_workers = n_max_workers
  }

  # Set the future plan to multisession or sequential
  if (n_workers > 1) future::plan(multisession, workers = n_workers) else future::plan(sequential)

  # Provide names for the sampling methods if not provided by the user
  if (is.null(names(repeated_explanations_list)) && !is.null(repeated_explanations_list)) {
    names(repeated_explanations_list) = paste("Sampling Method", seq(length(repeated_explanations_list)))
    message(paste0("The `repeated_explanations_list` was not a named list. Set the names to be: '",
                   paste(names(repeated_explanations_list), collapse = "', '"), "'."))
  }

  # Check for valid level
  if (!(is.numeric(level) && length(level) == 1 && level >= 0 && level <= 1)) stop("`level` must be a probability.")

  # Extract which evaluation criterion we are going to plot
  evaluation_criterion = match.arg(evaluation_criterion)

  # Get the number of repetitions
  n_repetitions = length(repeated_explanations_list[[1]])

  # Get the names
  repeated_explanations_list_names = names(repeated_explanations_list)

  # Convert to matrix
  true_explanations_shapley_mat = as.matrix(true_explanations$shapley_values)

  ### Make data.tables
  # Create list where each entry is a `n_coalitions` times `n_repetitions` matrix containing
  # the overall evaluation criterion (MAE or MSE) between the true Shapley values
  # (using all coalitions and a high value of `n_combinations`) and the repeated runs
  # (different seed values) with different sampling methods and number of used coalitions.
  system.time({
    results_list =
      future.apply::future_lapply(repeated_explanations_list, function (ith_method) {
        sapply(ith_method, function(ith_method_jth_repetition) {
          sapply(ith_method_jth_repetition, function(ith_method_jth_repetition_kth_coalition) {
            compute_MAE_MSE_fast(
              mat_1 = true_explanations_shapley_mat,
              mat_2 = as.matrix(ith_method_jth_repetition_kth_coalition$shapley_values),
              evaluation_criterion = evaluation_criterion)
          })
        })
      })
  })

  # For each method and `n_combination` value, compute the median and the quantile confidence interval
  # based on the user provided `level`. The default is a 95% confidence interval. Convert to a data.table.
  results =
    data.table::rbindlist(
      future.apply::future_lapply(results_list, function(ith_method) {
        median_and_ci = apply(ith_method, 1, quantile, probs = c((1 - level)/2, 0.5, 1 - (1 - level)/2), na.rm = TRUE)
        tmp_dt = data.table::data.table(n_combinations = as.numeric(sapply(strsplit(rownames(ith_method),
                                                                                    "_(?!.*_)", perl=TRUE), "[[", 2)),
                                        CI_lower = median_and_ci[1,],
                                        median = median_and_ci[2,],
                                        CI_upper = median_and_ci[3,],
                                        mean = apply(ith_method, 1, mean),
                                        min = apply(ith_method, 1, min),
                                        max = apply(ith_method, 1, max))
      }), idcol = "sampling")
  results$sampling = factor(results$sampling,
                                                    levels = repeated_explanations_list_names,
                                                    ordered = TRUE)

  # Remove the rows with missing entries. This is in case for paired sampling methods which
  # does not support odd number of combinations, while the other sampling methods do.
  results = results[!is.na(results$median)]

  # We also compute some alternative aggregated versions of the data not needed to make the figure.
  # Create an alternative aggregated results data.table
  result_dt_alternative =
    data.table::rbindlist(
      future.apply::future_lapply(results_list, function(ith_method) {
        data.table::data.table(n_combinations =
                                 as.numeric(sapply(strsplit(rownames(ith_method), "_(?!.*_)", perl=TRUE), "[[", 2)),
                               ith_method)
      }), idcol = "sampling")

  # Convert the sampling column to a factor
  result_dt_alternative$sampling = factor(result_dt_alternative$sampling,
                                          levels = repeated_explanations_list_names,
                                          ordered = TRUE)

  # Remove the rows with missing entries. This is in case for paired sampling methods which
  # does not support odd number of combinations, while the other sampling methods do.
  result_dt_alternative = result_dt_alternative[!is.na(result_dt_alternative$repetition_1)]

  # Change the column names
  data.table::setnames(result_dt_alternative, c(names(result_dt_alternative)[1:2], paste(seq(n_repetitions))))

  # Convert from a wide to long data.table
  result_dt_alternative = data.table::melt(data = result_dt_alternative,
                                           id.vars = c("sampling", "n_combinations"),
                                           variable.name = "repetition",
                                           value.name = paste0("evaluation_criterion_", evaluation_criterion))

  # Ensure that we are in sequential mode
  future::plan(sequential)

  return(list(dt_CI = results,
              dt_long = result_dt_alternative,
              level = level,
              evaluation_criterion = evaluation_criterion))
}




## Plot functions --------------------------------------------------------------------------------------------------
#' Plot the aggregated results
#'
#' @param dt_CI Data.table. Returned data.table from call to the `aggregate_results()` function.
#' @param dt_long Data.table Returned data.table from call to the `aggregate_results()` function.
#' @param file_path String. Path to file to the result data tables.
#' @param index_combinations Integer vector. Which of the coalitions (combinations) to plot.
#' E.g. if you we only want combinations with even number of coalitions, we can set
#' `index_combinations = seq(4, 2^{n_features}, 2)`.
#' @param only_these_sampling_methods Array of strings. If we are only to use/plot these sampling methods.
#' @param figures_to_make String. Which figures to make. Possible values are "figure_CI", "figure_mean",
#' "figure_median", "figure_lines", "figure_boxplot", "figure_lines_boxplot", and "figure_boxplot_lines".
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
#' @param scale_y_log10 Boolean. If we are to use [ggplot2::scale_y_log10()].
#' @param scale_x_log10 Boolean. If we are to use [ggplot2::scale_x_log10()].
#' @param plot_figures Boolean. If `TRUE`, then plot the figures.
#' @param n.dodge Integer. The number of rows to put the labels on the x-axis of the box plots.
#' I.e., the parameter to [scale_x_discrete(guide = guide_axis(n.dodge = n.dodge))].
#' @param level Numeric between 0 and 1. Must be the same as in [shapr::aggreate_results()]. Only needed if
#' user specifies `dt_CI` and `dt_long`. If user provides `file_path`, then `level` is stored in the file.
#' @param remove_last_value Boolean. Default is to be the same as `scale_y_log10`, as the logarithm of zero is
#' minus infinity. If `TRUE`, then we remove the last value where all coalitions are included
#' (i.e., here the error is 0). If `FALSE`, then we keep the last value.
#' @param evaluation_criterion String. The name of the evaluation criterion. Possible values are `MAE` and `MSE`.
#' Must be the same as in [shapr::aggreate_results()]. Only needed if user specifies `dt_CI` and `dt_long`.
#' If user provides `file_path`, then `evaluation_criterion` is stored in the file.
#'
#' @return List of [ggplot2::ggplot()] figures, based on the `figures_to_make` parameter.
#' @export
plot_results = function(dt_CI = NULL,
                        dt_long = NULL,
                        file_path = NULL,
                        level = NULL,
                        evaluation_criterion = NULL,
                        index_combinations = NULL,
                        only_these_sampling_methods = NULL,
                        figures_to_make = c("figure_CI",
                                            "figure_mean",
                                            "figure_median",
                                            "figure_lines",
                                            "figure_boxplot",
                                            "figure_lines_boxplot",
                                            "figure_boxplot_lines"),
                        ggplot_theme = NULL,
                        brewer_palette = NULL,
                        brewer_direction = 1,
                        flip_coordinates = FALSE,
                        legend_position = NULL,
                        scale_y_log10 = FALSE,
                        scale_x_log10 = FALSE,
                        n.dodge = 2,
                        plot_figures = FALSE,
                        remove_last_value = scale_y_log10) {
  ### Check parameters
  figures_to_make = match.arg(figures_to_make, several.ok = TRUE)
  if (!is.null(dt_CI) && is.null(level)) stop("`level` must be provided.")
  if (!is.null(dt_CI) && is.null(evaluation_criterion)) stop("`evaluation_criterion` must be provided.")
  if (!is.null(evaluation_criterion) && !evaluation_criterion %in% c("MSE", "MAE")) {
    stop("`evaluation_criterion` must be either 'MSE' or 'MAE'.")
  }

  # Load data.tables from file
  if (!is.null(file_path)) {
    if (!(is.null(dt_CI) && is.null(dt_long))) stop("Do not provide `file_path` when provding `dt_CI` and `dt_long`.")
    message(sprintf("Loading data tables from file_path = '%s'.\n", file_path))
    file = readRDS(file_path)
    dt_CI = file$dt_CI
    dt_long = file$dt_long
    level = file$level
  }

  ### Fix the data.tables
  # Extract the evaluation criterion and update name
  evaluation_criterion =
    tail(unlist(strsplit(colnames(dt_long)[grep("^evaluation_criterion_", colnames(dt_long))], "_")), 1)
  dt_long = data.table::copy(dt_long)
  setnames(dt_long, paste0("evaluation_criterion_", evaluation_criterion), "evaluation_criterion")

  # Only keep the desired combinations
  if (!is.null(index_combinations)) {
    dt_CI <- dt_CI[n_combinations %in% index_combinations]
    dt_long <- dt_long[n_combinations %in% index_combinations]
  }

  # Only use the specified sampling methods
  if (!is.null(only_these_sampling_methods)) {
    dt_long = dt_long[sampling %in% only_these_sampling_methods]
    dt_CI = dt_CI[sampling %in% only_these_sampling_methods]
  }

  # Remove the evaluation criterion for the last coalition
  if (isTRUE(remove_last_value)) {
    dt_long = dt_long[n_combinations != max(dt_CI$n_combinations)]
    dt_CI = dt_CI[n_combinations != max(dt_CI$n_combinations)]
  }

  # For the box plots to work we need the combinations to be a factor
  dt_long_combination_factor = data.table::copy(dt_long)
  dt_long_combination_factor$n_combinations = factor(dt_long_combination_factor$n_combinations)

  ### Make the figures
  figure_list = list()

  if ("figure_CI" %in% figures_to_make) {
    figure_list[["figure_CI"]] =
      ggplot2::ggplot(dt_CI, ggplot2::aes(x = n_combinations, y = median, col = sampling)) +
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
  }

  if ("figure_mean" %in% figures_to_make) {
    figure_list[["figure_mean"]] =
      ggplot2::ggplot(dt_CI, ggplot2::aes(x = n_combinations, y = mean, col = sampling)) +
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
  }

  if ("figure_median" %in% figures_to_make) {
    figure_list[["figure_median"]] =
      ggplot2::ggplot(dt_CI, ggplot2::aes(x = n_combinations, y = median, col = sampling)) +
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
  }

  if ("figure_lines" %in% figures_to_make) {
    figure_list[["figure_lines"]] =
      ggplot2::ggplot(dt_long,
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
  }

  if ("figure_boxplot" %in% figures_to_make) {
    figure_list[["figure_boxplot"]] =
      ggplot2::ggplot(dt_long_combination_factor,
                      ggplot2::aes(x = n_combinations,
                                   y = evaluation_criterion,
                                   fill = sampling)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(
        x = "Number of coalitions",
        y = evaluation_criterion,
        fill = "Sampling method") +
      scale_x_discrete(guide = guide_axis(n.dodge = n.dodge)) +
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
  }

  if ("figure_lines_boxplot" %in% figures_to_make) {
    figure_list[["figure_lines_boxplot"]] =
      ggplot2::ggplot(dt_long_combination_factor,
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
      scale_x_discrete(guide = guide_axis(n.dodge = n.dodge)) +
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
  }

  if ("figure_boxplot_lines" %in% figures_to_make) {
    figure_list[["figure_boxplot_lines"]] =
      ggplot2::ggplot(dt_long_combination_factor,
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
      scale_x_discrete(guide = guide_axis(n.dodge = n.dodge)) +
      {if (is.null(brewer_palette)) ggplot2::scale_fill_hue()} +
      {if (is.null(brewer_palette)) ggplot2::scale_color_hue()} +
      {if (!is.null(brewer_palette)) ggplot2::scale_fill_brewer(palette = brewer_palette,
                                                                direction = brewer_direction)} +
      {if (!is.null(brewer_palette)) ggplot2::scale_color_brewer(palette = brewer_palette,
                                                                 direction = brewer_direction)} +
      {if (!is.null(ggplot_theme)) ggplot_theme} +
      {if (flip_coordinates) ggplot2::coord_flip()} +
      {if (!is.null(legend_position)) ggplot2::theme(legend.position = legend_position)}
  }

  # If plot figures, then plot the figures
  if (plot_figures) lapply(rev(figure_list), plot)

  ### Return the results
  if (length(figure_list) == 1) figure_list = figure_list[[1]]
  return(figure_list)
}



## Old functions ---------------------------------------------------------------------------------------------------
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
#' @param scale_y_log10 Boolean. If we are to use [ggplot2::scale_y_log10()].
#' @param scale_x_log10 Boolean. If we are to use [ggplot2::scale_x_log10()].
#' @param n_workers Integer. The number of cores to run the computations.
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
  ### Setup and checks
  # Check that ggplot2 is installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table is not installed. Please run install.packages('data.table')")
  }
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("future is not installed. Please run install.packages('future')")
  }
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("future.apply is not installed. Please run install.packages('future.apply')")
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

  ### Make data.tables
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



  ### Make the figures
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

  ### Return the results
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

# pilot_estimates_paired_order_V2 = function(explanation,
#                                            figures_to_plot = NULL,
#                                            objects_to_return = "order") {
#   if (any(!figures_to_plot %in% c(NULL, "Rij_ind", "Rij_aggregated", "R_aggregated"))) stop("Invalid figures.")
#   if (any(!objects_to_return %in% c("order", "R_dt", "R_dt_aggregated", "R_matrix_list_all_individuals"))) {
#     stop("Invalid return objects.")
#   }
#
#   # Extract the internal list from the explanation object
#   internal = explanation$internal
#
#   # Get the number of features
#   M = internal$parameters$n_features
#
#   if (2^M != internal$parameters$n_combinations) {
#     stop("Currently we need the `explanation` to have `n_combinations` = 2^M, where M is the number of featuers.")
#     # TODO: we can later remove this if we do not need this as we can rather return the features to use
#     # internal$objects$X$features instead of using the index of these.
#   }
#
#   # We extract the W and S matrices
#   W = explanation$internal$objects$W
#   S = explanation$internal$objects$S
#
#   # Extract the v(S) elements
#   dt_vS = explanation$internal$output$dt_vS
#
#   # Compute the R's. I.e., W * v(s), but without adding them together.
#   # So not a matrix product, but rather an element wise multiplication.
#   # Do it for all test observations and store the results in a list.
#   R_matrix_list_all_individuals =
#     lapply(seq(ncol(dt_vS[, -"id_combination"])),
#            function (test_obs_idx) t(t(W)*as.matrix(dt_vS[, -"id_combination"])[,test_obs_idx]))
#
#   # Alternate them such that we extract the smallest, then the largest, then the second smallest,
#   # then the second largest and so on.
#   alternating_indices = c(rbind(seq(1, 2^(M-1)), seq(2^M, 2^(M-1) + 1)))
#
#   # Change the order of the coalitions such that we have S (odd indices) and
#   # then S_bar (even indices), for all possible coalitions.
#   # Note that we add 1 as we exclude the phi0.
#   R_matrix_paired_order_list =
#     lapply(seq(M), function (investigate_feature_number) {
#       sapply(R_matrix_list_all_individuals,
#              "[",
#              investigate_feature_number + 1, )[alternating_indices,]
#     })
#
#   # We compute the difference between the S and S_bar entries. Odd minus even indices.
#   R_matrix_paired_order_diff_list =
#     lapply(seq_along(R_matrix_paired_order_list),
#            function (feature_idx) {
#              R_matrix_paired_order_list[[feature_idx]][seq(1, 2^M,2), ] -
#                R_matrix_paired_order_list[[feature_idx]][seq(2, 2^M,2), ]
#            })
#
#   # Convert it to a data.table
#   R_dt_paired_order_diff =
#     data.table::rbindlist(
#       lapply(seq_along(R_matrix_paired_order_diff_list),
#              function (feature_idx) {
#                data.table(id = factor(seq(nrow(explanation$internal$data$x_explain))),
#                           D = t(R_matrix_paired_order_diff_list[[feature_idx]]))
#              }),
#       idcol = "id_feature",
#       use.names = TRUE
#     )
#
#   # Change the column names
#   setnames(R_dt_paired_order_diff, c("id_feature", "id", paste0("D", seq(2^(M-1)))))
#
#   # Go from wide data.table to long data.table format
#   R_dt_paired_order_diff_long = melt(R_dt_paired_order_diff,
#                                      id.vars = c("id_feature", "id"),
#                                      value.name = "Rij",
#                                      variable.name = "id_combination_diff",
#                                      variable.factor = TRUE)
#
#   if ("Rij_ind" %in% figures_to_plot) {
#     plot(ggplot(data = R_dt_paired_order_diff_long[id %in% 1:100],
#                 aes(fill = id, y = Rij, x = id_combination_diff)) +
#            geom_bar(position = "dodge", stat = "identity") +
#            facet_grid(rows = vars(id_feature)) +
#            scale_x_discrete(guide = guide_axis(n.dodge = 3)))
#   }
#
#   # Compute the mean of the Rij's summed over all test observations, and the same when also using the absolute value
#   # So, $\frac{1}{N_test}\sum_{j = 1}^N_test Rij$ and $\frac{1}{N_test}\sum_{j = 1}^N_test |Rij|$.
#   R_dt = R_dt_paired_order_diff_long[, .(mean_Rij = mean(Rij),
#                                          mean_abs_Rij = mean(abs(Rij))),
#                                      by = list(id_combination_diff, id_feature)]
#
#   # Add columns with the order of the mean_abs_Rij for each feature and each paired coalitions,
#   # we also add the index of the coalitions that are included in the paired coalitions.
#   R_dt[, `:=` (order_mean_abs_Rij = order(order(mean_abs_Rij, decreasing = TRUE), decreasing = FALSE),
#                id_combination_S = seq(1, 2^(M-1)),
#                id_combination_Sbar = seq(2^M, 2^(M-1) + 1)),
#        by = id_feature]
#
#   # We aggregate the `mean_Rij` and `mean_abs_Rij` over the features, so we get a single
#   # mean for each paired coalition. This is thus a value average over all features and test observations.
#   R_dt_aggregated = R_dt[, lapply(.SD, mean),
#                          .SDcols = c("mean_Rij", "mean_abs_Rij"),
#                          by = id_combination_diff][, setnames(.SD,
#                                                               c("mean_Rij", "mean_abs_Rij"),
#                                                               c("mean_R", "mean_abs_R"))]
#
#   if ("R_aggregated" %in% figures_to_plot) {
#     plot(ggplot(data = R_dt_aggregated, aes(x = id_combination_diff, y = mean_abs_R)) +
#            geom_bar(position="dodge", stat="identity") +
#            coord_flip())
#   }
#
#   if ("Rij_aggregated" %in% figures_to_plot) {
#     R_dt_aggregated2 =
#       data.table::copy(R_dt_aggregated)[,`id_feature` := "agr"][, setnames(.SD, "mean_abs_R", "mean_abs_Rij")]
#     R_dt2 = rbindlist(list(R_dt, R_dt_aggregated2), use.names = TRUE, fill = TRUE)
#     plot(ggplot(data = R_dt2, aes(x = id_combination_diff, y = mean_abs_Rij)) +
#            geom_bar(position = "dodge", stat = "identity") +
#            facet_grid(rows = vars(id_feature)) +
#            scale_x_discrete(guide = guide_axis(n.dodge = 2)))
#   }
#
#   # Add order values for the aggregated values. A low value means that it is how high importance.
#   # I.e., if `mean_abs_R_ordered = 1` then this is the most important paired coalition.
#   R_dt_aggregated[, `:=` (mean_abs_R_ordered = order(order(mean_abs_R, decreasing = TRUE), decreasing = FALSE),
#                           mean_R_ordered = order(order(mean_R, decreasing = TRUE), decreasing = FALSE))]
#
#   # Merge together to only add the "id_combination_S" and "id_combination_Sbar" columns to the dt.
#   R_dt_aggregated = R_dt_aggregated[unique(R_dt[,c("id_combination_diff", "id_combination_S", "id_combination_Sbar")]),
#                                     on = "id_combination_diff"]
#
#   # Extract which order we should add the paired coalitions based on the mean_abs_R score
#   specific_coalition_set = c(t(as.matrix(setorder(
#     R_dt_aggregated[, c("mean_abs_R_ordered", "id_combination_S", "id_combination_Sbar")],
#     mean_abs_R_ordered)[,-"mean_abs_R_ordered"])))
#
#   # Return the results
#   return_list = list()
#   if ("order" %in% objects_to_return) return_list[["order"]] = specific_coalition_set
#   if ("R_dt" %in% objects_to_return) return_list[["R_dt"]] = R_dt
#   if ("R_dt_aggregated" %in% objects_to_return) return_list[["R_dt_aggregated"]] = R_dt_aggregated
#   if ("R_matrix_list_all_individuals" %in% objects_to_return) {
#     return_list[["R_matrix_list_all_individuals"]] = R_matrix_list_all_individuals
#   }
#   if (length(return_list) == 1) return_list = return_list[[1]]
#
#   # Return the order of paired coalitions should be added
#   return(return_list)
# }

