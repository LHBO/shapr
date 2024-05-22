

# but we let the other entries be 0.
dt_vS = data.table(id_combination = rep(seq(2^M)))[, `:=` (paste0("p_hat1_", seq(n_test)), 0)]
dt_vS[id_combination == 1, `:=` (names(dt_vS)[-1], prediction_zero)] # can be directly given as it is a scalar.
dt_vS[id_combination == .N, `:=` (names(dt_vS)[-1], as.list(response_test))] # need to be a list as it is a vector.


# Create the shapr object. The Shapley value output will be rubbish,
# but we only need the object/list structure. Do not want any warnings.
progressr::with_progress({
  explanations_tmp = suppressWarnings(suppressMessages(
    shapr::explain(
      model = predictive_model,
      x_explain = data_test,
      x_train = data_train,
      approach = approach,
      prediction_zero = prediction_zero,
      keep_samp_for_vS = FALSE,
      exact = TRUE,
      # n_combinations = 2^ncol(x_explain), # Do not need it as we specify `exact = TRUE`.
      n_samples = 1,
      n_batches = 10,
      seed = 1,
      precomputed_vS = list(dt_vS = dt_vS)
    )))}, enable = TRUE)

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
precomputed_vS$dt_vS[,1:10]



result_list = list()

result_list[["unique_paired_900"]] = with_progress(
  future_compute_SV_function(
    compute_SV_function = compute_SV_function,
    used_sequence_n_combinations = c(900),
    n_combinations_total = 1,
    n_combinations_to = 1024,
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = approach,
    prediction_zero = prediction_zero,
    keep_samp_for_vS = FALSE,
    n_samples = n_samples,
    n_batches = 10,
    seed = 1,
    sampling_method = "unique_paired",
    sampling_method_full_name = "unique_paired",
    sampling_method_idx = 1,
    n_sampling_methods = 1,
    precomputed_vS = precomputed_vS,
    specific_coalition_set = NULL,
    specific_coalition_set_weights = NULL,
    n_repetitions = 1),
  enable = TRUE)


result_list[["unique_paired_sort_900"]] = with_progress(
  future_compute_SV_function(
    compute_SV_function = compute_SV_function,
    used_sequence_n_combinations = c(900),
    n_combinations_total = 1,
    n_combinations_to = 1024,
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = approach,
    prediction_zero = prediction_zero,
    keep_samp_for_vS = FALSE,
    n_samples = n_samples,
    n_batches = 10,
    seed = 1,
    sampling_method = "unique_paired",
    sampling_method_full_name = "unique_paired",
    sampling_method_idx = 1,
    n_sampling_methods = 1,
    precomputed_vS = precomputed_vS,
    specific_coalition_set = NULL,
    specific_coalition_set_weights = NULL,
    n_repetitions = 1,
    sort_combinations = TRUE),
  enable = TRUE)



result_list[["unique_paired_replace_900"]] = with_progress(
  future_compute_SV_function(
    compute_SV_function = compute_SV_function,
    used_sequence_n_combinations = c(900),
    n_combinations_total = 1,
    n_combinations_to = 1024,
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = approach,
    prediction_zero = prediction_zero,
    keep_samp_for_vS = FALSE,
    n_samples = n_samples,
    n_batches = 10,
    seed = 1,
    sampling_method = "unique_paired",
    sampling_method_full_name = "unique_paired",
    sampling_method_idx = 1,
    n_sampling_methods = 1,
    precomputed_vS = precomputed_vS,
    specific_coalition_set = NULL,
    specific_coalition_set_weights = NULL,
    n_repetitions = 1,
    replace_W = TRUE),
  enable = TRUE)


result_list[["unique_paired_SW_900"]] = with_progress(
  future_compute_SV_function(
    compute_SV_function = compute_SV_function,
    used_sequence_n_combinations = c(900),
    n_combinations_total = 1,
    n_combinations_to = 1024,
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = approach,
    prediction_zero = prediction_zero,
    keep_samp_for_vS = FALSE,
    n_samples = n_samples,
    n_batches = 10,
    seed = 1,
    sampling_method = "unique_paired_SW",
    sampling_method_full_name = "unique_paired_SW",
    sampling_method_idx = 1,
    n_sampling_methods = 1,
    precomputed_vS = precomputed_vS,
    specific_coalition_set = NULL,
    specific_coalition_set_weights = NULL,
    n_repetitions = 1),
  enable = TRUE)

result_list[["unique_paired_replace_SW_900"]] = with_progress(
  future_compute_SV_function(
    compute_SV_function = compute_SV_function,
    used_sequence_n_combinations = c(900),
    n_combinations_total = 1,
    n_combinations_to = 1024,
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = approach,
    prediction_zero = prediction_zero,
    keep_samp_for_vS = FALSE,
    n_samples = n_samples,
    n_batches = 10,
    seed = 1,
    sampling_method = "unique_paired_SW",
    sampling_method_full_name = "unique_paired_SW",
    sampling_method_idx = 1,
    n_sampling_methods = 1,
    precomputed_vS = precomputed_vS,
    specific_coalition_set = NULL,
    specific_coalition_set_weights = NULL,
    n_repetitions = 1,
    replace_W = TRUE),
  enable = TRUE)



remove_small_values = function (WW) {
  WW[abs(WW) < 0.00001] = 0
  WW
}

remove_small_values(result_list$unique_paired_20[[1]]$internal$objects$W)
remove_small_values(result_list$unique_paired_SW_20[[1]]$internal$objects$W)


result_list$unique_paired_20[[1]]$internal$objects$W

result_list$unique_paired_500[[1]]$internal$objects$W

result_list$unique_paired_SW_500


# View(result_list$unique_paired_900[[1]]$internal$objects$X[n_features == 9])
# View(result_list$unique_paired_sort_900[[1]]$internal$objects$X[n_features == 9])
# View(true_explanations$internal$objects$X[n_features == 9])



compute_MAE_MSE_fast(as.matrix(true_explanations$shapley_values), as.matrix(result_list$unique_paired_20[[1]]$shapley_values))
compute_MAE_MSE_fast(as.matrix(true_explanations$shapley_values), as.matrix(result_list$unique_paired_500[[1]]$shapley_values))
compute_MAE_MSE_fast(as.matrix(true_explanations$shapley_values), as.matrix(result_list$unique_paired_900[[1]]$shapley_values))
compute_MAE_MSE_fast(as.matrix(true_explanations$shapley_values), as.matrix(result_list$unique_paired_sort_900[[1]]$shapley_values))
compute_MAE_MSE_fast(as.matrix(true_explanations$shapley_values), as.matrix(result_list$unique_paired_replace_900[[1]]$shapley_values))


compute_MAE_MSE_fast(as.matrix(true_explanations$shapley_values), as.matrix(result_list$unique_paired_SW_20[[1]]$shapley_values))
compute_MAE_MSE_fast(as.matrix(true_explanations$shapley_values), as.matrix(result_list$unique_paired_SW_500[[1]]$shapley_values))
compute_MAE_MSE_fast(as.matrix(true_explanations$shapley_values), as.matrix(result_list$unique_paired_SW_900[[1]]$shapley_values))
compute_MAE_MSE_fast(as.matrix(true_explanations$shapley_values), as.matrix(result_list$unique_paired_replace_SW_900[[1]]$shapley_values))



compute_MAE_MSE_fast(as.matrix(true_explanations$shapley_values), as.matrix(result_list$paired_pilot_900[[1]]$shapley_values))
compute_MAE_MSE_fast(as.matrix(true_explanations$shapley_values), as.matrix(result_list$paired_pilot_sort_900[[1]]$shapley_values))
compute_MAE_MSE_fast(as.matrix(true_explanations$shapley_values), as.matrix(result_list$paired_pilot_sort_replace_900[[1]]$shapley_values))

comb_order = pilot_estimates_coal_order(true_explanations)


result_list[["paired_pilot_900"]] = with_progress(
  future_compute_SV_function(
    compute_SV_function = compute_SV_function,
    used_sequence_n_combinations = c(900),
    n_combinations_total = 1,
    n_combinations_to = 1024,
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = approach,
    prediction_zero = prediction_zero,
    keep_samp_for_vS = FALSE,
    n_samples = n_samples,
    n_batches = 10,
    seed = 1,
    sampling_method = "specific_coalition_set",
    sampling_method_full_name = "paired_coalitions",
    sampling_method_idx = 1,
    n_sampling_methods = 1,
    precomputed_vS = precomputed_vS,
    specific_coalition_set = comb_order$paired_coalitions[1:900],
    specific_coalition_set_weights = NULL,
    n_repetitions = 1),
  enable = TRUE)

result_list[["paired_pilot_sort_900"]] = with_progress(
  future_compute_SV_function(
    compute_SV_function = compute_SV_function,
    used_sequence_n_combinations = c(900),
    n_combinations_total = 1,
    n_combinations_to = 1024,
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = approach,
    prediction_zero = prediction_zero,
    keep_samp_for_vS = FALSE,
    n_samples = n_samples,
    n_batches = 10,
    seed = 1,
    sampling_method = "specific_coalition_set",
    sampling_method_full_name = "paired_coalitions",
    sampling_method_idx = 1,
    n_sampling_methods = 1,
    precomputed_vS = precomputed_vS,
    specific_coalition_set = sort(comb_order$paired_coalitions[1:900]),
    specific_coalition_set_weights = NULL,
    n_repetitions = 1),
  enable = TRUE)


result_list[["paired_pilot_sort_replace_900"]] = with_progress(
  future_compute_SV_function(
    compute_SV_function = compute_SV_function,
    used_sequence_n_combinations = c(900),
    n_combinations_total = 1,
    n_combinations_to = 1024,
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = approach,
    prediction_zero = prediction_zero,
    keep_samp_for_vS = FALSE,
    n_samples = n_samples,
    n_batches = 10,
    seed = 1,
    sampling_method = "specific_coalition_set",
    sampling_method_full_name = "paired_coalitions",
    sampling_method_idx = 1,
    n_sampling_methods = 1,
    precomputed_vS = precomputed_vS,
    specific_coalition_set = sort(comb_order$paired_coalitions[1:900]),
    specific_coalition_set_weights = NULL,
    n_repetitions = 1,
    replace_W = TRUE),
  enable = TRUE)
