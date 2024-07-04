# FILE TO RUN THE WINE DATA
library(ranger)
library(data.table)
devtools::load_all(".")


if (R.utils::System$getHostname() == "Larss-MacBook-Pro.local" || Sys.info()[[7]] == "larsolsen") {
  path_source = "/Users/larsolsen"
} else {
  path_source = "/mn/kadingir/biginsight_000000/lholsen"
}

sep_rf = readRDS(file.path(path_source, "PhD/Paper3/Paper3_save_location", paste0("Wine_data_sep_rf", ".rds")))
Wine = as.data.table(read.csv2(file.path(path_source, "PhD/Paper3/Paper3_save_location/winequality-red.csv")))



Wine = Wine[, lapply(.SD, as.numeric),]
setcolorder(Wine, "quality")
setnames(Wine, c("quality",
                 "fix_acid",
                 "vol_acid",
                 "cit_acid",
                 "res_sugar",
                 "chlorides",
                 "free_sulfur",
                 "total_sulfur",
                 "density",
                 "pH",
                 "sulphates",
                 "alcohol"))

No_test_obs = 99
No_train_obs = nrow(Wine) - No_test_obs

# Set seed for reproducibility
seed_sample = 2024
set.seed(seed_sample)

# Sample test indices
test_indices = sample(nrow(Wine), No_test_obs)

# Split the data into training and test data
data_train = Wine[-test_indices,]
data_test = Wine[test_indices,]

# Extract the features of the training and test data
x_train = data_train[,-1]
x_test = data_test[,-1]
head(x_test)

# Extract the response of the training and test dat
y_train = as.matrix(data_train[,1])
y_test = as.matrix(data_test[,1])

plot(y_train)
plot(y_test)

# Specifying the phi_0, i.e. the expected prediction without any features
phi_0 = mean(y_train)

set.seed(123)
library(ranger)
predictive_model = ranger(quality ~ ., data = data_train, num.trees = 200, mtry = 4, min.node.size = 3)
message(sprintf("Training MSE = %g and test MSE = %g.",
                mean((predict(predictive_model, data_train)$predictions - data_train$quality)^2),
                mean((predict(predictive_model, data_test)$predictions - data_test$quality)^2)))



# Get the prediction zero, i.e., the phi0 Shapley value.
p0 = mean(data_train$quality)
model = predictive_model
x_explain = x_test

# Things needed for the pilot versions
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
                                      "single_median_ranking_over_each_test_obs")
specific_coalition_set_strategies_sampling = c("paired_coalitions_weights",
                                               "paired_coalitions_weights_direct",
                                               "paired_coalitions_weights_equal_weights",
                                               "paired_coalitions_weights_direct_equal_weights")



source(file.path(path_source, "PhD/Paper3/shapr/R/Lars_explore_ideas_scripts/new_functions.R"))



specific_coalition_set = pilot_estimates_coal_order(sep_rf)
specific_coalition_set_weights = lapply(seq_along(specific_coalition_set), function(x) NULL)
names(specific_coalition_set_weights) = names(specific_coalition_set)
specific_coalition_set$paired_coalitions_weights = specific_coalition_set$paired_coalitions_weights
specific_coalition_set$paired_coalitions_weights_equal_weights = specific_coalition_set$paired_coalitions_weights
specific_coalition_set$paired_coalitions_weights_direct = specific_coalition_set$paired_coalitions_weights
specific_coalition_set$paired_coalitions_weights_direct_equal_weights = specific_coalition_set$paired_coalitions_weights
specific_coalition_set_weights$paired_coalitions_weights = specific_coalition_set$paired_coalitions_weights
specific_coalition_set_weights$paired_coalitions_weights_equal_weights = specific_coalition_set$paired_coalitions_weights
specific_coalition_set_weights$paired_coalitions_weights_direct = specific_coalition_set$paired_coalitions_weights
specific_coalition_set_weights$paired_coalitions_weights_direct_equal_weights = specific_coalition_set$paired_coalitions_weights







n_combinations_vec = c(2, 4, 6, 8, 10, 12, 14, 16)

n_combinations_vec = c(1600, 1700, 1800, 1900)
n_combinations_vec  = c(2, 10, 100, 500)
n_combinations_vec = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 200, 300, 400,
                       430, 440, 450, 460, 470, 480, 490, 500, 600, 700, 800, 900, 1000, 1075, 1100, 1110, 1120, 1130, 1140, 1200,
                       1250, 1500, 1750, 1800, 1900, 2000)

m = 11
n_features <- seq(ceiling((m - 1)/2))
n <- sapply(n_features, choose, n = m)
n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]


n_cumsum = (cumsum(n) + 2)
n_cumsum = n_cumsum[-length(n_cumsum)]
n_combinations_vec = unique(sort(c(n_combinations_vec, n_cumsum)))


B = 5
B = 25

res_dt = data.table(Strategy = character(), n_combinations = integer(), repetition = integer(), MAE = numeric())
res = list()

sampling_methods = c("unique_paired", "unique_paired_equal_weights", "unique_paired_SW", "paired_coalitions_weights_direct_equal_weights", "paired_coalitions")
sampling_methods = c("unique_paired", "unique_paired_equal_weights", "unique_paired_SW", "paired_coalitions_weights_direct_equal_weights")
sampling_methods = c("unique", "unique_paired", "unique_paired_equal_weights", "unique_paired_SW", "paired_coalitions_weights_direct_equal_weights", "paired_coalitions")

sampling_methods = c("paired_coalitions_weights_direct_equal_weights_new_weights_gompertz",
                     "unique_paired_new_weights_gompertz",
                     "paired_coalitions_new_weights_gompertz",
                     "unique_paired_new_weights_empirical",
                     "paired_coalitions_new_weights_empirical",
                     "paired_coalitions_weights_direct_equal_weights_new_weights_empirical",
                     "unique",
                     "unique_paired",
                     "unique_paired_equal_weights",
                     "unique_paired_SW",
                     "paired_coalitions",
                     "paired_coalitions_weights_direct_equal_weights",
                     "largest_weights",
                     "largest_weights_combination_size")


sampling_method = sampling_methods[1]
for (sampling_method in sampling_methods) {
  sampling_method_full_name = sampling_method
  if (!is.list(res[[sampling_method]])) res[[sampling_method]] = list()
  n_combinations = n_combinations_vec[1]
  for (n_combinations in n_combinations_vec) {
    if (!is.list(res[[sampling_method]][[paste0("n_combinations_", n_combinations)]])) res[[sampling_method]][[paste0("n_combinations_", n_combinations)]] = list()
    seed = 1
    for (seed in seq(B)) {
      cat(paste0("Strategy = ", sampling_method, ", n_combinations = ", n_combinations, ", seed = ", seed, ".\n"))

      sampling_method_full_name = sampling_method

      new_weights = grepl("_new_weights", sampling_method, fixed = TRUE)
      if (new_weights) {
        new_weights_string = tail(strsplit(sampling_method, "_")[[1]], 1)
        sampling_method_updated = gsub(paste0("_new_weights_", new_weights_string), "", sampling_method)
        file_name = file.path(path_source, paste0("PhD/Paper3/Paper3_save_location/Samp_prop_and_gompertz_M_", ncol(x_explain), ".rds"))

        if (!file.exists(file_name)) stop("There are no Samp_prop_and_gompertz file for this dimension.")
        dt_new_weights = readRDS(file_name)
      } else {
        new_weights_string = NULL
        dt_new_weights = NULL
        sampling_method_updated = gsub("_new_weights", "", sampling_method)
      }

      sampling_method_now = if (sampling_method_updated %in% specific_coalition_set_strategies) "specific_coalition_set" else sampling_method

      ## pilot stuff
      sampling_method_full_name_updated = gsub("_replace_W", "", sampling_method_full_name)
      if (!is.null(new_weights_string)) {
        sampling_method_full_name_updated = gsub(paste0("_new_weights_", new_weights_string), "", sampling_method_full_name)
      }

      # Extract only the relevant coalitions from `specific_coalition_set` (not for method in specific_coalition_set_strategies_sampling)
      specific_coalition_set_now = if (sampling_method_full_name_updated %in% specific_coalition_set_strategies) specific_coalition_set[[sampling_method_full_name_updated]] else NULL

      specific_coalition_set_weights_now =
        if (sampling_method_full_name_updated %in% specific_coalition_set_strategies) specific_coalition_set_weights[[sampling_method_full_name_updated]] else NULL

      if (!is.null(specific_coalition_set_now) && !sampling_method_full_name_updated %in% specific_coalition_set_strategies_sampling) {
        specific_coalition_set_now = specific_coalition_set_now[seq(n_combinations)]
      }

      # Extract only the relevant coalitions from `specific_coalition_set_weights` (not for method in specific_coalition_set_strategies_sampling)
      if (!is.null(specific_coalition_set_weights_now) && !sampling_method_full_name_updated %in% specific_coalition_set_strategies_sampling) {
        specific_coalition_set_weights_now = specific_coalition_set_weights_now[seq(n_combinations)]
      }


      explanation_now = suppressMessages(suppressWarnings(
        explain(
          model = model,
          x_explain = x_explain,
          x_train = x_train,
          prediction_zero = p0,
          n_batches = 1,
          seed = seed,
          n_combinations = n_combinations,
          approach = "regression_separate",
          regression.model = parsnip::rand_forest(engine = "ranger", mode = "regression"),
          sampling_method = sampling_method_now,
          sampling_method_full_name = sampling_method_full_name,
          precomputed_vS = list(dt_vS = sep_rf$internal$output$dt_vS),
          specific_coalition_set = specific_coalition_set_now,
          specific_coalition_set_weights = specific_coalition_set_weights_now,
          new_weights_string = new_weights_string,
          dt_new_weights = dt_new_weights
        )
      ))


      res_dt = rbind(res_dt,
                     data.table(Strategy = sampling_method_full_name,
                                n_combinations = n_combinations,
                                repetition = seed,
                                MAE = compute_MAE_MSE_fast(as.matrix(sep_rf$shapley_values),
                                                           as.matrix(explanation_now$shapley_values))))

      res[[sampling_method_full_name]][[paste0("n_combinations_", n_combinations)]][[paste0("repetition_", seed)]] =
        explanation_now
    }
  }
  saveRDS(list(res_dt = res_dt, res = res[[sampling_method_full_name]]),
          file.path(path_source, "PhD/Paper3/Paper3_save_location", paste0("NEW_Wine_data_res_", sampling_method_full_name, ".rds")))
}
saveRDS(res_dt, file.path(path_source, "PhD/Paper3/Paper3_save_location", paste0("NEW_Wine_data_res_only_res_dt", ".rds")))


