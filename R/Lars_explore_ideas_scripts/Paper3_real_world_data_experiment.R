file = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Xgboost_M_10_n_train_1000_n_test_1000_rho_0.5_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_estimated_repetition_1.rds")
file_true = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Xgboost_M_10_n_train_1000_n_test_1000_rho_0.5_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_true.rds")



strats = c("unique", "unique_paired", "unique_paired_equal_weights",  "unique_paired_SW", "paired_coalitions_weights_direct_equal_weights", "paired_coalitions")

strats = c("unique", "unique_paired_equal_weights", "unique_paired_SW", "paired_coalitions_weights_direct_equal_weights", "paired_coalitions")

strats = c("unique", "unique_paired_equal_weights", "paired_coalitions")


explanation_list = list()
for (strat in strats) {
  tmp = file[[strat]]$repetition_1
  # tmp$n_combinations_2$shapley_values


  combinations = as.integer(sub("n_combinations_", "", names(tmp)))


  combinations_relevant = c(10, 54, 104, 254, 1024)
  combinations_relevant = c(10, 54, 104)

  explanation_list_2 = tmp[c(paste0("n_combinations_", combinations_relevant))]
  names(explanation_list_2) = paste(strat, names(explanation_list_2), sep = "_")

  explanation_list_2 = lapply(explanation_list_2, function(x) {
    x$pred_explain = file_true$pred_explain
    x$internal$data$x_explain = file_true$internal$data$x_explain
    x$internal$parameters$is_groupwise = FALSE
    x
  }
  )

  explanation_list = c(explanation_list, explanation_list_2)
}

ss = seq(1, length(explanation_list), length(combinations_relevant))
reorder = c(sapply(seq(length(combinations_relevant)), function(x) ss + x - 1))
reorder = seq(length(explanation_list))
explanation_list = explanation_list[reorder]

explanation_list = c(explanation_list, list("True" = file_true))
n_explicands = 6
index_explicands = order(file_true$pred_explain)[seq(1, file_true$internal$parameters$n_explain, length.out = n_explicands)]


plot_SV_several_approaches(explanation_list, index_explicands = index_explicands, digits = 2) +
  scale_fill_manual(values=rev(c(cols[reorder], "black")))





rep()

n_combs = 3
cols = c(
  brewer.pal(n_combs + 1, "Blues")[-1],
  brewer.pal(n_combs + 1, "Reds")[-1],
  brewer.pal(n_combs + 1, "Greens")[-1],
  brewer.pal(n_combs + 1, "Purples")[-1],
  brewer.pal(n_combs + 1, "Oranges")[-1]
  )

#

library(RColorBrewer)
f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
f("Greens")
(cols <- f("Set1"))








# Red Wine --------------------------------------------------------------------------------------------------------



library(data.table)
Wine = as.data.table(read.csv2("/Users/larsolsen/PhD/Paper3/Paper3_save_location/winequality-red.csv"))
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



library(xgboost)

# USE tidymodels to do CV to find best hyperparameters
# library(tidymodels)
regression.workflow = workflows::add_recipe(
  workflows::add_model(
    workflows::workflow(),
    parsnip::boost_tree(trees = hardhat:::tune(),
                        tree_depth = hardhat:::tune(),
                        learn_rate = hardhat:::tune(),
                        engine = "xgboost",
                        mode = "regression")),
  recipes::recipe(as.formula("quality ~ ."),
                  data = data_train))

regression.results <- tune::tune_grid(
  object = regression.workflow,
  resamples = rsample::vfold_cv(data = data_train, v = 5),
  grid = expand.grid(tree_depth = c(2, 4, 6, 8),
                     trees = c(5, 10, 25, 50, 100, 200, 250, 500, 1000, 1500, 2000),
                     learn_rate = c(0.05, 0.1, 0.2)),
  # grid = dials::grid_regular(dials::trees(), dials::tree_depth(), dials::learn_rate(), dials::mtry(c(1, 10)), levels = 5),
  metrics = yardstick::metric_set(yardstick::rmse),
  control = tune::control_grid(verbose = TRUE)
)
print(tune::show_best(regression.results, metric = "rmse", n = 10))

# Look at the accuracy of the model
predictive_model = lm(quality ~ ., data = data_train)
message(sprintf("Training MSE = %g and test MSE = %g.",
                mean((predict(predictive_model, data_train) - data_train$quality)^2),
                mean((predict(predictive_model, data_test) - data_test$quality)^2)))

# Fit a GAM model.
library(mgcv)
predictive_model = gam(as.formula(paste0("quality ~ ", paste0("ti(", colnames(Wine)[-1], ")", collapse = " + "))),
                       data = data_train)
message(sprintf("Training MSE = %g and test MSE = %g.",
                mean((predict(predictive_model, data_train) - data_train$quality)^2),
                mean((predict(predictive_model, data_test) - data_test$quality)^2)))

# Train an xgboost model with the best hyperparameters
best_results = tune::select_best(regression.results, metric = "rmse")
# trees tree_depth learn_rate .config
# <dbl>      <dbl>      <dbl> <chr>
#  200          8       0.05 Preprocessor1_Model105
predictive_model = xgboost(data = as.matrix(data_train[,-"quality"]),
                           label = data_train$quality,
                           nrounds = best_results$trees,
                           params = list("eta" = best_results$learn_rate, "max_depth" = best_results$tree_depth),
                           verbose = FALSE)
message(sprintf("Training MSE = %g and test MSE = %g.",
                mean((predict(predictive_model, as.matrix(data_train[,-"quality"])) - data_train$quality)^2),
                mean((predict(predictive_model, as.matrix(data_test[,-"quality"])) - data_test$quality)^2)))

xgboost::xgb.save(predictive_model,
                  file.path("/Users/larsolsen/PhD/Paper3/Paper3_save_location/xgboost_models", "WINE_model.model"))



set.seed(2024)
regression.workflow_rf = workflows::add_recipe(
  workflows::add_model(
    workflows::workflow(),
    parsnip::rand_forest(trees = 200,
                         mtry = 4,
                         min_n = 3,
                         engine = "ranger",
                         mode = "regression")),
  recipes::recipe(as.formula("quality ~ ."),
                  data = data_train))
predictive_model = regression.workflow_rf %>% parsnip::fit(data = data_train)
message(sprintf("Training MSE = %g and test MSE = %g.",
                mean((predict(predictive_model, data_train)$.pred - data_train$quality)^2),
                mean((predict(predictive_model, data_test)$.pred - data_test$quality)^2)))

regression.workflow_rf = workflows::add_recipe(
  workflows::add_model(
    workflows::workflow(),
    parsnip::rand_forest(trees = hardhat:::tune(),
                         mtry = hardhat::tune(),
                         min_n = hardhat::tune(),
                        engine = "ranger",
                        mode = "regression")),
  recipes::recipe(as.formula("quality ~ ."),
                  data = data_train))

regression.results_rf <- tune::tune_grid(
  object = regression.workflow_rf,
  resamples = rsample::vfold_cv(data = data_train, v = 5),
  grid = expand.grid(
    trees = c(5, 10, 25, 50, 100, 200, 250, 500, 1000),
    mtry = c(1,2,3,4,5,7,10),
    min_n = c(3,4,5,6,7)
    ),
  #grid = dials::grid_regular(dials::trees(), dials::mtry(c(1, ncol(data_train))), levels = 1),
  metrics = yardstick::metric_set(yardstick::rmse),
  control = tune::control_grid(verbose = TRUE)
)
print(tune::show_best(regression.results_rf, metric = "rmse", n = 10))

tune::select_best(regression.results_rf, metric = "rmse")
# # A tibble: 1 × 4
# mtry trees min_n .config
# <dbl> <dbl> <dbl> <chr>
#  4   200     3 Preprocessor1_Model033
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

library(progressr)
progressr::handlers('cli')
library(future)
future::plan(multisession, workers = 2)

sep_xgboost <- with_progress(
  shapr::explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    prediction_zero = p0,
    n_batches = 100,
    approach = "regression_separate",
    regression.model = parsnip::boost_tree(engine = "xgboost", mode = "regression")
  )
)
future::plan(sequential) # To return to non-parallel computation


sep_rf <- with_progress(
  shapr::explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    prediction_zero = p0,
    n_batches = 100,
    approach = "regression_separate",
    regression.model = parsnip::rand_forest(engine = "ranger", mode = "regression")
  )
)
future::plan(sequential) # To return to non-parallel computation
#saveRDS(sep_rf, file.path("/Users/larsolsen/PhD/Paper3/Paper3_save_location", paste0("Wine_data_sep_rf", ".rds")))

sep_rf = readRDS(file.path("/Users/larsolsen/PhD/Paper3/Paper3_save_location", paste0("Wine_data_sep_rf", ".rds")))

sep_gaussian <- with_progress(
  shapr::explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    prediction_zero = p0,
    n_batches = 100,
    approach = "gaussian"
  )
)

sep_lm <- with_progress(
  shapr::explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    prediction_zero = p0,
    n_batches = 100,
    approach = "regression_separate",
    regression.model = parsnip::linear_reg()
  )
)


shapr::plot_MSEv_eval_crit(list(sep_xgboost = sep_xgboost,
                         sep_rf = sep_rf,
                         sep_gaussian = sep_gaussian,
                         sep_lm = sep_lm))



mean(abs(sep_rf$pred_explain - p0))




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
                                      "single_median_ranking_over_each_test_obs",
                                      "MAD")
specific_coalition_set_strategies_sampling = c("paired_coalitions_weights",
                                               "paired_coalitions_weights_direct",
                                               "paired_coalitions_weights_equal_weights",
                                               "paired_coalitions_weights_direct_equal_weights")

source("~/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts/new_functions.R")
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

sampling_methods = c("largest_weights_random",
                     "largest_weights_random_new_weights_empirical",
                     "MAD",
                     "MAD_new_weights_empirical",
                     "paired_coalitions_weights_direct_equal_weights_new_weights_gompertz",
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


sampling_method = sampling_methods[4]
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
          file.path("/Users/larsolsen/PhD/Paper3/Paper3_save_location", paste0("NEW_Wine_data_res_", sampling_method_full_name, ".rds")))
}
saveRDS(res_dt, file.path("/Users/larsolsen/PhD/Paper3/Paper3_save_location", paste0("NEW_Wine_data_res_only_res_dt", ".rds")))


res_dt = readRDS(file.path("/Users/larsolsen/PhD/Paper3/Paper3_save_location", paste0("NEW_Wine_data_res_only_res_dt", ".rds")))
res_dt_v2 = copy(res_dt)
res_dt_v2[, avg_MAE := mean(MAE), by = list(Strategy, n_combinations)]
res_dt_v2[, c("lower", "median", "upper") := as.list(quantile(MAE, c(0.025, 0.5, 0.975))), by = .(Strategy, n_combinations)]

res_dt_v2[, Strategy := factor(Strategy,
                               levels = c("unique_paired_unif_V2", "unique", "unique_paired", "unique_paired_equal_weights",  "unique_paired_SW",
                                          "unique_paired_new_weights_empirical", "unique_paired_new_weights_gompertz",
                                          "paired_coalitions_weights_direct_equal_weights",
                                          "paired_coalitions_weights_direct_equal_weights_new_weights_empirical",
                                          "paired_coalitions_weights_direct_equal_weights_new_weights_gompertz",
                                          "paired_coalitions",
                                          "paired_coalitions_new_weights_empirical",
                                          "paired_coalitions_new_weights_gompertz",
                                          "largest_weights",
                                          "largest_weights_combination_size"),
                               labels = c("Uniform", "Unique", "Paired", "Paired Average", "Paired Kernel",
                                          "Paired Empirical", "Paired Gompertz",
                                          "Pilot Average", "Pilot Sample Empirical", "Pilot Sample Gompertz",
                                          "Pilot Kernel",  "Pilot Order Empirical", "Pilot Order Gompertz",
                                          "Largest Weights", "Largest Weights Coalition"),
                            ordered = FALSE)]

library(ggplot2)
fig_wine = ggplot(data = res_dt_v2, aes(x = n_combinations, y = avg_MAE, col = Strategy, fill = Strategy)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, linewidth = 0.1) +
  geom_line(linewidth = 1) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(legend.position = 'bottom') +
  guides(col = guide_legend(nrow = 4)) +
  labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote(bar(MAE)*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
  theme(strip.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)))

fig_wine
ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Wine_Random_Forest_MAE_V3.png",
       plot = fig_wine,
       width = 14.2,
       height = 7,
       scale = 0.85,
       dpi = 350)



fig_wine_2 = ggplot(data = data.table(pred = sep_rf$pred_explain), aes(x = pred)) +
  geom_histogram(color="black", fill = "grey") +
  geom_vline(aes(xintercept = p0), color = "red", linewidth = 1, linetype = "dashed") +
  annotate('text', x = 6.15, y = 12.72,
           label = "phi[0]==5.65",
           parse = TRUE,
           size = 8,
           color = "red") +
  labs(color = "Strategy:", fill = "Strategy:", x = expression(f(bold(x)*"*")), y = "Count") +
  theme(strip.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)))



fig_wine_3 = gridExtra::grid.arrange(fig_wine, fig_wine_2, ncol = 2, widths = c(6,4))

ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Wine_Random_Forest_MAE_V5.png",
       plot = fig_wine_3,
       width = 14.2,
       height = 7,
       scale = 0.85,
       dpi = 350)

matplot(n_combinations_vec, MAE_res, type = "l", lty = 1)
matplot(n_combinations_vec, MAE_res, type = "l", lty = 1, log = "y")



sep_rf_4 <- with_progress(
  explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    prediction_zero = p0,
    n_batches = 2,
    n_combinations = 200,
    sampling_method = "unique_paired_equal_weights",
    approach = "regression_separate",
    regression.model = parsnip::rand_forest(engine = "ranger", mode = "regression"),
    precomputed_vS = list(dt_vS = sep_rf$internal$output$dt_vS)
  )
)
compute_MAE_MSE_fast(as.matrix(sep_rf$shapley_values), as.matrix(sep_rf_2$shapley_values))
compute_MAE_MSE_fast(as.matrix(sep_rf$shapley_values), as.matrix(sep_rf_3$shapley_values))
compute_MAE_MSE_fast(as.matrix(sep_rf$shapley_values), as.matrix(sep_rf_4$shapley_values))

shapr::plot_SV_several_approaches(list(Some = sep_rf_2, More = sep_rf_3, Many = sep_rf_4, All = sep_rf), index_explicands = 1:6)




progressr::with_progress({
  true_explanations_tmp <- explain(
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = approach,
    prediction_zero = prediction_zero,
    keep_samp_for_vS = FALSE,
    exact = TRUE,
    n_samples = 1,
    n_batches = 2^(M-2),
    # n_combinations = 2^M, # Do not need it as we specify `exact = TRUE`.
    gaussian.mu = mu,
    gaussian.cov_mat = sigma,
    seed = 1,
    precomputed_vS = list(dt_vS = dt_vS)
  )}, enable = TRUE)









strats = c("unique", "unique_paired", "unique_paired_equal_weights",  "unique_paired_SW", "paired_coalitions_weights_direct_equal_weights", "paired_coalitions")

strats = c("unique", "unique_paired_equal_weights", "unique_paired_SW", "paired_coalitions_weights_direct_equal_weights", "paired_coalitions")

strats = c("unique", "unique_paired_equal_weights", "paired_coalitions")


explanation_list = list()
for (strat in strats) {
  tmp = file[[strat]]$repetition_1
  # tmp$n_combinations_2$shapley_values


  combinations = as.integer(sub("n_combinations_", "", names(tmp)))


  combinations_relevant = c(10, 54, 104, 254, 1024)
  combinations_relevant = c(10, 54, 104)

  explanation_list_2 = tmp[c(paste0("n_combinations_", combinations_relevant))]
  names(explanation_list_2) = paste(strat, names(explanation_list_2), sep = "_")

  explanation_list_2 = lapply(explanation_list_2, function(x) {
    x$pred_explain = file_true$pred_explain
    x$internal$data$x_explain = file_true$internal$data$x_explain
    x$internal$parameters$is_groupwise = FALSE
    x
  }
  )

  explanation_list = c(explanation_list, explanation_list_2)
}

ss = seq(1, length(explanation_list), length(combinations_relevant))
reorder = c(sapply(seq(length(combinations_relevant)), function(x) ss + x - 1))
reorder = seq(length(explanation_list))
explanation_list = explanation_list[reorder]

explanation_list = c(explanation_list, list("True" = file_true))
n_explicands = 6
index_explicands = order(file_true$pred_explain)[seq(1, file_true$internal$parameters$n_explain, length.out = n_explicands)]


plot_SV_several_approaches(explanation_list, index_explicands = index_explicands, digits = 2) +
  scale_fill_manual(values=rev(c(cols[reorder], "black")))



n_combinations_vec = c(20, 50, 100, 200, 500, 1000)

explanation_list_org = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Wine_data_res_unique_paired_equal_weights.rds")
#explanation_list = res$unique_paired_equal_weights
explanation_list2 = lapply(explanation_list_org$res, function(x) x[[1]])
explanation_list2 = explanation_list2[paste0("n_combinations_", n_combinations_vec)]
names(explanation_list2) = paste0("n_combinations_", n_combinations_vec)
explanation_list2 = c(explanation_list2, list(True = sep_rf))
index_explicands = c(1:2)
n_explicands = 6
index_explicands = order(sep_rf$pred_explain)[seq(1, sep_rf$internal$parameters$n_explain, length.out = n_explicands)]

index_explicands
explanation_list2
explanation_list3 = lapply(seq(length(explanation_list2)), function(i) {
  print(i)
  x = explanation_list2[[i]]
  xx = copy(x)
  xx
  xx$shapley_values = xx$shapley_values[index_explicands,]
  xx$pred_explain = xx$pred_explain[index_explicands]
  names(xx$pred_explain) = paste0("p_hat1_", seq(n_explicands))
  xx$internal$data$x_explain = xx$internal$data$x_explain[index_explicands,]
  return(xx)
})
names(explanation_list3) = c(n_combinations_vec, 2^11)


plot_SV_several_approaches(explanation_list2, index_explicands = index_explicands, digits = 2)
plot_SV_several_approaches(explanation_list3, index_explicands = c(1:6), digits = 2) +
  theme(legend.position = 'bottom') +
  guides(fill = guide_legend(nrow = 1))



set.seed(123)
fig_beswarm_200 = plot(explanation_list_org$res$n_combinations_200$repetition_1, plot_type = "beeswarm") +
  theme(strip.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)))

fig_beswarm_500 = plot(explanation_list_org$res$n_combinations_500$repetition_1, plot_type = "beeswarm") +
  theme(strip.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)))

set.seed(123)
fig_beswarm_all = plot(sep_rf, plot_type = "beeswarm") +
  theme(strip.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)))


ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Wine_Random_Forest_Beeswarm.png",
       plot = fig_beswarm,
       width = 14.2,
       height = 7,
       scale = 0.85,
       dpi = 350)


plot_SV_several_approaches(list(
  "N_S = 50" = explanation_list_org$res$n_combinations_50$repetition_1,
  "N_S = 100" = explanation_list_org$res$n_combinations_100$repetition_1,
  "N_S = 200" = explanation_list_org$res$n_combinations_200$repetition_1,
  "N_S = 500" = explanation_list_org$res$n_combinations_500$repetition_1,
  "N_S = 1000" = explanation_list_org$res$n_combinations_1000$repetition_1,
  "N_S = 2000" = explanation_list_org$res$n_combinations_2000$repetition_1,
  "All" = sep_rf
), index_explicands = index_explicands, digits = 2)

explanation_list$res$n_combinations_50

