# FILE TO RUN THE WINE DATA
library(ranger)
library(data.table)
devtools::load_all(".")

relative_difference = function(dt, m, strat_ref,
                               strat_other = NULL,
                               y_breaks = waiver(),
                               y_limits = NULL,
                               scale = TRUE,
                               legend_n_row = 2,
                               include_coal_size_lines = FALSE,
                               hue_indices = NULL,
                               hue_length = NULL,
                               y_lab_frac = TRUE) {
  if (xor(is.null(hue_indices), is.null(hue_length))) stop("Both `hue_indices` and `hue_length` must be provided.")
  if (!is.null(hue_indices) && !is.null(hue_length)) {
    hues = seq(15, 375, length = hue_length + 1)
    colors = grDevices::hcl(h = hues, l = 65, c = 100)[1:hue_length][hue_indices]
  }

  library(latex2exp)
  library(ggallin)
  library(data.table)

  # Get all the names from the data table
  if (is.null(strat_other)) strat_other = levels(dt$sampling)

  # Extract the needed columns
  dt = dt[, c("rho", "sampling", "n_combinations", "mean")]

  # Only even n_combinations
  dt = dt[n_combinations %% 2 == 0]

  # Only get the wanted strategies
  dt = dt[sampling %in% strat_other,]

  # Compute the relative error for each rho and n_combination
  dt[, rel_error := (mean - mean[sampling == strat_ref]) / mean[sampling == strat_ref],
     by = list(rho, n_combinations)]

  # Convert sampling to a ordered factor
  dt = dt[, sampling := factor(sampling, levels = strat_other, ordered = TRUE)]

  #
  n_features <- seq(ceiling((m - 1)/2))
  n <- sapply(n_features, choose, n = m)
  n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
  n_cumsum = (cumsum(n) + 2) + 0.5

  # Make the plot
  fig = ggplot(dt, aes(x = n_combinations, y = rel_error, col = sampling)) +
    {if (include_coal_size_lines) geom_vline(xintercept = n_cumsum, col = "gray50", linetype = "dashed", linewidth = 0.4)} +
    geom_hline(yintercept = 0, col = "gray") +
    geom_line(linewidth = 0.65) +
    facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
    {if (scale) scale_y_log10(trans = ggallin:::ssqrt_trans, breaks = y_breaks)} +
    {if (!scale) scale_y_continuous(breaks = y_breaks)} +
    coord_cartesian(ylim = y_limits) +
    scale_x_continuous(labels = scales::label_number()) +
    # scale_y_continuous(limits = c(-1, 2.5)) +
    theme(legend.position = 'bottom') +
    guides(col = guide_legend(nrow = legend_n_row, theme = theme(legend.byrow = FALSE)),
           fill = guide_legend(nrow = legend_n_row, theme = theme(legend.byrow = FALSE)),
           linetype = "none") +
    labs(color = "Strategy:",
         fill = "Strategy:",
         # y = "Relative difference",
         x = expression(N[S])) +
    { if (y_lab_frac) {
      labs(y = latex2exp::TeX(r'($\frac{MAE_{Strategy} - MAE_{Paired~C-Kernel}}{MAE_{Paired~C-Kernel}}$)'))
    } else {
      labs(y = latex2exp::TeX(r'($(MAE_{Strategy} - MAE_{Paired~C-Kernel}) / MAE_{Paired~C-Kernel}$)'))
    }} +
    theme(strip.text = element_text(size = rel(1.6)),
          legend.title = element_text(size = rel(1.37)),
          legend.text = element_text(size = rel(1.37)),
          axis.title = element_text(size = rel(1.6)),
          axis.text = element_text(size = rel(1.5))) +
    {if (!is.null(hue_length)) scale_color_manual(values = colors)} +
    {if (is.null(hue_length))  scale_color_hue()} #added as we want ordered}


  return(fig)
}


relative_difference_wine = function(dt, m, strat_ref,
                               strat_other = NULL,
                               y_breaks = waiver(),
                               y_limits = NULL,
                               scale = TRUE,
                               legend_n_row = 2,
                               include_coal_size_lines = FALSE,
                               hue_indices = NULL,
                               hue_length = NULL,
                               y_lab_frac = TRUE) {
  if (xor(is.null(hue_indices), is.null(hue_length))) stop("Both `hue_indices` and `hue_length` must be provided.")
  if (!is.null(hue_indices) && !is.null(hue_length)) {
    hues = seq(15, 375, length = hue_length + 1)
    colors = grDevices::hcl(h = hues, l = 65, c = 100)[1:hue_length][hue_indices]
  }

  library(latex2exp)
  library(ggallin)
  library(data.table)

  # Get all the names from the data table
  if (is.null(strat_other)) strat_other = levels(dt$sampling)

  # Extract the needed columns
  dt = dt[, c("Strategy", "n_combinations", "avg_MAE")]

  # Only even n_combinations
  dt = dt[n_combinations %% 2 == 0]

  # Only get the wanted strategies
  dt = dt[Strategy %in% strat_other,]

  # Compute the relative error for each rho and n_combination
  dt[, rel_error := (avg_MAE - avg_MAE[Strategy == strat_ref]) / avg_MAE[Strategy == strat_ref],
     by = list(n_combinations)]

  # Convert sampling to a ordered factor
  dt = dt[, Strategy := factor(Strategy, levels = strat_other, ordered = TRUE)]

  #
  n_features <- seq(ceiling((m - 1)/2))
  n <- sapply(n_features, choose, n = m)
  n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
  n_cumsum = (cumsum(n) + 2) + 0.5

  # Make the plot
  fig = ggplot(dt, aes(x = n_combinations, y = rel_error, col = Strategy)) +
    {if (include_coal_size_lines) geom_vline(xintercept = n_cumsum, col = "gray50", linetype = "dashed", linewidth = 0.4)} +
    geom_hline(yintercept = 0, col = "gray") +
    geom_line(linewidth = 0.65) +
    {if (scale) scale_y_log10(trans = ggallin:::ssqrt_trans, breaks = y_breaks)} +
    {if (!scale) scale_y_continuous(breaks = y_breaks)} +
    coord_cartesian(ylim = y_limits) +
    scale_x_continuous(labels = scales::label_number()) +
    # scale_y_continuous(limits = c(-1, 2.5)) +
    theme(legend.position = 'bottom') +
    guides(col = guide_legend(nrow = legend_n_row, theme = theme(legend.byrow = FALSE)),
           fill = guide_legend(nrow = legend_n_row, theme = theme(legend.byrow = FALSE)),
           linetype = "none") +
    labs(color = "Strategy:",
         fill = "Strategy:",
         # y = "Relative difference",
         x = expression(N[S])) +
    { if (y_lab_frac) {
      labs(y = latex2exp::TeX(r'($\frac{MAE_{Strategy} - MAE_{Paired~C-Kernel}}{MAE_{Paired~C-Kernel}}$)'))
    } else {
      labs(y = latex2exp::TeX(r'($(MAE_{Strategy} - MAE_{Paired~C-Kernel}) / MAE_{Paired~C-Kernel}$)'))
    }} +
    theme(strip.text = element_text(size = rel(1.6)),
          legend.title = element_text(size = rel(1.37)),
          legend.text = element_text(size = rel(1.37)),
          axis.title.y = element_text(size = rel(1.25)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.65)),
          axis.text.y = element_text(size = rel(1.65))) +
    {if (!is.null(hue_length)) scale_color_manual(values = colors)} +
    {if (is.null(hue_length))  scale_color_hue()} #added as we want ordered}


  return(fig)

  # The relative difference on signed square root scale.
}


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
                                      "single_median_ranking_over_each_test_obs",
                                      "MAD")
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





args = commandArgs(trailingOnly = TRUE)
# Extract which repetition we are to do
samp_app = as.character(args[1])
if (!(samp_app %in% c("NULL", "NA", "NaN"))) {
  if (grepl(",", samp_app)) {
    samp_app = as.numeric(unlist(strsplit(samp_app, ",")))
  } else {
    samp_app = unlist(strsplit(samp_app, ":"))
    if (length(samp_app) > 1) {
      samp_app = seq(as.numeric(samp_app[1]), as.numeric(samp_app[2]))
    } else {
      samp_app = as.numeric(samp_app)
    }
  }
} else {
  samp_app = NULL
}

n_combinations_vec = c(2, 4, 6, 8, 10, 12, 14, 16)

n_combinations_vec = c(1600, 1700, 1800, 1900)
n_combinations_vec  = c(2, 10, 100, 500)
n_combinations_vec = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 200, 300, 400,
                       430, 440, 450, 460, 470, 480, 490, 500, 600, 700, 800, 900, 1000, 1070, 1100, 1110, 1120, 1130, 1140, 1200,
                       1250, 1500, 1750, 1800, 1900, 2000, 2010, 2020, 2030, 2040, 2044)

m = 11
n_features <- seq(ceiling((m - 1)/2))
n <- sapply(n_features, choose, n = m)
n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]


n_cumsum = (cumsum(n) + 2)
n_cumsum = n_cumsum[-length(n_cumsum)]
n_combinations_vec = unique(sort(c(n_combinations_vec, n_cumsum)))



B = 100

res_dt = data.table(Strategy = character(), n_combinations = integer(), repetition = integer(), MAE = numeric())
res = list()

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
                     "largest_weights_combination_size",
                     "largest_weights_new_weights_empirical",
                     "largest_weights_combination_size_new_weights_empirical")
if (!is.null(samp_app)) sampling_methods = sampling_methods[samp_app]




sampling_method = sampling_methods[1]
for (sampling_method in sampling_methods) {
  sampling_method_full_name = sampling_method
  if (!is.list(res[[sampling_method]])) res[[sampling_method]] = list()
  n_combinations = n_combinations_vec[1]
  for (n_combinations in n_combinations_vec) {
    if (!is.list(res[[sampling_method]][[paste0("n_combinations_", n_combinations)]])) res[[sampling_method]][[paste0("n_combinations_", n_combinations)]] = list()
    seed = 1
    for (seed in seq(B)) {
      if (seed %% 10 == 0) cat(paste0("Strategy = ", sampling_method, ", n_combinations = ", n_combinations, ", seed = ", seed, ".\n"))

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
                                                           as.matrix(explanation_now$shapley_values),
                                                           evaluation_criterion = "MAE")))

      print(sampling_method_full_name)

      # DELTE THE BIGGEST UNNECESSARY OBJECTS
      explanation_now$internal$data$x_train = NULL
      explanation_now$internal$data$x_explain = NULL
      explanation_now$internal$parameters$precomputed_vS = NULL
      explanation_now$internal$parameters$dt_new_weights = NULL

      res[[sampling_method_full_name]][[paste0("n_combinations_", n_combinations)]][[paste0("repetition_", seed)]] =
        explanation_now
    }
  }
  message("Start to save")
  saveRDS(res_dt[Strategy == sampling_method_full_name,],
          file.path(path_source, "PhD/Paper3/Paper3_save_location", paste0("NEW_Wine_data_res_dt_only_", sampling_method_full_name, ".rds")))
  saveRDS(list(res_dt = res_dt, res = res[[sampling_method_full_name]]),
          file.path(path_source, "PhD/Paper3/Paper3_save_location", paste0("NEW_Wine_data_res_", sampling_method_full_name, ".rds")))
  message("Done saving")
}
saveRDS(res_dt, file.path(path_source, "PhD/Paper3/Paper3_save_location", paste0("NEW_Wine_data_res_only_res_dt", ".rds")))








# Plots -----------------------------------------------------------------------------------------------------------
if (FALSE) {
  library(data.table)
  library(ggplot2)


  folder_name = "/Users/larsolsen/PhD/Paper3/Paper3_save_location"

  files <- list.files(path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location", pattern = "NEW_Wine_data_res_dt_only", full.names = TRUE)

  res_dt = rbindlist(lapply(files, function(x) {
    file_now = readRDS(x)
    print(unique(file_now$Strategy))
    return(file_now)
    }))
  res_dt_v2 = copy(res_dt)
  res_dt_v2[, .N, by = list(Strategy)]
  res_dt_v2[Strategy == "on_all_cond_paired_largest_weights_random_mean_L" & n_combinations == 4 & repetition == 1,]
  res_dt_v2[Strategy == "on_all_cond_paired_largest_weights_random_analytical" & n_combinations == 10 & repetition == 1,]
  res_dt_v2[, avg_MAE := mean(MAE), by = list(Strategy, n_combinations)]
  res_dt_v2[, c("lower", "median", "upper") := as.list(quantile(MAE, c(0.025, 0.5, 0.975))), by = .(Strategy, n_combinations)]

  res_dt_v2[Strategy == "largest_weights_random",]

  Paired Largest Kernel
  res_dt_v2 = res_dt_v2[, list("avg_MAE" = mean(MAE), "lower" = quantile(MAE, 0.025), "median" = quantile(MAE, 0.5), "upper" = quantile(MAE, 0.975)), by = list(Strategy, n_combinations)]

  res_dt_v2[, .N, by = list(Strategy)]


  res_dt_v2[, Strategy := factor(Strategy,
                                 levels = dt_strategy_names$Original,
                                 labels = dt_strategy_names$New,
                                 ordered = FALSE)]
  res_dt_v2[, .N, by = list(Strategy)]

  m = 11
  n_features <- seq(ceiling((m - 1)/2))
  n <- sapply(n_features, choose, n = m)
  n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
  n_cumsum = (cumsum(n) + 2) + 0.5


  library(ggplot2)
  fig_wine = ggplot(data = res_dt_v2, aes(x = n_combinations, y = avg_MAE, col = Strategy, fill = Strategy)) +
    geom_vline(xintercept = n_cumsum, col = "gray50", linetype = "dashed", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, linewidth = 0.1) +
    geom_line(linewidth = 1) +
    scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    theme(legend.position = 'bottom') +
    guides(col = guide_legend(nrow = 5)) +
    labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote(bar(MAE)*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.4)))

  fig_wine
  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Wine_Random_Forest_MAE_V7.png",
         plot = fig_wine,
         width = 14.2,
         height = 7,
         scale = 0.85,
         dpi = 350)


  samps = c("Unique",
            "Paired",
            "Paired Average",
            "Paired Kernel",
            "Paired C-Kernel",
            "Paired CEL-Kernel",
            "Paired Imp C-Kernel",
            "Paired Imp CEL-Kernel")

  dt_all2 = res_dt_v2[Strategy %in% samps & n_combinations %% 2 == 0, ]
  dt_all2 = dt_all2[, Strategy := factor(Strategy, levels = samps, ordered = TRUE)]
  fig_wine_2 = ggplot(data = dt_all2, aes(x = n_combinations, y = avg_MAE, col = Strategy, fill = Strategy)) +
    #theme_light() +
    geom_vline(xintercept = n_cumsum, col = "gray50", linetype = "dashed", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, linewidth = 0) +
    geom_line(linewidth = 0.65) +
    scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    coord_cartesian(ylim = c(10^(-4.15), 10^(-0.75))) +
    scale_x_continuous(labels = scales::label_number()) +
    # scale_x_log10(
    #   breaks = c(10, 20, 50, 100, 200, 500, 1000, 2000)
    #   #breaks = scales::trans_breaks("log10", function(x) 10^x),
    #   #labels = scales::label_number()
    # ) +
    guides(col = guide_legend(nrow = 2)) +
    labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote(bar(MAE)*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
    # theme(strip.text = element_text(size = rel(1.5)),
    #       legend.title = element_text(size = rel(1.39)),
    #       legend.text = element_text(size = rel(1.39)),
    #       axis.title = element_text(size = rel(1.5)),
    #       axis.text = element_text(size = rel(1.4))) +
    theme(strip.text = element_text(size = rel(1.6)),
          legend.title = element_text(size = rel(1.35)),
          legend.text = element_text(size = rel(1.35)),
          axis.title.y = element_text(size = rel(1.6)),
          axis.title.x = element_text(size = rel(1.35)),
          axis.text = element_text(size = rel(1.3)),
          legend.position = 'bottom',
          legend.justification = 0.9) +
    scale_color_hue() + #added as we want ordered
    scale_fill_hue()
  fig_wine_2

  fig_wine_4 = gridExtra::grid.arrange(fig_wine_2, fig_wine_3, ncol = 2, widths = c(8,4))


  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Wine_Random_Forest_MAE_and_Hist_V11.png",
         plot = fig_wine_4,
         width = 14.2,
         height = 7,
         scale = 0.85,
         dpi = 350)

  ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Wine_Random_Forest_MAE_Appendix_Pilot.png"),
         plot = fig_wine_2,
         width = 14.2,
         height = 9.98,
         scale = 0.85,
         dpi = 350)



  # Relative MAE ----------------------------------------------------------------------------------------------------
  fig_rel_dif_wine = relative_difference_wine(dt = res_dt_v2,
                           m = 11,
                           strat_ref = "Paired C-Kernel",
                           strat_other = c("Unique",
                                           "Paired",
                                           "Paired Average",
                                           "Paired Kernel",
                                           "Paired C-Kernel",
                                           "Paired CEL-Kernel",
                                           "Paired Imp C-Kernel",
                                           "Paired Imp CEL-Kernel"),
                           y_breaks = c(-1, -0.4, -0.1, 0, 0.1, 0.4, 1, 2, 4),
                           y_limits = c(-1, 5.1),
                           scale = TRUE,
                           legend_n_row = 1,
                           include_coal_size_lines = TRUE
                           #hue_length = 8,
                           #hue_indices = c(2,3,5,6)
  )

  library("ggpubr")

  fig_comb_wine_1 = ggpubr::ggarrange(fig_wine_2, fig_rel_dif_wine,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2,
                    align = "hv",
                    hjust = -0.5,
                    vjust = 1.2,
                    common.legend = TRUE, legend = "bottom",
                    font.label = list(size = 25, color = "black"))

  fig_comb_wine_2 = ggpubr::ggarrange(fig_comb_wine_1, fig_wine_3,
                    labels = c("A", "C"),
                    ncol = 2, nrow = 1,
                    widths = c(10,4),
                    hjust = -0.5,
                    vjust = 1.2,
                    common.legend = TRUE, legend = "bottom",
                    font.label = list(size = 25, color = "black"))

  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Wine_Random_Forest_All_Figs_V5.png",
         plot = fig_comb_wine_2,
         width = 14.2,
         height = 10,
         scale = 0.85,
         dpi = 350)



  relative_difference_wine(dt = res_dt_v2,
                      m = 11,
                      strat_ref = "Paired C-Kernel",
                      strat_other = c("Paired",
                                      "Paired Average",
                                      "Paired C-Kernel",
                                      "Paired CEL-Kernel"),
                      y_limits = c(-0.06, 0.15),
                      scale = FALSE,
                      legend_n_row = 1
                      #hue_length = 8,
                      #hue_indices = c(2,3,5,6)
                      )





  fig_wine_3 = ggplot(data = data.table(pred = sep_rf$pred_explain), aes(x = pred)) +
    #theme_light() +
    geom_histogram(color="black", fill = "grey") +
    geom_vline(aes(xintercept = p0), color = "black", linewidth = 1, linetype = "dashed") +
    annotate('text', x = 6.5, y = 12.72, #x = 6.15, y = 12.72,
             label = "phi[0]==5.65",
             parse = TRUE,
             size = 8,
             color = "black") +
    labs(color = "Strategy:", fill = "Strategy:", x = expression(f(bold(x)*"*")), y = "Count") +
    theme(strip.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.4)),
          axis.text = element_text(size = rel(1.4)))



  fig_wine_4 = gridExtra::grid.arrange(fig_wine_2, fig_wine_3, ncol = 2, widths = c(8,4))
  fig_wine_4

  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Wine_Random_Forest_MAE_and_Hist_V11.png",
         plot = fig_wine_4,
         width = 14.2,
         height = 7,
         scale = 0.85,
         dpi = 350)

  # Rscript Paper3_real_world_data_experiment_UIO.R 1
  # Rscript Paper3_real_world_data_experiment_UIO.R 2
  # Rscript Paper3_real_world_data_experiment_UIO.R 3
  # Rscript Paper3_real_world_data_experiment_UIO.R 4
  # Rscript Paper3_real_world_data_experiment_UIO.R 5
  # Rscript Paper3_real_world_data_experiment_UIO.R 6
  # Rscript Paper3_real_world_data_experiment_UIO.R 7
  # Rscript Paper3_real_world_data_experiment_UIO.R 8
  # Rscript Paper3_real_world_data_experiment_UIO.R 9
  # Rscript Paper3_real_world_data_experiment_UIO.R 10
  # Rscript Paper3_real_world_data_experiment_UIO.R 11
  # Rscript Paper3_real_world_data_experiment_UIO.R 12
  # Rscript Paper3_real_world_data_experiment_UIO.R 13
  # Rscript Paper3_real_world_data_experiment_UIO.R 14
  # Rscript Paper3_real_world_data_experiment_UIO.R 15
  # Rscript Paper3_real_world_data_experiment_UIO.R 16
  # Rscript Paper3_real_world_data_experiment_UIO.R 17
  # Rscript Paper3_real_world_data_experiment_UIO.R 18
  # Rscript Paper3_real_world_data_experiment_UIO.R 19
  # Rscript Paper3_real_world_data_experiment_UIO.R 20

  # module load R/4.2.1-foss-2022a
  # cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts



}
