



# Parameters ------------------------------------------------------------------------------------------------------
# The number of features
M = 10

# The correlation level
rhos = 0.0

# The number of training observations
n_train = 1000

# The number of test observations
n_test = 250

# Where the files are stored
folder = "/Users/larsolsen/PhD/Paper3/shapr"
folder_save = file.path(folder, "Paper3_rds_saves")
folder_save_figures = file.path(folder, "Paper3_result_figures")

# Set the working directory
setwd(folder)

#library(shapr)
#setwd("~/PhD/Paper3/Shapr_Lars_paper3/R")
pkgload::load_all()
source("~/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts/new_functions.R")

# The beta vector
betas = c(2, 1, 0.25, -3, -1, 1.5, -0.5, 0.75, 1.25, 1.5, -2)
betas = c(0, rep(1, M))
betas = betas[seq(M+1)]



# Load the results ------------------------------------------------------------------------------------------------
# Result list
repeated_explanations_list = list()
true_explanations_list = list()

# Iterate over the rhos
rho_idx = 1
for (rho_idx in seq_along(rhos)) {
  # Get the current rho
  rho = rhos[rho_idx]

  repeated_explanations_list[[paste0("rho_", rho)]] = list()

  # Make some of the save file names
  file_name = paste("Paper3_Experiment_M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "betas",
                    paste(as.character(betas), collapse = "_"), sep = "_")
  save_file_name_setup = file.path(folder_save, paste0(file_name, "_model.rds"))
  save_file_name_true = file.path(folder_save, paste0(file_name, "_true.rds"))

  files_in_dir = list.files(folder_save)
  relevant_files_in_dir = files_in_dir[grepl(paste0(file_name, "_estimated_repetition_"), files_in_dir)]
  relevant_repetitions = sort(as.integer(sapply(strsplit(unlist(strsplit(relevant_files_in_dir, '.rds')), '\\_'), tail, 1)))

  # Load the model
  setup = readRDS(save_file_name_setup)

  # Load the true
  true_explanations = readRDS(save_file_name_true)
  print(object.size(true_explanations), units = "MB")
  true_explanations$internal$output$dt_samp_for_vS = NULL
  print(object.size(true_explanations), units = "MB")
  true_explanations_list[[paste0("rho_", rho)]] = true_explanations

  # Iterate over the repetitions
  repetition_idx = 1
  for (repetition_idx in seq_along(relevant_repetitions)) {

    # Get the current repetition
    repetition = relevant_repetitions[repetition_idx]

    # Small printout to the user
    cat(sprintf("Working on rho = %g (%d of %d) and repetition = %d (%d of %d).\n",
                rho, rho_idx, length(rhos), repetition, repetition_idx, length(relevant_repetitions)))

    # Create the save file name
    save_file_name_rep = file.path(folder_save, paste0(file_name, "_estimated_repetition_", repetition, ".rds"))

    # Load the rds file
    current_repetition_results = readRDS(save_file_name_rep)

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
}



# Figures ---------------------------------------------------------------------------------------------------------

result_figures = aggregate_and_plot_results(repeated_explanations_list = repeated_explanations_list[[1]],
                                            true_explanations = true_explanations_list[[1]],
                                            evaluation_criterion = "MAE",
                                            scale_y_log10 = TRUE,
                                            plot_figures = FALSE,
                                            return_figures = TRUE,
                                            return_dt = TRUE)
saveRDS(result_figures$dt, file.path(folder_save, paste0(file_name, "_dt.rds")))
saveRDS(result_figures, file.path(folder_save, paste0(file_name, "_figures_dt.rds")))
result_figures$figures$figure_CI
result_figures$figures$figure_mean
result_figures$figures$figure_lines
result_figures$figures$figure_boxplot

# We see that it is better to use the paired version
aggregate_and_plot_results(repeated_explanations_list = repeated_explanations_list[[1]],
                           true_explanations = true_explanations_list[[1]],
                           evaluation_criterion = "MAE",
                           plot_figures = FALSE,
                           return_figures = TRUE,
                           return_dt = FALSE,
                           scale_y_log10 = TRUE,
                           dt_CI = result_figures$dt$dt_CI,
                           dt_long = result_figures$dt$dt_long,
                           only_these_sampling_methods = c("unique",
                                                           "non_unique",
                                                           "unique_paired"))$figure_CI

# We see that it is better to use the paired version
aggregate_and_plot_results(repeated_explanations_list = repeated_explanations_list[[1]],
                           true_explanations = true_explanations_list[[1]],
                           evaluation_criterion = "MAE",
                           plot_figures = FALSE,
                           return_figures = TRUE,
                           return_dt = FALSE,
                           scale_y_log10 = TRUE,
                           dt_CI = result_figures$dt$dt_CI,
                           dt_long = result_figures$dt$dt_long,
                           only_these_sampling_methods = c("unique",
                                                           "unique_SW",
                                                           "unique_paired",
                                                           "unique_paired_SW"))$figure_CI

# We see that adding the coalitions in increasing and decreasing order is a bad idea
aggregate_and_plot_results(repeated_explanations_list = repeated_explanations_list[[1]],
                           true_explanations = true_explanations_list[[1]],
                           evaluation_criterion = "MAE",
                           plot_figures = FALSE,
                           return_figures = TRUE,
                           return_dt = FALSE,
                           scale_y_log10 = TRUE,
                           dt_CI = result_figures$dt$dt_CI,
                           dt_long = result_figures$dt$dt_long,
                           only_these_sampling_methods = c("unique",
                                                           "unique_paired",
                                                           "chronological_order_increasing",
                                                           "chronological_order_decreasing"))$figure_CI

# Smarter to add the coalitions based on larger Shapley kernel weights
# Here we see that also adding those with small are better,
aggregate_and_plot_results(repeated_explanations_list = repeated_explanations_list[[1]],
                           true_explanations = true_explanations_list[[1]],
                           evaluation_criterion = "MAE",
                           plot_figures = FALSE,
                           return_figures = TRUE,
                           return_dt = FALSE,
                           scale_y_log10 = TRUE,
                           dt_CI = result_figures$dt$dt_CI,
                           dt_long = result_figures$dt$dt_long,
                           only_these_sampling_methods = c("unique",
                                                           "unique_paired",
                                                           "unique_paired_SW",
                                                           "largest_weights",
                                                           "smallest_weights"))$figure_CI

aggregate_and_plot_results(repeated_explanations_list = repeated_explanations_list[[1]],
                           true_explanations = true_explanations_list[[1]],
                           evaluation_criterion = "MAE",
                           plot_figures = FALSE,
                           return_figures = TRUE,
                           return_dt = FALSE,
                           scale_y_log10 = TRUE,
                           dt_CI = result_figures$dt$dt_CI,
                           dt_long = result_figures$dt$dt_long,
                           only_these_sampling_methods = c("unique",
                                                           "largest_weights",
                                                           "largest_weights_combination_size",
                                                           "smallest_weights",
                                                           "smallest_weights_combination_size"))$figure_CI

repeated_explanations_list$rho_0.5$repetition1$unique$repetition_1




# Load from disk --------------------------------------------------------------------------------------------------
rho = 0.0
file_name = paste("Paper3_Experiment_M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "betas",
                  paste(as.character(betas), collapse = "_"), sep = "_")
save_obj = readRDS(file.path(folder_save, paste0(file_name, "_dt.rds")))

save_obj = readRDS(file.path(folder_save, paste0(file_name, "_figures_dt.rds")))
save_obj = readRDS("/Users/larsolsen/PhD/Paper3/shapr/Paper3_rds_saves/Paper3_Experiment_M_10_n_train_1000_n_test_250_rho_0_betas_0_1_1_1_1_1_1_1_1_1_1_figures_dt.rds")
save_obj = save_obj$dt

tmp_fig = aggregate_and_plot_results(repeated_explanations_list = NULL,
                           true_explanations = NULL,
                           evaluation_criterion = "MAE",
                           plot_figures = FALSE,
                           return_figures = TRUE,
                           return_dt = FALSE,
                           scale_y_log10 = TRUE,
                           dt_CI = save_obj$dt_CI,
                           dt_long = save_obj$dt_long)$figure_CI +
  ggplot2::ggtitle(bquote("Resuts for:"~M==.(M)*", "~rho==.(rho)*", and"~beta=="["*.(paste(betas, collapse = ", "))*"]."))
ggplot2::ggsave(file.path(folder_save_figures, paste0(file_name, "_all_methods.png")),
                width = 10,
                height = 5)

tmp_fig = aggregate_and_plot_results(repeated_explanations_list = NULL,
                                     true_explanations = NULL,
                                     evaluation_criterion = "MAE",
                                     plot_figures = FALSE,
                                     return_figures = TRUE,
                                     return_dt = FALSE,
                                     scale_y_log10 = TRUE,
                                     dt_CI = save_obj$dt_CI,
                                     dt_long = save_obj$dt_long,
                                     only_these_sampling_methods = c("unique",
                                                                     "non_unique",
                                                                     "unique_paired"))$figure_CI +
  ggplot2::ggtitle(bquote("Resuts for:"~M==.(M)*", "~rho==.(rho)*", and"~beta=="["*.(paste(betas, collapse = ", "))*"]."))
ggplot2::ggsave(file.path(folder_save_figures, paste0(file_name, "_old.png")),
                width = 10,
                height = 5)

tmp_fig = aggregate_and_plot_results(repeated_explanations_list = NULL,
                                     true_explanations = NULL,
                                     evaluation_criterion = "MAE",
                                     plot_figures = FALSE,
                                     return_figures = TRUE,
                                     return_dt = FALSE,
                                     scale_y_log10 = TRUE,
                                     dt_CI = save_obj$dt_CI,
                                     dt_long = save_obj$dt_long,
                                     only_these_sampling_methods = c("unique",
                                                                     "unique_SW",
                                                                     "unique_paired",
                                                                     "unique_paired_SW"))$figure_CI +
  ggplot2::ggtitle(bquote("Resuts for:"~M==.(M)*", "~rho==.(rho)*", and"~beta=="["*.(paste(betas, collapse = ", "))*"]."))
ggplot2::ggsave(file.path(folder_save_figures, paste0(file_name, "_old_sw_or_sampling_frequency.png")),
                width = 10,
                height = 5)

tmp_fig = aggregate_and_plot_results(repeated_explanations_list = NULL,
                                     true_explanations = NULL,
                                     evaluation_criterion = "MAE",
                                     plot_figures = FALSE,
                                     return_figures = TRUE,
                                     return_dt = FALSE,
                                     scale_y_log10 = TRUE,
                                     dt_CI = save_obj$dt_CI,
                                     dt_long = save_obj$dt_long,
                                     only_these_sampling_methods = c("unique",
                                                                     "unique_paired",
                                                                     "chronological_order_increasing",
                                                                     "chronological_order_decreasing"))$figure_CI +
  ggplot2::ggtitle(bquote("Resuts for:"~M==.(M)*", "~rho==.(rho)*", and"~beta=="["*.(paste(betas, collapse = ", "))*"]."))
ggplot2::ggsave(file.path(folder_save_figures, paste0(file_name, "_chronological_order.png")),
                width = 10,
                height = 5)

tmp_fig = aggregate_and_plot_results(repeated_explanations_list = NULL,
                                     true_explanations = NULL,
                                     evaluation_criterion = "MAE",
                                     plot_figures = FALSE,
                                     return_figures = TRUE,
                                     return_dt = FALSE,
                                     scale_y_log10 = TRUE,
                                     dt_CI = save_obj$dt_CI,
                                     dt_long = save_obj$dt_long,
                                     only_these_sampling_methods = c("unique",
                                                                     "unique_paired",
                                                                     "unique_paired_SW",
                                                                     "largest_weights",
                                                                     "smallest_weights"))$figure_CI +
  ggplot2::ggtitle(bquote("Resuts for:"~M==.(M)*", "~rho==.(rho)*", and"~beta=="["*.(paste(betas, collapse = ", "))*"]."))
ggplot2::ggsave(file.path(folder_save_figures, paste0(file_name, "_largest_smallest.png")),
                width = 10,
                height = 5)


tmp_fig = aggregate_and_plot_results(repeated_explanations_list = NULL,
                                     true_explanations = NULL,
                                     evaluation_criterion = "MAE",
                                     plot_figures = FALSE,
                                     return_figures = TRUE,
                                     return_dt = FALSE,
                                     scale_y_log10 = TRUE,
                                     dt_CI = save_obj$dt_CI,
                                     dt_long = save_obj$dt_long,
                                     only_these_sampling_methods = c("unique",
                                                                     "unique_paired",
                                                                     "largest_weights",
                                                                     "largest_weights_combination_size",
                                                                     "smallest_weights",
                                                                     "smallest_weights_combination_size"))$figure_CI +
  ggplot2::ggtitle(bquote("Resuts for:"~M==.(M)*", "~rho==.(rho)*", and"~beta=="["*.(paste(betas, collapse = ", "))*"]."))
ggplot2::ggsave(file.path(folder_save_figures, paste0(file_name, "_largest_smallest_combination_size.png")),
                width = 10,
                height = 5)


tmp_fig = aggregate_and_plot_results(repeated_explanations_list = NULL,
                                     true_explanations = NULL,
                                     evaluation_criterion = "MAE",
                                     plot_figures = FALSE,
                                     return_figures = TRUE,
                                     return_dt = FALSE,
                                     scale_y_log10 = TRUE,
                                     dt_CI = save_obj$dt_CI,
                                     dt_long = save_obj$dt_long,
                                     only_these_sampling_methods = c("unique",
                                                                     "unique_paired",
                                                                     "unique_paired_SW",
                                                                     "largest_weights"))$figure_CI +
  ggplot2::ggtitle(bquote("Resuts for:"~M==.(M)*", "~rho==.(rho)*", and"~beta=="["*.(paste(betas, collapse = ", "))*"]."))
ggplot2::ggsave(file.path(folder_save_figures, paste0(file_name, "_most_promising.png")),
                width = 10,
                height = 5)





