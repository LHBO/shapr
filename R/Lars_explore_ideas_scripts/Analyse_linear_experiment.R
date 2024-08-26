# 2024 runs -------------------------------------------------------------------------------------------------------

# tmux new -s paper3
# module load R/4.2.1-foss-2022a
# cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R
# git checkout Lars/paper3_ideas
# git pull
# cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts

# Rscript Run_linear_experiment.R TRUE TRUE TRUE 1:10 10 1000000 1000000 1000 1000 8 0.0,0.5,0.9 1,1,1,1,1,1,1,1,1
# Rscript Analyse_linear_experiment.R TRUE FALSE 8 0.0,0.5,0.9 1000 1000 1,1,1,1,1,1,1,1,1 MAE

# Rscript Run_linear_experiment.R TRUE TRUE TRUE 1:10 8 1000000 1000000 1000 1000 8 0.0,0.5,0.9 NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 8 0.0,0.5,0.9 1000 1000 NULL MAE

# Rscript Analyse_linear_experiment.R TRUE FALSE 12 0.0,0.3,0.6,0.9 1000 500 NULL MAE TRUE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 12 0.6 1000 500 NULL MAE TRUE NULL NULL


# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.91 1000 1000 NULL MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.91 1000 1000 NULL MAE TRUE regression_surrogate NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.91 1000 1000 NULL MAE TRUE regression_separate NULL

#Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.91 1000 1000 1,1,1,1,1,1,1,1,1,1,1 MAE TRUE regression_separate NULL

#Rscript Run_linear_experiment.R TRUE TRUE TRUE TRUE 1:10 6 1000000 1000000 1000 1000 10 0.91 1,1,1,1,1,1,1,1,1,1,1 regression_separate NULL

# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.0 TRUE 1000 1000 1,1,1,1,1,1,1,1,1,1,1 MAE TRUE regression_separate NULL

# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.02,0.12,0.32,0.62,0.92,0.992 TRUE 1000 1000 NULL MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.03,0.13 TRUE 1000 1000 NULL MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.33,0.63 TRUE 1000 1000 NULL MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.93,0.993 TRUE 1000 1000 NULL MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.0 TRUE 1000 1000 NULL MAE TRUE regression_surrogate NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.0 TRUE 1000 1000 NULL MAE TRUE regression_separate NULL

# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.0 TRUE 1000 1000 1,1,1,1,1,1,1,1,1,1,1 MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.0 TRUE 1000 1000 1,1,1,1,1,1,1,1,1,1,1 MAE TRUE regression_surrogate NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.0 TRUE 1000 1000 1,1,1,1,1,1,1,1,1,1,1 MAE TRUE regression_separate NULL

# Rscript Analyse_linear_experiment.R TRUE FALSE 12 0.0,0.3,0.6,0.9 TRUE 1000 1000 NULL MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 12 0.0,0.3,0.6,0.9 TRUE 1000 1000 NULL MAE TRUE regression_surrogate NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 12 0.0,0.3,0.6,0.9 TRUE 1000 1000 NULL MAE TRUE regression_separate NULL

# Rscript Analyse_linear_experiment.R TRUE FALSE 12 0.0,0.3,0.6,0.9 TRUE 1000 1000 1,1,1,1,1,1,1,1,1,1,1,1,1 MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 12 0.0,0.3,0.6,0.9 TRUE 1000 1000 1,1,1,1,1,1,1,1,1,1,1,1,1 MAE TRUE regression_surrogate NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 12 0.0,0.3,0.6,0.9 TRUE 1000 1000 1,1,1,1,1,1,1,1,1,1,1,1,1 MAE TRUE regression_separate NULL

# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.04,0.14,0.24,0.34 TRUE 1000 1000 NULL MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0.64,0.74,0.84,0.94 TRUE 1000 1000 NULL MAE FALSE NULL NULL

# Rscript Analyse_linear_experiment.R TRUE FALSE 6 0.01,0.11,0.21,0.31,0.41,0.51,0.61,0.71,0.81,0.91 TRUE 1000 1000 NULL MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 12 0.01,0.11,0.31,0.41,0.71,0.81,0.91 TRUE 1000 1000 NULL MAE FALSE NULL NULL

# Rscript Analyse_linear_experiment.R TRUE FALSE 10 0,0.05,0.1,0.2,0.5,0.7,0.9 FALSE 1000 1000 NULL MAE FALSE NULL NULL
#
# Rscript Analyse_linear_experiment.R TRUE FALSE 9 0.5 FALSE 1000 1000 NULL MAE FALSE NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 9 0.5 FALSE NULL NULL NULL


# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 14 0,0.2,0.5,0.9 FALSE NULL NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 14 0,0.2,0.5,0.9 FALSE 1000 1000 NULL MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 17 0,0.2,0.5,0.9 FALSE 1000 500 NULL MAE FALSE NULL NULL

# Rscript Analyse_linear_experiment.R TRUE FALSE 17 0.2 FALSE 1000 500 NULL MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 17 0.5 FALSE 1000 500 NULL MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 17 0.9 FALSE 1000 500 NULL MAE FALSE NULL NULL
# Rscript Analyse_linear_experiment.R TRUE FALSE 17 0 FALSE 1000 500 NULL MAE FALSE NULL NULL

# Input From Command Line -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
args = commandArgs(trailingOnly = TRUE)
# test if there is at least one argument: if not, return an error
if (length(args) < 12) {
  stop("Must provide all parameters (do_dt, do_figures, M, rhos, n_train, n_test, beta, eval_crit)!", call.=FALSE)
}

do_dt = TRUE
do_figures = FALSE
M = 5
rhos = c(0.7)
n_train = 1000
n_test = 500
betas = c(2, 10, 0.25, -3, -1, 1.5, -0.5)[seq(M+1)]
evaluation_criterion = "MAE"
use_pilot_estimates_regression = TRUE
pilot_approach_regression = "regression_surrogate"
pilot_regression_model = "parsnip::linear_reg()"


# Extract if we are to compute the data.tables containing the aggregated results
do_dt = as.logical(args[1])

# Extract if we are to create the figures
compute_true_explanations = as.logical(args[2])

# Extract the number of features
M = as.integer(args[3])

# Extract the correlation level
rhos = unlist(strsplit(args[4], ","))
if (length(rhos) > 1) {
  rhos = unname(sapply(rhos, function(i) as.numeric(i)))
} else {
  rhos = as.numeric(rhos)
}

# If we use quie correlation matrix
rho_equi = as.logical(args[5])

# Extract the number of training observations
n_train = as.integer(args[6])

# Extract the number of test observations
n_test = as.integer(args[7])

# Extract the correlation level
betas = as.character(args[8])
if (betas != "NULL") {
  betas = unlist(strsplit(betas, ","))
  if (length(betas) > 1) {
    betas = unname(sapply(betas, function(i) as.numeric(i)))
  } else {
    betas = as.numeric(betas)
  }
} else {
  # Create the beta vector
  betas = c(2, 1, 0.25, -3, -1, 1.5, -0.5, 0.75, 1.25, 1.5, -2, 3, -1)
  betas = c(0, rep(1, M))
  betas = c(2, 10, 0.25, -3, -1, 1.5, -0.5, 10, 1.25, 1.5, -2, 3, -1)
  betas = c(2, 10, 0.25, -3, -1, 1.5, -0.5, 10, 1.25, 1.5, -2, 3, -1, -5, 4, -10, 2, 5, -0.5, -1, -2)
  betas = betas[seq(M+1)]
}

# Extract the evaluation criterion
evaluation_criterion = as.character(args[9])

# If we are to use pilot estimates or not
use_pilot_estimates_regression = as.logical(args[10])

# Extract the kind of regression model
pilot_approach_regression = as.character(args[11])
pilot_regression_model = as.character(args[12])
if (pilot_approach_regression %in% c("NULL", "NA", "NaN")) pilot_approach_regression = "regression_surrogate"
if (pilot_regression_model %in% c("NULL", "NA", "NaN")) pilot_regression_model = "parsnip::linear_reg()"



# Small printout to the user
message(paste0(
  "Set up:",
  "\ndo_dt = ", do_dt,
  "\ndo_figures = ", do_figures,
  "\nM = ", M,
  "\nrho = [", paste(rhos, collapse = ", "), "]",
  "\nrho_equi = ", rho_equi,
  "\nn_train = ", n_train,
  "\nn_test = ", n_test,
  "\nbeta = [", paste(betas, collapse = ", "), "]",
  "\nevaluation_criterion = ", evaluation_criterion,
  "\nuse_pilot_estimates_regression = ", use_pilot_estimates_regression,
  "\npilot_approach_regression = ", pilot_approach_regression,
  "\npilot_regression_model = ", pilot_regression_model, "\n"))



# Find computer location ------------------------------------------------------------------------------------------
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

print(folder_save)

# Set the working directory
setwd(folder)

# Load the new functions
source(file.path(folder, "R/Lars_explore_ideas_scripts/new_functions.R"))

# devtools::load_all(".")
if (Sys.info()[[4]] %in% c("nam-shub-01.uio.no", "nam-shub-02.uio.no") || R.utils::System$getHostname() %in% c("nam-shub-01.uio.no", "nam-shub-02.uio.no") ) {
  print("Hei1")

  devtools::clean_dll()
  # devtools::install_github(repo = "LHBO/shapr", ref = "Lars/paper3_ideas")
  devtools::install_github(repo = "LHBO/shapr", ref = "Lars/paper3_ideas")
  #devtools::load_all(".")
  library(shapr)
} else {
  print("Hei2")
  devtools::load_all(".")
}


# devtools::install_github(repo = "LHBO/shapr", ref = "Lars/paper3_ideas") # Only need
# library(shapr) # This must be the shapr version in the github repository.
# devtools::load_all(".")

# Create data.tables ----------------------------------------------------------------------------------------------
if (do_dt) {
  aggregated_results = combine_explanation_results(
    M = M,
    rhos = rhos,
    rho_equi = rho_equi,
    n_train = n_train,
    n_test = n_test,
    betas = betas,
    folder_save = folder_save,
    evaluation_criterion = evaluation_criterion,
    use_pilot_estimates_regression = use_pilot_estimates_regression,
    pilot_approach_regression = pilot_approach_regression,
    pilot_regression_model = pilot_regression_model,
    memory_efficient = TRUE,
    save_results = TRUE,
    level = 0.95,
    n_workers = 1,
    objects_to_return = "aggregated_results",
    name_prefix = "",
    resave = FALSE
    )
}


# Create figures --------------------------------------------------------------------------------------------------
if (do_figures) {
  # Get a list over the save file names containing the data tables
  if (!do_dt) {
    save_files = list()
    for (rho_idx in seq_along(rhos)) {
      file_name =
        paste0(paste("M", M, "n_train", n_train, "n_test", n_test, "rho", rhos[rho_idx], "betas",
                     paste(as.character(betas), collapse = "_"), sep = "_"), "_dt_", evaluation_criterion, ".rds")
      save_files[[paste0("rho_", rhos[rho_idx])]] = file.path(folder_save, file_name)
    }
  } else {
    save_files = aggregated_results$save_files
  }

  # Make the figures
  figures_list = lapply(save_files, function(save_file){
    plot_results(file_path = save_file,
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
                 plot_figures = FALSE)})
}






# All run setups --------------------------------------------------------------------------------------------------
stop()

files_in_dir = list.files(folder_save)
relevant_files_in_dir = files_in_dir[grepl("dt_M.E.rds", files_in_dir)]
relevant_files_full_path = file.path(folder_save, relevant_files_in_dir)
all_setups = t(sapply(relevant_files_in_dir,
                      function(one_path) extract_parameters_from_path(one_path),
                      USE.NAMES = TRUE))

# Create list containing the full path, but set name to only be the file name
relevant_files = as.list(file.path(folder_save, relevant_files_in_dir))
relevant_files <- setNames(relevant_files, relevant_files_in_dir)



# Make the figures
figures_list = lapply(relevant_files, function(save_file){
  plot_results(file_path = save_file,
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
               scale_y_log10 = TRUE,
               scale_x_log10 = FALSE,
               n.dodge = 2,
               plot_figures = FALSE)})

"/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.04_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds"
"/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_6_n_train_1000_n_test_1000_rho_0.91_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_dt_MAE.rds"
ff = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.5_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds",
                  index_combinations = NULL,
                  #only_these_sampling_methods = c("unique", "unique_unif", "unique_SW", "unique_paired", "unique_paired_unif", "unique_paired_SW", "paired_coalitions"),
                  #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                  #only_these_sampling_methods = c("unique", "unique_SW", "unique_paired_equal_weights", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                  only_these_sampling_methods = c("unique_paired_equal_weights", "unique_paired_equal_weights_1000", "unique_paired_equal_weights_5000", "unique_paired_equal_weights_10000", "unique_paired_equal_weights_50000", "unique_paired_SW", "paired_coalitions"),
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
                  scale_y_log10 = TRUE,
                  scale_x_log10 = FALSE,
                  n.dodge = 2,
                  plot_figures = FALSE)
ff$figure_mean

ff = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_17_n_train_1000_n_test_500_rho_0.5_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_dt_MAE.rds",
                  index_combinations = NULL,
                  #only_these_sampling_methods = c("unique", "unique_unif", "unique_SW", "unique_paired", "unique_paired_unif", "unique_paired_SW", "paired_coalitions"),
                  #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                  #only_these_sampling_methods = c("unique", "unique_SW", "unique_paired_equal_weights", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                  #only_these_sampling_methods = c("unique_paired_equal_weights", "unique_paired_equal_weights_1000", "unique_paired_equal_weights_5000", "unique_paired_equal_weights_10000", "unique_paired_equal_weights_50000", "unique_paired_SW", "paired_coalitions"),
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
                  scale_y_log10 = TRUE,
                  scale_x_log10 = FALSE,
                  n.dodge = 2,
                  plot_figures = FALSE)
ff$figure_mean


pilot = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_9_n_train_1000_n_test_1000_rho_0.7_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_dt_MAE.rds",
                  index_combinations = NULL,
                  #only_these_sampling_methods = c("unique", "unique_unif", "unique_SW", "unique_paired", "unique_paired_unif", "unique_paired_SW", "paired_coalitions"),
                  #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                  #only_these_sampling_methods = c("unique", "unique_SW", "unique_paired_equal_weights", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                  #only_these_sampling_methods = c("unique_paired_equal_weights", "unique_paired_equal_weights_1000", "unique_paired_equal_weights_5000", "unique_paired_equal_weights_10000", "unique_paired_equal_weights_50000", "unique_paired_SW", "paired_coalitions"),
                  # only_these_sampling_methods = c(
                  #   "paired_coalitions_weights",
                  #   "paired_coalitions_weights_direct",
                  #   "paired_coalitions_weights_equal_weights",
                  #   "paired_coalitions_weights_direct_equal_weights",
                  #   "paired_coalitions",
                  #   "unique_paired_equal_weights",
                  #   "unique_paired_SW"
                  #   # "paired_coalitions_weights_replace_W",
                  #   # "paired_coalitions_weights_direct_replace_W",
                  #   # "paired_coalitions_weights_equal_weights_replace_W",
                  #   # "paired_coalitions_weights_direct_equal_weights_replace_W",
                  #   # "paired_coalitions_replace_W",
                  #   # "unique_paired_equal_weights_replace_W",
                  #   # "unique_paired_SW_replace_W"
                  # ),
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
                  scale_y_log10 = TRUE,
                  scale_x_log10 = FALSE,
                  n.dodge = 2,
                  plot_figures = FALSE)
pilot$figure_mean



figures_list_equi = figures_list[grep("equi_TRUE", names(figures_list))]

figures_list_equi$`M_10_n_train_1000_n_test_1000_rho_0.3_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean
figures_list_equi$`M_10_n_train_1000_n_test_1000_rho_0.3_equi_TRUE_weights_unif_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean

figures_list_equi$M_10_n_train_1000_n_test_1000_rho_0_equi_TRUE_betas_1_1_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean

figures_list_equi$`M_10_n_train_1000_n_test_1000_rho_0.6_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean

figures_list_equi$`M_10_n_train_1000_n_test_1000_rho_0.3_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_CI

figures_list_equi$M_10_n_train_1000_n_test_1000_rho_0_equi_TRUE_betas_1_1_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean
figures_list_equi$M_10_n_train_1000_n_test_1000_rho_0.1_equi_TRUE_betas_1_1_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean
figures_list_equi$M_10_n_train_1000_n_test_1000_rho_0.6_equi_TRUE_betas_1_1_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean
figures_list_equi$M_10_n_train_1000_n_test_1000_rho_0.9_equi_TRUE_betas_1_1_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean
figures_list_equi$M_10_n_train_1000_n_test_1000_rho_0.99_equi_TRUE_betas_1_1_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean
figures_list_equi$M_10_n_train_1000_n_test_1000_rho_0.6_equi_TRUE_betas_1_1_1_1_1_1_1_1_1_1_1_pilot_separate_linear_reg_dt_MAE.rds$figure_mean

figures_list_equi$`M_10_n_train_1000_n_test_1000_rho_0.3_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_pilot_separate_linear_reg_dt_MAE.rds`$figure_mean

figures_list_equi$`M_10_n_train_1000_n_test_1000_rho_0_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean
figures_list_equi$`M_10_n_train_1000_n_test_1000_rho_0.1_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean
figures_list_equi$`M_10_n_train_1000_n_test_1000_rho_0.3_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean
figures_list_equi$`M_10_n_train_1000_n_test_1000_rho_0.6_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean
figures_list_equi$`M_10_n_train_1000_n_test_1000_rho_0.9_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean
figures_list_equi$`M_10_n_train_1000_n_test_1000_rho_0.99_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean

figures_list_equi$M_12_n_train_1000_n_test_1000_rho_0_equi_TRUE_betas_1_1_1_1_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean
figures_list_equi$`M_12_n_train_1000_n_test_1000_rho_0_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_dt_MAE.rds`$figure_mean

figures_list$`Paper3_Experiment_M_6_n_train_1000_n_test_5000_rho_0_betas_2_10_0.25_-3_-1_1.5_-0.5_dt_MAE.rds`$figure_CI

figures_list$Paper3_Experiment_M_8_n_train_1000_n_test_1000_rho_0_betas_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean
figures_list$Paper3_Experiment_M_8_n_train_1000_n_test_1000_rho_0.5_betas_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean
figures_list$Paper3_Experiment_M_8_n_train_1000_n_test_1000_rho_0.9_betas_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean

figures_list$`Paper3_Experiment_M_8_n_train_1000_n_test_1000_rho_0_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_dt_MAE.rds`$figure_mean
figures_list$`Paper3_Experiment_M_8_n_train_1000_n_test_1000_rho_0.5_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_dt_MAE.rds`$figure_mean
figures_list$`Paper3_Experiment_M_8_n_train_1000_n_test_1000_rho_0.9_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_dt_MAE.rds`$figure_mean

# TODO: when making fig, I should check if it exists and then add _1 ,_2 and so on.

figures_list_M10 = figures_list[grep("M_10", names(figures_list))]
figures_list_M10$Paper3_Experiment_M_10_n_train_1000_n_test_1000_rho_0_betas_1_1_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean
figures_list_M10$Paper3_Experiment_M_10_n_train_1000_n_test_1000_rho_0.5_betas_1_1_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean
figures_list_M10$Paper3_Experiment_M_10_n_train_1000_n_test_1000_rho_0.9_betas_1_1_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean
figures_list_M10$Paper3_Experiment_M_10_n_train_1000_n_test_1000_rho_0.99_betas_1_1_1_1_1_1_1_1_1_1_1_dt_MAE.rds$figure_mean

figures_list_M10$`Paper3_Experiment_M_10_n_train_1000_n_test_1000_rho_0_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean
figures_list_M10$`Paper3_Experiment_M_10_n_train_1000_n_test_1000_rho_0.5_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean
figures_list_M10$`Paper3_Experiment_M_10_n_train_1000_n_test_1000_rho_0.9_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean
figures_list_M10$`Paper3_Experiment_M_10_n_train_1000_n_test_1000_rho_0.99_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds`$figure_mean


stop()
# Exploration -----------------------------------------------------------------------------------------------------





system.time({
test1 = combine_explanation_results(M = 6,
                                    rhos = c(0.0, 0.5),
                                    rho_equi = FALSE,
                                    n_train = 1000,
                                    n_test = 5000,
                                    betas = c(2, 10, 0.25, -3, -1, 1.5, -0.5),
                                    folder_save = "/Users/larsolsen/PhD/Paper3/Paper3_save_location",
                                    memory_efficient = TRUE,
                                    save_results = TRUE,
                                    evaluation_criterion = "MAE",
                                    level = 0.95,
                                    n_workers = 1,
                                    objects_to_return = "aggregated_results")
})

test1

test1$aggregated_results
test1_plot = plot_results(dt_CI = test1$aggregated_results$rho_0$dt_CI,
                          dt_long = test1$aggregated_results$rho_0$dt_long)
test1_plot = plot_results(file_path = test1$Save_files$rho_0)
test1_plot$figure_mean
test1_plot$figure_boxplot


## Parameters ------------------------------------------------------------------------------------------------------
# The number of features
M = 6
M = 8

# The correlation level
rhos = c(0.0, 0.5, 0.9)
rhos = 0.7

# The number of training observations
n_train = 1000

# The number of test observations
n_test = 1000

# Get the name of the computer we are working on
hostname = R.utils::System$getHostname()
cat(sprintf("We are working on '%s'.\n", R.utils::System$getHostname()))

# If we are working on UiO computer or not
UiO = NULL

# set the working directory and define the correct folder based on system
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
  # TBA
  folder = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr"
  folder_save = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location"

  UiO = TRUE

} else {
  stop("We do not recongize the system at which the code is run (not Lars's MAC, HPC, nor UiO).")
}

# Set the working directory
setwd(folder)

#library(shapr)
#setwd("~/PhD/Paper3/Shapr_Lars_paper3/R")
#pkgload::load_all()
source(file.path(folder, "R/Lars_explore_ideas_scripts/new_functions.R"))


# The beta vector
betas = c(0, rep(1, M))
betas = c(2, 1, 0.25, -3, -1, 1.5, -0.5, 0.75, 1.25, 1.5, -2, 3, -1)
betas = c(2, 10, 0.25, -3, -1, 1.5, -0.5, 10, 1.25, 1.5, -2, 3, -1)
betas = betas[seq(M+1)]

# If we are to remove redundant stuff from the explanations
memory_efficient = TRUE


## Load the results ------------------------------------------------------------------------------------------------

# Iterate over the rhos
rho_idx = 1
for (rho_idx in seq_along(rhos)) {
  # Get the current rho
  rho = rhos[rho_idx]

  # Result list (moved it inside here as it becomes quite large and takes up many GBs of memory)
  repeated_explanations_list = list()
  true_explanations_list = list()

  repeated_explanations_list[[paste0("rho_", rho)]] = list()

  # Make some of the save file names
  file_name = paste("Paper3_Experiment_M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "betas",
                    paste(as.character(betas), collapse = "_"), sep = "_")
  save_file_name_setup = file.path(folder_save, paste0(file_name, "_model.rds"))
  save_file_name_true = file.path(folder_save, paste0(file_name, "_true.rds"))

  files_in_dir = list.files(folder_save)
  relevant_files_in_dir = files_in_dir[grepl(paste0(file_name, "_estimated_repetition_"), files_in_dir)]
  relevant_files_in_dir = relevant_files_in_dir[!grepl("tmp", relevant_files_in_dir)] # remove any tmp files
  relevant_repetitions = sort(as.integer(sapply(strsplit(unlist(strsplit(relevant_files_in_dir, '.rds')), '\\_'), tail, 1)))

  # Only want the first 50 repetitions
  relevant_repetitions = relevant_repetitions[seq(min(50, length(relevant_repetitions)))]

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

    # We remove all non-essential stuff from the list
    if (memory_efficient) {
      cat(sprintf("Using memory efficient version (before): "))
      print(object.size(current_repetition_results), units = "MB")

      for (met in names(current_repetition_results)) {
        for (rep in names(current_repetition_results[[met]])) {
          for (comb in names(current_repetition_results[[met]][[rep]])) {
            #print(object.size(current_repetition_results[[met]][[rep]][[comb]]), units = "KB")
            tmp_res = current_repetition_results[[met]][[rep]][[comb]]
            tmp_res[["only_save"]] = NULL
            tmp_res$internal = NULL
            tmp_res$timing = NULL
            tmp_res$pred_explain = NULL
            current_repetition_results[[met]][[rep]][[comb]] = tmp_res
            #print(object.size(current_repetition_results[[met]][[rep]][[comb]]), units = "KB")
          }
        }
      }
      cat(sprintf("Using memory efficient version (after): "))
      print(object.size(current_repetition_results), units = "MB")
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
    print(length(repeated_explanations_list))
  }

  result_figures = aggregate_and_plot_results(repeated_explanations_list = repeated_explanations_list[[1]],
                                              true_explanations = true_explanations_list[[1]],
                                              evaluation_criterion = "MAE",
                                              scale_y_log10 = FALSE,
                                              plot_figures = FALSE,
                                              return_figures = TRUE,
                                              return_dt = TRUE,
                                              n_workers = 1)
  saveRDS(result_figures$dt, file.path(folder_save, paste0(file_name, "_dt.rds")))
}



## Figures ---------------------------------------------------------------------------------------------------------
result_figures$figures$figure_CI
result_figures$figures$figure_mean

result_figures = aggregate_and_plot_results(repeated_explanations_list = repeated_explanations_list[[1]],
                                            true_explanations = true_explanations_list[[1]],
                                            evaluation_criterion = "MAE",
                                            scale_y_log10 = TRUE,
                                            plot_figures = FALSE,
                                            return_figures = TRUE,
                                            return_dt = TRUE,
                                            n_workers = 1)



# Just the specific methods
aggregate_and_plot_results(repeated_explanations_list = repeated_explanations_list[[1]],
                           true_explanations = true_explanations_list[[1]],
                           evaluation_criterion = "MAE",
                           plot_figures = FALSE,
                           return_figures = TRUE,
                           return_dt = FALSE,
                           scale_y_log10 = FALSE,
                           dt_CI = result_figures$dt$dt_CI,
                           dt_long = result_figures$dt$dt_long,
                           only_these_sampling_methods = c("paired_coalitions",
                                                           # "single_mean_coalition_effect",
                                                           # "single_median_coalition_effect",
                                                           # "single_mean_ranking_over_each_test_obs",
                                                           # "single_median_ranking_over_each_test_obs",
                                                           "unique",
                                                           "unique_paired"))$figure_mean


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
                           scale_y_log10 = FALSE,
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
                           scale_y_log10 = FALSE,
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
                           scale_y_log10 = FALSE,
                           dt_CI = result_figures$dt$dt_CI,
                           dt_long = result_figures$dt$dt_long,
                           only_these_sampling_methods = c("unique",
                                                           "largest_weights",
                                                           "largest_weights_combination_size",
                                                           "smallest_weights",
                                                           "smallest_weights_combination_size"))$figure_CI

repeated_explanations_list$rho_0.5$repetition1$unique$repetition_1




## Load from disk --------------------------------------------------------------------------------------------------
rho = 0.5
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





