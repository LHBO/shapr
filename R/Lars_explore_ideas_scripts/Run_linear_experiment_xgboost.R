# cd ~/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts

# # #Rscript Run_linear_experiment.R TRUE TRUE FALSE NULL 1 250 1000 250 10 0.0 NULL
# #  ixion bastet
# # #
# tmux new -s paper3
# module load R/4.2.0-foss-2021b
# module load R/4.2.1-foss-2022a
# cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R
# git checkout Lars/paper3_ideas
# git pull
# cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts
# Rscript Run_linear_experiment.R TRUE TRUE FALSE NULL 11 5000 NULL 1000 250 10 0.9 NULL
# # # Rscript Run_linear_experiment.R FALSE FALSE TRUE 1:50 6 NULL 500 1000 250 10 0.9 NULL
# # # Rscript Run_linear_experiment.R FALSE FALSE TRUE 51:100 8 NULL 500 1000 250 10 0.5 NULL
# # Rscript Run_linear_experiment.R TRUE TRUE TRUE 1:100 3 5000 500 1000 250 10 0.6 NULL
# # Rscript Run_linear_experiment.R TRUE TRUE TRUE 1:100 3 5000 500 1000 250 10 0.9 NULL


# To do the setup, i.e., create the data and predictive model
# Rscript Run_linear_experiment.R TRUE TRUE TRUE 1:100 3 5000 500 1000 250 10 0.5 NULL
# Then, we create the true Shapley values
# Rscript Run_linear_experiment.R FALSE TRUE FALSE NULL 6 1000000 250 1000 250 10 0.5 NULL
# Then, we can run the repeated experiments
# Rscript Run_linear_experiment.R FALSE FALSE TRUE 1:10 6 1000000 250 1000 250 10 0.5 NULL

# Rscript Run_linear_experiment.R TRUE TRUE TRUE 1:10 6 1000000 250 1000 250 10 0.5 NULL

# Rscript Run_linear_experiment.R TRUE TRUE TRUE 1:50 8 1000000 250 1000 250 10 0.0 NULL (y)
# Rscript Run_linear_experiment.R TRUE TRUE TRUE 1:50 8 1000000 250 1000 250 10 0.3 NULL
# Rscript Run_linear_experiment.R TRUE TRUE TRUE 1:50 8 1000000 250 1000 250 10 0.5 NULL (y)
# Rscript Run_linear_experiment.R TRUE TRUE TRUE 1:50 8 1000000 250 1000 250 10 0.7 NULL (y)
# Rscript Run_linear_experiment.R TRUE TRUE TRUE 1:50 8 1000000 250 1000 250 10 0.9 NULL (y)

# Rscript Run_linear_experiment.R TRUE TRUE TRUE 1 4 1000000 250 1000 250 5 0.5 NULL

#

# 2024 runnings ---------------------------------------------------------------------------------------------------

# Rscript Run_linear_experiment.R TRUE TRUE FALSE 1:10 4 5000 5000 1000 250 5 0.7 NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE 1:10 4 5000 5000 1000 250 5 0.7 NULL

# Rscript Run_linear_experiment.R
# do_setup (Boolean)
# compute_true_explanations (Boolean)
# compute_repeated_explanations (Boolean)
# use_pilot_estimates_regression (Boolean)
# repetitions (Integer, or array of integers "c(1, 4, 6)", "1:10". Different seeds)
# n_workers (Integer)
# n_samples_true (Integer, but can also be NULL)
# n_samples (Integer, but can also be NULL)
# n_train (Integer)
# n_test (Integer)
# M (Integer)
# rhos (Numeric, or list of numeric, e.g. "0.0,0.4,0.9")
# pilot_approach_regression (string either "regression_separate" or "regression_surrogate" (default))
# pilot_regression_model (String, e.g., "parsnip::linear_reg()" (default))


# Rscript Run_linear_experiment_xgboost.R TRUE TRUE FALSE FALSE 1:10 2 10000 10000 1000 1000 10 0,0.05 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R TRUE TRUE FALSE FALSE 1:10 2 10000 10000 1000 1000 10 0.1,0.2 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R TRUE TRUE FALSE FALSE 1:10 2 10000 10000 1000 1000 10 0.5,0.7 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R TRUE TRUE FALSE FALSE 1:10 2 10000 10000 1000 1000 10 0.3,0.9 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R TRUE TRUE FALSE FALSE 1:10 2 10000 10000 1000 1000 10 0,0.05 FALSE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R TRUE TRUE FALSE FALSE 1:10 2 10000 10000 1000 1000 10 0.1,0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R TRUE TRUE FALSE FALSE 1:10 2 10000 10000 1000 1000 10 0.5,0.7 FALSE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R TRUE TRUE FALSE FALSE 1:10 2 10000 10000 1000 1000 10 0.3,0.9 FALSE NULL NULL NULL
#
#
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 10000 10000 1000 1000 10 0,0.05 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 10000 10000 1000 1000 10 0.1,0.2 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 10000 10000 1000 1000 10 0.5,0.7 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 10000 10000 1000 1000 10 0.3,0.9 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 10000 10000 1000 1000 10 0,0.05 FALSE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 10000 10000 1000 1000 10 0.1,0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 10000 10000 1000 1000 10 0.5,0.7 FALSE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 10000 10000 1000 1000 10 0.3,0.9 FALSE NULL NULL NULL

#
#
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:5 2 10000 10000 1000 1000 8 0,0.05 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:5 2 10000 10000 1000 1000 8 0.1,0.2 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:5 2 10000 10000 1000 1000 8 0.5,0.7 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:5 2 10000 10000 1000 1000 8 0.3,0.9 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:5 2 10000 10000 1000 1000 8 0,0.05 FALSE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:5 2 10000 10000 1000 1000 8 0.1,0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:5 2 10000 10000 1000 1000 8 0.5,0.7 FALSE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:5 2 10000 10000 1000 1000 8 0.3,0.9 FALSE NULL NULL NULL


# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 5000 5000 1000 500 8 0,0.05 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 5000 5000 1000 500 8 0.1,0.2 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 5000 5000 1000 500 8 0.5,0.7 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 5000 5000 1000 500 8 0.3,0.9 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 5000 5000 1000 500 8 0,0.05 FALSE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 5000 5000 1000 500 8 0.1,0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 5000 5000 1000 500 8 0.5,0.7 FALSE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:10 6 5000 5000 1000 500 8 0.3,0.9 FALSE NULL NULL NULL


#Rscript Run_linear_experiment.R TRUE TRUE FALSE FALSE 1:5 6 5000 5000 1000 1000 8 0.5 FALSE NULL NULL NULL
#Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 5000 5000 1000 1000 8 0.5 FALSE NULL NULL NULL



# NSM2024
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:20 6 10000 10000 1000 1000 10 0.0 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:20 6 10000 10000 1000 1000 10 0.2 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:20 6 10000 10000 1000 1000 10 0.5 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 1:20 6 10000 10000 1000 1000 10 0.9 TRUE NULL NULL NULL
#
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 21:40 6 10000 10000 1000 1000 10 0.0 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 21:40 6 10000 10000 1000 1000 10 0.2 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 21:40 6 10000 10000 1000 1000 10 0.5 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 21:40 6 10000 10000 1000 1000 10 0.9 TRUE NULL NULL NULL
#
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 41:50 6 10000 10000 1000 1000 10 0.0,0.2 TRUE NULL NULL NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE FALSE 41:50 6 10000 10000 1000 1000 10 0.5,0.9 TRUE NULL NULL NULL

# Linear reg pilots
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE TRUE 1:20 6 10000 10000 1000 1000 10 0.0 TRUE NULL regression_separate NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE TRUE 1:20 6 10000 10000 1000 1000 10 0.2 TRUE NULL regression_separate NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE TRUE 1:20 6 10000 10000 1000 1000 10 0.5 TRUE NULL regression_separate NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE TRUE 1:20 6 10000 10000 1000 1000 10 0.9 TRUE NULL regression_separate NULL
#
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE TRUE 21:40 6 10000 10000 1000 1000 10 0.0 TRUE NULL regression_separate NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE TRUE 21:40 6 10000 10000 1000 1000 10 0.2 TRUE NULL regression_separate NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE TRUE 21:40 6 10000 10000 1000 1000 10 0.5 TRUE NULL regression_separate NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE TRUE 21:40 6 10000 10000 1000 1000 10 0.9 TRUE NULL regression_separate NULL
#
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE TRUE 41:50 6 10000 10000 1000 1000 10 0.0,0.2 TRUE NULL regression_separate NULL
# Rscript Run_linear_experiment_xgboost.R FALSE FALSE TRUE TRUE 41:50 6 10000 10000 1000 1000 10 0.5,0.9 TRUE NULL regression_separate NULL





# Input From Command Line ----------------------------------------------------------------------------------------------
args = commandArgs(trailingOnly = TRUE)
# test if there is at least one argument: if not, return an error
if (length(args) < 16) {
  stop("Must provide all parameters (do_setup, compute_true_explanations, compute_repeated_explanations,
       use_pilot_estimates_regression, repetitions, n_workers, n_samples_true, n_samples, n_train, n_test, M,
       rho, rho_equi, beta, pilot_approach_regression, pilot_regression_model)!", call.=FALSE)
}

do_setup = FALSE
compute_true_explanations = FALSE
compute_repeated_explanations = TRUE
use_pilot_estimates_regression = FALSE
repetitions = 5
n_workers = 6
n_samples_true = 10000
n_samples = 10000
n_train = 1000
n_test = 1000
M = 10
rhos = 0.5
rho_equi = FALSE
pilot_approach_regression = "regression_separate"
pilot_regression_model = "parsnip::linear_reg()"

# Extract if we are to generate the data and model
do_setup = as.logical(args[1])

# Extract if we are to compute the true Shapley values
compute_true_explanations = as.logical(args[2])

# Extract if we are to compute the repeated estimated Shapley values
compute_repeated_explanations = as.logical(args[3])

# If we are to use pilot estimates or not
use_pilot_estimates_regression = as.logical(args[4])

# Extract which repetition we are to do
repetitions = as.character(args[5])
if (!(repetitions %in% c("NULL", "NA", "NaN"))) {
  if (grepl(",", repetitions)) {
    repetitions = as.numeric(unlist(strsplit(repetitions, ",")))
  } else {
    repetitions = unlist(strsplit(repetitions, ":"))
    if (length(repetitions) > 1) {
      repetitions = seq(as.numeric(repetitions[1]), as.numeric(repetitions[2]))
    } else {
      repetitions = as.numeric(repetitions)
    }
  }
} else {
  repetitions = NULL
}

# Extract the number of workers / cores to run on
n_workers = as.integer(args[6])

# Extract the number of MC samples
n_samples_true = as.integer(args[7])
n_samples = as.integer(args[8])

# Extract the number of training observations
n_train = as.integer(args[9])

# Extract the number of test observations
n_test = as.integer(args[10])

# Extract the number of features
M = as.integer(args[11])

# Extract the correlation level
rhos = unlist(strsplit(args[12], ","))
if (length(rhos) > 1) {
  rhos = unname(sapply(rhos, function(i) as.numeric(i)))
} else {
  rhos = as.numeric(rhos)
}

# If we are to use equicorrelation matrix
rho_equi = as.logical(args[13])

# Extract the correlation level
betas = as.character(args[14])
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
  betas = betas[seq(M+1)]
}


# Extract the kind of regression model
pilot_approach_regression = as.character(args[15])
pilot_regression_model = as.character(args[16])
if (pilot_approach_regression %in% c("NULL", "NA", "NaN")) pilot_approach_regression = "regression_surrogate"
if (pilot_regression_model %in% c("NULL", "NA", "NaN")) pilot_regression_model = "parsnip::linear_reg()"

# Small printout to the user
message(paste0(
  "Set up:",
  "\ndo_setup = ", do_setup,
  "\ncompute_true_explanations = ", compute_true_explanations,
  "\ncompute_repeated_explanations = ", compute_repeated_explanations,
  "\nuse_pilot_estimates_regression = ", use_pilot_estimates_regression,
  "\nrepetitions = [", paste(repetitions, collapse = ", "), "]",
  "\nn_workers = ", n_workers,
  "\nn_samples_true = ", n_samples_true,
  "\nn_samples = ", n_samples,
  "\nn_train = ", n_train,
  "\nn_test = ", n_test,
  "\nM = ", M,
  "\nrho = [", paste(rhos, collapse = ", "), "]",
  "\nrho_equi = ", rho_equi,
  "\nbeta = [", paste(betas, collapse = ", "), "]",
  "\npilot_approach_regression = ", pilot_approach_regression,
  "\npilot_regression_model = ", pilot_regression_model, "\n"))




# Setup -----------------------------------------------------------------------------------------------------------
# Get the name of the computer we are working on
hostname = R.utils::System$getHostname()
message(sprintf("We are working on '%s'.", R.utils::System$getHostname()))

# Check if we are working on an UiO computer or not and define the correct folder based on system
if (hostname == "Larss-MacBook-Pro.local" || Sys.info()[[7]] == "larsolsen") {
  folder = "/Users/larsolsen/PhD/Paper3/shapr"
  folder_save = "/Users/larsolsen/PhD/Paper3/Paper3_save_location"
  UiO = FALSE
} else if (grepl("hpc.uio.no", hostname)) {
  # To be added
  folder = ""
  UiO = TRUE
} else if (grepl("uio.no", hostname)) {
  folder = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr"
  folder_save = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location"
  UiO = TRUE
} else {
  stop("We do not recongize the system at which the code is run (not Lars's MAC, HPC, nor UiO).")
}

# Set the working directory
setwd(folder)

# Load the new functions
source(file.path(folder, "R/Lars_explore_ideas_scripts/new_functions.R"))

message("Loading my version of the `shapr` package.")
#library(shapr)
#setwd("~/PhD/Paper3/Shapr_Lars_paper3/R")
if (UiO) {
  # print("#HE")
  # devtools::clean_dll()
  # #remotes::install_github(repo = "LHBO/shapr", ref = "Lars/paper3_ideas")
  # devtools::install_github(repo = "LHBO/shapr", ref = "Lars/paper3_ideas")
  # # If they have the old version, then we install the one on GitHub
  # print(any(as.list(args(shapr::explain)) == "x"))
  # if (any(as.list(args(shapr::explain)) == "x")) {
  # devtools::clean_dll()
  # devtools::install_github(repo = "LHBO/shapr", ref = "Lars/paper3_ideas")
  # }
}
# devtools::load_all(".")
if (Sys.info()[[4]] == "nam-shub-02.uio.no") {
  # devtools::clean_dll()
  # devtools::install_github(repo = "LHBO/shapr", ref = "Lars/paper3_ideas")
}
#devtools::install_github(repo = "LHBO/shapr", ref = "Lars/paper3_ideas")
# library(shapr)
devtools::load_all(".")


# Libraries -------------------------------------------------------------------------------------------------------
library(data.table)
library(mvtnorm)
library(condMVNorm)
suppressPackageStartupMessages(library(mgcv))
library(progressr)
library(cli)
library(future)



# Small variable check --------------------------------------------------------------------------------------------
n_max_workers = future::availableCores()
if (n_workers > n_max_workers) {
  warning(sprintf("Too many workers. Change from %d to %d (max available cores).", n_workers, n_max_workers))
  n_workers = n_max_workers
}


# Fixed parameters -----------------------------------------------------------------------------------------------------
# Mean of the multivariate Gaussian distribution
mu = rep(0, times = M)

# We use the Gaussian approach
approach = "gaussian"

# Which coalition/combination sampling schemes to use
sampling_methods = c("unique",
                     "unique_SW",
                     "unique_paired",
                     "unique_paired_SW",
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
                     "single_mean_coalition_effect",
                     "single_median_coalition_effect",
                     "single_mean_ranking_over_each_test_obs",
                     "single_median_ranking_over_each_test_obs",
                     "pilot_estimates_paired")

sampling_methods = c("paired_coalitions",
                     "single_mean_coalition_effect",
                     "single_median_coalition_effect",
                     "single_mean_ranking_over_each_test_obs",
                     "single_median_ranking_over_each_test_obs",
                     "unique",
                     "unique_paired",
                     "unique_paired_SW",
                     "chronological_order_increasing",
                     "largest_weights",
                     "smallest_weights")

sampling_methods = c("unique",
                     "unique_paired",
                     "paired_coalitions",
                     "single_mean_coalition_effect",
                     "single_median_coalition_effect",
                     "single_mean_ranking_over_each_test_obs",
                     "single_median_ranking_over_each_test_obs")

sampling_methods = c("paired_coalitions_weights",
                     "paired_coalitions_weights_direct",
                     "paired_coalitions_weights_equal_weights",
                     "paired_coalitions_weights_direct_equal_weights",
                     "paired_coalitions",
                     "unique",
                     "unique_unif",
                     "unique_unif_V2",
                     "unique_SW",
                     "unique_equal_weights",
                     "unique_equal_weights_symmetric",
                     "unique_paired",
                     "unique_paired_unif",
                     "unique_paired_unif_V2",
                     "unique_paired_SW",
                     "unique_paired_equal_weights",
                     "unique_paired_equal_weights_100",
                     "unique_paired_equal_weights_500",
                     "unique_paired_equal_weights_1000",
                     "unique_paired_equal_weights_5000",
                     "unique_paired_equal_weights_10000",
                     "unique_paired_equal_weights_50000",
                     "unique_paired_equal_weights_symmetric",
                     # "paired_coalitions_sub",
                     # "paired_coalitions_scaled",
                     # "paired_coalitions_avg",
                     # "paired_coalitions_norm",
                     "single_mean_coalition_effect"
                     # "single_median_coalition_effect",
                     # "single_mean_ranking_over_each_test_obs",
                     # "single_median_ranking_over_each_test_obs"
)

sampling_methods = c("unique_paired_unif_V2",
                     "unique",
                     "unique_paired",
                     "unique_paired_equal_weights",
                     "unique_paired_SW",
                     "paired_coalitions",
                     "paired_coalitions_weights_direct_equal_weights")

sampling_methods = c("largest_weights",
                     "unique_paired_unif_V2",
                     "unique_paired_SW",
                     "unique_paired_equal_weights",
                     "unique_paired_equal_weights_100",
                     "unique_paired_equal_weights_500",
                     "unique_paired_equal_weights_1000",
                     "unique_paired_equal_weights_2500",
                     "unique_paired_equal_weights_5000",
                     "unique_paired_equal_weights_10000",
                     "unique_paired_equal_weights_50000",
                     "unique_paired_equal_weights_symmetric",
                     "paired_coalitions")

# sampling_methods = c("paired_coalitions_weights",
#                      "paired_coalitions_weights_direct",
#                      "paired_coalitions_weights_equal_weights",
#                      "paired_coalitions_weights_direct_equal_weights",
#                      "paired_coalitions",
#                      "unique_paired_equal_weights",
#                      "unique_paired_SW")
#
# sampling_methods = c(
#   "paired_coalitions_weights_replace_W",
#   "paired_coalitions_weights_direct_replace_W",
#   "paired_coalitions_weights_equal_weights_replace_W",
#   "paired_coalitions_weights_direct_equal_weights_replace_W",
#   "paired_coalitions_replace_W",
#   "unique_paired_equal_weights_replace_W",
#   "unique_paired_SW_replace_W",
#   "paired_coalitions_weights",
#   "paired_coalitions_weights_direct",
#   "paired_coalitions_weights_equal_weights",
#   "paired_coalitions_weights_direct_equal_weights",
#   "paired_coalitions",
#   "unique_paired_equal_weights",
#   "unique_paired_SW"
# )




# First value where the coalition/combination sampling scheme has an effect, (first two are empty and full coalitions)
n_combinations_from = 2

# We increase by one each time
n_combinations_increment = 10

# Or we can define it to do more coalitions in the beginning as it seems to be then that we get the largest
# changes in the MAE, i.e, it is better to use some extra time to do the computations there and be more coarse
# for the coalition sizes in the middle.
middle_part = seq(M + choose(M, 2), 2^M - M, n_combinations_increment)
middle_part[middle_part %% 2 == 1] = middle_part[middle_part %% 2 == 1] - 1 # Ensure that we do it with even numbers due to the paired methods
n_combinations_array =
  sort(unique(c(seq(2, M + choose(M, 2) - 1), # Include all with 1 or 2 features # They can contain other combinations with many features
                middle_part, # Then include `n_combinations_increment` new coalitions at the time
                seq(2^M-M, 2^M)))) # Include the coalitions that are missing 1 feature

# n_combinations_array =
#   sort(unique(c(seq(2, M + choose(M, 2) - 1), # Include all with 1 or 2 features # They can contain other combinations with many features
#                 middle_part, # Then include `n_combinations_increment` new coalitions at the time
#                 seq(2^M-M, 2^M-M)))) # Include the coalitions that are missing 1 feature
#
# n_combinations_array =
#   sort(unique(c(seq(2, M + choose(M, 2) - 1), # Include all with 1 or 2 features # They can contain other combinations with many features
#                 middle_part))) # Include the coalitions that are missing 1 feature

if (M <= 7) n_combinations_array = seq(2, 2^M)

# We start with 2 ad we used 1 for the true Shapley values above.
seed_start_value = 2

# Iterate over the rhos
rho_idx = 1
for (rho_idx in seq_along(rhos)) {
  # Get the current rho
  rho = rhos[rho_idx]

  # Small message to the user
  message(paste0("Working on rho = ", rho, " (", rho_idx, " of ", length(rhos), ")"))

  # Create the covariance matrix
  sigma = matrix(ncol = M, nrow = M) # Old
  if (rho_equi) {
    sigma = matrix(rho, ncol = M, nrow = M) # Old
  } else {
    for (i in seq(1, M-1)) {
      for (j in seq(i+1, M))
        sigma[i,j] = sigma[j,i] = rho^abs(i-j)
    }
  }
  diag(sigma) = 1

  # Make some of the save file names
  # TODO: REMOVE NSM2024 in the future
  file_name = paste("Samp_VS_kernel_Xgboost_M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "equi", rho_equi,
                    "betas", paste(as.character(betas), collapse = "_"), sep = "_")
  save_file_name_setup = file.path(folder_save, paste0(file_name, "_model.rds"))
  save_file_name_true = file.path(folder_save, paste0(file_name, "_true.rds"))

  if (do_setup) {
    message("Create the data and predictive model.")

    if (!M %in% c(8, 10)) stop("M must be 8 or 10")
    if (M == 8) {
      gamma_coefficients = c(2, -3, 3) # Pair-wise interactions between [X1, X2], [X3,X5], [X4,X8]
      gamma_terms = list(c("X1", "X2"), c("X3", "X5"), c("X4", "X8"))
      eta_coefficients = c(5, -7) # [X1, X3, X7] and [X2, X6, X8]
      eta_terms = list(c("X1", "X3", "X7"), c("X2", "X6", "X8"))
      alpha_coefficients = c(8) # [X1, X4, X7, X8]
      alpha_terms = list(c("X1", "X4", "X7", "X8"))
    } else if (M == 10) {
      gamma_coefficients = c(1, -2, 2, -3) # Pair-wise interactions between [X1, X2], [X3,X5], [X4,X8]
      gamma_terms = list(c("X1", "X2"), c("X3", "X5"), c("X4", "X8"), c("X9", "X10"))
      eta_coefficients = c(3, -1, -2) # [X1, X3, X7] and [X2, X6, X8] and [X3, X8, X10]
      eta_terms = list(c("X1", "X3", "X7"), c("X2", "X6", "X8"), c("X3", "X8", "X10"))
      alpha_coefficients = c(4) # [X1, X4, X7, X9]
      alpha_terms = list(c("X1", "X4", "X7", "X9"))
    }

    # Set seed for reproducibility
    seed_setup = 2000
    set.seed(seed_setup)

    # Make Gaussian data
    data_train = data.table(rmvnorm(n = n_train + 10, mean = mu, sigma = sigma))
    data_test  = data.table(rmvnorm(n = n_test + 10,  mean = mu, sigma = sigma))
    colnames(data_train) = paste("X", seq(M), sep = "")
    colnames(data_test) = paste("X", seq(M), sep = "")

    tmp_add_interaction_values = function(terms, coefficients, data) {
      rowSums(sapply(seq_along(terms), function(i) {
        terms_now = terms[[i]]
        coefficient_now = coefficients[i]
        return(apply(as.matrix(data[,..terms_now]), 1, prod) * coefficient_now)
      }))
    }

    # Make the response
    response_train_org = as.vector(cbind(1, as.matrix(data_train)) %*% betas)
    response_test_org = as.vector(cbind(1, as.matrix(data_test)) %*% betas)

    response_train = response_train_org +
      tmp_add_interaction_values(gamma_terms, gamma_coefficients, data_train) +
      tmp_add_interaction_values(eta_terms, eta_coefficients, data_train) +
      tmp_add_interaction_values(alpha_terms, alpha_coefficients, data_train)

    response_test = response_test_org +
      tmp_add_interaction_values(gamma_terms, gamma_coefficients, data_test) +
      tmp_add_interaction_values(eta_terms, eta_coefficients, data_test) +
      tmp_add_interaction_values(alpha_terms, alpha_coefficients, data_test)

    # Remove the ten largest absolute values
    train_indices = order(abs(response_train), decreasing = TRUE)[-c(1:10)]
    test_indices = order(abs(response_test), decreasing = TRUE)[-c(1:10)]
    response_train = response_train[train_indices]
    response_train_org = response_train_org[train_indices]
    response_test = response_test[test_indices]
    response_test_org = response_test_org[test_indices]
    data_test = data_test[test_indices]
    data_train = data_train[train_indices]

    c(min(response_train), max(response_train))
    c(min(response_test), max(response_test))

    plot_data = data.table(values = c(response_train_org, response_train), type = rep(c("without interactions", "with interactions"), times = c(n_train, n_train)))
    ggplot(plot_data, aes(values, fill = type)) + geom_density(alpha = 0.5)
    # ggplot(plot_data, aes(values, fill = type)) + geom_histogram(alpha = 0.5, aes(y = after_stat(density)), position = 'identity')

    plot_data = data.table(values = c(response_test_org, response_test), type = rep(c("without interactions", "with interactions"), times = c(n_test, n_test)))
    ggplot(plot_data, aes(values, fill = type)) + geom_density(alpha = 0.5)
    # ggplot(plot_data, aes(values, fill = type)) + geom_histogram(alpha = 0.5, aes(y = after_stat(density)), position = 'identity')


    # Put together the data
    data_train_with_response = copy(data_train)[,y := response_train]
    data_test_with_response  = copy(data_test)[,y := response_test]


    # Predictive model ------------------------------------------------------------------------------------------------
    # Fit the xgboost model
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
      recipes::recipe(as.formula("y ~ ."),
                      data = data_train_with_response))
    regression.results <- tune::tune_grid(
      object = regression.workflow,
      resamples = rsample::vfold_cv(data = data_train_with_response, v = 5),
      grid = expand.grid(tree_depth = c(2, 4, 6, 8),
                         trees = c(5, 10, 25, 50, 100, 200, 250, 500, 1000, 1500, 2000),
                         learn_rate = c(0.05, 0.1, 0.2)),
      # grid = dials::grid_regular(dials::trees(), dials::tree_depth(), dials::learn_rate(), dials::mtry(c(1, 10)), levels = 5),
      metrics = yardstick::metric_set(yardstick::rmse),
      control = tune::control_grid(verbose = TRUE)
    )
    print(tune::show_best(regression.results, metric = "rmse", n = 10))

    # Look at the accuracy of the model
    predictive_model = lm(y ~ ., data = data_train_with_response)
    message(sprintf("Training MSE = %g and test MSE = %g.",
                    mean((predict(predictive_model, data_train_with_response) - data_train_with_response$y)^2),
                    mean((predict(predictive_model, data_test_with_response) - data_test_with_response$y)^2)))

    # Fit a GAM model.
    predictive_model = gam(as.formula(paste0("y ~ ", paste0("ti(X", seq(M), ")", collapse = " + "))),
                           data = data_train_with_response)
    message(sprintf("Training MSE = %g and test MSE = %g.",
                    mean((predict(predictive_model, data_train_with_response) - data_train_with_response$y)^2),
                    mean((predict(predictive_model, data_test_with_response) - data_test_with_response$y)^2)))

    # Train an xgboost model with the best hyperparameters
    best_results = tune::select_best(regression.results, metric = "rmse")
    predictive_model = xgboost(data = as.matrix(data_train), label = response_train,
                               nrounds = best_results$trees,
                               params = list("eta" = best_results$learn_rate, "max_depth" = best_results$tree_depth),
                               verbose = FALSE)

    message(sprintf("Training MSE = %g and test MSE = %g.",
                    mean((predict(predictive_model, as.matrix(data_train)) - data_train_with_response$y)^2),
                    mean((predict(predictive_model, as.matrix(data_test)) - data_test_with_response$y)^2)))

    # plot(predict(predictive_model, as.matrix(data_train)), data_train_with_response$y)
    # plot(predict(predictive_model, as.matrix(data_test)), data_test_with_response$y)


    # print(tune::show_best(regression.results, metric = "rmse", n = 10))
    #
    # # Update the workflow by finalizing it using the hyperparameters that attained the best rmse
    # regression.workflow <-
    #   tune::finalize_workflow(regression.workflow, tune::select_best(regression.results, metric = "rmse"))
    #
    # # Fit the model to the augmented training data and return the trained model
    # predictive_model = parsnip::fit(regression.workflow, data = data_train_with_response)
    #
    # # Look at the accuracy of the model
    # message(sprintf("Training MSE = %g and test MSE = %g.",
    #                 mean((predict(predictive_model, data_train)$.pred - data_train_with_response$y)^2),
    #                 mean((predict(predictive_model, data_test)$.pred - data_test_with_response$y)^2)))

    # regression.workflow2 = workflows::add_recipe(
    #   workflows::add_model(
    #     workflows::workflow(),
    #     parsnip::boost_tree(trees = 500,
    #                         tree_depth = 6,
    #                         learn_rate = 0.05,
    #                         engine = "xgboost",
    #                         mode = "regression")),
    #   recipes::recipe(as.formula("y ~ ."),
    #                   data = data_train_with_response))
    # predictive_model2 = parsnip::fit(regression.workflow2, data = data_train_with_response)
    # message(sprintf("Training MSE = %g and test MSE = %g.",
    #                 mean((predict(predictive_model2, data_train)$.pred - data_train_with_response$y)^2),
    #                 mean((predict(predictive_model2, data_test)$.pred - data_test_with_response$y)^2)))
    #
    # plot(predict(predictive_model, data_train)$.pred, data_train_with_response$y)
    # plot(predict(predictive_model, data_test)$.pred, data_test_with_response$y)

    # Get the prediction zero, i.e., the phi0 Shapley value.
    prediction_zero = mean(response_train)

    # Collect the variables
    save_list = list(seed_setup = seed_setup,
                     betas = betas,
                     data_train = data_train,
                     data_test = data_test,
                     response_train = response_train,
                     response_test = response_test,
                     data_train_with_response = data_train_with_response,
                     data_test_with_response = data_test_with_response,
                     prediction_zero = prediction_zero,
                     predictive_model = predictive_model)

    # Save the results
    if (file.exists(save_file_name_setup)) warning(paste0("The file `", save_file_name_setup, "` already exists."))
    saveRDS(save_list, save_file_name_setup)

  } else {
    message("Loading the data and predictive model.")

    save_list = readRDS(save_file_name_setup)

    seed_setup = save_list$seed_setup
    betas = save_list$betas
    data_train = save_list$data_train
    data_test = save_list$data_test
    response_train = save_list$response_train
    response_test = save_list$response_test
    data_train_with_response = save_list$data_train_with_response
    data_test_with_response = save_list$data_test_with_response
    prediction_zero = save_list$prediction_zero
    predictive_model = save_list$predictive_model
  }


  # True explanations -----------------------------------------------------------------------------------------------
  if (compute_true_explanations) {
    message(paste0("Start computing the true explanations (n_workers = ", n_workers, ")."))

    # Set future in the right plan
    if (n_workers > 1) {
      future::plan(multisession, workers = n_workers)
    } else {
      future::plan(sequential)
    }

    # Specify the progressr bar
    progressr::handlers("cli")

    # Check if we are in the special case of linear model and Gaussian approach
    # as we can then compute the `precomputed_vS` much faster and more precise.
    if (approach == "gaussian" && class(predictive_model) == "lm") {
      # We are using the Gaussian approach and a linear predictive model

      # Small message to the user
      message(sprintf("Using the LM-Gaussian strategy to compute the true Shapley values (not using `n_samples = %d`).",
                      n_samples_true))

      # We are going to call `shapr::explain()` once to set up the `shapr` object. To do this we do not need
      # to estimate the contribution functions accurately hence we could set n_samples = 1, but it is faster
      # to use a precomputed dt_vS list with just rubbish. We add the special cases for the empty and full set
      # but we let the other entries be 0.
      dt_vS = data.table(id_combination = rep(seq(2^M)))[, `:=` (paste0("p_hat1_", seq(n_test)), 0)]
      dt_vS[id_combination == 1, `:=` (names(dt_vS)[-1], prediction_zero)] # can be directly given as it is a scalar.
      dt_vS[id_combination == .N, `:=` (names(dt_vS)[-1], as.list(response_test))] # need to be a list as it is a vector.

      # Create the shapr object. The Shapley value output will be rubbish, but we only need the object/list structure.
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


      # progressr::with_progress({
      #   true_explanations_tmp_reg <- explain(
      #     model = predictive_model,
      #     x_explain = data_test,
      #     x_train = data_train,
      #     approach = "regression_separate",
      #     prediction_zero = prediction_zero,
      #     keep_samp_for_vS = FALSE,
      #     exact = TRUE,
      #     n_batches = 2^(M-2),
      #     seed = 1
      #   )}, enable = TRUE)

      # Compute the true explanations using the LM-Gaussian strategy
      progressr::with_progress({
        true_explanations = explain_linear_model_Gaussian_data(
          explanation = true_explanations_tmp,
          linear_model = predictive_model)
      }, enable = TRUE)

    } else {
      # Small message to the user
      message("Using the Monte Carlo Integrations strategy to compute the true Shapley values.")

      # We are either using a non-Gaussian approach or a non-linear predictive model
      # Compute the true explanations
      progressr::with_progress({
        true_explanations <- shapr::explain(
          model = predictive_model,
          x_explain = data_test,
          x_train = data_train,
          approach = approach,
          prediction_zero = prediction_zero,
          keep_samp_for_vS = FALSE,
          exact = TRUE,
          n_samples = n_samples_true,
          n_batches = 2^M-1,
          gaussian.mu = mu,
          gaussian.cov_mat = sigma,
          seed = 1
        )}, enable = TRUE)
    }

    # Set future back to sequential plan
    future::plan(sequential)

    # Save the true explanations just in case
    message("Start saving the true explanations.")
    if (file.exists(save_file_name_true)) warning(paste0("The file `", save_file_name_true, "` already exists."))
    saveRDS(true_explanations, save_file_name_true)
    message("Saved the true explanations.")
  } else {
    true_explanations = readRDS(save_file_name_true)
  }


  # Repeated explanations -------------------------------------------------------------------------------------------
  if (compute_repeated_explanations) {
    message("Start computing the repeated estimated explanations.")

    # Iterate over the repetitions
    repetition_idx = 1
    for (repetition_idx in seq_along(repetitions)) {

      # Get the current repetition
      repetition = repetitions[repetition_idx]

      # Small printout to the user
      message(sprintf("Working on rho = %g (%d of %d) and repetition = %d (%d of %d).\n",
                      rho, rho_idx, length(rhos), repetition, repetition_idx, length(repetitions)))

      # Create the save file name
      file_name_update = file_name
      if (use_pilot_estimates_regression) {
        file_name_update = paste(file_name, "pilot", strsplit(pilot_approach_regression, "_")[[1]][2],
                                 sub(".*::([^\\(]+)\\(.*", "\\1",  pilot_regression_model), sep = "_")
      }
      save_file_name_rep = file.path(folder_save, paste0(file_name_update, "_estimated_repetition_", repetition, ".rds"))
      save_file_name_rep_tmp = file.path(folder_save, paste0(file_name_update, "_estimated_repetition_tmp", repetition, ".rds"))

      # Estimated Shapley -----------------------------------------------------------------------------------------------
      # Get the seed for the current repetition
      seed_start_value_now = seed_start_value + repetition - 1

      # # Path to save the results temporary
      # tmp_save_path = paste("~/PhD/Paper3/shapr/Paper3_rds_saves/Paper3_Experiment_M", M, "rho", rho,
      #                       "MC", n_samples, "estimated_specific_tmp.rds", sep = "_")

      # Set if we are doing the computations in parallel or sequential
      if (n_workers > 1) {
        future::plan(multisession, workers = n_workers)
      } else {
        future::plan(sequential)
      }

      # Compute the repeated estimated Shapley values using the different sampling methods
      repeated_estimated_explanations = suppressWarnings(repeated_explanations(
        model = predictive_model,
        x_explain = data_test,
        x_train = data_train,
        approach = approach,
        gaussian.cov_mat = sigma,
        gaussian.mu = mu,
        prediction_zero = prediction_zero,
        keep_samp_for_vS = FALSE,
        n_repetitions = 1,
        n_samples = n_samples,
        n_batches = 2^M-1,#max(1, floor(2^M/20)),
        seed_start_value = seed_start_value_now,
        n_combinations_from = n_combinations_from,
        n_combinations_increment = n_combinations_increment,
        n_combinations_to = 2^ncol(data_test),
        n_combinations_array = n_combinations_array,
        use_precomputed_vS = TRUE,
        use_pilot_estimates_regression = use_pilot_estimates_regression,
        pilot_approach_regression = pilot_approach_regression,
        pilot_regression_model = pilot_regression_model,
        sampling_methods = sampling_methods,
        save_path = save_file_name_rep_tmp,
        true_shapley_values_path = save_file_name_true))
      # model = predictive_model
      # x_explain = data_test
      # x_train = data_train
      # approach = "gaussian"
      # gaussian.cov_mat = sigma
      # gaussian.mu = mu
      # prediction_zero = prediction_zero
      # keep_samp_for_vS = FALSE
      # n_repetitions = 1
      # n_samples = n_samples
      # n_batches = floor(2^M/20)
      # seed_start_value = seed_start_value_now
      # n_combinations_from = n_combinations_from
      # n_combinations_increment = n_combinations_increment
      # n_combinations_array = n_combinations_array
      # use_precomputed_vS = TRUE
      # use_pilot_estimates_regression = TRUE
      # sampling_methods = sampling_methods
      # save_path = save_file_name_rep_tmp

      future::plan(sequential)

      # Save them just in case
      saveRDS(repeated_estimated_explanations, save_file_name_rep)
      file.remove(save_file_name_rep_tmp)
    }

    # Add a white space line after each iteration.
    message("\n")
  }

  # Add a white space line after each iteration.
  message("\n")
}
message("Done with everything.\nPrint warnings if any ('-ne' means no warnings).")

warnings()







