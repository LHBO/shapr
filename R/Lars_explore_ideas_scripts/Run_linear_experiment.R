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

# Rscript Run_linear_experiment.R TRUE TRUE FALSE 1:10 4 1000000 1000000 1000 250 5 0.7 NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE 1:10 4 1000000 1000000 1000 250 5 0.7 NULL

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

# Rscript Run_linear_experiment.R FALSE FALSE TRUE TRUE 1:10 4 1000000 1000000 1000 250 5 0.7 NULL NULL NULL

# Rscript Run_linear_experiment.R TRUE TRUE TRUE TRUE 1:10 6 1000000 1000000 1000 500 12 0.0 NULL NULL NULL (sumeru)
# Rscript Run_linear_experiment.R TRUE TRUE TRUE TRUE 1:10 6 1000000 1000000 1000 500 12 0.3 NULL NULL NULL (bastet)
# Rscript Run_linear_experiment.R TRUE TRUE TRUE TRUE 1:10 6 1000000 1000000 1000 500 12 0.6 NULL NULL NULL (nyx)
# Rscript Run_linear_experiment.R TRUE TRUE TRUE TRUE 1:10 6 1000000 1000000 1000 500 12 0.9 NULL NULL NULL (ixion)

# Rscript Run_linear_experiment.R TRUE TRUE TRUE TRUE 1:10 32 1000000 1000000 1000 500 15 0.3,0.6,0.9,0.0 NULL NULL NULL (nam2)

# Rscript Run_linear_experiment.R TRUE TRUE TRUE TRUE 1:10 6 1000000 1000000 1000 500 12 0.6 NULL regression_separate NULL (sidana)
# Rscript Run_linear_experiment.R TRUE TRUE TRUE TRUE 1:10 6 1000000 1000000 1000 500 10 0.9 NULL regression_separate NULL (sumeru)
# Rscript Run_linear_experiment.R TRUE TRUE TRUE FALSE 1:10 6 1000000 1000000 1000 500 10 0.9 NULL NULL NULL

# Rscript Run_linear_experiment.R TRUE TRUE TRUE TRUE 1:10 6 1000000 1000000 1000 1000 10 0.7 NULL regression_separate NULL (sidana)
# Rscript Run_linear_experiment.R TRUE TRUE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 10 0.7 NULL NULL NULL (Sumeru)

# Rscript Run_linear_experiment.R TRUE TRUE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 10 0.21 1,1,1,1,1,1,1,1,1,1,1 NULL NULL
# Rscript Run_linear_experiment.R TRUE TRUE TRUE TRUE 1:10 6 1000000 1000000 1000 1000 10 0.21 1,1,1,1,1,1,1,1,1,1,1 regression_separate NULL
# Rscript Run_linear_experiment.R TRUE TRUE TRUE TRUE 1:10 6 1000000 1000000 1000 1000 10 0.21 1,1,1,1,1,1,1,1,1,1,1 regression_surrogate NULL

# Rscript Run_linear_experiment.R TRUE TRUE FALSE FALSE 1:10 6 1000000 1000000 1000 1000 12 0.0,0.3,0.6,0.9 TRUE 1,1,1,1,1,1,1,1,1,1,1,1,1 NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 12 0.0,0.3,0.6,0.9 TRUE 1,1,1,1,1,1,1,1,1,1,1,1,1 NULL NULL (adroa)
# Rscript Run_linear_experiment.R FALSE FALSE TRUE TRUE 1:10 6 1000000 1000000 1000 1000 12 0.0,0.3,0.6,0.9 TRUE 1,1,1,1,1,1,1,1,1,1,1,1,1 regression_separate NULL (bastet)
# Rscript Run_linear_experiment.R FALSE FALSE TRUE TRUE 1:10 6 1000000 1000000 1000 1000 12 0.0,0.3,0.6,0.9 TRUE 1,1,1,1,1,1,1,1,1,1,1,1,1 regression_surrogate NULL (ixion)

# Rscript Run_linear_experiment.R TRUE TRUE FALSE FALSE 1:10 6 1000000 1000000 1000 1000 12 0.0,0.3,0.6,0.9 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 12 0.0,0.3,0.6,0.9 TRUE NULL NULL NULL (nyx)
# Rscript Run_linear_experiment.R FALSE FALSE TRUE TRUE 1:10 6 1000000 1000000 1000 1000 12 0.0,0.3,0.6,0.9 TRUE NULL regression_separate NULL (sidana)
# Rscript Run_linear_experiment.R FALSE FALSE TRUE TRUE 1:10 6 1000000 1000000 1000 1000 12 0.0,0.3,0.6,0.9 TRUE NULL regression_surrogate NULL (sumeru)

#Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:2 6 1000000 1000000 1000 1000 10 0.0,0.1,0.3 TRUE NULL NULL NULL (Sumeru)
#Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:2 6 1000000 1000000 1000 1000 10 0.6,0.9,0.99 TRUE NULL NULL NULL ()
#Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:2 6 1000000 1000000 1000 1000 10 0.0,0.1,0.3 TRUE 1,1,1,1,1,1,1,1,1,1,1 NULL NULL (adroa)
#Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:2 6 1000000 1000000 1000 1000 10 0.6,0.9,0.99 TRUE 1,1,1,1,1,1,1,1,1,1,1 NULL NULL ()

# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.04 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.14 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.34 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.64 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.84 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.94 TRUE NULL NULL NULL


# Rscript Run_linear_experiment.R TRUE TRUE FALSE FALSE 1:5 6 1000000 1000000 1000 1000 12 0.01,0.11,0.21,0.31,0.41,0.51,0.61,0.71,0.81,0.91 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 12 0.01 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 12 0.11 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 12 0.31 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 12 0.41 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 12 0.61 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 12 0.71 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 12 0.81 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 12 0.91 TRUE NULL NULL NULL


# Rscript Run_linear_experiment.R TRUE TRUE FALSE FALSE 1:5 6 1000000 1000000 1000 1000 10 0,0.05,0.1,0.2,0.5,0.7,0.9 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0,0.05,0.1 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.2,0.5,0.7,0.9 TRUE NULL NULL NULL

# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 10 0 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 10 0.05 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 10 0.1 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 10 0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 10 0.5 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 10 0.7 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 10 0.9 FALSE NULL NULL NULL


# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.05 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.1 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.2 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.5 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.7 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 10 0.9 TRUE NULL NULL NULL



# New sampling frequency pilot methods
# Rscript Run_linear_experiment.R TRUE TRUE FALSE FALSE 6:10 6 1000000 1000000 1000 1000 9 0,0.05,0.1,0.2,0.5,0.7,0.9 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R TRUE TRUE FALSE FALSE 6:10 6 1000000 1000000 1000 1000 9 0,0.05,0.1,0.2,0.5,0.7,0.9 FALSE NULL NULL NULL

# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 9 0,0.05 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 9 0.1,0.2 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 9 0.5,0.7 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 9 0.3,0.9 TRUE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 9 0,0.05 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 9 0.1,0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 9 0.5,0.7 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 9 0.3,0.9 FALSE NULL NULL NULL


#Rscript Run_linear_experiment.R TRUE TRUE FALSE FALSE 1:5 6 1000000 1000000 1000 1000 9 0.5 FALSE NULL NULL NULL
#Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:5 6 1000000 1000000 1000 1000 9 0.5 FALSE NULL NULL NULL


# High-dim --------------------------------------------------------------------------------------------------------
# Rscript Run_linear_experiment.R TRUE TRUE FALSE FALSE 1:10 6 1000000 1000000 1000 1000 20 0,0.2,0.5,0.9 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1:10 6 1000000 1000000 1000 1000 14 0,0.2,0.5,0.9 FALSE NULL NULL NULL




# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 9:10 4 1000000 1000000 1000 500 15 0,0.2,0.5,0.9 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 2:3 4 1000000 1000000 1000 500 17 0,0.2,0.5,0.9 FALSE NULL NULL NULL



# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1 4 1000000 1000000 1000 500 17 0 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1 4 1000000 1000000 1000 500 17 0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1 4 1000000 1000000 1000 500 17 0.5 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1 4 1000000 1000000 1000 500 17 0.9 FALSE NULL NULL NULL
#
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 2 4 1000000 1000000 1000 500 17 0.9 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 2 4 1000000 1000000 1000 500 17 0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 2 4 1000000 1000000 1000 500 17 0.5 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 2 4 1000000 1000000 1000 500 17 0 FALSE NULL NULL NULL
#
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 3 4 1000000 1000000 1000 500 17 0.5 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 3 4 1000000 1000000 1000 500 17 0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 3 4 1000000 1000000 1000 500 17 0.9 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 3 4 1000000 1000000 1000 500 17 0 FALSE NULL NULL NULL
#
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 4 4 1000000 1000000 1000 500 17 0.9 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 4 4 1000000 1000000 1000 500 17 0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 4 4 1000000 1000000 1000 500 17 0.5 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 4 4 1000000 1000000 1000 500 17 0 FALSE NULL NULL NULL
#
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 5 4 1000000 1000000 1000 500 17 0 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 5 4 1000000 1000000 1000 500 17 0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 5 4 1000000 1000000 1000 500 17 0.5 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 5 4 1000000 1000000 1000 500 17 0.9 FALSE NULL NULL NULL
#
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 6 4 1000000 1000000 1000 500 17 0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 6 4 1000000 1000000 1000 500 17 0.5 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 6 4 1000000 1000000 1000 500 17 0 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 6 4 1000000 1000000 1000 500 17 0.9 FALSE NULL NULL NULL
#
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 7 4 1000000 1000000 1000 500 17 0.9 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 7 4 1000000 1000000 1000 500 17 0.5 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 7 4 1000000 1000000 1000 500 17 0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 7 4 1000000 1000000 1000 500 17 0 FALSE NULL NULL NULL
#
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 8 4 1000000 1000000 1000 500 17 0 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 8 4 1000000 1000000 1000 500 17 0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 8 4 1000000 1000000 1000 500 17 0.5 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 8 4 1000000 1000000 1000 500 17 0.9 FALSE NULL NULL NULL
#
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 9 4 1000000 1000000 1000 500 17 0 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 9 4 1000000 1000000 1000 500 17 0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 9 4 1000000 1000000 1000 500 17 0.5 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 9 4 1000000 1000000 1000 500 17 0.9 FALSE NULL NULL NULL
#
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 10 4 1000000 1000000 1000 500 17 0.9 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 10 4 1000000 1000000 1000 500 17 0.5 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 10 4 1000000 1000000 1000 500 17 0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 10 4 1000000 1000000 1000 500 17 0 FALSE NULL NULL NULL



# Rscript Run_linear_experiment.R TRUE TRUE FALSE FALSE 2:3 4 1000000 1000000 1000 500 20 0,0.2,0.5,0.9 FALSE NULL NULL NULL


# Rscript Run_linear_experiment.R TRUE TRUE FALSE FALSE 1:10 16 1000000 1000000 1000 250 20 0,0.2,0.5,0.9 FALSE NULL NULL NULL


# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1 4 1000000 1000000 1000 250 20 0.9 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1 4 1000000 1000000 1000 250 20 0.5 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1 4 1000000 1000000 1000 250 20 0.2 FALSE NULL NULL NULL
# Rscript Run_linear_experiment.R FALSE FALSE TRUE FALSE 1 4 1000000 1000000 1000 250 20 0 FALSE NULL NULL NULL



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
repetitions = 1
n_workers = 1
n_samples_true = 1000000
n_samples = 1000000
n_train = 1000
n_test = 1000
M = 9
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
  betas = c(2, 10, 0.25, -3, -1, 1.5, -0.5, 10, 1.25, 1.5, -2, 3, -1, -5, 4, -10, 2, 5, -0.5, -1, -2)
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
if (Sys.info()[[4]] %in% c("nam-shub-01.uio.no", "nam-shub-02.uio.no") || R.utils::System$getHostname() %in% c("nam-shub-01.uio.no", "nam-shub-02.uio.no") ) {

  # devtools::clean_dll()
  # # devtools::install_github(repo = "LHBO/shapr", ref = "Lars/paper3_ideas")
  # devtools::install_github(repo = "LHBO/shapr", ref = "Lars/paper3_ideas")
  #devtools::load_all(".")
  library(shapr)
} else {
  devtools::load_all(".")
}
#devtools::install_github(repo = "LHBO/shapr", ref = "Lars/paper3_ideas")
# library(shapr)
# devtools::load_all(".")


# Libraries -------------------------------------------------------------------------------------------------------
library(data.table)
library(mvtnorm)
library(condMVNorm)
suppressPackageStartupMessages(library(mgcv))
library(progressr)
library(cli)
library(future)


options(future.globals.maxSize = 5000*1024^2)


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

sampling_methods = c(
  "largest_weights_random",
  "largest_weights_random_new_weights_empirical",
  "MAD",
  "MAD_new_weights_empirical",
  "paired_coalitions_new_weights_empirical",
  "paired_coalitions_weights_direct_equal_weights_new_weights_empirical",
  "paired_coalitions",
  "paired_coalitions_weights_direct_equal_weights",
  "largest_weights_new_weights_empirical",
  "unique_paired_new_weights_empirical",
  "unique",
  "unique_paired",
  "unique_paired_equal_weights",
  "unique_paired_SW",
  "largest_weights"
  #"largest_weights_combination_size",
  #"largest_weights_combination_size_new_weights_empirical"
)

sampling_methods = c(
  "unique",
  "unique_paired",
  "unique_paired_equal_weights",
  "unique_paired_SW",
  "largest_weights_random"
  # "largest_weights_random_new_weights_empirical",
  # "MAD",
  # "MAD_new_weights_empirical",
  # "paired_coalitions_new_weights_empirical",
  # "paired_coalitions_weights_direct_equal_weights_new_weights_empirical",
  # "paired_coalitions",
  # "paired_coalitions_weights_direct_equal_weights",
  # "largest_weights_new_weights_empirical",
  # "unique_paired_new_weights_empirical",
  # "largest_weights"
  #"largest_weights_combination_size",
  #"largest_weights_combination_size_new_weights_empirical"
)



# sampling_methods = c("paired_coalitions_weights_direct_equal_weights_new_weights_gompertz",
#                      "unique_paired_new_weights_gompertz",
#                      "paired_coalitions_new_weights_gompertz",
#                      "unique_paired_new_weights_empirical",
#                      "paired_coalitions_new_weights_empirical",
#                      "paired_coalitions_weights_direct_equal_weights_new_weights_empirical",
#                      "unique",
#                      "unique_paired",
#                      "unique_paired_equal_weights",
#                      "unique_paired_SW",
#                      "paired_coalitions",
#                      "paired_coalitions_weights_direct_equal_weights",
#                      "largest_weights",
#                      "largest_weights_combination_size")


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

if (M > 12) n_combinations_increment = 500

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

if (M <= 8) n_combinations_array = seq(2, 2^M)

if (M == 17) {
  n_combinations_array = c(seq(2, 200, 2), 250, 500, 750, 1000, 2500, 5000, 10000, 20000, 30000, 40000, 50000, 60000, 70000,
                           80000, 90000, 100000, 110000, 120000, 130000, 131000)

  n_combinations_vec_extra <- sapply(seq(ceiling((M - 1)/2)), choose, n = M)
  n_combinations_vec_extra[seq(floor((M - 1)/2))] = 2*n_combinations_vec_extra[seq(floor((M - 1)/2))]
  n_combinations_vec_extra = cumsum(n_combinations_vec_extra) + 2
  n_combinations_vec_extra = n_combinations_vec_extra[-length(n_combinations_vec_extra)]
  n_combinations_array = n_combinations_vec_extra[!(n_combinations_vec_extra %in% n_combinations_array)]
}
length(n_combinations_array)

if (M == 20) {
  n_combinations_array = c(seq(10, 200, 10), 250, 500, 750, seq(1000, 9000, 1000), c(seq(10000, 1040000, 10000), seq(1040000, 1048000, 1000)))
  n_combinations_array = c(seq(10, 200, 10), 250, 500, 750, seq(1000, 9000, 1000), c(seq(250000, 1000000, 25000)))
  n_combinations_vec_extra <- sapply(seq(ceiling((M - 1)/2)), choose, n = M)
  n_combinations_vec_extra[seq(floor((M - 1)/2))] = 2*n_combinations_vec_extra[seq(floor((M - 1)/2))]
  n_combinations_vec_extra = cumsum(n_combinations_vec_extra[-length(n_combinations_vec_extra)]) + 2
  n_combinations_vec_extra = c(n_combinations_vec_extra - 50, n_combinations_vec_extra, n_combinations_vec_extra + 50)
  n_combinations_vec_extra = n_combinations_vec_extra[-length(n_combinations_vec_extra)]
  n_combinations_array = sort(unique(c(n_combinations_array, n_combinations_vec_extra)))
  #n_combinations_array = c(n_combinations_array, 1048500)
  n_combinations_array = n_combinations_array[n_combinations_array > 2]
  n_combinations_array = n_combinations_array[n_combinations_array <= 1048501]

}
length(n_combinations_array)


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
    sigma = as.matrix(do.call(Matrix::bdiag, lapply(c(3,4,3,5,2,2,1), function(dim_now) matrix(rho, ncol = dim_now, nrow = dim_now))))
    sigma = sigma[1:M, 1:M]
  }
  diag(sigma) = 1

  # Make some of the save file names
  file_name = paste("M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "equi", rho_equi,
                    "betas", paste(as.character(betas), collapse = "_"), sep = "_")
  save_file_name_setup = file.path(folder_save, paste0(file_name, "_model.rds"))
  save_file_name_true = file.path(folder_save, paste0(file_name, "_true.rds"))

  if (do_setup) {
    message("Create the data and predictive model.")

    # Set seed for reproducibility
    seed_setup = 1996
    set.seed(seed_setup)

    # Make Gaussian data
    data_train = data.table(rmvnorm(n = n_train, mean = mu, sigma = sigma))
    data_test  = data.table(rmvnorm(n = n_test,  mean = mu, sigma = sigma))
    colnames(data_train) = paste("X", seq(M), sep = "")
    colnames(data_test) = paste("X", seq(M), sep = "")

    # Make the response
    response_train = as.vector(cbind(1, as.matrix(data_train)) %*% betas)
    response_test = as.vector(cbind(1, as.matrix(data_test)) %*% betas)

    # Put together the data
    data_train_with_response = copy(data_train)[,y := response_train]
    data_test_with_response  = copy(data_test)[,y := response_test]

    # X = cbind(1, as.matrix(data_train))
    # solve(t(X) %*% X) %*% t(X) %*% response_train



    # Predictive model ------------------------------------------------------------------------------------------------
    # Fit a GAM model.
    predictive_model = gam(as.formula(paste0("y ~ ", paste0("ti(X", seq(M), ")", collapse = " + "))),
                           data = data_train_with_response)
    predictive_model = lm(y ~ ., data = data_train_with_response)


    # Look at the accuracy of the model
    message(sprintf("Training MSE = %g and test MSE = %g.",
                mean((predict(predictive_model, data_train_with_response) - data_train_with_response$y)^2),
                mean((predict(predictive_model, data_test_with_response) - data_test_with_response$y)^2)))

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
          n_combinations = 2^M, # Do not need it as we specify `exact = TRUE`.
          gaussian.mu = mu,
          gaussian.cov_mat = sigma,
          seed = 1,
          precomputed_vS = list(dt_vS = dt_vS),
          only_return_internal = TRUE
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
        true_explanations <- explain(
          model = predictive_model,
          x_explain = data_test,
          x_train = data_train,
          approach = approach,
          prediction_zero = prediction_zero,
          keep_samp_for_vS = TRUE,
          exact = TRUE,
          n_samples = n_samples_true,
          n_batches = 2^(M-2),
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


      message("Loading presampled coalitions unique")
      presampled_coalitions_unique = file.path(folder_save, paste0("Unique_sampling_M_", M, "_repetition_", repetition, ".rds"))
      if (file.exists(presampled_coalitions_unique)) {
        presampled_coalitions_unique = readRDS(presampled_coalitions_unique)
      } else {
        presampled_coalitions_unique = NULL
      }
      message("Done loading presampled coalitions unique")
      message("Converting presampled coalitions to integers unique")
      presampled_coalitions_unique$all_coalitions = lapply(stringr::str_split(presampled_coalitions_unique$all_coalitions, ','), as.integer)
      message("Done presampled coalitions to integers unique")


      message("Loading presampled coalitions paired")
      presampled_coalitions_paired = file.path(folder_save, paste0("Paired_sampling_M_", M, "_repetition_", repetition, ".rds"))
      if (file.exists(presampled_coalitions_paired)) {
        presampled_coalitions_paired = readRDS(presampled_coalitions_paired)
      } else {
        presampled_coalitions_paired = NULL
      }
      message("Done loading presampled coalitions paired")
      message("Converting presampled coalitions to integers paired")
      presampled_coalitions_paired$all_coalitions = lapply(stringr::str_split(presampled_coalitions_paired$all_coalitions, ','), as.integer)
      message("Done presampled coalitions to integers paired")


      # Create the save file name
      file_name_update = file_name
      if (use_pilot_estimates_regression) {
        file_name_update = paste(file_name, "pilot", strsplit(pilot_approach_regression, "_")[[1]][2],
                                  sub(".*::([^\\(]+)\\(.*", "\\1",  pilot_regression_model), sep = "_")
      }
      save_file_name_rep = file.path(folder_save, paste0(file_name_update, "_estimated_repetition_", repetition, "_extra.rds"))
      save_file_name_rep_tmp = file.path(folder_save, paste0(file_name_update, "_estimated_repetition_tmp", repetition, "_extra.rds"))

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
      repeated_estimated_explanations =
        # suppressWarnings(
        repeated_explanations(
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
        n_batches = max(1, floor(2^M/20)),
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
        true_shapley_values_path = save_file_name_true,
        presampled_coalitions_unique = presampled_coalitions_unique,
        presampled_coalitions_paired = presampled_coalitions_paired)
      #)
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







