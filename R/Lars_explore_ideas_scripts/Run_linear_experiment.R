# cd ~/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts
# Rscript Run_linear_experiment.R 7 0.5 1000 250 TRUE FALSE 1 TRUE 250
# Rscript Run_linear_experiment.R 7 0.5 1000 250 FALSE TRUE 1 FALSE 250
# Rscript Run_linear_experiment.R 7 0.5 1000 250 FALSE TRUE 1:5 FALSE 250
# Rscript Run_linear_experiment.R 7 0.5 1000 250 FALSE TRUE 6:10 FALSE 250

#Rscript Run_linear_experiment.R 10 0.0 1000 250 TRUE FALSE 1 TRUE 250
#Rscript Run_linear_experiment.R 10 0.0 1000 250 FALSE TRUE 1:5 FALSE 250
#Rscript Run_linear_experiment.R 10 0.0 1000 250 FALSE TRUE 6:10 FALSE 250
#
#
# module load R/4.2.1-foss-2022a
# cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R
# git checkout Lars/paper3_ideas
# cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts
# Rscript Run_linear_experiment.R 10 0.0 1000 250 TRUE FALSE 1 TRUE 250 NULL

# Input From Command Line -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
args = commandArgs(trailingOnly = TRUE)
# test if there is at least one argument: if not, return an error
if (length(args) < 10) {
  stop("Must provide if we run onlyon UiO(boolean), do setup(boolean), compute true Shapley values(boolean), do sampling(boolean),
       do regression(boolean), do evaluation(boolean), and the correlations rho or scale burr_p.\n", call.=FALSE)
}

# Extract the number of features
M = as.integer(args[1])

# Extract the correlation level
rhos = unlist(strsplit(args[2], ","))
if (length(rhos) > 1) {
  rhos = unname(sapply(rhos, function(i) as.numeric(i)))
} else {
  rhos = as.numeric(rhos)
}

# Extract the number of training observations
n_train = as.numeric(args[3])

# Extract the number of test observations
n_test = as.numeric(args[4])

# Extract if we are to compute the true Shapley values
compute_true_explanations = as.logical(args[5])

# Extract if we are to compute the repeated estimated Shapley values
compute_repeated_explanations = as.logical(args[6])

# Extract which repetition we are to do
repetitions = as.character(args[7])
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

# Extract if we are to generate the data and model
do_setup = as.logical(args[8])

# Extract the number of MC samples
n_samples = as.numeric(args[9])

# # Extract the number of cores
# n_workers = as.numeric(args[8])
# n_max_workers = future::availableCores()
# if (n_workers > n_max_workers) {
#   warning(sprintf("Too many workers. Change from %d to %d (max available cores).",
#                   n_workers, n_max_workers))
# }

# Extract the correlation level
betas = as.character(args[10])
print(betas)
if (betas != "NULL") {
  print("hei")
  betas = unlist(strsplit(betas, ","))
  if (length(betas) > 1) {
    betas = unname(sapply(betas, function(i) as.numeric(i)))
  } else {
    betas = as.numeric(betas)
  }
  print(betas)
} else {
  # Create the beta vector
  betas = c(0, rep(1, M))
  betas = c(2, 1, 0.25, -3, -1, 1.5, -0.5, 0.75, 1.25, 1.5, -2)
  betas = betas[seq(M+1)]
}
print(betas)


# Setup -----------------------------------------------------------------------------------------------------------
# Get the name of the computer we are working on
hostname = R.utils::System$getHostname()
cat(sprintf("We are working on '%s'.\n", R.utils::System$getHostname()))

# If we are working on UiO computer or not
UiO = NULL

# set the working directory and define the correct folder based on system
if (hostname == "Larss-MacBook-Pro.local" || Sys.info()[[7]] == "larsolsen") {
  folder = "/Users/larsolsen/PhD/Paper3/shapr"
  folder_save = file.path(folder, "Paper3_rds_saves")
  # basename(folder)
  # dirname(folder)
  UiO = FALSE

} else if (grepl("hpc.uio.no", hostname)) {
  # TBA
  folder = ""

  UiO = TRUE

} else if (grepl("uio.no", hostname)) {
  # TBA
  folder = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr"
  folder_save = file.path(folder, "Paper3_rds_saves")

  UiO = TRUE

} else {
  stop("We do not recongize the system at which the code is run (not Lars's MAC, HPC, nor UiO).")
}

# Set the working directory
setwd(folder)

source(file.path(folder, "R/Lars_explore_ideas_scripts/new_functions.R"))

message("loading my version of the package")
#library(shapr)
#setwd("~/PhD/Paper3/Shapr_Lars_paper3/R")
if (UiO) {
  #devtools::clean_dll()
}
devtools::load_all(".")
message("done")

# Libraries -------------------------------------------------------------------------------------------------------
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(mgcv)
library(progressr)
library(cli)
library(future)



# Fixed parameters ------------------------------------------------------------------------------------------------------
# Mean of the multivariate Gaussian distribution
mu = rep(0, times = M)

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
                     "smallest_weights_combination_size")

# First value where the coalition/combination sampling scheme has an effect, (first two are empty and full coalitions)
n_combinations_from = 2

# We increase by one each time
n_combinations_increment = 4

# We start with 2 ad we used 1 for the true Shapley values above.
seed_start_value = 2


# Iterate over the rhos
rho_idx = 1
for (rho_idx in seq_along(rhos)) {
  # Get the current rho
  rho = rhos[rho_idx]

  # Create the covariance matrix
  sigma = matrix(rho, ncol = M, nrow = M) # Old
  for (i in seq(1, M-1)) {
    for (j in seq(i+1, M))
      sigma[i,j] = sigma[j,i] = rho^abs(i-j)
  }
  diag(sigma) = 1

  # Make some of the save file names
  file_name = paste("Paper3_Experiment_M", M, "n_train", n_train, "n_test", n_test,  "rho", rho, "betas",
                    paste(as.character(betas), collapse = "_"), sep = "_")
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



    # Predictive model ------------------------------------------------------------------------------------------------
    # Fit a GAM model.
    predictive_model = gam(as.formula(paste0("y ~ ", paste0("ti(X", seq(M), ")", collapse = " + "))),
                           data = data_train_with_response)

    # Look at the accuracy of the model
    cat(sprintf("Training MSE = %g and test MSE = %g.\n",
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
    saveRDS(save_list, save_file_name_setup)

  } else {

    message("Load the data and predictive model.")

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

  if (compute_true_explanations) {
    message("Start computing the true explanations.")
    if (n_workers > 1) {
      future::plan(multisession, workers = n_workers)
    } else {
      future::plan(sequential)
    }
    progressr::handlers("cli")
    progressr::with_progress({
      true_explanations <- explain(
      model = predictive_model,
      x_explain = data_test,
      x_train = data_train,
      approach = "gaussian",
      prediction_zero = prediction_zero,
      keep_samp_for_vS = FALSE,
      n_combinations = 2^M,
      n_samples = 2500,
      n_batches = 2^(M-2),
      gaussian.mu = mu,
      gaussian.cov_mat = sigma,
      seed = 1
    )}, enable = TRUE)
    message("Start saving the true explanations.")
    future::plan(sequential)

    # Save them just in case
    saveRDS(true_explanations, save_file_name_true)
    message("Saved the true explanations.")
  }

  if (compute_repeated_explanations) {
    message("Start computing the repeated estimated explanations.")

    # Iterate over the repetitions
    for (repetition_idx in seq_along(repetitions)) {

      # Get the current repetition
      repetition = repetitions[repetition_idx]

      # Small printout to the user
      cat(sprintf("Working on rho = %g (%d of %d) and repetition = %d (%d of %d).\n",
                  rho, rho_idx, length(rhos), repetition, repetition_idx, length(repetitions)))

      # Create the save file name
      save_file_name_rep = file.path(folder_save, paste0(file_name, "_estimated_repetition_", repetition, ".rds"))
      save_file_name_rep_tmp = file.path(folder_save, paste0(file_name, "_estimated_repetition_", repetition, "_tmp.rds"))

      # Estimated Shapley -----------------------------------------------------------------------------------------------
      # Get the seed for the current repetition
      seed_start_value_now = seed_start_value + repetition - 1

      # Path to save the results temporary
      tmp_save_path = paste("~/PhD/Paper3/shapr/Paper3_rds_saves/Paper3_Experiment_M", M, "rho", rho,
                            "MC", n_samples, "estimated_tmp.rds", sep = "_")

      if (n_workers > 1) {
        future::plan(multisession, workers = n_workers)
      } else {
        future::plan(sequential)
      }
      # Compute the repeated estimated Shapley values using the different sampling methods
      repeated_estimated_explanations = repeated_explanations(
        model = predictive_model,
        x_explain = data_test,
        x_train = data_train,
        approach = "gaussian",
        gaussian.cov_mat = sigma,
        gaussian.mu = mu,
        prediction_zero = prediction_zero,
        keep_samp_for_vS = FALSE,
        n_repetitions = 1,
        n_samples = n_samples,
        n_batches = floor(2^M/20),
        seed_start_value = seed_start_value_now,
        n_combinations_from = n_combinations_from,
        n_combinations_increment = n_combinations_increment,
        use_precomputed_vS = TRUE,
        sampling_methods = sampling_methods,
        save_path = save_file_name_rep_tmp)

      future::plan(sequential)

      # Save them just in case
      saveRDS(repeated_estimated_explanations, save_file_name_rep)
      file.remove(save_file_name_rep_tmp)
    }
  }
}
message("Done with everything. Print warnings if any.")

warnings()
print(warnings())







