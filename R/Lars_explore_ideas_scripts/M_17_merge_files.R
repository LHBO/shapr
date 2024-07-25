library(data.table)

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

compute_MAE_MSE_fast = function(mat_1, mat_2, evaluation_criterion = c("MSE", "MAE")) {
  evaluation_criterion = match.arg(evaluation_criterion)
  if (evaluation_criterion == "MSE") mean((mat_1[,-1] - mat_2[,-1])^2) else mean(abs(mat_1[,-1] - mat_2[,-1]))
}


# The parameters of the expirment
M = 17
n_train = 1000
n_test = 500
rho_equi = FALSE
name_prefix = ""

betas = c(2, 10, 0.25, -3, -1, 1.5, -0.5, 10, 1.25, 1.5, -2, 3, -1, -5, 4, -10, 2, 5, -0.5, -1, -2)
betas = betas[seq(M+1)]
use_pilot_estimates_regression = FALSE
max_repetitions = 50


# The values of rho that we have to fix
rhos = 0.5#c(0.9, 0.5)# c(0.9, 0.2, 0.5)
# Ferdig 0

# Iterate over the rhos
rho_idx = 2
for (rho_idx in seq(length(rhos))) {
  rho = rhos[rho_idx]

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

  # #  Find the relevant files in the folder and their repetition numbers/indices
  # files_in_dir = list.files(folder_save)
  # relevant_files_in_dir = files_in_dir[grepl(paste0(file_name_updated, "_estimated_repetition_"), files_in_dir)]
  # relevant_files_in_dir = relevant_files_in_dir[!grepl("tmp", relevant_files_in_dir)] # remove any tmp files
  # if (length(relevant_files_in_dir) == 0) {
  #   stop(paste0("Cannot find any files for the provided paremeters. ",
  #               "Looking for file name structures '", file_name, "'."))
  # }
  # relevant_repetitions =
  #   sort(as.integer(sapply(strsplit(unlist(strsplit(relevant_files_in_dir, '.rds')), '\\_'), tail, 1)))
  # relevant_repetitions = relevant_repetitions[seq(min(max_repetitions, length(relevant_repetitions)))]
  # if (!is.null(max_repetitions) && max_repetitions > length(relevant_repetitions)) {
  #   message(paste0("The parameter `max_repetitions` (", max_repetitions, ") is larger than the number of available ",
  #                  "repetitions (", length(relevant_repetitions), "). Use all available repetitions.\n"))
  # }


  # Iterate over the repetitions
  repetition_idx = 1
  if (rho == 0) relevant_repetitions = 1:9
  if (rho == 0.2) relevant_repetitions = 3:9 # Mangler 3
  if (rho == 0.5) relevant_repetitions = c(7,8,9,10) #c(1, 6:9)
  if (rho == 0.9) relevant_repetitions = c(8,7,6) #c(2, 6:9)
  # Mangler 1, 10
  for (repetition_idx in seq_along(relevant_repetitions)) {

    # Get the current repetition
    repetition = relevant_repetitions[repetition_idx]
    #if (repetition == 7) next # Already fixed 7

    # Small printout to the user
    cat(sprintf("Working on rho = %g (%d of %d) and repetition = %d (%d of %d).\n",
                rho, rho_idx, length(rhos), repetition, repetition_idx, length(relevant_repetitions)))

    # Create the save file name
    save_file_name_rep = file.path(folder_save, paste0(file_name_updated, "_estimated_repetition_", repetition, ".rds"))
    save_file_name_rep_tmp = file.path(folder_save, paste0(file_name_updated, "_estimated_repetition_", repetition, "tmp_new.rds"))
    save_file_name_rep_extra = file.path(folder_save, paste0(file_name_updated, "_estimated_repetition_", repetition, "_extra.rds"))

    # We only combine them if they exists
    if (!file.exists(save_file_name_rep)) next
    if (!file.exists(save_file_name_rep_extra)) next

    # Load the rds file
    message("Start reading original...")
    current_repetition_results = readRDS(save_file_name_rep)
    message("Done reading original...")

    message("Start reading extra...")
    current_repetition_results_extra = readRDS(save_file_name_rep_extra)
    message("Done reading extra...")

    if (!isTRUE(all.equal(names(current_repetition_results), names(current_repetition_results_extra)))) {
      print(all.equal(names(current_repetition_results), names(current_repetition_results_extra)))
    }

    # Iterate over the sampling methods and merge the lists
    for (sampling_method in names(current_repetition_results)) {
      if (sampling_method == "True_vs_Pilot_Order") next
      print(sampling_method)
      list1 = current_repetition_results[[sampling_method]]$repetition_1
      list2 = current_repetition_results_extra[[sampling_method]]$repetition_1

      if (is.null(list1)) next
      if (is.null(list2)) next

      list2[[1]]$only_save = list(X = list2[[1]]$internal$objects$X,
                                  W = list2[[1]]$internal$objects$W,
                                  S = list2[[1]]$internal$objects$S)
      list2[[1]]$internal = NULL
      list2[[1]]$pred_explain = NULL
      list2[[1]]$timing = NULL

      list_new = c(list1, list2)
      #print(names(list_new))
      new_order = order(as.integer(sapply(strsplit(names(list_new), "_(?!.*_)", perl=TRUE), "[[", 2)))
      list_new = list_new[new_order]
      #print(names(list_new))
      # print(sampling_method)
      current_repetition_results[[sampling_method]]$repetition_1 = list_new
    }


    # Save the updated version of the results
    message("Start saving...")
    saveRDS(current_repetition_results, save_file_name_rep_tmp)
    message("Done saving...")
    file.rename(save_file_name_rep_tmp, save_file_name_rep)
    file.remove(save_file_name_rep_extra)
  }



  # current_repetition_results = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_0_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_estimated_repetition_7.rds")
  #
  # current_repetition_results_extra = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_0_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_estimated_repetition_8.rds")
  #
  #
  # current_repetition_results_extra = current_repetition_results_extra[1:2]
  # current_repetition_results_extra$paired_coalitions_weights_direct_equal_weights_new_weights_gompertz$repetition_1 = current_repetition_results_extra$paired_coalitions_weights_direct_equal_weights_new_weights_gompertz$repetition_1[4]
  # names(current_repetition_results_extra$paired_coalitions_weights_direct_equal_weights_new_weights_gompertz$repetition_1) = "n_combinations_3"
  # current_repetition_results_extra$unique_paired_new_weights_gompertz$repetition_1 = current_repetition_results_extra$unique_paired_new_weights_gompertz$repetition_1[4]
  # names(current_repetition_results_extra$unique_paired_new_weights_gompertz$repetition_1) = "n_combinations_3"
  #
  # current_repetition_results_extra
  #
  #
  #
  # for (sammpling_mehtod in names)
  #
}






