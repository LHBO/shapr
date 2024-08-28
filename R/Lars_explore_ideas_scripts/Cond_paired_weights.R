library(data.table)
library(ggplot2)

# The dimension and number of repetitions
M = 17
M_seq = 17
repetitions = 10

# Data table to sotre the values

dt_res = NULL
repetition = 1
for (repetition in seq(repetitions)) {
  message(paste0("Working on repetition ", repetition ," of ", repetitions ,"."))
  file = readRDS(paste0("/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location/M_17_n_train_1000_n_test_500_rho_0_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_estimated_repetition_", repetition, "_on_all_cond_paired.rds"))
  file_relevant = file$on_all_cond_paired$repetition_1

  # Get the values
  dt_res = rbind(dt_res,
                 data.table::rbindlist(
                   lapply(seq_along(file_relevant),
                          function(idx) {
                            if (idx <= 1) return(NULL)
                            tmp2 = file_relevant[[idx]]$only_save$X
                            if (nrow(tmp2) == 2^M) return(NULL)
                            tmp3 = tmp2[-c(1,.N), c("n_features", "N", "shapley_weight", "cond")][,`:=` (n_combinations = nrow(tmp2), rep = repetition)]
                            return(unique(tmp3))
                          })))
}
dt_res[, M := as.integer(M)]
dt_res[, M := factor(M, levels = M_seq, labels = paste0("M = ", M_seq))]

dt = copy(dt_res)

# Set order
setkeyv(dt, c("M", "n_combinations", "rep", "n_features"))
setcolorder(dt, c("M", "n_combinations", "rep", "n_features", "N", "shapley_weight", "cond"))

# Normalize
dt[, shapley_weight := shapley_weight / sum(shapley_weight), by = c("M", "n_combinations", "rep")]

{
  # To make the file containnt the values
  dt_copy = copy(dt)
  # Compute the mean shapley weight over the repetitions
  dt2 = dt_copy[, as.list(c(mean = sum(shapley_weight)/repetitions,
                            sd = sqrt(sum(shapley_weight - sum(shapley_weight)/repetitions)^2 / repetitions),
                            #sd2 = sd(shapley_weight),
                            quantile(shapley_weight, c(0.025, 0.5, 0.975)))), by = c("M", "n_combinations", "n_features")]
  data.table::setnames(dt2, old = c("2.5%", "50%", "97.5%"), new = c("lower", "median", "upper"))
  data.table::setorderv(dt2, c("M", "n_combinations", "n_features"))
  saveRDS(dt2, paste0("/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location/Analytical_prop_M_", M, "_res.rds"))
}
