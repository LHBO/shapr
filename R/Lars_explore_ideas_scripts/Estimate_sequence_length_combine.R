library(data.table)
library(shapr)
library(future)
library(future.apply)
library(progressr)
progressr::handlers("cli")
#handlers("txtprogressbar")
options(future.globals.maxSize = 8000*1024^2)

sum_shapley_weights <- function(m){
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  return(sum(w))
}


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
  folder_save = "/home/lholsen/Paper3/Paper3_save_location"
  UiO = TRUE
} else if (grepl("uio.no", hostname)) {
  folder = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr"
  folder_save = "/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location"
  UiO = TRUE
} else {
  stop("We do not recongize the system at which the code is run (not Lars's MAC, HPC, nor UiO).")
}

# module load R/4.2.1-foss-2022a
# cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts
# Rscript samp_freq_plots.R 15

args = commandArgs(trailingOnly = TRUE)
m_seq = as.character(args[1])
if (grepl(",", m_seq)) {
  m_seq = as.numeric(unlist(strsplit(m_seq, ",")))
} else {
  m_seq = unlist(strsplit(m_seq, ":"))
  if (length(m_seq) > 1) {
    m_seq = seq(as.numeric(m_seq[1]), as.numeric(m_seq[2]))
  } else {
    m_seq = as.numeric(m_seq)
  }
}

n_workers <- as.numeric(args[2])  # Number of workers 4



## Create the files ------------------------------------------------------------------------------------------------
#m_seq = 20

dt_avg_L = data.table(M = numeric(), N_S = numeric(), type = character(), Size = numeric(), Ps_tilde = numeric())
dt_avg_ps = data.table(M = numeric(), N_S = numeric(), type = character(), Size = numeric(), Ps_tilde = numeric())
dt_combined = data.table(M = numeric(), N_S = numeric(), type = character(), Size = numeric(), Ps_tilde = numeric(), version = factor())
dt_diff = data.table(M = numeric(), N_S = numeric(), Size = numeric(), "mean L" = numeric(), "mean ps" = numeric(), diff = numeric())

m_idx = 1
for (m_idx in seq_along(m_seq)) {
  m = m_seq[m_idx]
  message(paste0("Working on m = ", m, " (", m_idx, " of ", length(m_seq)  ,")."))
  #if (m <= 17) next

  # Create the ps values
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  ps = shapr:::shapley_weights(m = m, N = n, n_features) / sum_shapley_weights(m)

  # Read in the files
  file_names = list.files(folder_save, pattern = "Sequence_length_M_20_rep_")
  # file_names = file_names[1:2]

  if (length(file_names) == 0) stop("No files")
  tmp = readRDS(file.path(folder_save, file_names[1]))
  dt = copy(tmp$dt)#[N_S %% 2 == 0, ]
  dt_avg = copy(tmp$dt_avg)#[N_S %% 2 == 0, ]

  #dt[, .(L_avg = mean(L)), by = .(N_S)]
  if (length(file_names) > 1) {
    file_name_idx = 1
    for (file_name_idx in seq_along(file_names[-1])) {
      file_name = file_names[-1][file_name_idx]
      message(file_name)
      tmp2 = readRDS(file.path(folder_save, file_name))
      #dt = rbind(dt, tmp2$dt[N_S %% 2 == 0, ])
      dt = rbind(dt, tmp2$dt)
      # dt_avg[, L_avg := L_avg + (tmp2$dt_avg[N_S %% 2 == 0, L_avg] - L_avg) / (file_name_idx + 1)]
      dt_avg[, L_avg := L_avg + (tmp2$dt_avg[, L_avg] - L_avg) / (file_name_idx + 1)]
    }
  }



  message("Computing average")
  # dt = dt[N_S <= 2000]
  #dt_avg2 = dt[, .(L_avg = mean(L)), by = N_S]

  # file = readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, ".rds"))
  # dt = copy(file$dt)
  # dt_avg = copy(file$dt_avg)

  ### Get the pstilde valued when using the average L
  message("E[L]")
  dt_L_avg = data.table(N_S = dt_avg$N_S[dt_avg$N_S %% 2 == 0],
                        type = "mean",
                        t(sapply(dt_avg$L_avg[seq(2, length(dt_avg$L_avg), 2)], function(L) {
                          pstilde = 2*ps / (1 - (1 - 2*ps)^(L/2))
                          pstilde = pstilde / sum(pstilde)
                          pstilde
                        })))
  setnames(dt_L_avg, old = paste0("V", n_features), new = paste(n_features))

  # Convert from wide to long
  message("Wide to long")
  dt_L_avg_long = melt.data.table(dt_L_avg, id.vars = c("N_S", "type"), value.name = "Ps_tilde", variable.name = "Size")

  # Extract only the lower coalition sizes with unique values
  dt_L_avg_long[, Size := as.integer(Size)]
  dt_L_avg_long_lower = dt_L_avg_long[Size <= ceiling((m - 1) / 2)]
  # dt_L_avg_long_lower[, Size := as.factor(Size)]

  # Add dimension, reorder, and add to the overall data.table
  dt_L_avg_long_lower[, M := m]
  setcolorder(dt_L_avg_long_lower, c("M"))
  dt_avg_L = rbind(dt_avg_L, dt_L_avg_long_lower)


  message("SAVE 1")
  dt_L_avg_long_lower_tmp = copy(dt_L_avg_long_lower)[, version := "mean L"]
  dt_L_avg_long_lower_tmp[, version := as.factor(version)]
  saveRDS(dt_L_avg_long_lower_tmp, file.path(folder_save, paste0("Sequence_length_M_", m, "_combined.rds")))


  ### Get the average pstilde values
  message("E[pS]")
  # Set up a parallel plan with a specified number of workers

  future::plan(multisession, workers = n_workers)

  # Wrap the operations within progressr
  progressr::with_progress({
    p <- progressr::progressor(dt[, max(N_S)] %/% 20000) # along = seq(2, dt[, max(N_S)], 2), steps

    N_S_now_arr = seq(2, dt[, max(N_S)], 2)
    N_S_now_arr_length = length(N_S_now_arr)

    dt_ps <- data.table::rbindlist(
      future.apply::future_lapply(seq_along(N_S_now_arr), function(N_S_now_idx) {
        N_S_now = N_S_now_arr[N_S_now_idx]
        if (N_S_now %% 20000 == 0) p(sprintf("Working on N_s = %d (%d of %d)", N_S_now, N_S_now_idx, N_S_now_arr_length))
        if (N_S_now %% 20000 == 0) message(sprintf("Working on N_s = %d (%d of %d)", N_S_now, N_S_now_idx, N_S_now_arr_length))
        # Iterate over all values of L and compute the corresponding ps values
        ps_hat <- t(vapply(dt[N_S == N_S_now, L], function(L_now) {
          pstilde <- 2 * ps / (1 - (1 - 2 * ps)^(L_now / 2))
          return(pstilde / sum(pstilde))
        }, numeric(length(ps))))
        # Create a data table with the mean, 2.5% percentile, median, and 97.5% percentile
        data.table(type = c("mean", "lower", "median", "upper"),
                   rbind(colMeans(ps_hat), apply(ps_hat, 2, quantile, probs = c(0.025, 0.5, 0.975))))
      }, future.seed = TRUE),
      use.names = TRUE,
      idcol = "N_S")
  })


  future::plan(sequential)

  # dt_ps2 = rbindlist(lapply(seq(2, dt[, max(N_S)], 2), function(N_S_now) {
  #   if (N_S_now %% 10000 == 0) message(N_S_now)
  #   # Iterate over all values of L and compute the corresponding ps values
  #   ps_hat = t(sapply(dt[N_S == N_S_now, L], function(L_now) {
  #     pstilde = 2*ps / (1 - (1 - 2*ps)^(L_now/2))
  #     pstilde = pstilde / sum(pstilde)
  #     pstilde
  #   }))
  #   # Create a data table with the mean, 2.5% percentile, median, and 97.5% percentile
  #   data.table(type = c("mean", "lower", "median", "upper"),
  #              rbind(colMeans(ps_hat), apply(ps_hat, 2, quantile, probs = c(0.025, 0.5, 0.975))))
  # }),
  # use.names = TRUE, idcol = "N_S")
  dt_ps[, N_S := rep(seq(2, dt[, max(N_S)], 2), each = 4)]
  setnames(dt_ps, old = paste0("V", n_features), new = paste(n_features))

  # old version without quantiles
  # dt_ps = data.table(N_S = seq(dt[, max(N_S)]),
  #                               t(sapply(seq(dt[, max(N_S)]), function(N_S_now) {
  #                                 colMeans(t(sapply(dt[N_S == N_S_now, L], function(L_now) {
  #                                   pstilde = 2*ps / (1 - (1 - 2*ps)^(L_now/2))
  #                                   pstilde = pstilde / sum(pstilde)
  #                                   pstilde
  #                                 })))
  #                               })))

  # Wide to long
  message("Wide to long")
  dt_ps_long = melt.data.table(dt_ps, id.vars = c("N_S", "type"), value.name = "Ps_tilde", variable.name = "Size")

  # Only get the lower coalition sizes
  dt_ps_long[, Size := as.integer(Size)]
  dt_ps_long_lower = dt_ps_long[Size <= ceiling((m - 1) / 2)]
  # dt_ps_long_lower[, Size := as.factor(Size)]

  # Add dimension, reorder, and add to the overall data.table
  dt_ps_long_lower[, M := m]
  setcolorder(dt_ps_long_lower, c("M"))
  dt_avg_ps = rbind(dt_avg_ps, dt_ps_long_lower)


  ### Combine
  message("Combine")
  dt_combined_ps = rbind(copy(dt_ps_long_lower)[, version := "mean ps"],
                         copy(dt_L_avg_long_lower)[, version := "mean L"])
  dt_combined_ps[, version := as.factor(version)]
  saveRDS(dt_combined_ps, file.path(folder_save, paste0("Sequence_length_M_", m, "_combined.rds")))
  if (length(m_seq) > 1) dt_combined = rbind(dt_combined, dt_combined_ps)

  ### Difference
  # dt_diff_now = dcast(dt_combined_ps, M + N_S + Size + type ~ version, value.var = "Ps_tilde")[, diff := `mean L` - `mean ps`]
  # dt_diff_now

  message("Dcast")
  dt_diff_now = dcast(dt_combined_ps[type == "mean",-"type"], M + N_S + Size ~ version, value.var = "Ps_tilde")[, diff := `mean L` - `mean ps`]
  if (length(m_seq) > 1) dt_diff = rbind(dt_diff, dt_diff_now)

  message("Save")
  # Save to disk
  # saveRDS(dt_combined_ps, file.path(folder_save, paste0("Sequence_length_M_", m, "_combined.rds")))
  saveRDS(dt_diff_now, file.path(folder_save, paste0("Sequence_length_M_", m, "_diff.rds")))
}

