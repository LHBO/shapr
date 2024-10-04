create_X_dt_KernelSHAP_corrected = function(m,
                                            presampled_coalitions,
                                            prefixed_coalitions,
                                            dt_all_coalitions,
                                            weight_zero_m = 10^6) {

  # Find weights for given number of features
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  # Weight the different coalition sizes (KernelSHAP version)
  num_subset_sizes = as.integer(ceiling((m - 1) / 2))
  num_paired_subset_sizes = as.integer(floor((m - 1) / 2))
  weight_vector = sapply(seq(num_subset_sizes), function(i) (m - 1.0) / (i * (m - i)))
  weight_vector[seq(num_paired_subset_sizes)] = 2*weight_vector[seq(num_paired_subset_sizes)]
  weight_vector = weight_vector / sum(weight_vector)

  # Get the sampled coalitions for which we have to compute the frequencies
  if (!is.null(prefixed_coalitions)) {
    presampled_coal_wo_prefixed_coal = presampled_coalitions[-seq(nrow(prefixed_coalitions))]
    num_full_subsets = length(prefixed_coalitions[.N - 1, features][[1]]) # This relies on the list version
    weights_remaining = weight_vector[-seq(num_full_subsets)]
    weight_left = sum(weights_remaining)
  } else {
    presampled_coal_wo_prefixed_coal = presampled_coalitions
    num_full_subsets = 0
    weights_remaining = weight_vector
    weight_left = sum(weight_vector)
  }

  # Get the number of coalitions of all sizes
  n_coal_of_each_size = choose(m, seq(m-1))

  # Get the number of coalitions in the sizes that are not deterministically included
  if (num_full_subsets >= floor(m/2)) stop("Too many full subsets. No sampling is done.")
  n_coal_of_each_size_reamaining = n_coal_of_each_size[seq(num_full_subsets + 1, m - 1 - num_full_subsets)]

  # Get the shapley kernel weights for the remaining coalition sizes
  p_reamaining = p[seq(num_full_subsets + 1, m - 1 - num_full_subsets)]
  p_reamaining = p_reamaining / sum(p_reamaining)

  # Get the shapley kernel weight for each coalition
  shapley_kernel_weight_reweighted = p_reamaining / n_coal_of_each_size_reamaining

  # Pad it such that index corresponds to caolition size
  shapley_kernel_weight_reweighted = c(rep(0, num_full_subsets), shapley_kernel_weight_reweighted, rep(0, num_full_subsets))

  # Convert the list column to a comma-separated string for each row
  if (!is.null(prefixed_coalitions)) {
    prefixed_coalitions[, features := sapply(features, function(x) paste(unlist(x), collapse = ","))]
    setnames(prefixed_coalitions, "w", "shapley_weight")

    # Add that these coalitions are NOT sampled
    prefixed_coalitions[, sampled := FALSE]
  }

  # String version
  # Insert all sampled coalitions into a data table and find their frequencies
  dt_freq = data.table::data.table(features = presampled_coal_wo_prefixed_coal)[, .(shapley_weight = .N), by = features]

  # Add that these coalitions are sampled
  dt_freq[, sampled := TRUE]

  # Put together with the prefixed samples
  dt_freq = rbind(prefixed_coalitions, dt_freq)

  # Get the number of features in each coalition
  dt_freq[, n_features := stringr::str_count(features, ",") + 1]

  # Add the number of coalitions of each size
  dt_freq[, N := n[n_features]]
  dt_freq[, p := p[n_features]]

  # Get the id_combination if we had used all combinations and sort it
  dt_freq[, id_combination_full := dt_all_coalitions[dt_freq, id, on = "features"]]
  data.table::setorder(dt_freq, "id_combination_full")

  # Get the number of samples it took the sample the `dt_freq[sampled == TRUE, .N]` unique
  # coalitions for those coalitions that are not deterministically included.
  K <- dt_freq[sampled == TRUE, sum(shapley_weight)]

  # Ensure that the shapley weight column is numeric, as it is int when prefixed_coalitions is NULL
  dt_freq[, shapley_weight := as.numeric(shapley_weight)]

  # Add the reweighted Shapley kernel weights
  dt_freq[sampled == TRUE, shapley_weight := shapley_kernel_weight_reweighted[n_features]]

  # Compute the corrected values shapley weights (see paper)
  dt_freq[sampled == TRUE, shapley_weight := 2*shapley_weight / (1 - (1 - 2*shapley_weight)^(K/2))]

  # Reweight the weights such that they sums to the remaining weight
  dt_freq[sampled == TRUE, shapley_weight := weight_left * shapley_weight / sum(shapley_weight)]

  # Convert from string to list of integer vectors. stringr is faster than base and stringi
  dt_freq[, features := lapply(stringr::str_split(features, ','), as.integer)]

  # Remove the sampled column as we no longer need it
  dt_freq[, sampled := NULL]

  # Add the empty and grand coalitions
  dt_freq = rbindlist(
    list(
      data.table(features = list(integer(0)), shapley_weight = weight_zero_m, n_features = 0L, N = 1L, p = NA, id_combination_full = 1),
      dt_freq,
      data.table(features = list(1:m), shapley_weight = weight_zero_m, n_features = as.integer(m), N = 1L,  p = NA, id_combination_full = 2^m)
    )
  )

  # Create new id column and reorder the columns
  dt_freq[, id_combination := .I]
  data.table::setcolorder(dt_freq, c("id_combination", "id_combination_full", "features", "n_features", "N", "shapley_weight", "p"))

  # Optional to match the old setup
  dt_freq[, N := as.integer(N)]
  dt_freq[, n_features := as.integer(n_features)]

  return(dt_freq)
}


m = 10

message("Loading presampled coalitions KernelSHAP_paired")
presampled_coalitions_KernelSHAP_paired = file.path(folder_save_KernelSHAP_paired, paste0("KernelSHAP_sampling_paired_M_", M, "_repetition_", repetition, ".rds"))
if (file.exists(presampled_coalitions_KernelSHAP_paired)) {
  presampled_coalitions_KernelSHAP_paired = readRDS(presampled_coalitions_KernelSHAP_paired)
} else {
  presampled_coalitions_KernelSHAP_paired = NULL
  stop("`presampled_coalitions_KernelSHAP_paired` does not exist")
}
# print(presampled_coalitions_KernelSHAP)
message("Done loading presampled coalitions KernelSHAP_paired")


presampled_coalitions_KernelSHAP_paired = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/KernelSHAP_sampling_M_10_repetition_1.rds")






# Create list of all feature combinations
all_coalitions = unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)
dt_all_coalitions = data.table(features = sapply(all_coalitions, function(x) paste(x, collapse = ",")))[, id := .I]




cat(paste("Rscript M_20_run_simulations.R 0.9 FALSE", 43:50, "\n"))
Rscript M_20_run_simulations.R 0.5 FALSE 99:83

presampled_coalitions_KernelSHAP_paired$look_up$dt_n_comb_needed
n_combination = 970
n_combinations = seq(4, 2^m-4, 10)
n_combinations = c(100, 250, 750, 1000)

dt_list_animate = data.table::rbindlist(
  lapply(n_combinations, function(n_combination) {
    message(n_combination)
    # Figure out which list to look at
    dt_id = presampled_coalitions_KernelSHAP_paired$look_up$dt_n_comb_needed_sample[N_S == n_combination, dt_id]

    # Get the n_combinations coalitions to include
    to_this_index = presampled_coalitions_KernelSHAP_paired$samples[[dt_id]]$dt_N_S_and_L_small[N_S == n_combination, L]
    presampled_coalitions = copy(presampled_coalitions_KernelSHAP_paired$samples[[dt_id]]$all_coalitions_small[seq(to_this_index)])
    prefixed_coalitions = copy(presampled_coalitions_KernelSHAP_paired$samples[[dt_id]]$dt_res)



    # Get the X data.table
    X_now = create_X_dt_KernelSHAP_corrected(m = m,
                                             presampled_coalitions = presampled_coalitions,
                                             prefixed_coalitions = copy(prefixed_coalitions),
                                             dt_all_coalitions = dt_all_coalitions)

    # Get the X data.table
    X_now_avg = create_X_dt_KernelSHAP(m = m,
                                       presampled_coalitions = presampled_coalitions,
                                       prefixed_coalitions = copy(prefixed_coalitions),
                                       dt_all_coalitions = dt_all_coalitions,
                                       version_scaled = TRUE)

    X_now_copy = copy(X_now_avg)
    #plot(X_now_copy[-c(1, .N), id_combination_full], X_now_copy[-c(1, .N), shapley_weight])
    X_now_copy[, shapley_weight := as.numeric(shapley_weight)]

    # Average the weights on the coalition sizes
    shapley_reweighting(X = X_now_copy, reweight = "on_N")


    res_tmp = cbind(
      X_now_avg[-c(1, .N), id_combination_full],
      X_now_avg[-c(1, .N), shapley_weight],
      X_now_copy[-c(1, .N), shapley_weight],
      X_now[-c(1, .N), shapley_weight]
    )

    dt_res_tmp = as.data.table(res_tmp)
    setnames(dt_res_tmp, c("id", "KernelSHAP", "KernelSHAP Average", "C-KernelSHAP"))


    # matplot(X_now_avg[-c(1, .N), id_combination_full],
    #         res_tmp, col = c("black", "red", "blue"), type = c("p", "l", "l"), pch = 16, lwd = 2)


  return(dt_res_tmp)

}), use.names = TRUE, idcol = "n_comb")

# update n_comb
dt_list_animate[, n_comb := as.integer(n_combinations[n_comb])]
dt_list_animate[, id := as.integer(id)]
dt_list_animate



# Melt
dt_list_animate2 = melt(dt_list_animate, id.vars = c("n_comb", "id"), variable.name = "strategy", value.name = "weight")

library(ggplot2)
library(gganimate)
ggplot(dt_list_animate2[n_comb == 104,], aes(x = id, y = weight, col = strategy)) +
  geom_point()

ggplot() +

  geom_point(data = dt_list_animate2[n_comb == 514 & strategy == "KernelSHAP",],
             aes(x = id, y = weight, lty = strategy, col = strategy)) +
  geom_line(data = dt_list_animate2[n_comb == 514 & strategy != "KernelSHAP",],
            aes(x = id, y = weight, col = strategy, lty = strategy), lwd = 1) +
  scale_colour_manual(values = c("gray50", "#F8766D", "#00BFC4")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(x = 'Coalition index', y = 'Normalized weights',
       col = "Strategy", lty = "Strategy")



ggplot() +
  geom_point(data = dt_list_animate2[n_comb == 514 & strategy == "KernelSHAP",],
             aes(x = id, y = weight, lty = strategy, col = strategy)) +
  geom_line(data = dt_list_animate2[n_comb == 514 & strategy != "KernelSHAP",],
            aes(x = id, y = weight, col = strategy, lty = strategy), lwd = 1) +
  scale_colour_manual(values = c("gray50", "#F8766D", "#00BFC4")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  # Here comes the gganimate specific bits
  labs(title = 'N_comb: {frame_time}', x = 'Coalition index', y = 'Normalized weights',
       col = "Strategy", lty = "Strategy")
  transition_time(n_comb) +
  ease_aes('linear')

tt = ggplot() +
  geom_point(data = dt_list_animate2[strategy == "KernelSHAP",],
             aes(x = id, y = weight, lty = strategy, col = strategy)) +
  geom_line(data = dt_list_animate2[strategy != "KernelSHAP",],
            aes(x = id, y = weight, col = strategy, lty = strategy), lwd = 1) +
  scale_colour_manual(values = c("gray50", "#F8766D", "#00BFC4")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  # Here comes the gganimate specific bits
  labs(title = 'N_comb: {frame_time}', x = 'Coalition index', y = 'Normalized weights',
       col = "Strategy", lty = "Strategy") +
  transition_states(
    n_comb,
    transition_length = 1,
    state_length = 1
  )
  ease_aes('linear')

anim_save("/Users/larsolsen/PhD/Paper3/Paper3_save_location/animated.gif",
          animation = tt,
          nframes = 102,
          fps = 20,
          renderer = gifski_renderer(width = 500, height = 500))






# Plot ------------------------------------------------------------------------------------------------------------
m = M = 10
presampled_coalitions_KernelSHAP = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/KernelSHAP_sampling_M_10_repetition_1.rds")
presampled_coalitions_KernelSHAP_paired = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/KernelSHAP_sampling_paired_M_10_repetition_1.rds")
dt_new_weights_sequence = readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Sequence_length_M_", m, "_combined.rds"))
dt_new_weights_sequence = dcast(dt_new_weights_sequence[type == "mean"], M + N_S + Size ~ version, value.var = "Ps_tilde")

# Which N_S values to plot
n_combinations = c(100, 250, 750, 1000)

# Create the data table
dt_plot = data.table::rbindlist(
  lapply(n_combinations, function(n_combination) {
    message(n_combination)
    ### PySHAP
    # Figure out which list to look at
    dt_id = presampled_coalitions_KernelSHAP$look_up$dt_n_comb_needed_sample[N_S == n_combination, dt_id]

    # Get the n_combinations coalitions to include
    to_this_index = presampled_coalitions_KernelSHAP$samples[[dt_id]]$dt_N_S_and_L_small[N_S == n_combination, L]
    presampled_coalitions = copy(presampled_coalitions_KernelSHAP$samples[[dt_id]]$all_coalitions_small[seq(to_this_index)])
    prefixed_coalitions = copy(presampled_coalitions_KernelSHAP$samples[[dt_id]]$dt_res)

    # Work on the PySHAP* strategy
    X_now_PySHAP = create_X_dt_KernelSHAP(m = m,
                                           presampled_coalitions = presampled_coalitions,
                                           prefixed_coalitions = copy(prefixed_coalitions),
                                           dt_all_coalitions = dt_all_coalitions,
                                           version_scaled = TRUE)

    dt_res_PySHAP = data.table("id" = X_now_PySHAP[-c(1, .N), id_combination_full],
                               "Strategy" = "PySHAP",
                               "weight" = X_now_PySHAP[-c(1, .N), shapley_weight])

    ### PySHAP*
    # Figure out which list to look at
    dt_id = presampled_coalitions_KernelSHAP_paired$look_up$dt_n_comb_needed_sample[N_S == n_combination, dt_id]

    # Get the n_combinations coalitions to include
    to_this_index = presampled_coalitions_KernelSHAP_paired$samples[[dt_id]]$dt_N_S_and_L_small[N_S == n_combination, L]
    presampled_coalitions = copy(presampled_coalitions_KernelSHAP_paired$samples[[dt_id]]$all_coalitions_small[seq(to_this_index)])
    prefixed_coalitions = copy(presampled_coalitions_KernelSHAP_paired$samples[[dt_id]]$dt_res)

    # Work on the PySHAP* strategy
    X_now_PySHAPs = create_X_dt_KernelSHAP(m = m,
                                           presampled_coalitions = presampled_coalitions,
                                           prefixed_coalitions = copy(prefixed_coalitions),
                                           dt_all_coalitions = dt_all_coalitions,
                                           version_scaled = TRUE)

    # Work on the PySHAP* Average strategy
    X_now_PySHAPs_avg = copy(X_now_PySHAPs)
    X_now_PySHAPs_avg[, shapley_weight := as.numeric(shapley_weight)]
    shapley_reweighting(X = X_now_PySHAPs_avg, reweight = "on_N") # Average the weights on the coalition sizes

    # Work on the PySHAP* C-Kernel strategy
    X_now_PySHAPs_c_kernel = create_X_dt_KernelSHAP_corrected(m = m,
                                                              presampled_coalitions = presampled_coalitions,
                                                              prefixed_coalitions = copy(prefixed_coalitions),
                                                              dt_all_coalitions = dt_all_coalitions)

    # Work on the PySHAP* CEL-Kernel strategy
    X_now_PySHAPs_cel_kernel = copy(X_now_PySHAPs)
    X_now_PySHAPs_cel_kernel[, shapley_weight := as.numeric(shapley_weight)]

    # Get the weights
    dt_new_weights = copy(dt_new_weights_sequence)

    # Find the weights of the combination closest to n_combinations
    n_comb_use = dt_new_weights$N_S[which.min(abs(dt_new_weights$N_S - n_combination))]
    dt_new_weights_now = dt_new_weights[N_S == n_comb_use]
    dt_new_weights_now <- rbind(dt_new_weights_now, dt_new_weights_now[(.N - ifelse(M[1] %% 2 == 1, 0, 1)):1])
    dt_new_weights_now[, Size := .I]
    setnames(dt_new_weights_now, "Size", "n_features")

    # Update the weights with the provided weights for each coalition size
    X_now_PySHAPs_cel_kernel[dt_new_weights_now, on = "n_features", shapley_weight := get(gsub("_", " ", "mean_L"))]
    X_now_PySHAPs_cel_kernel[-c(1,.N), shapley_weight := shapley_weight / sum(shapley_weight)]

    # Extract and combine the sampled coalition indices
    dt_res_PySHAPs = melt(data.table(
      "id" = X_now_PySHAPs[-c(1, .N), id_combination_full],
      "PySHAP*" = X_now_PySHAPs[-c(1, .N), shapley_weight],
      "PySHAP* Average" = X_now_PySHAPs_avg[-c(1, .N), shapley_weight],
      "PySHAP* C-Kernel" = X_now_PySHAPs_c_kernel[-c(1, .N), shapley_weight],
      "PySHAP* CEL-Kernel" = X_now_PySHAPs_cel_kernel[-c(1, .N), shapley_weight]
    ), id.vars = "id", variable.name = "Strategy", value.name = "weight")

    dt_res = rbind(dt_res_PySHAP, dt_res_PySHAPs)
    return(dt_res)

  }), use.names = TRUE, idcol = "N_S")

# update n_comb
dt_plot[, N_S := as.integer(n_combinations[N_S])]
dt_plot[, id := as.integer(id)]
dt_plot

# Lag ggplottet

fig = ggplot(dt_plot, aes(x = id, y = weight, col = Strategy)) +
  geom_vline(xintercept = 512.5, color = "darkgrey", linetype = "dashed", linewidth = 0.9) +
  geom_point(alpha = 0.4) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  facet_wrap(. ~ N_S, labeller = label_bquote(cols = N[S] ==.(N_S)), ncol = 2) +
  theme(legend.position = 'bottom') +
  guides(col = guide_legend(nrow = 1)) +
  labs(color = "Strategy:", x = "Coalition index", y = "Normalized weights") +
  theme(strip.text = element_text(size = rel(1.6)),
        legend.title = element_text(size = rel(1.6)),
        legend.text = element_text(size = rel(1.55)),
        axis.title = element_text(size = rel(1.6)),
        axis.text = element_text(size = rel(1.5)))
fig

ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_N_combinations_weights_appendix.png", # Saving 14.2 x 9.98 in image
       plot = fig,
       width = 14.2,
       height = 9.5,
       scale = 0.85,
       dpi = 350)

