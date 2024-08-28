library(data.table)
library(ggplot2)

# The dimension and number of repetitions
M = 10
M_seq = 10
repetitions = 50

# Data table to sotre the values

dt_res = NULL
repetition = 1
for (repetition in seq(repetitions)) {
  message(paste0("Working on repetition ", repetition ," of ", repetitions ,"."))
  file = readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_0.9_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_estimated_repetition_", repetition, "_on_all_cond_paired.rds"))

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
  saveRDS(dt2, paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Analytical_prop_M_", M, "_res.rds"))
}

# Only keep lower half of coalition size
rel_n_features = ceiling((M - 1)/2)
dt = dt[n_features <= rel_n_features,]
dt[, n_features := factor(n_features)]


# Compute the mean shapley weight over the repetitions
dt2 = dt[, as.list(c(mean = sum(shapley_weight)/repetitions,
               sd = sqrt(sum(shapley_weight - sum(shapley_weight)/repetitions)^2 / repetitions),
               #sd2 = sd(shapley_weight),
               quantile(shapley_weight, c(0.025, 0.5, 0.975)))), by = c("M", "n_combinations", "n_features")]
data.table::setnames(dt2, old = c("2.5%", "50%", "97.5%"), new = c("lower", "median", "upper"))


dt2[n_combinations == 4,]
sum(dt2[n_combinations == 4, mean] * c(2,2,2,2,1))


# Get the true Shapley values
M_seq = M
tmp_list = lapply(M_seq, function(m) {
  tmp = shapr:::shapley_weights(m = m,
                                N = sapply(seq(m - 1), choose, n = m),
                                n_components = seq(m - 1))
  tmp = tmp/sum(tmp)
  data.table(n_combinations = 2^m,
             col = factor(seq(ceiling((m-1)/2))),
             weight = tmp[seq(1, ceiling((m-1)/2))])
})
names(tmp_list) = M_seq
tmp_list = data.table::rbindlist(tmp_list, idcol = "M")
tmp_list[, M := factor(M, levels = M_seq, labels = paste0("M = ", M_seq))]


# Plot
fig = ggplot(dt2, aes(x = n_combinations, y = mean, group = n_features, col = n_features)) +
  #geom_ribbon(aes(x = n_combinations, ymin = lower, ymax = upper, group = n_features,  col = n_features, fill = n_features), alpha = 0.4, linewidth = 0.1) +
  geom_line() +
  scale_x_continuous(labels = scales::label_number()) +
  geom_point(tmp_list, mapping = aes(x = n_combinations, y = weight, colour = col), size = 2)

fig


# Legge til Paired empirical
{
  M_seq = c(10)
  dt_res_list = lapply(M_seq, function (m) {
    dt_full = readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Samp_prop_M_", m, "_res.rds"))
    dt_full = dt_full[n_features %in% seq(1, ceiling((m-1)/2))]
    return(dt_full)
  })
  names(dt_res_list) = M_seq

  dt_res = data.table::rbindlist(dt_res_list, idcol = "M")
  dt_res[, M := as.integer(M)]
  dt_res[, M := factor(M, levels = M_seq, labels = paste0("M = ", M_seq))]


  tmp_list = lapply(M_seq, function(m) {
    tmp = shapr:::shapley_weights(m = m,
                                  N = sapply(seq(m - 1), choose, n = m),
                                  n_components = seq(m - 1))
    tmp = tmp/sum(tmp)
    data.table(n_combinations = 2^m,
               col = factor(seq(ceiling((m-1)/2))),
               weight = tmp[seq(1, ceiling((m-1)/2))])
  })
  names(tmp_list) = M_seq
  tmp_list = data.table::rbindlist(tmp_list, idcol = "M")
  tmp_list[, M := factor(M, levels = M_seq, labels = paste0("M = ", M_seq))]

}


## MAKE THE PLOT (change if we want ribbons and log-scale)
fig_samp = ggplot(data = dt_res[n_combinations > 2], aes(x = n_combinations, y = mean)) +
  #geom_ribbon(aes(x = n_combinations, ymin = lower, ymax = upper, group = n_features,  col = n_features, fill = n_features), alpha = 0.4, linewidth = 0.1) +
  #ylim(c(0, 0.5)) +
  geom_line(aes(x = n_combinations, y = mean, group = n_features, col = n_features), linewidth = 1) +
  facet_wrap("M ~ .", ncol = 3, scales = "free_x") +
  #facet_wrap("M ~ .", ncol = 2) +
  geom_point(tmp_list, mapping = aes(x = n_combinations, y = weight, colour = col), size = 2) +
  #expand_limits(y = 0) +
  #scale_x_log10() +
  scale_x_continuous(labels = scales::label_number()) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  guides(fill = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
         color = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1)) +
  labs(x = expression(N[S]), y = "Normalized Shapley kernel weight/sampling frequency") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(strip.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.4)),
        legend.text = element_text(size = rel(1.4)),
        axis.title.x = element_text(size = rel(1.4)),
        axis.title.y = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.4)))
fig_samp


dt_emp = dt_res[n_combinations > 2]
dt_comb = rbind(dt_emp, dt2)
dt_comb[, strategy := factor(rep(c("Paired Empirical", "Paired Cond Empirical"), times = c(nrow(dt_emp), nrow(dt2))), levels = c("Paired Empirical", "Paired Cond Empirical"), ordered = TRUE)]


fig_samp = ggplot(data = dt_comb, aes(x = n_combinations, y = mean)) +
  #geom_ribbon(aes(x = n_combinations, ymin = lower, ymax = upper, group = n_features,  col = n_features, fill = n_features), alpha = 0.4, linewidth = 0.1) +
  #ylim(c(0, 0.5)) +
  geom_line(aes(x = n_combinations, y = mean, col = n_features, linetype = strategy), linewidth = 0.75) +
  facet_wrap("M ~ .", ncol = 3, scales = "free_x") +
  #facet_wrap("M ~ .", ncol = 2) +
  geom_point(tmp_list, mapping = aes(x = n_combinations, y = weight, colour = col), size = 2) +
  scale_linetype_manual(values = c("dashed", "dotted")) +
  #expand_limits(y = 0) +
  #scale_x_log10() +
  scale_x_continuous(labels = scales::label_number()) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  guides(fill = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
         color = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1)) +
  labs(x = expression(N[S]), y = "Normalized Shapley kernel weight/sampling frequency") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(strip.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.4)),
        legend.text = element_text(size = rel(1.4)),
        axis.title.x = element_text(size = rel(1.4)),
        axis.title.y = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.4)))
fig_samp



dt_emp[strategy]
dt2
dt2[ ]





scale_y_log10(
  breaks = scales::trans_breaks("log10", function(x) 10^x),
  labels = scales::trans_format("log10", scales::math_format(10^.x))
)




tmp2 = tmp$on_all_cond_paired$repetition_1$n_combinations_6$only_save$X
tmp2[-c(1,.N), c("n_features", "N", "shapley_weight", "cond")][,n_combinations := 2]

lappl

data.table::rbindlist(
  lapply(seq_along(tmp$on_all_cond_paired$repetition_1$), function(idx) {
  tmp2 = tmp$on_all_cond_paired$repetition_1[[idx]]only_save$X
  tmp3 = tmp2[-c(1,.N), c("n_features", "N", "shapley_weight", "cond")][,n_combinations := nrow(tmp2)]
  return(tmp3)
}))


dt_res2 = data.table::rbindlist(
  lapply(seq_along(tmp$on_all_cond_paired$repetition_1),
         function(idx) {
           if (idx == 1) return(NULL)
           tmp2 = tmp$on_all_cond_paired$repetition_1[[idx]]$only_save$X
           tmp3 = tmp2[-c(1,.N), c("n_features", "N", "shapley_weight", "cond")][,n_combinations := nrow(tmp2)]
           return(unique(tmp3))
         }))

setcolorder(dt_res2, c("n_combinations", "n_features", "N", "shapley_weight", "cond"))
setkeyv(dt_res2, c("n_combinations", "n_features"))
dt_res = copy(dt_res2)


M = 10
dt_res
dt_res[, shapley_weight := shapley_weight / sum(shapley_weight), by = c("n_combinations")]
dt_res = dt_res[n_features <= ceiling((M - 1)/2),]
dt_res[, n_features := factor(n_features)]
dt_res = dt_res[n_combinations != 2^M]
dt_res







  fig +
    geom_point(tmp_list, mapping = aes(x = n_combinations, y = weight, colour = col), size = 2)


## MAKE THE PLOT (change if we want ribbons and log-scale)
fig_samp = ggplot(data = dt_res[n_combinations > 2], aes(x = n_combinations, y = mean)) +
  #geom_ribbon(aes(x = n_combinations, ymin = lower, ymax = upper, group = n_features,  col = n_features, fill = n_features), alpha = 0.4, linewidth = 0.1) +
  geom_line(aes(x = n_combinations, y = mean, group = n_features, col = n_features), linewidth = 1) +
  facet_wrap("M ~ .", ncol = 2, scales = "free") +
  #facet_wrap("M ~ .", ncol = 2) +
  geom_point(tmp_list,
             mapping = aes(x = n_combinations, y = weight, colour = col),
             size = 2) +
  # ylim(c(0, 0.5)) +
  #expand_limits(y = 0) +
  # scale_y_log10(
  #   breaks = scales::trans_breaks("log10", function(x) 10^x),
  #   labels = scales::trans_format("log10", scales::math_format(10^.x))
  # ) +
  scale_x_continuous(labels = scales::label_number()) +
  # scale_x_log10(
  #   breaks = scales::trans_breaks("log10", function(x) 10^x),
  #   labels = scales::trans_format("log10", scales::math_format(10^.x))
  # ) +
  guides(fill = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1),
         color = guide_legend(title = expression("Coalition size |"*S*"|: "), nrow = 1)) +
  labs(x = expression(N[S]), y = "Normalized Shapley kernel weight/sampling frequency") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(strip.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)))
fig_samp




















bla = tmp = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_0.9_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_estimated_repetition_50_on_all_cond.rds")
bla$on_all_cond$repetition_1$n_combinations_4$only_save$X
