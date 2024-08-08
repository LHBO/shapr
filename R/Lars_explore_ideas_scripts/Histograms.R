library(data.table)
library(ggplot2)
rhos = c(0, 0.2, 0.5, 0.9)


# XGBoost ---------------------------------------------------------------------------------------------------------
tmp = lapply(rhos, function(rho) {
  readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_", rho, "_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_true.rds"))
})
names(tmp) = rhos

sapply(names(tmp), function(x) {
  round(mean(abs(tmp[[x]]$pred_explain - tmp[[x]]$internal$parameters$prediction_zero)), 2)
})

sapply(names(tmp), function(x) {
  round(colMeans(abs(tmp[[x]]$shapley_values)), 2)
})

sapply(names(tmp), function(x) {
  round(apply(as.matrix(abs(tmp[[x]]$shapley_values)), 2, sd), 2)
})


sapply(names(tmp), function(x) {
  round(apply(as.matrix(abs(tmp[[x]]$shapley_values)), 2, max), 2)
})

phi0s = data.table(rho = names(tmp), phi0 = sapply(names(tmp), function(x) tmp[[x]]$internal$parameters$prediction_zero))
phi0s_text = data.table(
  label = paste0("phi[0]=='", format(round(phi0s$phi0, 2), drop0Trailing = FALSE), "'"), # Add "'" so that i can parse the string but keep trailing zeros
  rho   = rhos,
  x     = round(phi0s$phi0, 2),
  y     = rep(150, 4)
)

explicands = rbindlist(lapply(names(tmp), function(x) data.table(rho = x, id_explicand = seq(length(tmp[[x]]$pred_explain)), prediction = tmp[[x]]$pred_explain)))

fig_hist = ggplot(explicands, aes(x = prediction)) +
  geom_histogram(binwidth = 6, color="black", fill = "grey") +
  facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), ncol = 4) +
  geom_vline(data = phi0s, mapping = aes(xintercept = phi0), color = "black", linewidth = 1, linetype = "dashed") +
  geom_text(data = phi0s_text, mapping = aes(x = x, y = y, label = label), parse = TRUE, size = 6, color = "black", hjust = -0.2, vjust = 0.15) +
  labs(color = "Strategy:", fill = "Strategy:", x = expression(f(bold(x)*"*")), y = "Count") +
  #lims(y = c(0, 90)) +
  theme(strip.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)))
fig_hist

ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_ALL_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_hists_V4.png"),
       plot = fig_hist,
       width = 14.2,
       height = 5,
       scale = 0.85,
       dpi = 350)


library(shapr)
plot.shapr(tmp[["0"]], plot_type = "beeswarm", index_x_explain = c(1:1000))

dt_plot = data.table::rbindlist(lapply(names(tmp), function(x) {
  plot.shapr(tmp[[x]], plot_type = "beeswarm")$data[, rho := x]
}))

dt_plot[, variable := factor(variable, )]

col <- c("#F8766D", "yellow", "#00BA38")
gg <- ggplot2::ggplot(dt_plot, ggplot2::aes(x = variable, y = phi, color = feature_value_scaled)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey60", linewidth = 0.5) +
  #ggbeeswarm::geom_beeswarm(priority = "random", cex = 0.1) +
  ggbeeswarm::geom_beeswarm(corral = "wrap", priority = "random", corral.width = 0.7) +
  ggplot2::coord_flip() +
  #ggplot2::theme_classic() +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey75", linetype = "dashed")) +
  ggplot2::labs(x = "", y = "Shapley value") +
  ggplot2::guides(color = ggplot2::guide_colourbar(
    ticks = FALSE,
    #barwidth = 0.5, barheight = 10
    barwidth = 10, barheight = 0.5
  )) +
  ggplot2::facet_wrap(. ~ rho,  labeller = label_bquote(cols = rho ==.(rho)), ncol = 2, scales = "fixed") +
  ggplot2::scale_color_gradient2(
    low = col[3], mid = col[2], high = col[1],
    midpoint = 0.5,
    breaks = c(0, 1),
    limits = c(0, 1),
    labels = c("       Low", "High       "),
    name = "Feature value: "
  ) +
  ggplot2::theme(legend.position = 'bottom') +
  ggplot2::guides(fill = guide_legend(nrow = 1)) +
  ggplot2::scale_x_discrete(limits = rev, label = parse(text = paste("X[", seq(10,1), "]", sep = ""))) +
  ggplot2::theme(strip.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
        legend.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
        legend.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
        axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
        axis.text = ggplot2::element_text(size = ggplot2::rel(1.4)))
gg

ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_ALL_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_beeswarm_V1.png"),
       plot = gg,
       width = 14.2,
       height = 11,
       scale = 0.85,
       dpi = 350)

# Linear model ----------------------------------------------------------------------------------------------------

tmp = lapply(rhos, function(rho) {
  readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_17_n_train_1000_n_test_500_rho_", rho, "_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_true.rds"))
})
names(tmp) = rhos

sapply(names(tmp), function(x) {
  round(mean(abs(tmp[[x]]$pred_explain - tmp[[x]]$internal$parameters$prediction_zero)), 2)
})

sapply(names(tmp), function(x) {
  round(mean(abs(tmp[[x]]$pred_explain - tmp[[x]]$internal$parameters$prediction_zero)) / 17, 2)
})

sapply(names(tmp), function(x) {
  round(colMeans(abs(tmp[[x]]$shapley_values)), 2)
})

sapply(names(tmp), function(x) {
  round(apply(as.matrix(abs(tmp[[x]]$shapley_values)), 2, sd), 2)
})


sapply(names(tmp), function(x) {
  round(apply(as.matrix(abs(tmp[[x]]$shapley_values)), 2, max), 2)
})

phi0s = data.table(rho = names(tmp), phi0 = sapply(names(tmp), function(x) tmp[[x]]$internal$parameters$prediction_zero))
phi0s_text = data.table(
  label = paste0("phi[0]=='", format(round(phi0s$phi0, 2), drop0Trailing = FALSE), "'"), # Add "'" so that i can parse the string but keep trailing zeros
  rho   = rhos,
  x     = round(phi0s$phi0, 2),
  y     = rep(70, 4)
)

explicands = rbindlist(lapply(names(tmp), function(x) data.table(rho = x, id_explicand = seq(length(tmp[[x]]$pred_explain)), prediction = tmp[[x]]$pred_explain)))

fig_hist = ggplot(explicands, aes(x = prediction)) +
  geom_histogram(binwidth = 6, color="black", fill = "grey") +
  facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), ncol = 4) +
  geom_vline(data = phi0s, mapping = aes(xintercept = phi0), color = "black", linewidth = 1, linetype = "dashed") +
  geom_text(data = phi0s_text, mapping = aes(x = x, y = y, label = label), parse = TRUE, size = 6, color = "black", hjust = -0.2, vjust = 0.15) +
  labs(color = "Strategy:", fill = "Strategy:", x = expression(f(bold(x)*"*")), y = "Count") +
  #lims(y = c(0, 90)) +
  theme(strip.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)))
fig_hist

ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_17_n_train_1000_n_test_500_rho_ALL_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_hist_V1.png"),
       plot = fig_hist,
       width = 14.2,
       height = 5,
       scale = 0.85,
       dpi = 350)



dt_plot = data.table::rbindlist(lapply(names(tmp), function(x) {
  plot.shapr(tmp[[x]], plot_type = "beeswarm")$data[, rho := x]
}))

col <- c("#F8766D", "yellow", "#00BA38")
gg <- ggplot2::ggplot(dt_plot, ggplot2::aes(x = variable, y = phi, color = feature_value_scaled)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey60", linewidth = 0.5) +
  #ggbeeswarm::geom_beeswarm(priority = "random", cex = 0.1) +
  ggbeeswarm::geom_beeswarm(corral = "wrap", priority = "random", corral.width = 0.65) +
  ggplot2::coord_flip() +
  #ggplot2::theme_classic() +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey75", linetype = "dashed")) +
  ggplot2::labs(x = "", y = "Shapley value") +
  ggplot2::guides(color = ggplot2::guide_colourbar(
    ticks = FALSE,
    #barwidth = 0.5, barheight = 10
    barwidth = 10, barheight = 0.5
  )) +
  ggplot2::facet_wrap(. ~ rho,  labeller = label_bquote(cols = rho ==.(rho)), ncol = 2, scales = "fixed") +
  ggplot2::scale_color_gradient2(
    low = col[3], mid = col[2], high = col[1],
    midpoint = 0.5,
    breaks = c(0, 1),
    limits = c(0, 1),
    labels = c("       Low", "High       "),
    name = "Feature value: "
  ) +
  ggplot2::theme(legend.position = 'bottom') +
  ggplot2::guides(fill = guide_legend(nrow = 1)) +
  ggplot2::scale_x_discrete(limits = rev, label = parse(text = paste("X[", seq(17,1), "]", sep = ""))) +
  ggplot2::theme(strip.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
                 legend.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
                 legend.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
                 axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
                 axis.text = ggplot2::element_text(size = ggplot2::rel(1.4)))
gg

ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_17_n_train_1000_n_test_500_rho_ALL_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_beeswarm_V1.png"),
       plot = gg,
       width = 14.2,
       height = 14.2,
       scale = 0.85,
       dpi = 350)



# WINE ------------------------------------------------------------------------------------------------------------
tmp_wine = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Wine_data_sep_rf.rds")
tmp_wine_plot = plot.shapr(tmp_wine, plot_type = "beeswarm") +
  ggplot2::scale_x_discrete(limits = rev) +
  ggplot2::theme(strip.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
                 legend.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
                 legend.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
                 axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
                 axis.text = ggplot2::element_text(size = ggplot2::rel(1.4)))

ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Wine_beeswarm_V1.png"),
       plot = tmp_wine_plot,
       width = 14.2,
       height = 8,
       scale = 0.85,
       dpi = 350)
