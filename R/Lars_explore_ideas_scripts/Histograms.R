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
## M = 17 ----------------------------------------------------------------------------------------------------
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



## M = 20 ----------------------------------------------------------------------------------------------------
tmp = lapply(rhos, function(rho) {
  readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_20_n_train_1000_n_test_250_rho_", rho, "_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_-0.5_-1_-2_true.rds"))
})
names(tmp) = rhos

sapply(names(tmp), function(x) {
  round(mean(abs(tmp[[x]]$pred_explain - tmp[[x]]$internal$parameters$prediction_zero)), 2)
})

sapply(names(tmp), function(x) {
  round(mean(abs(tmp[[x]]$pred_explain - tmp[[x]]$internal$parameters$prediction_zero)) / 20, 2)
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
  rho   = phi0s$rho,
  x     = round(phi0s$phi0, 2),
  y     = rep(33.25, 4)
)

explicands = rbindlist(lapply(names(tmp), function(x) data.table(rho = x, id_explicand = seq(length(tmp[[x]]$pred_explain)), prediction = tmp[[x]]$pred_explain)))

fig_hist = ggplot(explicands, aes(x = prediction)) +
  geom_histogram(binwidth = 6, color="black", fill = "grey") +
  facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), ncol = 4) +
  geom_vline(data = phi0s, mapping = aes(xintercept = phi0), color = "black", linewidth = 1, linetype = "dashed") +
  geom_text(data = phi0s_text, mapping = aes(x = x, y = y, label = label), parse = TRUE, size = 6, color = "black", hjust = -0.4, vjust = 0.15) +
  labs(color = "Strategy:", fill = "Strategy:", x = expression(f(bold(x)*"*")), y = "Count") +
  #lims(y = c(0, 90)) +
  theme(strip.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.7)),# margin = margin(r = 25)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)))
fig_hist2 = fig_hist + theme(plot.margin = margin(t = 5.5,  # Top margin
                                      r = 5.5,  # Right margin
                                      b = 5.5,  # Bottom margin
                                      l = 57))
fig_hist2


fig_M_20_comb_V2 = ggarrange(
                             fig_M_20_MAE + theme(plot.margin = margin(t = 5.5,  # Top margin
                                                                      r = 5.5,  # Right margin
                                                                      b = 5.5,  # Bottom margin
                                                                      l = 33)),
                             fig_M_20_rel,
                             fig_hist2,
                             labels = c("A", "B", "C"),
                             ncol = 1,
                             heights = c(2.125, 2.125, 1),
                             common.legend = TRUE, legend = "bottom",
                             font.label = list(size = 25, color = "black"))
ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_20_MAE_Relative_Diff_V14.png"),
       plot = fig_M_20_comb_V2,
       width = 14.2,
       height = 19.25,
       scale = 0.85,
       dpi = 350)

fig_M_20_comb_V2_reg_scale = ggarrange(
  fig_M_20_MAE + theme(plot.margin = margin(t = 5.5,  # Top margin
                                            r = 5.5,  # Right margin
                                            b = 5.5,  # Bottom margin
                                            l = 32)),
  fig_M_20_rel_reg_scale,# + scale_x_continuous(guide = guide_axis(check.overlap = TRUE), labels = addUnits),
  fig_hist2,
  labels = c("A", "B", "C"),
  ncol = 1,
  #heights = c(2.125, 2.125, 1),
  heights = c(2.5, 1.6, 1),
  common.legend = TRUE, legend = "bottom",
  font.label = list(size = 25, color = "black"))
ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_20_MAE_Relative_Diff_reg_scale_V5.png"),
       plot = fig_M_20_comb_V2_reg_scale,
       width = 14.2,
       height = 19.25,
       scale = 0.85,
       dpi = 350)

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}




ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_20_n_train_1000_n_test_250_rho_ALL_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_-0.5_-1_-2_hist_V1.png"),
       plot = fig_hist,
       width = 14.2,
       height = 5,
       scale = 0.85,
       dpi = 350)

dt_plot = data.table::rbindlist(lapply(names(tmp), function(x) {
  shapr:::plot.shapr(tmp[[x]], plot_type = "beeswarm")$data[, rho := x]
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
  ggplot2::scale_x_discrete(limits = rev, label = parse(text = paste("X[", seq(20,1), "]", sep = ""))) +
  ggplot2::theme(strip.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
                 legend.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
                 legend.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
                 axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
                 axis.text = ggplot2::element_text(size = ggplot2::rel(1.4)))
gg

ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_20_n_train_1000_n_test_250_rho_ALL_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_-0.5_-1_-2_beeswarm_V3.png"),
       plot = gg,
       width = 14.2,
       height = 20,
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
