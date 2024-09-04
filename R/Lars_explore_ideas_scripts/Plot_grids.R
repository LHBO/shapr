if (FALSE) {
  # Linear model ----------------------------------------------------------------------------------------------------
  # M = 6 -----------------------------------------------------------------------------------------------------------
  {
    p1 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_6_n_train_1000_n_test_1000_rho_0.01_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.01") + guides(color = "none")

    p2 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_6_n_train_1000_n_test_1000_rho_0.11_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.11") + guides(color = "none")

    p3 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_6_n_train_1000_n_test_1000_rho_0.31_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_dt_MAE.rds",
                      index_combinations = NULL,
                      # only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.31") + guides(color = "none")

    p4 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_6_n_train_1000_n_test_1000_rho_0.41_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.41") + guides(color = "none")

    p5 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_6_n_train_1000_n_test_1000_rho_0.61_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.61") + guides(color = "none")

    p6 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_6_n_train_1000_n_test_1000_rho_0.71_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.71") + guides(color = "none")

    p7 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_6_n_train_1000_n_test_1000_rho_0.81_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.81") + guides(color = "none")

    p8 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_6_n_train_1000_n_test_1000_rho_0.91_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      #only_these_sampling_methods = c("unique", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_equal_weights", "unique_SW", "unique_paired_SW", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.91")

    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)

  }







  # M = 10 ----------------------------------------------------------------------------------------------------------
  {
    p1 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.04_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.04") + guides(color = "none")

    p2 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.14_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.14") + guides(color = "none")

    p3 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.24_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.24") + guides(color = "none")

    p4 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.34_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.34") + guides(color = "none")

    p5 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.64_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.64") + guides(color = "none")

    p6 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.74_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.74") + guides(color = "none")

    p7 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.84_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.84") + guides(color = "none")

    p8 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_0.94_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.94")



    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)
  }


  ## With extra samples of coalitions --------------------------------------------------------------------------------


  {
    only_these_sampling_methods = c("unique",
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
                                    "paired_coalitions",
                                    "single_mean_coalition_effect")

    only_these_sampling_methods = c("unique_paired_SW",
                                    "unique_paired_unif_V2",
                                    "unique_paired_equal_weights",
                                    "unique_paired_equal_weights_100",
                                    "unique_paired_equal_weights_500",
                                    "unique_paired_equal_weights_1000",
                                    "unique_paired_equal_weights_5000",
                                    "unique_paired_equal_weights_10000",
                                    "unique_paired_equal_weights_50000",
                                    "paired_coalitions")
    rhos = c(0,0.05,0.1,0.2,0.5,0.7,0.9,0.9)
    rho_equi = FALSE
    figs = list()
    for (rho_idx in seq_along(rhos)) {
      rho = rhos[rho_idx]
      figs[[rho_idx]] = plot_results(file_path = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_", rho, "_equi_", rho_equi ,"_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds"),
                                     index_combinations = NULL,
                                     only_these_sampling_methods = only_these_sampling_methods,
                                     figures_to_make = c("figure_CI",
                                                         "figure_mean",
                                                         "figure_median",
                                                         "figure_lines",
                                                         "figure_boxplot",
                                                         "figure_lines_boxplot",
                                                         "figure_boxplot_lines"),
                                     ggplot_theme = NULL,
                                     brewer_palette = NULL,
                                     brewer_direction = 1,
                                     flip_coordinates = FALSE,
                                     legend_position = NULL,
                                     scale_y_log10 = TRUE,
                                     scale_x_log10 = FALSE,
                                     n.dodge = 2,
                                     plot_figures = FALSE)$figure_mean + ggplot2::ggtitle(paste0("rho = ", rho, " (equi = ", rho_equi, ")"))
      if (rho_idx != length(rhos)) {
        figs[[rho_idx]] = figs[[rho_idx]] + ggplot2::guides(color = "none")
      }

    }
    gridExtra::grid.arrange(grobs = figs, nrow = 2)


  }


  {
    only_these_sampling_methods = c("unique",
                                    "unique_paired",
                                    "unique_paired_SW",
                                    "unique_paired_equal_weights",
                                    "paired_coalitions")
    rhos = c(0,0.05,0.1,0.2,0.5,0.7,0.9,0.9)
    rho_equi = TRUE
    figs = list()
    for (rho_idx in seq_along(rhos)) {
      rho = rhos[rho_idx]
      figs[[rho_idx]] = plot_results(file_path = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_10_n_train_1000_n_test_1000_rho_", rho, "_equi_", rho_equi ,"_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds"),
                                     index_combinations = NULL,
                                     only_these_sampling_methods = only_these_sampling_methods,
                                     figures_to_make = c("figure_CI",
                                                         "figure_mean",
                                                         "figure_median",
                                                         "figure_lines",
                                                         "figure_boxplot",
                                                         "figure_lines_boxplot",
                                                         "figure_boxplot_lines"),
                                     ggplot_theme = NULL,
                                     brewer_palette = NULL,
                                     brewer_direction = 1,
                                     flip_coordinates = FALSE,
                                     legend_position = NULL,
                                     scale_y_log10 = TRUE,
                                     scale_x_log10 = FALSE,
                                     n.dodge = 2,
                                     plot_figures = FALSE)$figure_mean + ggplot2::ggtitle(paste0("rho = ", rho, " (equi = ", rho_equi, ")"))
      if (rho_idx != length(rhos)) {
        figs[[rho_idx]] = figs[[rho_idx]] + ggplot2::guides(color = "none")
      }

    }
    gridExtra::grid.arrange(grobs = figs, nrow = 2)

    library(wesanderson)
    library("tidyquant")
    dt_all = rbindlist(lapply(figs, "[[", 1), idcol = "rho")[, rho := rhos[rho]]
    ggplot(dt_all, aes(x = n_combinations, y = mean, col = sampling)) +
      #geom_smooth(method = "loess", se = FALSE) +
      #stat_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +
      #geom_line() +
      geom_ma(ma_fun = SMA, n = 10, linetype = "solid") +
      facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) + scale_color_brewer(palette="Set1")



  }


  # M = 12 ----------------------------------------------------------------------------------------------------------
  {
    p1 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_12_n_train_1000_n_test_1000_rho_0.01_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.01") + guides(color = "none")

    p2 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_12_n_train_1000_n_test_1000_rho_0.11_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.11") + guides(color = "none")

    p3 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_12_n_train_1000_n_test_1000_rho_0.31_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_dt_MAE.rds",
                      index_combinations = NULL,
                      # only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.31") + guides(color = "none")

    p4 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_12_n_train_1000_n_test_1000_rho_0.41_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.41") + guides(color = "none")

    p5 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_12_n_train_1000_n_test_1000_rho_0.61_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.61") + guides(color = "none")

    p6 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_12_n_train_1000_n_test_1000_rho_0.71_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.71") + guides(color = "none")

    p7 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_12_n_train_1000_n_test_1000_rho_0.81_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.81") + guides(color = "none")

    p8 = plot_results(file_path = "/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_12_n_train_1000_n_test_1000_rho_0.91_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_dt_MAE.rds",
                      index_combinations = NULL,
                      #only_these_sampling_methods = c("unique", "unique_equal_weights", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_unif", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      only_these_sampling_methods = c("unique", "unique_SW", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_SW", "unique_paired_equal_weights", "paired_coalitions"),
                      #only_these_sampling_methods = c("unique", "unique_equal_weights_symmetric", "unique_paired", "unique_paired_equal_weights", "unique_SW", "unique_paired_SW", "paired_coalitions"),
                      figures_to_make = c("figure_CI",
                                          "figure_mean",
                                          "figure_median",
                                          "figure_lines",
                                          "figure_boxplot",
                                          "figure_lines_boxplot",
                                          "figure_boxplot_lines"),
                      ggplot_theme = NULL,
                      brewer_palette = NULL,
                      brewer_direction = 1,
                      flip_coordinates = FALSE,
                      legend_position = NULL,
                      scale_y_log10 = TRUE,
                      scale_x_log10 = FALSE,
                      n.dodge = 2,
                      plot_figures = FALSE)$figure_mean + ggplot2::ggtitle("rho = 0.91")

    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)

  }



  # M = 17 ----------------------------------------------------------------------------------------------------------
  {
    # Parameters
    rhos = c(0,0.2,0.5,0.9)
    rho_equi = FALSE # Can NOT change this

    only_these_sampling_methods = c("unique",
                                    "unique_paired",
                                    "unique_paired_equal_weights",
                                    "unique_paired_SW")

    only_these_sampling_methods = sampling_methods = c("largest_weights_random",
                                                       "largest_weights_random_new_weights_empirical",
                                                       "MAD",
                                                       "MAD_new_weights_empirical",
                                                       "paired_coalitions_weights_direct_equal_weights_new_weights_gompertz",
                                                       "unique_paired_new_weights_gompertz",
                                                       "paired_coalitions_new_weights_gompertz",
                                                       "unique_paired_new_weights_empirical",
                                                       "paired_coalitions_new_weights_empirical",
                                                       "paired_coalitions_weights_direct_equal_weights_new_weights_empirical",
                                                       "unique",
                                                       "unique_paired",
                                                       "unique_paired_equal_weights",
                                                       "unique_paired_SW",
                                                       "paired_coalitions",
                                                       "paired_coalitions_weights_direct_equal_weights",
                                                       "largest_weights",
                                                       "largest_weights_combination_size",
                                                       "largest_weights_new_weights_empirical",
                                                       "largest_weights_combination_size_new_weights_empirical",
                                                       "on_all_cond",
                                                       "on_all_cond_paired",
                                                       "on_all_cond_largest_weights_random",
                                                       "on_all_cond_largest_weights_random_analytical",
                                                       "on_all_cond_largest_weights_random_non_analytical",
                                                       "on_all_cond_paired_largest_weights_random",
                                                       "on_all_cond_paired_largest_weights_random_analytical",
                                                       "on_all_cond_paired_largest_weights_random_non_analytical",
                                                       "on_all_cond_unique_paired_analytical",
                                                       "on_all_cond_unique_paired_non_analytical",
                                                       "on_all_cond_paired_unique_paired_analytical",
                                                       "on_all_cond_paired_unique_paired_non_analytical",
                                                       "on_all_cond_paired_unique_paired_mean_L",
                                                       "on_all_cond_paired_unique_paired_mean_ps",
                                                       "on_all_cond_paired_largest_weights_random_mean_L",
                                                       "on_all_cond_paired_largest_weights_random_mean_ps")

    # Pilot linear regression
    # Load the results and make the figures
    figs = list()
    for (rho_idx in seq_along(rhos)) {
      rho = rhos[rho_idx]
      figs[[rho_idx]] = plot_results(file_path = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_17_n_train_1000_n_test_500_rho_", rho, "_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_dt_MAE.rds"),
                                     index_combinations = NULL,
                                     only_these_sampling_methods = only_these_sampling_methods,
                                     figures_to_make = c("figure_CI",
                                                         "figure_mean",
                                                         "figure_median",
                                                         "figure_lines",
                                                         "figure_boxplot",
                                                         "figure_lines_boxplot",
                                                         "figure_boxplot_lines"),
                                     ggplot_theme = NULL,
                                     brewer_palette = NULL,
                                     brewer_direction = 1,
                                     flip_coordinates = FALSE,
                                     legend_position = NULL,
                                     scale_y_log10 = TRUE,
                                     scale_x_log10 = FALSE,
                                     n.dodge = 2,
                                     plot_figures = FALSE,
                                     remove_last_value = FALSE)$figure_mean + ggplot2::ggtitle(paste0("rho = ", rho, " (equi = ", rho_equi, ")"))
      if (rho_idx != length(rhos)) {
        figs[[rho_idx]] = figs[[rho_idx]] + ggplot2::guides(color = "none") + ggplot2::guides(fill = "none")
      }

    }
    gridExtra::grid.arrange(grobs = figs, nrow = 2)

    dt_all = rbindlist(lapply(figs, "[[", 1), idcol = "rho")[, rho := rhos[rho]]

    unique(dt_all$sampling)
    dt_all[, sampling := factor(sampling,
                                levels = c("unique_paired_unif_V2", "unique", "unique_paired", "unique_paired_equal_weights",  "unique_paired_SW",
                                           "unique_paired_new_weights_empirical", "unique_paired_new_weights_gompertz",
                                           "paired_coalitions_weights_direct_equal_weights",
                                           "paired_coalitions_weights_direct_equal_weights_new_weights_empirical",
                                           "paired_coalitions_weights_direct_equal_weights_new_weights_gompertz",
                                           "paired_coalitions",
                                           "paired_coalitions_new_weights_empirical",
                                           "paired_coalitions_new_weights_gompertz",
                                           "largest_weights",
                                           "largest_weights_combination_size",
                                           "largest_weights_new_weights_empirical",
                                           "largest_weights_combination_size_new_weights_empirical",
                                           "largest_weights_random", "largest_weights_random_new_weights_empirical",
                                           "MAD", "MAD_new_weights_empirical",
                                           "on_all_cond",
                                           "on_all_cond_paired",
                                           "on_all_cond_largest_weights_random",
                                           "on_all_cond_paired_largest_weights_random",
                                           "on_all_cond_largest_weights_random_analytical",
                                           "on_all_cond_paired_largest_weights_random_analytical",
                                           "on_all_cond_unique_paired_analytical",
                                           "on_all_cond_paired_unique_paired_analytical",
                                           "on_all_cond_paired_unique_paired_mean_L",
                                           "on_all_cond_paired_unique_paired_mean_ps",
                                           "on_all_cond_paired_largest_weights_random_mean_L",
                                           "on_all_cond_paired_largest_weights_random_mean_ps",

                                           "on_all_cond_paired_largest_weights_random_non_analytical",
                                           "on_all_cond_largest_weights_random_non_analytical",
                                           "on_all_cond_unique_paired_non_analytical",
                                           "on_all_cond_paired_unique_paired_non_analytical"
                                ),
                                labels = c("Uniform", "Unique", "Paired", "Paired Average", "Paired Kernel",
                                           "Paired Empirical", "Paired Gompertz",
                                           "Pilot Average", "Pilot Empirical", "Pilot Gompertz",
                                           "Pilot Largest Kernel",  "Pilot Largest Empirical", "Pilot Order Gompertz",
                                           "Paired Largest Order Kernel", "Largest Coalition",
                                           "Paired Largest Order Empirical", "Paired Largest Order Coalition Empirical",
                                           "Paired Largest Kernel", "Paired Largest Empirical",
                                           "MAD Largest Kernel", "MAD Largest Empirical",
                                           "Cond",
                                           "Paired Cond OLD",
                                           "Cond Largest",
                                           "Paired Cond Largest",
                                           "Cond Largest Empirical",
                                           "Paired Cond Largest Empirical",
                                           "Cond Empirical",
                                           "Paired Cond Empirical",
                                           "Paired Cond L",
                                           "Paired Cond pS",
                                           "Paired Cond Largest L",
                                           "Paired Cond Largest pS",
                                           "Paired Cond Largest",
                                           "Cond Largest",
                                           "Cond",
                                           "Paired Cond"

                                ),
                                ordered = FALSE)]
    unique(dt_all$sampling)

    fig2 = ggplot(dt_all[n_combinations %% 2 ==0], aes(x = n_combinations, y = median, col = sampling, fill = sampling)) +
      #geom_smooth(method = "loess", se = FALSE) +
      #stat_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.4, linewidth = 0.1) +
      geom_line(linewidth = 1) +
      facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
      #geom_ma(ma_fun = SMA, n = 10, linetype = "solid") +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      theme(legend.position = 'bottom') +
      guides(col = guide_legend(nrow = 3), fill = guide_legend(nrow = 3)) +
      #labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote("Mean absolute error between"~bold(phi)~"and"~bold(phi)[italic(D)])) +
      labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote(bar(MAE)*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
      theme(strip.text = element_text(size = rel(1.6)),
            legend.title = element_text(size = rel(1.6)),
            legend.text = element_text(size = rel(1.6)),
            axis.title = element_text(size = rel(1.6)),
            axis.text = element_text(size = rel(1.5)))
    #   scale_color_manual(values = scales::hue_pal()(6)[1:4]) +
    #   scale_fill_manual(values = scales::hue_pal()(6)[1:4])
    fig2

    ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_17_n_train_1000_n_test_500_rho_4_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5.png",
           plot = fig2,
           width = 14.2,
           height = 9.98,
           scale = 0.85,
           dpi = 350)



    m = 17
    n_features <- seq(ceiling((m - 1)/2))
    n <- sapply(n_features, choose, n = m)
    n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
    n_cumsum = (cumsum(n) + 2)
    n_cumsum = n_cumsum[-length(n_cumsum)] + 0.5

    m = 17
    n_features <- seq(ceiling((m - 1)/2))
    n <- sapply(n_features, choose, n = m)
    n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
    n_cumsum = (cumsum(n) + 2) + 0.5



    samps = c("Unique", "Paired", "Paired Average", "Paired Empirical", "Paired Kernel", "Paired Largest Empirical", "Paired Largest")
    samps = c("Unique", "Paired", "Paired Average", "Paired Kernel", "Paired Empirical", "Paired Largest Empirical")

    samps = c("Unique", "Paired", "Paired Average", "Paired Empirical", "Paired Kernel", "Paired Largest Empirical")
    samps = c("Paired Empirical", "Paired Kernel", "Paired Largest Empirical", "Paired Largest Kernel", "Paired Largest Order Empirical", "Paired Largest Order Kernel", "MAD Largest Empirical", "MAD Largest Kernel")
    samps = c("Paired Empirical", "Paired Kernel", "Paired Largest Empirical", "Paired Largest Kernel", "Pilot Empirical", "Pilot Average", "Pilot Largest Empirical", "Pilot Largest Kernel")


    samps = c("Unique", "Paired", "Paired Average", "Paired Kernel", "Paired Empirical", "Paired Largest Empirical")
    samps = c("Unique", "Paired", "Paired Average", "Paired Empirical", "Paired Kernel", "Paired Largest Empirical", "Paired Cond", "Paired Cond Largest")
    samps = c("Unique", "Paired", "Paired Average", "Paired Empirical", "Paired Kernel", "Paired Largest Empirical",
              "Paired Cond", "Paired Cond Empirical", "Paired Cond Largest", "Paired Cond Largest Empirical")

    plot(dt_all[sampling == "Paired Cond", mean] - dt_all[sampling == "Paired Empirical", mean])

    {
      par(mfrow = c(2,2))
      plot(dt_all[sampling %in% samps & n_combinations %% 2 == 0][sampling == "Paired Cond" & rho == 0.9, n_combinations],
           dt_all[sampling %in% samps & n_combinations %% 2 == 0][sampling == "Paired Cond" & rho == 0.0, mean] -
             dt_all[sampling %in% samps & n_combinations %% 2 == 0][sampling == "Paired Empirical" & rho == 0.0, mean],
           type = "l", xlab = "n_combinations", ylab = "Paired Cond - Paired Empirical", ylim = c(-0.000001, 0.000001))
      abline(h = 0, col = "red")
      plot(dt_all[sampling %in% samps & n_combinations %% 2 == 0][sampling == "Paired Cond" & rho == 0.9, n_combinations],
           dt_all[sampling %in% samps & n_combinations %% 2 == 0][sampling == "Paired Cond" & rho == 0.2, mean] -
             dt_all[sampling %in% samps & n_combinations %% 2 == 0][sampling == "Paired Empirical" & rho == 0.2, mean],
           type = "l", xlab = "n_combinations", ylab = "Paired Cond - Paired Empirical", ylim = c(-0.00005, 0.00005))
      abline(h = 0, col = "red")
      plot(dt_all[sampling %in% samps & n_combinations %% 2 == 0][sampling == "Paired Cond" & rho == 0.9, n_combinations],
           dt_all[sampling %in% samps & n_combinations %% 2 == 0][sampling == "Paired Cond" & rho == 0.5, mean] -
             dt_all[sampling %in% samps & n_combinations %% 2 == 0][sampling == "Paired Empirical" & rho == 0.5, mean],
           type = "l", xlab = "n_combinations", ylab = "Paired Cond - Paired Empirical", ylim = c(-0.0001, 0.0001))
      abline(h = 0, col = "red")
      plot(dt_all[sampling %in% samps & n_combinations %% 2 == 0][sampling == "Paired Cond" & rho == 0.9, n_combinations],
           dt_all[sampling %in% samps & n_combinations %% 2 == 0][sampling == "Paired Cond" & rho == 0.9, mean] -
             dt_all[sampling %in% samps & n_combinations %% 2 == 0][sampling == "Paired Empirical" & rho == 0.9, mean],
           type = "l", xlab = "n_combinations", ylab = "Paired Cond - Paired Empirical", ylim = c(-0.0005, 0.0005))
      abline(h = 0, col = "red")
    }


    samps = c("Unique",
              "Paired",
              "Paired Average",
              "Paired Kernel",
              "Paired Cond",
              "Paired Cond L",
              "Paired Cond Largest",
              "Paired Cond Largest L")

    dt_all2 = dt_all[sampling %in% samps]
    dt_all2 = dt_all2[, sampling := factor(sampling, levels = samps, ordered = TRUE)]
    fig3 = ggplot(dt_all2[sampling %in% samps & n_combinations %% 2 == 0],
                  aes(x = n_combinations, y = mean, col = sampling, fill = sampling)) +
      geom_vline(xintercept = n_cumsum, col = "gray50", linetype = "dashed", linewidth = 0.4) +
      #geom_vline(xintercept = 131000, col = "red") +
      #geom_smooth(method = "loess", se = FALSE) +
      #stat_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.4, linewidth = 0.0) +
      geom_line(linewidth = 0.65) +
      facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
      #geom_ma(ma_fun = SMA, n = 10, linetype = "solid") +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      scale_x_continuous(labels = scales::label_number()) +
      # scale_x_log10(
      #   breaks = scales::trans_breaks("log10", function(x) 10^x),
      #   labels = scales::trans_format("log10", scales::math_format(10^.x))
      # ) +
      theme(legend.position = 'bottom') +
      guides(col = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
      labs(color = "Strategy:", fill = "Strategy:", linetype = "Strategy:", x = expression(N[S]), y = bquote(bar(MAE)*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
      theme(strip.text = element_text(size = rel(1.6)),
            legend.title = element_text(size = rel(1.4)),
            legend.text = element_text(size = rel(1.4)),
            axis.title = element_text(size = rel(1.6)),
            axis.text = element_text(size = rel(1.5))) +
      scale_color_hue() + #added as we want ordered
      scale_fill_hue()

    fig3


    ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_17_n_train_1000_n_test_500_rho_4_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_NEW_STRAT_3.png",
           plot = fig3,
           width = 14.2,
           height = 9.98,
           scale = 0.85,
           dpi = 350)



    plot(dt_all[sampling == "Paired Average" & n_combinations %% 2 == 0 & rho == 0.5, mean] -
      dt_all[sampling == "Paired Empirical" & n_combinations %% 2 == 0 & rho == 0.5, mean])


    fig = relative_difference(dt = dt_all,
                              m = 17,
                              strat_ref = "Paired Cond",
                              strat_other = c("Unique",
                                              "Paired",
                                              "Paired Average",
                                              "Paired Kernel",
                                              "Paired Cond",
                                              "Paired Cond L",
                                              "Paired Cond Largest",
                                              "Paired Cond Largest L"),
                              y_breaks = c(-1, -0.4, -0.1, 0, 0.1, 0.4, 1, 2, 4, 8, 12, 16, 20),
                              y_limits = c(-1, 22),
                              include_coal_size_lines = TRUE)
    fig
    ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_17_n_train_1000_n_test_500_rho_4_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_Relative_difference.png"),
           plot = fig,
           width = 14.2,
           height = 9.98,
           scale = 0.85,
           dpi = 350)

    fig2 = relative_difference(dt = dt_all,
                              m = 17,
                              strat_ref = "Paired Cond",
                              strat_other = c("Unique",
                                              "Paired",
                                              "Paired Average",
                                              "Paired Kernel",
                                              "Paired Cond",
                                              "Paired Cond L",
                                              "Paired Cond Largest",
                                              "Paired Cond Largest L"),
                              y_breaks = c(-1, -0.4, -0.1, 0, 0.1, 0.4),
                              y_limits = c(-1, 5.1),
                              include_coal_size_lines = TRUE)
    fig2
    ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_17_n_train_1000_n_test_500_rho_4_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_Relative_difference_zoom.png"),
           plot = fig2,
           width = 14.2,
           height = 9.98,
           scale = 0.85,
           dpi = 350)

    fig3 = relative_difference(dt = dt_all,
                        m = 17,
                        strat_ref = "Paired Cond",
                        strat_other = c("Paired",
                                        "Paired Average",
                                        "Paired Cond",
                                        "Paired Cond L"),
                        y_limits = c(-0.05, 0.15),
                        scale = FALSE,
                        legend_n_row = 1,
                        hue_length = 8,
                        hue_indices = c(2,3,5,6))

    fig3
    ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/M_17_n_train_1000_n_test_500_rho_4_equi_FALSE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_3_-1_-5_4_-10_2_5_Relative_difference_paired.png"),
           plot = fig3,
           width = 14.2,
           height = 9.98,
           scale = 0.85,
           dpi = 350)



  }





}


if (FALSE) {
  # Xgboost ---------------------------------------------------------------------------------------------------------
  # M = 8 -----------------------------------------------------------------------------------------------------------


  {
    # Parameters
    rhos = c(0,0.05,0.1,0.2,0.3,0.5,0.7,0.9,0.9)
    rho_equi = TRUE # Can change this

    only_these_sampling_methods = c(
      "paired_coalitions_weights",
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
      "single_mean_coalition_effect"
    )

    only_these_sampling_methods = c(
      #"paired_coalitions_weights",
      #"paired_coalitions_weights_direct",
      "paired_coalitions_weights_equal_weights",
      "paired_coalitions_weights_direct_equal_weights",
      "paired_coalitions",
      "unique",
      #"unique_unif",
      #"unique_unif_V2",
      #"unique_SW",
      #"unique_equal_weights",
      #"unique_equal_weights_symmetric",
      "unique_paired",
      #"unique_paired_unif",
      #"unique_paired_unif_V2",
      "unique_paired_SW",
      "unique_paired_equal_weights"
      #"unique_paired_equal_weights_100",
      #"unique_paired_equal_weights_500",
      #"unique_paired_equal_weights_1000",
      #"unique_paired_equal_weights_5000",
      #"unique_paired_equal_weights_10000",
      #"unique_paired_equal_weights_50000",
      #"unique_paired_equal_weights_symmetric"
      #"single_mean_coalition_effect"
    )

    # Load the results and make the figures
    figs = list()
    for (rho_idx in seq_along(rhos)) {
      rho = rhos[rho_idx]
      figs[[rho_idx]] = plot_results(file_path = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Xgboost_M_8_n_train_1000_n_test_1000_rho_", rho, "_equi_", rho_equi ,"_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_dt_MAE.rds"),
                                     index_combinations = NULL,
                                     only_these_sampling_methods = only_these_sampling_methods,
                                     figures_to_make = c("figure_CI",
                                                         "figure_mean",
                                                         "figure_median",
                                                         "figure_lines",
                                                         "figure_boxplot",
                                                         "figure_lines_boxplot",
                                                         "figure_boxplot_lines"),
                                     ggplot_theme = NULL,
                                     brewer_palette = NULL,
                                     brewer_direction = 1,
                                     flip_coordinates = FALSE,
                                     legend_position = NULL,
                                     scale_y_log10 = TRUE,
                                     scale_x_log10 = FALSE,
                                     n.dodge = 2,
                                     plot_figures = FALSE)$figure_mean + ggplot2::ggtitle(paste0("rho = ", rho, " (equi = ", rho_equi, ")"))
      if (rho_idx != length(rhos)) {
        figs[[rho_idx]] = figs[[rho_idx]] + ggplot2::guides(color = "none") + ggplot2::guides(fill = "none")
      }

    }
    gridExtra::grid.arrange(grobs = figs, nrow = 2)
  }
  #Xgboost_M_8_n_train_1000_n_test_1000_rho_MANY_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25



  # M = 10 ----------------------------------------------------------------------------------------------------------


  {
    # Parameters
    rhos = c(0,0.05,0.1,0.2,0.3,0.5,0.7,0.9,0.9)
    rho_equi = TRUE # Can change this

    only_these_sampling_methods = c(
      "paired_coalitions_weights",
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
      "single_mean_coalition_effect"
    )

    only_these_sampling_methods = c(
      "paired_coalitions_weights",
      "paired_coalitions_weights_direct",
      "paired_coalitions_weights_equal_weights",
      "paired_coalitions_weights_direct_equal_weights",
      "paired_coalitions",
      # "unique",
      # "unique_unif",
      # "unique_unif_V2",
      # "unique_SW",
      # "unique_equal_weights",
      # "unique_equal_weights_symmetric",
      # "unique_paired",
      # "unique_paired_unif",
      # "unique_paired_unif_V2",
      # "unique_paired_SW",
       "unique_paired_equal_weights"
      # "unique_paired_equal_weights_100",
      # "unique_paired_equal_weights_500",
      # "unique_paired_equal_weights_1000",
      # "unique_paired_equal_weights_5000",
      # "unique_paired_equal_weights_10000",
      # "unique_paired_equal_weights_50000",
      # "unique_paired_equal_weights_symmetric",
      # "single_mean_coalition_effect"
    )

    only_these_sampling_methods = c(
      #"paired_coalitions_weights",
      #"paired_coalitions_weights_direct",
      "paired_coalitions_weights_equal_weights",
      "paired_coalitions_weights_direct_equal_weights",
      "paired_coalitions",
      "unique",
      #"unique_unif",
      #"unique_unif_V2",
      #"unique_SW",
      #"unique_equal_weights",
      #"unique_equal_weights_symmetric",
      "unique_paired",
      #"unique_paired_unif",
      #"unique_paired_unif_V2",
      "unique_paired_SW",
      "unique_paired_equal_weights",
      #"unique_paired_equal_weights_100",
      "unique_paired_equal_weights_500"
      #"unique_paired_equal_weights_1000",
      #"unique_paired_equal_weights_5000",
      #"unique_paired_equal_weights_10000",
      #"unique_paired_equal_weights_50000",
      #"unique_paired_equal_weights_symmetric"
      #"single_mean_coalition_effect"
    )

    # Load the results and make the figures
    figs = list()
    for (rho_idx in seq_along(rhos)) {
      rho = rhos[rho_idx]
      figs[[rho_idx]] = plot_results(file_path = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Xgboost_M_10_n_train_1000_n_test_1000_rho_", rho, "_equi_", rho_equi ,"_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds"),
                                     index_combinations = NULL,
                                     only_these_sampling_methods = only_these_sampling_methods,
                                     figures_to_make = c("figure_CI",
                                                         "figure_mean",
                                                         "figure_median",
                                                         "figure_lines",
                                                         "figure_boxplot",
                                                         "figure_lines_boxplot",
                                                         "figure_boxplot_lines"),
                                     ggplot_theme = NULL,
                                     brewer_palette = NULL,
                                     brewer_direction = 1,
                                     flip_coordinates = FALSE,
                                     legend_position = NULL,
                                     scale_y_log10 = TRUE,
                                     scale_x_log10 = FALSE,
                                     n.dodge = 2,
                                     plot_figures = FALSE)$figure_mean + ggplot2::ggtitle(paste0("rho = ", rho, " (equi = ", rho_equi, ")"))
      if (rho_idx != length(rhos)) {
        figs[[rho_idx]] = figs[[rho_idx]] + ggplot2::guides(color = "none") + ggplot2::guides(fill = "none")
      }

    }
    gridExtra::grid.arrange(grobs = figs, nrow = 2)
  }
  #Xgboost_M_8_n_train_1000_n_test_1000_rho_MANY_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25




  ## NSM2024 ---------------------------------------------------------------------------------------------------------

  {
    # Parameters
    rhos = c(0,0.2,0.5,0.9)
    rho_equi = TRUE # Can change this

    only_these_sampling_methods = c("unique_paired_unif_V2",
                                    "unique",
                                    "unique_paired",
                                    "unique_paired_equal_weights",
                                    "unique_paired_SW",
                                    "paired_coalitions",
                                    "paired_coalitions_weights_direct_equal_weights")

    # Load the results and make the figures
    figs = list()
    for (rho_idx in seq_along(rhos)) {
      rho = rhos[rho_idx]
      figs[[rho_idx]] = plot_results(file_path = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/NSM2024_Xgboost_M_10_n_train_1000_n_test_1000_rho_", rho, "_equi_", rho_equi ,"_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_dt_MAE.rds"),
                                     index_combinations = NULL,
                                     #only_these_sampling_methods = only_these_sampling_methods,
                                     figures_to_make = c("figure_CI",
                                                         "figure_mean",
                                                         "figure_median",
                                                         "figure_lines",
                                                         "figure_boxplot",
                                                         "figure_lines_boxplot",
                                                         "figure_boxplot_lines"),
                                     ggplot_theme = NULL,
                                     brewer_palette = NULL,
                                     brewer_direction = 1,
                                     flip_coordinates = FALSE,
                                     legend_position = NULL,
                                     scale_y_log10 = FALSE, #TRUE,
                                     scale_x_log10 = FALSE,
                                     n.dodge = 2,
                                     plot_figures = FALSE)$figure_mean + ggplot2::ggtitle(paste0("rho = ", rho, " (equi = ", rho_equi, ")")) +
        xlim(0, 200)
      if (rho_idx != length(rhos)) {
        figs[[rho_idx]] = figs[[rho_idx]] + ggplot2::guides(color = "none") + ggplot2::guides(fill = "none")
      }

    }
    gridExtra::grid.arrange(grobs = figs, nrow = 2)

    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }

    dt_all = rbindlist(lapply(figs, "[[", 1), idcol = "rho")[, rho := rhos[rho]]

    dt_all[, sampling := factor(sampling,
                        levels = c("unique_paired_unif_V2", "unique", "unique_paired", "unique_paired_equal_weights",  "unique_paired_SW", "paired_coalitions_weights_direct_equal_weights", "paired_coalitions"),
                        labels = c("Uniform", "Unique", "Paired", "Paired avg.", "Paired kernel", "Pilot", "Pilot kernel"))]

    library(latex2exp)
    fig2 = ggplot(dt_all, aes(x = n_combinations, y = mean, col = sampling)) +
      #geom_smooth(method = "loess", se = FALSE) +
      #stat_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +
      geom_line(linewidth = 1) +
      #geom_ma(ma_fun = SMA, n = 10, linetype = "solid") +
      facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
      # scale_y_log10(
      #   breaks = scales::trans_breaks("log10", function(x) 10^x),
      #   labels = scales::trans_format("log10", scales::math_format(10^.x))
      # ) +
      theme(legend.position = 'bottom') +
      guides(col = guide_legend(nrow = 1)) +
      labs(color = "Strategy:", x = expression(N[combinations]), y = bquote("Mean absolute error between"~bold(phi)~"and"~bold(phi)[italic(D)])) +
      theme(strip.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.4))) + scale_color_manual(values = gg_color_hue(7))

    fig2
    fig2 + xlim(0, 200)

    ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/NSM2024_Xgboost_M_10_n_train_1000_n_test_1000_rho_MANY_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_plot.png",
           plot = fig2,
           scale = 0.8,
           dpi = 400)





    # Pilot linear regression
    # Load the results and make the figures
    figs = list()
    for (rho_idx in seq_along(rhos)) {
      rho = rhos[rho_idx]
      figs[[rho_idx]] = plot_results(file_path = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/NSM2024_Xgboost_M_10_n_train_1000_n_test_1000_rho_", rho, "_equi_", rho_equi ,"_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_pilot_separate_linear_reg_dt_MAE.rds"),
                                     index_combinations = NULL,
                                     only_these_sampling_methods = only_these_sampling_methods,
                                     figures_to_make = c("figure_CI",
                                                         "figure_mean",
                                                         "figure_median",
                                                         "figure_lines",
                                                         "figure_boxplot",
                                                         "figure_lines_boxplot",
                                                         "figure_boxplot_lines"),
                                     ggplot_theme = NULL,
                                     brewer_palette = NULL,
                                     brewer_direction = 1,
                                     flip_coordinates = FALSE,
                                     legend_position = NULL,
                                     scale_y_log10 = TRUE,
                                     scale_x_log10 = FALSE,
                                     n.dodge = 2,
                                     plot_figures = FALSE)$figure_mean + ggplot2::ggtitle(paste0("rho = ", rho, " (equi = ", rho_equi, ")"))
      if (rho_idx != length(rhos)) {
        figs[[rho_idx]] = figs[[rho_idx]] + ggplot2::guides(color = "none") + ggplot2::guides(fill = "none")
      }

    }
    gridExtra::grid.arrange(grobs = figs, nrow = 2)

    dt_all = rbindlist(lapply(figs, "[[", 1), idcol = "rho")[, rho := rhos[rho]]

    dt_all[, sampling := factor(sampling,
                                levels = c("unique_paired_unif_V2", "unique", "unique_paired", "unique_paired_equal_weights",  "unique_paired_SW", "paired_coalitions_weights_direct_equal_weights", "paired_coalitions"),
                                labels = c("Uniform", "Unique", "Paired", "Paired avg.", "Paired kernel", "Pilot", "Pilot kernel"))]

    fig2 = ggplot(dt_all, aes(x = n_combinations, y = mean, col = sampling)) +
      #geom_smooth(method = "loess", se = FALSE) +
      #stat_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +
      geom_line(linewidth = 1) +
      #geom_ma(ma_fun = SMA, n = 10, linetype = "solid") +
      facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      theme(legend.position = 'bottom') +
      guides(col = guide_legend(nrow = 1)) +
      labs(color = "Strategy:", x = expression(N[combinations]), y = bquote("Mean absolute error between"~bold(phi)~"and"~bold(phi)[italic(D)])) +
      theme(strip.text = element_text(size = rel(1.6)),
            legend.title = element_text(size = rel(1.6)),
            legend.text = element_text(size = rel(1.6)),
            axis.title = element_text(size = rel(1.6)),
            axis.text = element_text(size = rel(1.5))) + scale_color_manual(values = gg_color_hue(7))

    fig2
    ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/NSM2024_Xgboost_M_10_n_train_1000_n_test_1000_rho_MANY_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_pilot_separate_linear_reg_plot_8.png",
           plot = fig2,
           scale = 0.8,
           dpi = 400)

  }


  ## Paper3 ----------------------------------------------------------------------------------------------------------
  {
    # Parameters
    rhos = c(0,0.2,0.5,0.9)
    rho_equi = TRUE # Can change this

    only_these_sampling_methods = c("unique",
                                    "unique_paired",
                                    "unique_paired_equal_weights",
                                    "unique_paired_SW",
                                    "paired_coalitions",
                                    "paired_coalitions_weights_direct_equal_weights")

    # Pilot linear regression
    # Load the results and make the figures
    figs = list()
    for (rho_idx in seq_along(rhos)) {
      rho = rhos[rho_idx]
      figs[[rho_idx]] = plot_results(file_path = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/NSM2024_Xgboost_M_10_n_train_1000_n_test_1000_rho_", rho, "_equi_", rho_equi ,"_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_pilot_separate_linear_reg_dt_MAE.rds"),
                                     index_combinations = NULL,
                                     only_these_sampling_methods = only_these_sampling_methods,
                                     figures_to_make = c("figure_CI",
                                                         "figure_mean",
                                                         "figure_median",
                                                         "figure_lines",
                                                         "figure_boxplot",
                                                         "figure_lines_boxplot",
                                                         "figure_boxplot_lines"),
                                     ggplot_theme = NULL,
                                     brewer_palette = NULL,
                                     brewer_direction = 1,
                                     flip_coordinates = FALSE,
                                     legend_position = NULL,
                                     scale_y_log10 = TRUE,
                                     scale_x_log10 = FALSE,
                                     n.dodge = 2,
                                     plot_figures = FALSE)$figure_mean + ggplot2::ggtitle(paste0("rho = ", rho, " (equi = ", rho_equi, ")"))
      if (rho_idx != length(rhos)) {
        figs[[rho_idx]] = figs[[rho_idx]] + ggplot2::guides(color = "none") + ggplot2::guides(fill = "none")
      }

    }
    gridExtra::grid.arrange(grobs = figs, nrow = 2)

    dt_all = rbindlist(lapply(figs, "[[", 1), idcol = "rho")[, rho := rhos[rho]]

    dt_all[, sampling := factor(sampling,
                                levels = c("unique_paired_unif_V2", "unique", "unique_paired", "unique_paired_equal_weights",  "unique_paired_SW", "paired_coalitions_weights_direct_equal_weights", "paired_coalitions"),
                                labels = c("Uniform", "Unique", "Paired", "Paired Average", "Paired Kernel", "Pilot Average", "Pilot Kernel"),
                                ordered = FALSE)]

    fig2 = ggplot(dt_all, aes(x = n_combinations, y = mean, col = sampling, fill = sampling)) +
      #geom_smooth(method = "loess", se = FALSE) +
      #stat_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.4, linewidth = 0.1) +
      geom_line(linewidth = 1) +
      facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
      ggplot2::scale_fill_hue() +
      ggplot2::scale_color_hue() +
      #geom_ma(ma_fun = SMA, n = 10, linetype = "solid") +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      theme(legend.position = 'bottom') +
      guides(col = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
      #labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote("Mean absolute error between"~bold(phi)~"and"~bold(phi)[italic(D)])) +
      labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote(bar(MAE)*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
      theme(strip.text = element_text(size = rel(1.6)),
            legend.title = element_text(size = rel(1.6)),
            legend.text = element_text(size = rel(1.6)),
            axis.title = element_text(size = rel(1.6)),
            axis.text = element_text(size = rel(1.5)))

    fig2
    ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_Xgboost_M_10_n_train_1000_n_test_1000_rho_4_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_pilot_separate_linear_reg_plot_with_V3.png",
           plot = fig2,
           width = 14.2,
           height = 9.98,
           scale = 0.85,
           dpi = 350)
  }



  ### With new methods ------------------------------------------------------------------------------------------------
  {
    # Parameters
    rhos = c(0,0.2,0.5,0.9)
    rho_equi = TRUE # Can NOT change this

    # only_these_sampling_methods = c("unique",
    #                                 "unique_paired",
    #                                 "unique_paired_equal_weights",
    #                                 "unique_paired_SW",
    #                                 "paired_coalitions",
    #                                 "paired_coalitions_weights_direct_equal_weights")

    # Pilot linear regression
    # Load the results and make the figures
    figs = list()
    for (rho_idx in seq_along(rhos)) {
      rho = rhos[rho_idx]
      figs[[rho_idx]] = plot_results(file_path = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_", rho, "_equi_", rho_equi ,"_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_pilot_separate_linear_reg_dt_MAE.rds"),
                                     index_combinations = NULL,
                                     #only_these_sampling_methods = only_these_sampling_methods,
                                     figures_to_make = c("figure_CI",
                                                         "figure_mean",
                                                         "figure_median",
                                                         "figure_lines",
                                                         "figure_boxplot",
                                                         "figure_lines_boxplot",
                                                         "figure_boxplot_lines"),
                                     ggplot_theme = NULL,
                                     brewer_palette = NULL,
                                     brewer_direction = 1,
                                     flip_coordinates = FALSE,
                                     legend_position = NULL,
                                     scale_y_log10 = TRUE,
                                     scale_x_log10 = FALSE,
                                     n.dodge = 2,
                                     plot_figures = FALSE)$figure_mean + ggplot2::ggtitle(paste0("rho = ", rho, " (equi = ", rho_equi, ")"))
      if (rho_idx != length(rhos)) {
        figs[[rho_idx]] = figs[[rho_idx]] + ggplot2::guides(color = "none") + ggplot2::guides(fill = "none")
      }

    }
    gridExtra::grid.arrange(grobs = figs, nrow = 2)

    dt_all = rbindlist(lapply(figs, "[[", 1), idcol = "rho")[, rho := rhos[rho]]

    dt_all[, sampling := factor(sampling,
                                levels = c("unique_paired_unif_V2", "unique", "unique_paired", "unique_paired_equal_weights",  "unique_paired_SW",
                                           "unique_paired_new_weights_empirical", "unique_paired_new_weights_gompertz",
                                           "paired_coalitions_weights_direct_equal_weights",
                                           "paired_coalitions_weights_direct_equal_weights_new_weights_empirical",
                                           "paired_coalitions_weights_direct_equal_weights_new_weights_gompertz",
                                           "paired_coalitions",
                                           "paired_coalitions_new_weights_empirical",
                                           "paired_coalitions_new_weights_gompertz",
                                           "largest_weights",
                                           "largest_weights_combination_size",
                                           "largest_weights_new_weights_empirical",
                                           "largest_weights_combination_size_new_weights_empirical"),
                                labels = c("Uniform", "Unique", "Paired", "Paired Average", "Paired Kernel",
                                           "Paired Empirical", "Paired Gompertz",
                                           "Pilot Average", "Pilot Sample Empirical", "Pilot Sample Gompertz",
                                           "Pilot Kernel",  "Pilot Order Empirical", "Pilot Order Gompertz",
                                           "Largest Weights", "Largest Weights Coalition",
                                           "Largest Weights Empirical", "Largest Weights Coalition Empirical"),
                                ordered = FALSE)]

    fig2 = ggplot(dt_all, aes(x = n_combinations, y = mean, col = sampling, fill = sampling)) +
      #geom_smooth(method = "loess", se = FALSE) +
      #stat_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +
      #ggplot2::geom_ribbon(ggplot2::aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.4, linewidth = 0.1) +
      geom_line(linewidth = 1) +
      facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
      ggplot2::scale_fill_hue() +
      ggplot2::scale_color_hue() +
      #geom_ma(ma_fun = SMA, n = 10, linetype = "solid") +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      theme(legend.position = 'bottom') +
      guides(col = guide_legend(nrow = 4), fill = guide_legend(nrow = 4)) +
      #labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote("Mean absolute error between"~bold(phi)~"and"~bold(phi)[italic(D)])) +
      labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote(bar(MAE)*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
      theme(strip.text = element_text(size = rel(1.6)),
            legend.title = element_text(size = rel(1.6)),
            legend.text = element_text(size = rel(1.6)),
            axis.title = element_text(size = rel(1.6)),
            axis.text = element_text(size = rel(1.5)))

    fig2
    ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_4_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_pilot_separate_linear_reg_plot_with_V3.png",
           plot = fig2,
           width = 14.2,
           height = 9.98,
           scale = 0.85,
           dpi = 350)
  }


  ### Freq vs kernel shap values ----------
  {
    # Parameters
    rhos = c(0,0.2,0.5,0.9)
    rho_equi = TRUE # Can change this

    only_these_sampling_methods = c(
      "largest_weights",
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
      "paired_coalitions"
      )

    only_these_sampling_methods = c(
      "largest_weights",
      #"unique_paired_unif_V2",
      "unique_paired_SW",
      #"unique_paired_equal_weights",
      #"unique_paired_equal_weights_symmetric",
      "paired_coalitions"
    )

    only_these_sampling_methods = c(
      # "largest_weights",
      # "unique_paired_unif_V2",
      "unique_paired_SW",
      "unique_paired_equal_weights",
      "unique_paired_equal_weights_100",
      "unique_paired_equal_weights_500",
      "unique_paired_equal_weights_1000",
      "unique_paired_equal_weights_2500",
      "unique_paired_equal_weights_5000",
      "unique_paired_equal_weights_10000",
      "unique_paired_equal_weights_50000"
      # "unique_paired_equal_weights_symmetric",
      # "paired_coalitions"
    )

    # Pilot linear regression
    # Load the results and make the figures
    figs = list()
    for (rho_idx in seq_along(rhos)) {
      rho = rhos[rho_idx]
      figs[[rho_idx]] = plot_results(file_path = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Samp_VS_kernel_Xgboost_M_10_n_train_1000_n_test_1000_rho_", rho, "_equi_", rho_equi ,"_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_pilot_separate_linear_reg_dt_MAE.rds"),
                                     index_combinations = NULL,
                                     only_these_sampling_methods = only_these_sampling_methods,
                                     figures_to_make = c("figure_CI",
                                                         "figure_mean",
                                                         "figure_median",
                                                         "figure_lines",
                                                         "figure_boxplot",
                                                         "figure_lines_boxplot",
                                                         "figure_boxplot_lines"),
                                     ggplot_theme = NULL,
                                     brewer_palette = NULL,
                                     brewer_direction = 1,
                                     flip_coordinates = FALSE,
                                     legend_position = NULL,
                                     scale_y_log10 = TRUE,
                                     scale_x_log10 = FALSE,
                                     n.dodge = 2,
                                     plot_figures = FALSE)$figure_mean + ggplot2::ggtitle(paste0("rho = ", rho, " (equi = ", rho_equi, ")"))
      if (rho_idx != length(rhos)) {
        figs[[rho_idx]] = figs[[rho_idx]] + ggplot2::guides(color = "none") + ggplot2::guides(fill = "none")
      }

    }
    gridExtra::grid.arrange(grobs = figs, nrow = 2)

    dt_all = rbindlist(lapply(figs, "[[", 1), idcol = "rho")[, rho := rhos[rho]]

    dt_all[, sampling := factor(sampling,
                                levels =  c(
                                  "largest_weights",
                                  "unique_paired_unif_V2",
                                  "unique_paired_equal_weights",
                                  "unique_paired_equal_weights_100",
                                  "unique_paired_equal_weights_500",
                                  "unique_paired_equal_weights_1000",
                                  "unique_paired_equal_weights_2500",
                                  "unique_paired_equal_weights_5000",
                                  "unique_paired_equal_weights_10000",
                                  "unique_paired_equal_weights_50000",
                                  "unique_paired_equal_weights_symmetric", # Same as "unique_paired_equal_weights"
                                  "paired_coalitions",
                                  "unique_paired_SW"
                                ),
                                labels = c(
                                  "Largest weights",
                                  "Uniform",
                                  "Paired Average",
                                  "Paired Average (L = 100)",
                                  "Paired Average (L = 500)",
                                  "Paired Average (L = 1000)",
                                  "Paired Average (L = 2500)",
                                  "Paired Average (L = 5000)",
                                  "Paired Average (L = 10000)",
                                  "Paired Average (L = 50000)",
                                  "Paired Average Symmetric", # Same as "Paired Average"
                                  "Pilot kernel",
                                  "Paired Kernel"
                                ),
                                ordered = FALSE)]

    fig2 = ggplot(dt_all[!(sampling %in% c("Largest weights",
                                      "Uniform",
                                      "Unique Kernel",
                                      "Pilot kernel"))], aes(x = n_combinations, y = mean, col = sampling, fill = sampling)) +
      #geom_smooth(method = "loess", se = FALSE) +
      #stat_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +
      #ggplot2::geom_ribbon(ggplot2::aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
      geom_line(linewidth = 1) +
      facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
      ggplot2::scale_fill_hue() +
      ggplot2::scale_color_hue() +
      #geom_ma(ma_fun = SMA, n = 10, linetype = "solid") +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      theme(legend.position = 'bottom') +
      guides(col = guide_legend(nrow = 3), fill = guide_legend(nrow = 3)) +
      #labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote("Mean absolute error between"~bold(phi)~"and"~bold(phi)[italic(D)])) +
      labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote(bar(MAE)*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
      theme(strip.text = element_text(size = rel(1.6)),
            legend.title = element_text(size = rel(1.6)),
            legend.text = element_text(size = rel(1.6)),
            axis.title = element_text(size = rel(1.6)),
            axis.text = element_text(size = rel(1.5)))

    fig2
    ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Samp_VS_kernel_Paper3_Xgboost_M_10_n_train_1000_n_test_1000_rho_4_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_V3.png",
           plot = fig2,
           width = 14.2,
           height = 11,
           scale = 0.85,
           dpi = 350)

  }


  ### Gompertz --------------------------------------------------------------------------------------------------------

  {
    # Parameters
    rhos = c(0,0.2,0.5,0.9)
    rho_equi = TRUE # Can NOT change this
    pilot = FALSE
    ext_str = ifelse(pilot, "_pilot_separate_linear_reg", "")

    only_these_sampling_methods = dt_strategy_names$Original

    # Pilot linear regression
    # Load the results and make the figures
    figs = list()
    for (rho_idx in seq_along(rhos)) {
      rho = rhos[rho_idx]
      file_path = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_",
                         rho, "_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2", ext_str ,"_dt_MAE.rds")
      figs[[rho_idx]] = plot_results(file_path = file_path ,
                                     index_combinations = NULL,
                                     only_these_sampling_methods = only_these_sampling_methods,
                                     figures_to_make = c("figure_CI",
                                                         "figure_mean",
                                                         "figure_median",
                                                         "figure_lines",
                                                         "figure_boxplot",
                                                         "figure_lines_boxplot",
                                                         "figure_boxplot_lines"),
                                     ggplot_theme = NULL,
                                     brewer_palette = NULL,
                                     brewer_direction = 1,
                                     flip_coordinates = FALSE,
                                     legend_position = NULL,
                                     scale_y_log10 = TRUE,
                                     scale_x_log10 = FALSE,
                                     n.dodge = 2,
                                     plot_figures = FALSE)$figure_mean + ggplot2::ggtitle(paste0("rho = ", rho, " (equi = ", rho_equi, ")"))
      if (rho_idx != length(rhos)) {
        figs[[rho_idx]] = figs[[rho_idx]] + ggplot2::guides(color = "none") + ggplot2::guides(fill = "none")
      }

    }
    gridExtra::grid.arrange(grobs = figs, nrow = 2)

    dt_all = rbindlist(lapply(figs, "[[", 1), idcol = "rho")[, rho := rhos[rho]]

    dt_all[, sampling := factor(sampling,
                                levels = dt_strategy_names$Original,
                                labels = dt_strategy_names$New,
                                ordered = FALSE)]

    m = 10
    n_features <- seq(ceiling((m - 1)/2))
    n <- sapply(n_features, choose, n = m)
    n[seq(floor((m - 1)/2))] = 2*n[seq(floor((m - 1)/2))]
    n_cumsum = (cumsum(n) + 2) + 0.5

    fig2 = ggplot(dt_all[n_combinations %% 2 == 0], aes(x = n_combinations, y = mean, col = sampling, fill = sampling)) +
      geom_vline(xintercept = n_cumsum, col = "gray60", linetype = "dashed", linewidth = 0.4) +
      #geom_smooth(method = "loess", se = FALSE) +
      #stat_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +
      #ggplot2::geom_ribbon(ggplot2::aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.4, linewidth = 0.1) +
      geom_line(linewidth = 0.6) +
      facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
      #geom_ma(ma_fun = SMA, n = 10, linetype = "solid") +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      theme(legend.position = 'bottom') +
      guides(col = guide_legend(nrow = 3), fill = guide_legend(nrow = 3)) +
      #labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote("Mean absolute error between"~bold(phi)~"and"~bold(phi)[italic(D)])) +
      labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote(bar(MAE)*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
      theme(strip.text = element_text(size = rel(1.6)),
            legend.title = element_text(size = rel(1.6)),
            legend.text = element_text(size = rel(1.6)),
            axis.title = element_text(size = rel(1.6)),
            axis.text = element_text(size = rel(1.5)))
      # scale_color_manual(values = scales::hue_pal()(6)[1:4]) +
      # scale_fill_manual(values = scales::hue_pal()(6)[1:4])
    fig2

    ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_4_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2", ext_str, "_V1.png"),
           plot = fig2,
           width = 14.2,
           height = 9.98,
           scale = 0.85,
           dpi = 350)







    samps = c("Unique",
              "Paired",
              "Paired Average",
              "Paired Kernel",
              "Paired C-Kernel",
              "Paired CEL-Kernel",
              "Paired Imp C-Kernel",
              "Paired Imp CEL-Kernel")



    dt_all2 = dt_all[sampling %in% samps & n_combinations %% 2 == 0,]
    dt_all2 = dt_all2[, sampling := factor(sampling, levels = samps, ordered = TRUE)]


    {
      # Without bands and with dotted lines
      fig_wo_bands = ggplot(dt_all2, aes(x = n_combinations, y = mean, col = sampling, fill = sampling, linetype = sampling)) +
        geom_vline(xintercept = n_cumsum, col = "gray50", linetype = "dashed", linewidth = 0.4) +
        #ggplot2::geom_ribbon(ggplot2::aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.4, linewidth = 0) +
        geom_line(linewidth = 0.65) +
        facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
        scale_y_log10(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::trans_format("log10", scales::math_format(10^.x))
        ) +
        scale_x_continuous(labels = scales::label_number()) +
        theme(legend.position = 'bottom') +
        guides(col = guide_legend(nrow = 2, theme = theme(legend.byrow = FALSE)),
               fill = guide_legend(nrow = 2, theme = theme(legend.byrow = FALSE)),
               linetype = "none") +
        labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote(bar(MAE)*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
        theme(strip.text = element_text(size = rel(1.6)),
              legend.title = element_text(size = rel(1.37)),
              legend.text = element_text(size = rel(1.37)),
              axis.title = element_text(size = rel(1.6)),
              axis.text = element_text(size = rel(1.5))) +
        scale_color_hue() + #added as we want ordered
        scale_fill_hue()

      fig_wo_bands

      ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_4_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2", ext_str, "_FINAL_wo_bands.png"),
             plot = fig_wo_bands,
             width = 14.2,
             height = 9.98,
             scale = 0.85,
             dpi = 350)
    }

    {
      # With bands
      fig_bands = ggplot(dt_all2, aes(x = n_combinations, y = mean, col = sampling, fill = sampling)) +
        geom_vline(xintercept = n_cumsum, col = "gray50", linetype = "dashed", linewidth = 0.4) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, linewidth = 0) +
        geom_line(linewidth = 0.65) +
        facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
        scale_y_log10(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::trans_format("log10", scales::math_format(10^.x))
        ) +
        scale_x_continuous(labels = scales::label_number()) +
        theme(legend.position = 'bottom') +
        guides(col = guide_legend(nrow = 2, theme = theme(legend.byrow = FALSE)),
               fill = guide_legend(nrow = 2, theme = theme(legend.byrow = FALSE)),
               linetype = "none") +
        labs(color = "Strategy:", fill = "Strategy:", x = expression(N[S]), y = bquote(bar(MAE)*"("*bold(phi)*", "*bold(phi)[italic(D)]*")")) +
        theme(strip.text = element_text(size = rel(1.6)),
              legend.title = element_text(size = rel(1.37)),
              legend.text = element_text(size = rel(1.37)),
              axis.title = element_text(size = rel(1.6)),
              axis.text = element_text(size = rel(1.5))) +
        scale_color_hue() + #added as we want ordered
        scale_fill_hue()

      fig_bands

      ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_4_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2", ext_str, "_FINAL_w_bands_V2.png"),
             plot = fig_bands,
             width = 14.2,
             height = 9.98,
             scale = 0.85,
             dpi = 350)
    }







    dt3 = dcast(dt_all2[n_combinations %% 2 == 0], rho + n_combinations ~ sampling, value.var = "mean")
    dt3


    dt3[, diff := (`Paired Cond` - `Paired Average`) / `Paired Average`]
    dt3[, diff := (`Paired Cond` - `Paired Cond L`) / `Paired Cond L`]
    dt3[, diff := (`Paired Cond` - `Paired Cond pS`) / `Paired Cond pS`]
    dt3[, diff := (`Paired Cond Largest L` - `Paired Cond pS`) / `Paired Cond pS`]

    ggplot(dt3, aes(x = n_combinations, y = diff)) +
      geom_hline(yintercept = 0, col = "gray") +
      geom_line() +
      facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="free_y") +
      labs(y = "Relative difference")



    fig_rel = relative_difference(dt = dt_all,
                              m = 10,
                        strat_ref = "Paired C-Kernel",
                        strat_other = c("Unique",
                                        "Paired",
                                        "Paired Average",
                                        "Paired Kernel",
                                        "Paired C-Kernel",
                                        "Paired CEL-Kernel",
                                        "Paired Imp C-Kernel",
                                        "Paired Imp CEL-Kernel"),
                        y_breaks = c(-1, -0.4, -0.1, 0, 0.1, 0.4, 1, 2, 4),
                        y_limits = c(-1, 5.1),
                        include_coal_size_lines = TRUE,
                        y_lab_frac = TRUE)
    ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Xgboost_M_10_n_train_1000_n_test_1000_rho_4_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2", ext_str, "_relative_difference_FINAL_v1.png"),
           plot = fig,
           width = 14.2,
           height = 9.98,
           scale = 0.85,
           dpi = 350)

    library("ggpubr")
    fig_comb = ggarrange(fig_bands, fig_rel,
                         labels = c("A", "B"),
                         ncol = 1, nrow = 2,
                         common.legend = TRUE, legend = "bottom",
                         font.label = list(size = 25, color = "black"))
    ggsave(filename = paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Xgboost_M_10_n_train_1000_n_test_1000_rho_4_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2", ext_str, "_Comb_Final_V2.png"),
           plot = fig_comb,
           width = 14.2,
           height = 18,
           scale = 0.85,
           dpi = 350)


    relative_difference(dt = dt_all,
                        m = 10,
                        strat_ref = "Paired C-Kernel",
                        strat_other = c("Paired",
                                        "Paired Average",
                                        "Paired C-Kernel",
                                        "Paired CEL-Kernel",),
                        y_limits = c(-0.11, 0.15),
                        scale = FALSE,
                        legend_n_row = 1,
                        hue_length = 8,
                        hue_indices = c(2,3,5,6))

    relative_difference(dt = dt_all,
                        m = 10,
                        strat_ref = "Paired C-Kernel",
                        strat_other = c("Paired",
                                        "Paired Average",
                                        "Paired Empirical",
                                        "Paired C-Kernel",
                                        "Paired CEL-Kernel",
                                        "Paired CEPS-Kernel"),
                        y_limits = c(-0.11, 0.15),
                        scale = FALSE,
                        legend_n_row = 2,
                        hue_length = 8,
                        hue_indices = c(2,3,1,5,6,7))



  }



}




# New -------------------------------------------------------------------------------------------------------------
library(data.table)
library(ggplot2)
true = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Xgboost_M_10_n_train_1000_n_test_1000_rho_0.5_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_true.rds")
S_true = true$internal$objects$S
X_true = true$internal$objects$X

ll = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Xgboost_M_10_n_train_1000_n_test_1000_rho_0.5_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_estimated_repetition_1.rds")
X_tmp = ll$unique$repetition_1$n_combinations_75$only_save$X
S_tmp = ll$unique$repetition_1$n_combinations_1020$only_save$S
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

version = "Paper3"
if (version == "NSM") {
  sampling_methods = c("unique_paired_unif_V2", "unique", "unique_paired", "unique_paired_equal_weights", "paired_coalitions_weights_direct_equal_weights")
} else if (version == "Paper3") {
  ll = readRDS("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Gompertz_Xgboost_M_10_n_train_1000_n_test_1000_rho_0.5_equi_TRUE_betas_2_10_0.25_-3_-1_1.5_-0.5_10_1.25_1.5_-2_estimated_repetition_1.rds")
  sampling_methods = c("unique", "unique_paired", "unique_paired_equal_weights", "unique_paired_SW", "unique_paired_new_weights_empirical", "largest_weights_random_new_weights_empirical")
} else {
  stop("Unknown version.")
}


n_combinations_vec = c(104, 254, 754, 1004)

dt_list = data.table::rbindlist(
  lapply(n_combinations_vec, function(n_combinations) {
    rbind(data.table(type = "kernel", n_combinations =  n_combinations-4, id = X_true$id_combination, weight = X_true$shapley_weight),
          data.table::rbindlist(
            lapply(sampling_methods, function(sampling_method) {
              X_tmp = ll[[sampling_method]]$repetition_1[[paste0("n_combinations_", n_combinations)]]$only_save$X
              S_tmp = ll[[sampling_method]]$repetition_1[[paste0("n_combinations_", n_combinations)]]$only_save$S

              correct_order = order_feature_list(X_tmp$features)
              X = X_tmp[correct_order,]
              S = S_tmp[correct_order,]

              # Get a mapping from the indices of the current set of combinations/coalitions to the indices
              # in the version where we use all 2^M combinations/coalitions.
              current_combination_idx_in_all_combinations =
                sapply(seq(nrow(S)), function(idx) which(apply(S_true, 1, function(x) identical(x, S[idx,]))))

              return_dt = data.table(type = sampling_method, n_combinations = n_combinations - 4, id = current_combination_idx_in_all_combinations, weight = X$shapley_weight)

              if (sampling_method == "unique_paired") {
                return_dt = rbind(
                  return_dt,
                  data.table(type = "unique_paired_kernel", n_combinations = n_combinations - 4, id = current_combination_idx_in_all_combinations, weight = X_true$shapley_weight[current_combination_idx_in_all_combinations])
                )
              }

              return(return_dt)
            })
          ))
  })
)
# Order
setorderv(dt_list, c("n_combinations", "id", "type"))

dt = copy(dt_list)
# Remove empty and grand coalition
dt = dt[!id %in% c(1, 1024)]

# Make
dt[, weight := weight / sum(weight), by = list(type, n_combinations)]

if (version == "NSM") {
  dt[, type := factor(type,
                      levels = c("unique_paired_unif_V2", "unique", "unique_paired", "unique_paired_equal_weights", "kernel", "paired_coalitions_weights_direct_equal_weights"),
                      labels = c("Uniform", "Unique", "Paired", "Paired Avg.", "Kernel", "Pilot"))]

  fig = ggplot(dt, aes(x = id, y = weight, col = type)) +
    geom_point(alpha = 0.75) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    facet_wrap(. ~ n_combinations, labeller = label_bquote(cols = N[combinations] ==.(n_combinations)),
               ncol = 2) +
    theme(legend.position = 'bottom') +
    guides(col = guide_legend(nrow = 1)) +
    labs(color = "Strategy:", x = "Coalition index", y = "Normalized Shapley kernel weight/sampling frequency") +
    theme(strip.text = element_text(size = rel(1.6)),
          legend.title = element_text(size = rel(1.6)),
          legend.text = element_text(size = rel(1.6)),
          axis.title = element_text(size = rel(1.6)),
          axis.title.y = element_text(size = rel(1.1)),
          axis.text = element_text(size = rel(1.5))) + scale_color_manual(values = gg_color_hue(7)[-7])

  fig
  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/N_combinations_weights97.png",
         plot = fig,
         scale = 0.8,
         dpi = 400)

} else if (version == "paper3") {
  dt[, type := factor(type,
                              levels = c("unique_paired_unif_V2", "unique", "unique_paired", "unique_paired_equal_weights",  "unique_paired_SW",
                                         "unique_paired_new_weights_empirical", "unique_paired_new_weights_gompertz",
                                         "paired_coalitions_weights_direct_equal_weights",
                                         "paired_coalitions_weights_direct_equal_weights_new_weights_empirical",
                                         "paired_coalitions_weights_direct_equal_weights_new_weights_gompertz",
                                         "paired_coalitions",
                                         "paired_coalitions_new_weights_empirical",
                                         "paired_coalitions_new_weights_gompertz",
                                         "largest_weights",
                                         "largest_weights_combination_size",
                                         "largest_weights_new_weights_empirical",
                                         "largest_weights_combination_size_new_weights_empirical",
                                         "largest_weights_random", "largest_weights_random_new_weights_empirical",
                                         "MAD", "MAD_new_weights_empirical", "kernel"
                              ),
                              labels = c("Uniform", "Unique", "Paired", "Paired Average", "Paired Kernel",
                                         "Paired Empirical", "Paired Gompertz",
                                         "Pilot Average", "Pilot Sample Empirical", "Pilot Sample Gompertz",
                                         "Pilot Kernel",  "Pilot Order Empirical", "Pilot Order Gompertz",
                                         "Largest", "Largest Coalition",
                                         "Largest Order Empirical", "Largest Order Coalition Empirical",
                                         "Paired Largest", "Paired Largest Empirical",
                                         "MAD", "MAD Empirical",
                                         "Shapley Kernel Weights"
                              ),
                              ordered = FALSE)]

  fig = ggplot(dt[type != "Shapley Kernel Weights"], aes(x = id, y = weight, col = type)) +
    geom_vline(xintercept = 512.5, color = "darkgrey", linetype = "dashed", linewidth = 0.9) +
    geom_point(alpha = 0.6) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    facet_wrap(. ~ n_combinations, labeller = label_bquote(cols = N[S] ==.(n_combinations)),
               ncol = 2) +
    theme(legend.position = 'bottom') +
    guides(col = guide_legend(nrow = 1)) +
    labs(color = "Strategy:", x = "Coalition index", y = "Normalized Shapley kernel weight/sampling frequency") +
    theme(strip.text = element_text(size = rel(1.6)),
          legend.title = element_text(size = rel(1.4)),
          legend.text = element_text(size = rel(1.4)),
          axis.title = element_text(size = rel(1.4)),
          axis.title.y = element_text(size = rel(1.1)),
          axis.text = element_text(size = rel(1.5)))
    #geom_step(data = dt[type == "Shapley Kernel Weights"]) +
    #scale_color_manual(values = c(gg_color_hue(3), "#000000", "#000000"))

  fig
  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Paper3_N_combinations_weights_4.png", # Saving 14.2 x 9.98 in image
         plot = fig,
         width = 14.2,
         height = 9.98,
         scale = 0.85,
         dpi = 350)
} else {
  stop("Unknown version.")
}





colors_str = c('#e6194b', '#f58231', '#ffe119', '#3cb44b', '#4363d8', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')

"#F8766D" "#C49A00" "#53B400" "#00C094" "#00B6EB" "#A58AFF"



# New things
kk = rbindlist(lapply(ll$unique_paired_equal_weights$repetition_1, function(x) {
  tmp = x$only_save$X[-c(1, .N), c("n_features", "shapley_weight")]
  if (is.null(tmp)) return(NULL)
  tmp[, shapley_weight := shapley_weight / sum(shapley_weight)]
  return(unique(tmp))
}), idcol = "n_combinations")
kk[, n_combinations := as.integer(sub("n_combinations_", "", n_combinations))]
kk[, n_features := as.factor(n_features)]
ggplot(kk, aes(x = n_combinations, y = shapley_weight, col = n_features)) +
  geom_line() +
  scale_y_continuous(trans='log10')

kk

X_true[-c(1,.N), shapley_weight2 := shapley_weight/sum(shapley_weight)]
X_true2 = unique(X_true[-c(1,.N), c("n_features", "shapley_weight2")])
X_true2[, n_features := as.factor(n_features)]
dt_new = merge(kk, X_true2, by = "n_features", all.x = TRUE)
dt_new[, shapley_weight_diff := abs(shapley_weight - shapley_weight2)]

ggplot(dt_new, aes(x = n_combinations, y = shapley_weight_diff, col = n_features)) +
  geom_line() +
  scale_y_continuous(trans='sqrt')


# Sort functions --------------------------------------------------------------------------------------------------
# Copied from `gtools` package
mixedsort <- function(x, decreasing = FALSE, na.last = TRUE, blank.last = FALSE, numeric.type = c("decimal", "roman"),
                      roman.case = c("upper", "lower", "both"), scientific = TRUE) {
  x[mixedorder(x,
               decreasing = decreasing, na.last = na.last, blank.last = blank.last, numeric.type = numeric.type,
               roman.case = roman.case, scientific = scientific
  )]
}

mixedorder <- function(x, decreasing = FALSE, na.last = TRUE, blank.last = FALSE, numeric.type = c("decimal", "roman"),
                       roman.case = c("upper", "lower", "both"), scientific = TRUE) {
  numeric.type <- match.arg(numeric.type)
  roman.case <- match.arg(roman.case)
  if (length(x) < 1) return(NULL) else if (length(x) == 1) return(1)
  if (!is.character(x)) return(order(x, decreasing = decreasing, na.last = na.last))
  delim <- "\\$\\@\\$"
  if (numeric.type == "decimal") {
    if (scientific) {
      regex <- "((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)(?:(?:[eE])(?:(?:[-+]?)(?:[0123456789]+))|)))"
    } else {
      regex <- "((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)))"
    }
    numeric <- function(x) as.numeric(x)
  } else if (numeric.type == "roman") {
    regex <- switch(roman.case, both = "([IVXCLDMivxcldm]+)", upper = "([IVXCLDM]+)", lower = "([ivxcldm]+)")
    numeric <- function(x) roman2int(x)
  } else {
    stop("Unknown value for numeric.type: ", numeric.type)
  }
  x <- as.character(x)
  which.nas <- which(is.na(x))
  which.blanks <- which(x == "")
  delimited <- gsub(regex, paste(delim, "\\1", delim, sep = ""), x, perl = TRUE)
  step1 <- strsplit(delimited, delim)
  step1 <- lapply(step1, function(x) x[x > ""])
  suppressWarnings(step1.numeric <- lapply(step1, numeric))
  suppressWarnings(step1.character <- lapply(step1, function(x) ifelse(is.na(numeric(x)), toupper(x), NA)))
  maxelem <- max(sapply(step1, length))
  step1.numeric.t <- lapply(1:maxelem, function(i) sapply(step1.numeric, function(x) x[i]))
  step1.character.t <- lapply(1:maxelem, function(i) sapply(step1.character, function(x) x[i]))
  rank.numeric <- sapply(step1.numeric.t, rank)
  rank.character <- sapply(step1.character.t, function(x) as.numeric(factor(x)))
  rank.numeric[!is.na(rank.character)] <- 0
  rank.character <- t(t(rank.character) + apply(matrix(rank.numeric), 2, max, na.rm = TRUE))
  rank.overall <- ifelse(is.na(rank.character), rank.numeric, rank.character)
  order.frame <- as.data.frame(rank.overall)
  if (length(which.nas) > 0) order.frame[which.nas, ] <- if (is.na(na.last)) NA else if (na.last) Inf else -Inf
  if (length(which.blanks) > 0) {
    order.frame[which.blanks, ] <- if (is.na(blank.last)) NA else if (blank.last) 1e+99 else -1e+99
  }
  order.frame <- as.list(order.frame)
  order.frame$decreasing <- decreasing
  order.frame$na.last <- NA
  retval <- do.call("order", order.frame)
  return(retval)
}

#' Sort the list of features
#'
#' The function sort a list of positive integer vectors of variable size by first
#' sorting the elements in each vector in increasing order (if `sort_features = TRUE`),
#' then sorts the whole list based on the vector lengths (if `sort_size = TRUE`),
#' and for vectors of the same length, the function sort based on the coalitions indices
#' (if `sort_coalitions = TRUE`), i.e., vector `c(1,2,3)` comes before `c(1,3,4)`.
#'
#' @param feature_list List of positive integer vectors of variable size.
#' @param sort_size Logical. If `TRUE` (default), then we sort the `features_list` based on the coalition sizes.
#' @param sort_coalitions Logical. If `TRUE` (default), then we sort the coalitions of equal size in `features_list`
#' based on the feature indices. Note that the features in each coalition must be sorted for this to work.
#' The user either has to do this prior or set `sort_features = TRUE`.
#' @param sort_features Logical. If `TRUE` (default), then we sort the features in each coalition.
#'
#' @return
#' @keyword internal
#' @examples
#' sort_features_list(list(3, 1, c(101, 7, 111), c(7,101,1), c(7, 99, 111), c(2,1), c(1, 101)))
sort_feature_list = function(features_list, sort_features = TRUE, sort_size = TRUE, sort_coalitions = TRUE) {
  # Sort each coalition to have increasing feature indices
  if (sort_features) features_list = lapply(features_list, sort)

  # Sort the list such that the coalition size is increasing
  if (sort_size) features_list = features_list[order(sapply(features_list, length))]

  # Sort the coalitions with equal size such coalitions with lower feature indices come first (1,2,3 before 1,3,4)
  if (sort_coalitions) {
    lengths = sapply(features_list, length)
    lengths_unique = unique(lengths)
    index_list = lapply(lengths_unique, function(length) which(lengths == length))
    names(index_list) = lengths_unique
    features_list_order = c()
    for (length in lengths_unique) {
      length_order = mixedorder(sapply(features_list[lengths == length], paste, collapse = ","))
      features_list_order = c(features_list_order, index_list[[as.character(length)]][length_order])
    }
    features_list = features_list[features_list_order]
  }

  # Return the sorted features list
  return(features_list)
}

order_feature_list = function(features_list, sort_features = TRUE, sort_size = TRUE, sort_coalitions = TRUE) {
  # Sort each coalition to have increasing feature indices
  if (sort_features) features_list = lapply(features_list, sort)

  # Sort the list such that the coalition size is increasing
  if (sort_size) features_list = features_list[order(sapply(features_list, length))]

  # Sort the coalitions with equal size such coalitions with lower feature indices come first (1,2,3 before 1,3,4)
  if (sort_coalitions) {
    lengths = sapply(features_list, length)
    lengths_unique = unique(lengths)
    index_list = lapply(lengths_unique, function(length) which(lengths == length))
    names(index_list) = lengths_unique
    features_list_order = c()
    for (length in lengths_unique) {
      length_order = mixedorder(sapply(features_list[lengths == length], paste, collapse = ","))
      features_list_order = c(features_list_order, index_list[[as.character(length)]][length_order])
    }
  }

  # Return the sorted features list
  return(features_list_order)
}



