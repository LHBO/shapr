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

    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }

    dt_all = rbindlist(lapply(figs, "[[", 1), idcol = "rho")[, rho := rhos[rho]]

    dt_all[, sampling := factor(sampling,
                        levels = c("unique_paired_unif_V2", "unique", "unique_paired", "unique_paired_equal_weights",  "unique_paired_SW", "paired_coalitions_weights_direct_equal_weights", "paired_coalitions"),
                        labels = c("Uniform", "Unique", "Paired", "Paired avg.", "Paired kernel", "Pilot", "Pilot Kernel"))]

    fig2 = ggplot(dt_all, aes(x = n_combinations, y = mean, col = sampling)) +
      #geom_smooth(method = "loess", se = FALSE) +
      #stat_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +
      geom_line(size = 1) +
      #geom_ma(ma_fun = SMA, n = 10, linetype = "solid") +
      facet_wrap(.~rho, labeller = label_bquote(cols = rho ==.(rho)), scales="fixed") +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      theme(legend.position = 'bottom') +
      guides(col = guide_legend(nrow = 1)) +
      labs(color = "Strategy:", x = "Coalition index", y = "Mean absolute error") +
      theme(strip.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.4))) + scale_color_manual(values = gg_color_hue(7))

    fig2
    ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Results2.png",
           plot = fig2,
           scale = 0.8,
           dpi = 400)


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

sampling_methods = c("unique_paired_unif_V2", "unique", "unique_paired", "unique_paired_equal_weights", "paired_coalitions_weights_direct_equal_weights")


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

              data.table(type = sampling_method, n_combinations = n_combinations - 4, id = current_combination_idx_in_all_combinations, weight = X$shapley_weight)
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
  labs(color = "Strategy:", x = "Coalition index", y = "Normalized weight") +
  theme(strip.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4))) + scale_color_manual(values = gg_color_hue(7)[-7])

fig
ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/N_combinations_weights7.png",
       plot = fig,
       scale = 0.8,
       dpi = 400)


colors_str = c('#e6194b', '#f58231', '#ffe119', '#3cb44b', '#4363d8', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')





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



