library(data.table)
library(ggplot2)
library(shapr)

saveRDS(c(1,2,3), file.path("/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location", paste0("Samp_prop_M_test_res.rds")))



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




Repeated_sampling_coalitions = function(m, n_combinations, B = 10, weight_zero_m = 10^6, seed = 123) {
  set.seed(seed)

  # Find weights for given number of features
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  results = matrix(0, nrow = B, ncol = 2^m)
  results2 = matrix(0, nrow = B, ncol = 2^m)
  dt_avg = data.table(n_features = seq(1, m-1))

  # Create the S matrix if we had used all combinations
  S_all = shapr::feature_matrix_cpp(unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE), m)
  S_all_list = as.list(seq(nrow(S_all)))
  names(S_all_list) = apply(S_all, 1, paste, collapse = "")

  for (b in seq(B)) {
    if (b %% 50 == 0) message(paste0("(m = ", m, ", n_combinations = ", n_combinations, ") Working on iteration: ", b))
    feature_sample_all <- list()
    unique_samples <- 0
    while (unique_samples < n_combinations - 2) {

      n_features_sample <- sample(
        x = n_features,
        size = (n_combinations - unique_samples - 2)/2, # Sample -2 as we add zero and m samples below. Divide by two due to paired sampling
        replace = TRUE,
        prob = p
      )

      feature_sample <- shapr:::sample_features_cpp(m, n_features_sample)
      feature_sample_paired <- lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)
      feature_sample_all <- c(feature_sample_all, feature_sample, feature_sample_paired)
      unique_samples <- length(unique(feature_sample_all))
    }

    # Add zero and m features
    feature_sample_all <- c(list(integer(0)), feature_sample_all, list(c(1:m)))
    X <- data.table(n_features = sapply(feature_sample_all, length))
    X[, n_features := as.integer(n_features)]

    # Get number of occurrences and duplicated rows
    is_duplicate <- NULL # due to NSE notes in R CMD check
    r <- shapr:::helper_feature(m, feature_sample_all)
    X[, is_duplicate := r[["is_duplicate"]]]

    # When we sample combinations the Shapley weight is equal
    # to the frequency of the given combination
    X[, shapley_weight := r[["sample_frequence"]]]

    # Populate table and remove duplicated rows
    X[, features := feature_sample_all]
    if (any(X[["is_duplicate"]])) X <- X[is_duplicate == FALSE]
    X[, is_duplicate := NULL]
    data.table::setkeyv(X, "n_features")

    # Make feature list into character
    X[, features_tmp := sapply(features, paste, collapse = " ")]

    # Aggregate weights by how many samples of a combination we observe
    X <- X[, .(
      n_features = data.table::first(n_features),
      shapley_weight = sum(shapley_weight),
      features = features[1]
    ), features_tmp]

    # Add shapley weight and number of combinations
    X[c(1, .N), shapley_weight := weight_zero_m]
    X[, N := 1]
    ind <- X[, .I[data.table::between(n_features, 1, m - 1)]]
    X[ind, p := p[n_features]]
    X[ind, N := n[n_features]]

    # Set column order and key table
    data.table::setkeyv(X, "n_features")
    X[, id_combination := .I]
    X[, N := as.integer(N)]
    nms <- c("id_combination", "features", "n_features", "N", "shapley_weight", "p")
    data.table::setcolorder(X, nms)

    # Sort X
    X = X[order_feature_list(X$features),]
    X[, id_combination := .I]

    dt = copy(X)

    # Avg metoden til Martin
    dt[, shapley_weight_paired_avg := as.numeric(shapley_weight)]
    dt[, shapley_weight_paired_avg := mean(shapley_weight), by = n_features]
    dt[-c(1, .N), shapley_weight_paired_avg := shapley_weight_paired_avg / sum(shapley_weight_paired_avg)]

    #
    dt_tmp = dt[, mean(shapley_weight), by = n_features][-c(1,.N),]
    dt_tmp[, V1 := list(V1 / sum(V1))]
    dt_tmp = data.table::merge.data.table(
      data.table(n_features = seq(1, m-1)),
      dt_tmp,
      by = "n_features",
      all = TRUE
    )
    dt_tmp[is.na(V1)] = 0
    data.table::setnames(dt_tmp, "V1", paste0("Rep_", b))

    # Merge
    dt_avg = cbind(dt_avg, dt_tmp[,2])

    dt[, shapley_weight := shapley_weight_paired_avg]
    dt[, shapley_weight_true := shapr:::shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]
    dt[c(1, .N), shapley_weight := weight_zero_m]

    # W <- shapr:::weight_matrix(
    #   X = dt,
    #   normalize_W_weights = TRUE,
    #   is_groupwise = FALSE
    # )

    S <- shapr:::feature_matrix_cpp(features = X[["features"]], m = m)

    # Get a mapping from the indices of the current set of combinations/coalitions to the indices
    # in the version where we use all 2^M combinations/coalitions.
    current_combination_idx_in_all_combinations = as.numeric(S_all_list[apply(S, 1, paste, collapse = "")])

    results[b, current_combination_idx_in_all_combinations] = X$shapley_weight
    results2[b, current_combination_idx_in_all_combinations] = dt$shapley_weight_paired_avg
  }

  dt_avg
  #colSums(dt_avg, na.rm = TRUE)
  dt_avg_long = melt(dt_avg, id.vars = "n_features", value.name = "weight")
  dt_avg_long = dt_avg_long[, as.list(c(mean(weight, na.rm = TRUE), sd(weight, na.rm = TRUE), quantile(weight, c(0.025, 0.5, 0.975)))), by = n_features]
  setnames(dt_avg_long, c("n_features", "mean", "sd", "lower", "median", "upper"))


  # results = results[,-c(1, 2^m)]
  # results2 = results2[,-c(1, 2^m)]

  return(dt_avg_long)

  # apply(results, 1, function(x) sum(x != 0))
  # results = results / rowSums(results)
  #
  # matplot(t(results), type = "l", log = "y")
  #
  # matplot(colMeans(results), type = "s", log = "y")
  # true_sv = rep(shapr:::shapley_weights(m = m, N = n, n_features), times = sapply(seq(m - 1), choose, n = m))
  # true_sv = true_sv / sum(true_sv)
  # lines(true_sv, type = "s", col = 2, lwd = 2, log = "y")
  #
  # results2 = results2 / rowSums(results2)
  # matplot(colMeans(results2), type = "s", log = "y")
  # lines(true_sv, type = "s", col = 2, lwd = 2, log = "y")
  #
  #
  # results_scaled = scale(results)
  # apply(results, 2, mean)


}

# module load R/4.2.1-foss-2022a
# cd /mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/shapr/R/Lars_explore_ideas_scripts
# Rscript samp_freq_plots.R 15


# Code starts here ------------------------------------------------------------------------------------------------
args = commandArgs(trailingOnly = TRUE)
M_vec = unlist(strsplit(args[1], ","))
if (length(M_vec) > 1) {
  M_vec = unname(sapply(M_vec, function(i) as.numeric(i)))
} else {
  M_vec = as.numeric(M_vec)
}

# M_vec = seq(16,20)
B = 250
dt_avg_list = list()
res_list = list()

# library(future.apply)
# library(progressr)
# progressr::handlers('cli')

for (m in M_vec) {
  print(m)
  if (m <= 8) {
    n_combinations_vec = seq(4, 2^m, 2)
  } else if (m <= 12) {
    n_combinations_vec = seq(4, 2^m, 8)
  } else {
    increase = 10*(((floor(0.9*2^m) - 10) / 1000) %/% 10)
    if (increase == 0) increase = 10
    n_combinations_vec = seq(10, floor(0.9*2^m), increase)
  }
  n_combinations_vec = unique(sort(c(seq(4, 250, 2), n_combinations_vec)))
  n_combinations_vec = n_combinations_vec[n_combinations_vec < 2^m]

  if (m == 17) {
    n_combinations_vec = c(2:200, 250, 500, 750, 1000, 2500, 5000, 10000, 20000, 30000, 40000, 50000, 60000, 70000,
                             80000, 90000, 100000, 11000, 12000, 13000)
  }

  # # Set up the parallel plan
  # plan(multisession, workers = 4)
  # with_progress({
  #   p <- progressor(along = n_combinations_vec)
  #   tmp = future.apply::future_lapply(n_combinations_vec, function(n_combinations) {
  #     p()
  #     Repeated_sampling_coalitions(m, n_combinations, B = B)
  #   }, future.seed = TRUE)
  # })
  # plan(sequential)   # Clean up the future plan
  tmp = list()
  for (n_combinations_idx in seq(length(n_combinations_vec))) {
    n_combinations = n_combinations_vec[n_combinations_idx]
    tmp[[n_combinations_idx]] = Repeated_sampling_coalitions(m, n_combinations, B = B)
    saveRDS(list(n_combinations = n_combinations, tmp = tmp),
            file.path("/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location", paste0("Samp_prop_M_", m, "_res_tmp.rds")))
  }
  # tmp = lapply(n_combinations_vec, function(n_combinations) {
  #   Repeated_sampling_coalitions(m, n_combinations, B = B)
  # })
  names(tmp) = n_combinations_vec
  res_list[[m]] = rbindlist(tmp, idcol = "n_combinations")
  res_list[[m]][, n_combinations := as.numeric(n_combinations)]
  res_list[[m]][, n_features := as.numeric(n_features)]
  res_list[[m]][, n_features := as.factor(n_features)]
  saveRDS(res_list[[m]], file.path("/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location", paste0("Samp_prop_M_", m, "_res_2.rds")))

  dt_avg = res_list[[m]]
  tmp = shapr:::shapley_weights(m = m,
                                N = sapply(seq(m - 1), choose, n = m),
                                n_components = seq(m - 1))
  tmp = tmp/sum(tmp)

  fig = ggplot(data = dt_avg[n_features %in% seq(1, ceiling((m-1)/2))],aes(x = n_combinations, y = mean)) +
    geom_ribbon(aes(x = n_combinations, ymin = lower, ymax = upper, group = n_features,  col = n_features, fill = n_features), alpha = 0.4, linewidth = 0.1) +
    geom_line(aes(x = n_combinations, y = mean, group = n_features, col = n_features), linewidth = 1) +
    geom_point(data.table(n_combinations = 2^m,
                          col = factor(seq(ceiling((m-1)/2))),
                          weight = tmp[seq(1, ceiling((m-1)/2))]),
               mapping = aes(x = n_combinations, y = weight, colour = col),
    ) +
    expand_limits(y = 0)
  #plot(fig)

  ggsave(file.path("/mn/kadingir/biginsight_000000/lholsen/PhD/Paper3/Paper3_save_location", paste0("Samp_prop_M_", m, "_fig.png")),
         plot = fig,
         width = 14.2,
         height = 10,
         scale = 0.85,
         dpi = 350)
}



# Plot the graphs together
if (FALSE) {

  # Load the data
  {
    M_seq = seq(6,13)

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
  fig_samp = ggplot(data = dt_res, aes(x = n_combinations, y = mean)) +
    geom_ribbon(aes(x = n_combinations, ymin = lower, ymax = upper, group = n_features,  col = n_features, fill = n_features),
                alpha = 0.4, linewidth = 0.1) + ylim(c(0, 0.5)) +
    geom_line(aes(x = n_combinations, y = mean, group = n_features, col = n_features), linewidth = 1) +
    facet_wrap("M ~ .", ncol = 2, scales = "free_x") +
    #facet_wrap("M ~ .", ncol = 2) +
    geom_point(tmp_list,
               mapping = aes(x = n_combinations, y = weight, colour = col),
               size = 2) +
    expand_limits(y = 0) +
    #     scale_y_log10(
    #   breaks = scales::trans_breaks("log10", function(x) 10^x),
    #   labels = scales::trans_format("log10", scales::math_format(10^.x))
    # )
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

  ggsave("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Samp_freq_development_V2.png",
         plot = fig_samp,
         width = 14.2,
         height = 18,
         scale = 0.85,
         dpi = 350)




  # Look at the true shapley kernel weights
  {
    # Compute the Shapley kernel weights
    M_seq2 = seq(6, 30)
    tmp_list2 = lapply(M_seq2, function(m) {
      tmp = shapr:::shapley_weights(m = m,
                                    N = sapply(seq(m - 1), choose, n = m),
                                    n_components = seq(m - 1))
      tmp = tmp/sum(tmp)
      data.table(n_combinations = 2^m,
                 col = factor(seq(ceiling((m-1)/2))),
                 weight = tmp[seq(1, ceiling((m-1)/2))])
    })
    names(tmp_list2) = M_seq2
    tmp_list2 = data.table::rbindlist(tmp_list2, idcol = "M")
    tmp_list2[, M := as.factor(as.integer(M))]


    ## MAKE THE PLOT (change if we want log-scale)
    ggplot(tmp_list2, aes(x = M, y = weight, group = col, col = col)) +
      geom_line() +
      guides(fill=guide_legend(title=expression("Coalition size |"*S*"|")),
             color = guide_legend(title=expression("Coalition size |"*S*"|"))) +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      labs(y = "Normalized Shapley kernel weight/sampling frequency") +
      theme(legend.position="bottom", legend.box = "horizontal")
  }
}
