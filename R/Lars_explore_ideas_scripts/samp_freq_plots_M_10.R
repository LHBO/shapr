library(ggplot2)
library(data.table)
library(shapr)

# Load the M = 10 dim file
m = 17
dt_avg = readRDS(paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Samp_prop_M_", m, "_res.rds"))

# Get the Shapley kernel weights
Shapley_kernel_weights_aux = shapr:::shapley_weights(m = m,
                              N = sapply(seq(m - 1), choose, n = m),
                              n_components = seq(m - 1))
Shapley_kernel_weights = Shapley_kernel_weights_aux / sum(Shapley_kernel_weights_aux)

# Get the relevant coalition sizes
relevant_n_features = seq(ceiling((m-1)/2))

# Create the figure
fig = ggplot(data = dt_avg[n_features %in% relevant_n_features], aes(x = n_combinations, y = mean)) +
  # geom_ribbon(aes(x = n_combinations, ymin = lower, ymax = upper, group = n_features,  col = n_features, fill = n_features), alpha = 0.4, linewidth = 0.1) +
  geom_line(aes(x = n_combinations, y = mean, group = n_features, col = n_features), linewidth = 1) +
  geom_point(data.table(n_combinations = 2^m,
                        col = factor(relevant_n_features),
                        weight = Shapley_kernel_weights[relevant_n_features]),
             mapping = aes(x = n_combinations, y = weight, colour = col),
  ) + expand_limits(y = 0)
fig

# Just look at potential starting values (either the mean, or the accumulated Shapley kernel
# weights for each coalition size times the number of coalitions of said size.
n_features <- seq(m - 1)
n <- sapply(n_features, choose, n = m)
w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
p <- w / sum(w)
dt_points = data.table(n_features = relevant_n_features,
                       start_val = 1/(m-1),
                       start_val_2 = p[relevant_n_features],
                       end_val = Shapley_kernel_weights[relevant_n_features],
                       col = factor(relevant_n_features),
                       n_combinations = 1)


# Look at the potential starting points.
# It looks like it is better to start at the mean due to the strange shape of the curves
fig + geom_point(dt_points, mapping = aes(x = n_combinations, y = start_val_2, colour = col, size = 1.5)) + geom_hline(yintercept = 1/(m-1))

# If we are to fit a curve, it might be best to skip the first 50 combintations to, to get a more monotonic curve
fig + geom_point(dt_points, mapping = aes(x = n_combinations, y = start_val_2, colour = col, size = 1.5)) + geom_hline(yintercept = 1/(m-1)) + xlim(c(50, 1024))


# LOOK AT SOME CONVEX COMBINTAIONS
# The stepsize
n_combinations_max = 2^m
n_combinations_arr = seq(1, 2^m, length.out = 100)
lambda = (n_combinations_arr - 1) / (n_combinations_max - 1)

# Which feature to look at
n_features_now = 1
start_val = dt_points[n_features == n_features_now, start_val]
end_val = dt_points[n_features == n_features_now, end_val]

# The shape of the convex combination
alpha = 1/5
alpha = 0.34
alpha = 1
values = (1-lambda^alpha)*start_val + lambda^alpha*end_val
alpha = 2.81
beta = 0.5
values = (1-lambda^alpha)*start_val + lambda^beta*end_val

# Plot it
fig + geom_line(data = data.table(n_combinations = n_combinations_arr, values = values), aes(x = n_combinations, y = values))

# It does not seem to work that well even if we optimize for the parmeters
dt_avg_now = dt_avg[n_features == n_features_now]

n_combinations_arr = dt_avg_now[n_combinations > 50, n_combinations]
lambda = (n_combinations_arr - 1) / (n_combinations_max - 1)
mean_now = dt_avg_now[n_combinations > 50, mean]

min_f = function(x) {
  alpha = x[1]
  beta = x[2]
  # a = x[2]
  # b = x[3]
  values = (1-lambda^alpha)*start_val + lambda^beta*end_val

  #values = alpha^a * shapley_weight_true + (1 - alpha^b) * shapley_weight_unif
  sum((mean_now - values)^2)
}
min_f(c(1, 1))
optim_res = optim(c(1, 1), fn = min_f, method = "L-BFGS-B", lower = c(0,0), upper = c(10, 10))
optim_res



##### Then we try Gompertz model
gompertz_model <- function(x, a, b, c, d = 1, x_max) {
  x = x / x_max
  x = x / (1 - x)
  a * exp(-b * exp(-c * x^d))
}

# new_model <- function(x, alpha, beta, y0, y1, x_max) {
#   x = x / x_max
#   x = x / (1 - x)
#
#   (y0 - y1) / (alpha*x^beta) + y1
# }

relevant_n_features
# Which feature to look at
n_combinations_start = 60
dt_gompertz = rbindlist(lapply(relevant_n_features, function (i) {
  n_features_now = i
  start_val = dt_points[n_features == n_features_now, start_val]
  end_val = dt_points[n_features == n_features_now, end_val]
  a = end_val
  b = log(end_val/start_val)
  dt_avg_now = dt_avg[n_features == n_features_now]
  n_combinations_arr_all = dt_avg_now[,n_combinations]
  n_combinations_arr = dt_avg_now[n_combinations > n_combinations_start, n_combinations]
  mean_now = dt_avg_now[n_combinations > n_combinations_start, mean]

  n_combinations_arr = n_combinations_arr #- n_combinations_start
  n_combinations_max = 2^m #- n_combinations_start

  min_f = function(x) {
    values = gompertz_model(x = n_combinations_arr, a = a, b = b, c = x[1], d = x[2], x_max = n_combinations_max)
    sum((mean_now - values)^2)
  }

  # min_f_new = function(x) {
  #   values = new_model(x = n_combinations_arr, alpha = x[1], beta = x[2], y0 = start_val, y1 = end_val, x_max = n_combinations_max)
  #   sum((mean_now - values)^2)
  # }
  # min_f_new(c(1,1))
  # optim_res_new = optim(c(1, 1), fn = min_f_new, method = "L-BFGS-B", lower = c(0.01, 0.01), upper = c(100, 100))
  # print(round(optim_res_new$par, 3))
  # values = new_model(x = n_combinations_arr_all, alpha = optim_res_new$par[1], beta = optim_res_new$par[2], y0 = start_val, y1 = end_val, x_max = n_combinations_max)

  min_f(c(1,1))
  optim_res = optim(c(1, 1), fn = min_f, method = "L-BFGS-B", lower = c(0,-10), upper = c(10000, 10))
  print(round(optim_res$par, 3))
  # M = 10
  # [1] 4.102 0.761
  # [1] 7.516 1.349
  # [1] 2.035 1.037
  # [1] 1.371 0.964
  # [1] 1.190 0.917
  # M = 11
  # [1] 5.612 0.732
  # [1] 17.581  1.414
  # [1] 3.178 1.037
  # [1] 1.935 0.956
  # [1] 1.609 0.931
  # M = 17
  # [1] 118.356   0.861
  # [1] 3123.998    1.523
  # [1] 175.31   1.22
  # [1] 74.770  1.149
  # [1] 46.114  1.108
  # [1] 31.130  1.065
  # [1] 26.892  1.054
  # [1] 26.148  1.056
  values = gompertz_model(n_combinations_arr_all, a = end_val, b = log(end_val/start_val), c = optim_res$par[1], d = optim_res$par[2], x_max = n_combinations_max)
  data.table(n_features = n_features_now, n_combinations = n_combinations_arr_all, values = values, paired = if (m %% 2 == 0 && i == relevant_n_features[length(relevant_n_features)]) 1 else 2)
}))

dt_gompertz[, values := values / sum(values*paired), by = n_combinations]
dt_gompertz[, n_features := as.factor(n_features)]

fig + geom_vline(xintercept = n_combinations_start, linetype = "dotted", col = "gray50") +
  geom_line(data = dt_gompertz,  aes(x = n_combinations, y = values, col = n_features), linetype = "dashed", linewidth = 1) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )



# Combine the results into one matrix that I can use in my sampling procedures
dt_gompertz[n_features == 1, .SD, .SDcols = c("n_combinations", "values")]
dt_gompertz[, n_features := as.numeric(n_features)]

dt_gompertz2 = copy(dt_gompertz)
dt_gompertz2[, n_features := m - n_features]

dt_gompertz3 = rbind(copy(dt_gompertz), copy(dt_gompertz2))

dt_gompertz3 = dt_gompertz3[order(n_features)]
dt_gompertz3 = unique(dt_gompertz3)
dt_gompertz3[, paired := NULL]
dt_gompertz3
dt_gompertz3[, n_features := as.factor(n_features)]

dt_final = merge(dt_avg[, .SD, .SDcols = c("n_combinations", "n_features", "mean")],
      dt_gompertz3,
      by = c("n_combinations", "n_features"))
setnames(dt_final, c("mean", "values"), c("empirical", "gompertz"))
dt_final[, n_features := as.integer(n_features)]
dt_final

saveRDS(dt_final, paste0("/Users/larsolsen/PhD/Paper3/Paper3_save_location/Samp_prop_and_gompertz_M_", m, ".rds"))



if (m == 10) dt_final[n_combinations %in% c(4, 1020)]
if (m == 11) dt_final[n_combinations %in% c(4, 2044)]

