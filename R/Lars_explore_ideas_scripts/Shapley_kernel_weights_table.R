M_seq = 4:20
normalize_version = "coalition"
normalize_version = "coalition_size"

# data table with the final ps values for each coal size
dt_exact_ps = get_exact_ps_values(m_seq, normalize = normalize_version)


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
#tmp_list[, M := factor(M, levels = M_seq, labels = paste0("M = ", M_seq))]
tmp_list[, M := as.factor(as.integer(M))]


dt_print = dcast(tmp_list, M + n_combinations ~ col, value.var = "weight")
dt_print[, n_combinations := NULL]
dt_print

# Create the xtable object
library(xtable)
xtable_dt <- xtable::xtable(dt_print,
                            caption = "",
                            label = "",
                            digits = -2)

# Print the LaTeX code for the table
print(xtable_dt, type = "latex", include.rownames = FALSE, NA.string = "---")





library(shapr)
library(data.table)
library(xtable)

#' Title
#'
#' Get the normalised ps values for each coalition size or coalition
#'
#' @param m_seq vector of positive integers
#'
#' @return
#' @export
#'
#' @examples
get_exact_ps_values = function(m_seq, M_as_factor = TRUE, normalize = c("coalition_size", "coalition")) {
  normalize = match.arg(normalize)
  tmp_list = lapply(m_seq, function(m) {
    tmp = shapr:::shapley_weights(m = m,
                                  N = sapply(seq(m - 1), choose, n = m),
                                  n_components = seq(m - 1))
    if (normalize == "coalition_size") tmp = tmp/sum(tmp)
    if (normalize == "coalition") tmp = tmp/sum_shapley_weights(m)
    data.table(N_S = 2^m,
               Size = factor(seq(ceiling((m-1)/2))),
               weight = tmp[seq(1, ceiling((m-1)/2))])
  })
  names(tmp_list) = m_seq
  tmp_list = data.table::rbindlist(tmp_list, idcol = "M")
  if (M_as_factor) tmp_list[, M := factor(M, levels = m_seq, labels = paste0("M = ", m_seq))]
  return(tmp_list)
}

sum_shapley_weights <- function(m){
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  return(sum(w))
}



# Code starts here ------------------------------------------------------------------------------------------------
m_seq = 4:20
normalize_version = "coalition_size"
normalize_version = "coalition"
digits = ifelse(normalize_version == "coalition_size", 7, 8)
digits = -3

# data table with the final ps values for each coal size
dt_exact_ps = get_exact_ps_values(m_seq, normalize = normalize_version)
dt_exact_ps_cast = dcast(dt_exact_ps, M + N_S ~ Size, value.var = "weight")
dt_exact_ps_cast[, M := as.integer(gsub("M = ", "", levels(M)))][, N_S := NULL]
dt_exact_ps_cast[, M := as.factor(M)]

sum(dt_exact_ps_cast[.N,-1] * choose(20, seq(10)) * c(rep(2, 9), 1))


# Create the xtable object
xtable_dt <- xtable::xtable(dt_exact_ps_cast, caption = "", label = "", digits = digits)
xtable_dt <- xtable::xtable(dt_exact_ps_cast[M %in% c(10, 11, 20)], caption = "", label = "", digits = digits)

# Print the LaTeX code for the table
print(xtable_dt, type = "latex", include.rownames = FALSE, math.style.exponents = TRUE, NA.string = "---")
print(xtable_dt, type = "latex", include.rownames = FALSE, math.style.exponents = FALSE, NA.string = "---")


