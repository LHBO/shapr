p = c(1/4, 1/2, 1/4)

B = 5000
res = rep(NA, B)
for (b in seq(B)) {
  unique = 0
  sampled = c()
  while (unique < 2) {
    sampled = c(sampled, sample(x = length(p), size = 2 - unique, prob = p, replace = TRUE))
    unique = length(unique(sampled))
  }

  res[b] = sum(sampled == 2)


}
mean(res)


# Get all coalition sizes
m = 4
n_features <- seq(m - 1)

# Get the number of coalitions of each coalition size
n <- sapply(n_features, choose, n = m)

# Get the probabilities for each coalition
all_probs = rep(shapr:::shapley_weights(m = m, N = n, n_features), times = n)

# Consider only the first half of the coalitions as they are paired.
# Each coalition should then have twice the prob of being samples, but we normalize them afterwards, so skip it.
all_probs = all_probs[seq(length(all_probs) / 2)]
prob = all_probs / sum(all_probs)

alphabet = c("a1", "a2", "a3", "a4", "b1", "b2", "b3")
N_s_vec = seq(length(alphabet) - 1)
B = 500000
res = array(data = NA, dim = c(length(N_s_vec), B))
N_s = 3

res_samps = list()
for (N_s_idx in seq_along(N_s_vec)) {
  N_s = N_s_vec[N_s_idx]
  for (b in seq(B)) {
    if (b %% (B/100) == 0) print(b/B*100)
    unique = 0
    sampled = c()
    while (unique < N_s) {
      new_samples = sample(x = alphabet, size = N_s - unique, prob = prob, replace = TRUE)
      sampled = c(sampled, new_samples)
      unique = length(unique(sampled))
    }
    if (sampled[length(sampled)] == "a1") res_samps = c(res_samps, paste(sampled, collapse = " "))
    res[N_s_idx, b] = mean(sampled == "a1")
  }
}
apply(res, 1, mean)

sample(3, 2, replace = TRUE)


sum(res_samps %in% c("a2 a2 a3 a1", "a2 a3 a2 a1", "a3 a2 a2 a1"))/B
3 * prob[1]^4



mean(c(
  sum(res_samps %in% c("a2 a2 a3 a3 a1", "a2 a3 a2 a3 a1", "a3 a2 a2 a3 a1", "a3 a2 a3 a2 a1", "a3 a3 a2 a2 a1", "a2 a3 a3 a2 a1"))/B,
  sum(res_samps %in% c("a2 a2 a4 a4 a1", "a2 a4 a2 a4 a1", "a4 a2 a2 a4 a1", "a4 a2 a4 a2 a1", "a4 a4 a2 a2 a1", "a2 a4 a4 a2 a1"))/B,
  sum(res_samps %in% c("a3 a3 a4 a4 a1", "a3 a4 a3 a4 a1", "a4 a3 a3 a4 a1", "a4 a3 a4 a3 a1", "a4 a4 a3 a3 a1", "a3 a4 a4 a3 a1"))/B
))
fac_fun(c(2,2)) * prob[1]^5

res_samps

fac_fun = function(sizes) {
  factorial(sum(sizes)) / prod(factorial(sizes))
}

fac_fun(c(2,2))



# Antall feature combinations
n_feat_comb = ((length(alphabet)-1) * (length(alphabet) - 2)) / 2
prob_now = rep(0, n_feat_comb)
feat_comb_now = 0
for (v1 in seq(2, length(alphabet) - 1)) {
  for (v2 in seq(v1 + 1, length(alphabet))) {
    feat_comb_now = feat_comb_now + 1
    print(c(feat_comb_now, v1, v2))
    for (i in seq(6)) {
      for (j in seq(6)) {
        prob_now[feat_comb_now] = prob_now[feat_comb_now] +
          prob[v1]^i * prob[v2]^j * prob[1] * fac_fun(c(i,j))
      }
    }
  }
}
prob_now
table(prob_now)
sum(prob_now)
length(res_samps) / B



mchoose = function(n, k) choose(n + k - 1, k)

mchoose(2, 3)

fac_fun(c(2,1))



choose(3, 2) * fac_fun(c(2,1))



target = 6
parts = 3
part_sizes = rep(target - parts + 1, parts)



find_combinations <- function(target, parts, part_sizes = rep(target - parts + 1, parts)) {
  print(paste0("target = ", target, " parts = ", parts, " part_sizes = ", paste(part_sizes, collapse = ", ")))
  #if (any(part_sizes < 1) || sum(part_sizes) < target) stop("Invalid part sizes")
  #if (part_sizes[1] > target - parts + 1) print("hdfj")

  if (parts > target) stop("target must be larger or equal to the number of parts")
  if (parts == 1) {

    # Base case: Only one number left, which must be the target itself (if allowed)
    if (target <= part_sizes[1]) {
      return(matrix(target, ncol = 1))
    } else {
      print(paste0("BLAA target = ", target, " parts = ", parts, " part_sizes = ", paste(part_sizes, collapse = ", ")))
      return(NULL)
    }


  }

  # Initialize an empty list to store combinations
  combinations <- list()

  # Iterate over all possible values of the first element (0 to target)
  x = 1
  max_x = min(part_sizes[1], target - parts + 1)
  for (x in seq(max_x)) {
    # Recursive call: find combinations of the remaining parts-1 elements
    sub_combinations <- find_combinations(target = target - x,
                                          parts = parts - 1,
                                          part_sizes = part_sizes[-1])

    # Combine the current element with each sub-combination
    if (!is.null(sub_combinations)) {
      new_combinations <- cbind(x, sub_combinations)

      # Store the new combinations
      combinations <- append(combinations, list(new_combinations))
    }
  }

  # Combine all combinations into a single matrix
  ret_mat = do.call(rbind, combinations)
  colnames(ret_mat) = paste0("V", seq(ncol(ret_mat)))
  return(ret_mat)
}

k = find_combinations(target = 6, parts = 3)
k = find_combinations(target = 6, parts = 3, part_sizes = c(4,3,2))
find_combinations(target = 6, parts = 2, part_sizes = c(4,5))
find_combinations(target = 6, parts = 2)


k[1,]
k
part_sizes = c(4,3,2)
comb_per_coal_size = t(sapply(seq(nrow(k)), function(y) sapply(seq(ncol(k)), function(x) choose(part_sizes[x], k[y,x]))))
sum(apply(comb_per_coal_size, 1, prod))
combinations_fun = function()


utils::com

system.time({combinations_q = utils::combn(x = sum(part_sizes), m = target, simplify = FALSE)})
length(combinations_q)

find_combinations(4, 2)

xx = find_combinations(4, 2)
sapply(seq(nrow(xx)), function(x) fac_fun(xx[x,]) - 2)



find_combinations <- function(target, parts, part_sizes = rep(target, parts)) {
  # print(paste0("target = ", target, " parts = ", parts, " part_sizes = ", paste(part_sizes, collapse = ", ")))
  #if (any(part_sizes < 1) || sum(part_sizes) < target) stop("Invalid part sizes")
  #if (part_sizes[1] > target - parts + 1) print("hdfj")

  #if (parts > target) stop("target must be larger or equal to the number of parts")
  if (parts == 1) {
    # Base case: Only one number left, which must be the target itself (if allowed)
    return(if (target <= part_sizes[1]) matrix(target) else NULL)
  }

  # Initialize an empty list to store combinations
  combinations <- list()

  # Iterate over all possible values of the first element (0 to target)
  for (x in seq(0, min(part_sizes[1], target))) {
    # Recursive call: find combinations of the remaining parts-1 elements
    sub_combinations <- find_combinations(target = target - x, parts = parts - 1, part_sizes = part_sizes[-1])

    # Combine the current element with each sub-combination and store them
    if (!is.null(sub_combinations)) combinations <- append(combinations, list(cbind(x, sub_combinations)))
  }

  # Combine all combinations into a single matrix
  ret_mat = do.call(rbind, combinations)
  if (!is.null(ret_mat)) colnames(ret_mat) = paste0("V", seq(ncol(ret_mat)))
  return(ret_mat)
}


find_combinations(target = 6, parts = 3)
find_combinations(target = 6, parts = 3, part_sizes = c(4,3,2))

find_combinations(target = 14, parts = 5, part_sizes = c(4,3,2,3,4))

find_combinations(target = 14, parts = 5, part_sizes = c(4,3,2,3,4))



target = 6
parts = 3
part_sizes = c(4,3,2)
k = find_combinations(target = target, parts = parts, part_sizes = part_sizes)
comb_per_coal_size = t(sapply(seq(nrow(k)), function(y) sapply(seq(ncol(k)), function(x) choose(part_sizes[x], k[y,x]))))
comb_per_coal_size_prod = apply(comb_per_coal_size, 1, prod)
sum(comb_per_coal_size_prod)

k %*% c(1,10, 100)

prob = c(1/10, 4/10, 5/10)

prob_per_coal_size = sapply(seq(nrow(k)), function(row_idx) 1 / (1 - sum(prob^k[row_idx,])))

prob_per_coal_size * comb_per_coal_size_prod
sum(prob_per_coal_size * comb_per_coal_size_prod)

c(1,2,3)^(2)
pow

length(utils::combn(x = sum(part_sizes), m = target, simplify = FALSE))

prod


bases <- c(2, 3, 4)
exponents <- c(3, 2, 1)
bases ^ exponents



library(partitions)
restrictedparts(15,10)
kk = t(compositions(6, 3))


jj = kk[apply(kk, 1, FUN = function(x) all(x <= part_sizes)),]
jj

find_combinations(target = 6, parts = 3, part_sizes = c(4,3,2))

