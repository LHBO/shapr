# This is code related to Mitchell paper on approximating shapley values for the permutation version of SV
# Algorithm 2 and 3 in https://jmlr.org/papers/volume23/21-0439/21-0439.pdf
M = 4
U = matrix(0, nrow = M - 1, ncol = M - 1)
diag(U) = seq(-1, -(M-1))
U[lower.tri(U)] = 1
U = cbind(1, U)
U


Uhat = t(apply(U, 1, function(x) x / norm(x, type = "2")))
apply(Uhat, 1, function(x) sqrt(sum(x^2)))
Uhat



set.seed(1)
B = 500000
xx = matrix(rnorm(B*(M-1)), nrow = B)
xx = apply(xx, 1, function(xxx) xxx/norm(xxx, type = "2"))
xxtilde = t(Uhat) %*% xx
unif_order = t(apply(xxtilde, 2, order))
tt = table(apply(unif_order, 1, paste, collapse = ","))
plot(tt)


B = 1
res = lapply(1:B, function(B_i) {
  k = 2*(M-1)
  X = matrix(rnorm(k/2 * (M - 1)), nrow = k/2, ncol = M - 1)
  X
  Y = matrix(NA, nrow = k, ncol = M)
  Y

  for (i in seq(1, k/2)) {
    for (j in seq(1, i)) {
      X[i,] = X[i,] - t(X[j,]) %*% X[i,] %*% X[j,]
    }
    X[i,] = X[i,] / norm(X[i,], type = "2")
    Y[2*i - 1,] = order(t(Uhat) %*% X[i,])
    Y[2*i,] = order(t(Uhat) %*% -X[i,])
  }

  Y
})
res = do.call(rbind, res)
unique(res)

res_dt = data.table(res)
res_dt_names = names(res_dt)
res_dt[, .N, by = res_dt_names]

