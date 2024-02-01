

# Define the GG function
GG <- function(A, B) {
  matrix(c(
    dot(A, B), -norm(crossprod(A, B)), 0,
    norm(crossprod(A, B)), dot(A, B), 0,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
}

# Define the FFi function
FFi <- function(A, B) {
  matrix(cbind(A, (B - dot(A, B) * A) / norm(B - dot(A, B) * A), crossprod(B, A)), nrow = 3, ncol = 3)
}

# Define the UU function
UU <- function(Fi, G) {
  Fi %*% G %*% solve(Fi)
}

# Example usage:
A <- as.matrix(c(1, 0, 0))
B <- as.matrix(c(0, 1, 0))

GG_matrix <- GG(A, B)
FFi_matrix <- FFi(A, B)
UU_matrix <- UU(FFi_matrix, GG_matrix)

print("GG_matrix:")
print(GG_matrix)

print("FFi_matrix:")
print(FFi_matrix)

print("UU_matrix:")
print(UU_matrix)



# Function returns a rotation matrix transforming x into y
rotation = function(x,y){
  u=x/sqrt(sum(x^2))

  v=y-sum(u*y)*u
  v=v/sqrt(sum(v^2))

  cost=sum(x*y)/sqrt(sum(x^2))/sqrt(sum(y^2))

  sint=sqrt(1-cost^2);

  diag(length(x)) - u %*% t(u) - v %*% t(v) +
    cbind(u,v) %*% matrix(c(cost,-sint,sint,cost), 2) %*% t(cbind(u,v))
}


kk = lapply(1:10, function(idx) {
  x = shapley_regular[idx,]
  y = shapley_pca[idx,]
  # x = x/sqrt(sum(x^2))
  # y = y/sqrt(sum(y^2))
  rotation(x,y)
})

kk


x = shapley_regular[1,]
y = shapley_pca[1,]
x = x/sqrt(sum(x^2))
y = y/sqrt(sum(y^2))

x=c(2,4,5,3,6)
y=c(6,2,0,1,7)

# Same norm
sqrt(sum(x^2))
sqrt(sum(y^2))

Rx2y = rotation(x,y)
Rx2y

x %*% Rx2y
