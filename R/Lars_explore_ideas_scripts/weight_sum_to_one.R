

p <- seq(0.05, 0.95, by = 0.05)
states <- expand.grid(p, p, p) # Generate all combinations
states <- states[rowSums(round(states, 2)) == 1, ] # Select simplex where elements sum to 1
rownames(states) = seq(nrow(states))
states

states[sample(1:nrow(states), 1), ] # Sample a random element



