# Gradient function
Fn <- function(x, y) cbind(sin(y), -2 * sin(x))

# Mesh for plot
l <- 11
m <- seq(-2, 2, l = l)
y <- rep(m, l)
x <- rep(m, each = l)

# Time step
dt <- 0.03

# Arrow head length
arwl <- 0.04

# Gradient over mesh
Fxy <- Fn(x, y)

# Plot vector field - gives warnings for points with zero gradient
plot(x, y, type = 'n')
arrows(x, y, x + dt * Fxy[, 1], y + dt * Fxy[, 2], length = arwl)

# Number of time steps
T <- 100

# Number of solutions
n_s <- 4

# Randomly initialize solutions
x_s <- y_s <- matrix(nrow = T, ncol = n_s)
x_s[1, ] <- sample(m, n_s)
y_s[1, ] <- sample(m, n_s)
  
# Approximate solutions with Euler's method
for (t in 2:T) {
  Fxys <- Fn(x_s[t - 1, ], y_s[t - 1, ])
  x_s[t, ] <- x_s[t - 1, ] + dt * Fxys[, 1]
  y_s[t, ] <- y_s[t - 1, ] + dt * Fxys[, 2]
}

# Plot solutions
matlines(x_s, y_s, col = 'blue', lty = 1)
arrows(x_s[T - 1, ], y_s[T - 1, ], x_s[T, ], y_s[T, ], length = arwl, 
       col = 'blue')
