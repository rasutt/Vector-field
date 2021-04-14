# Function to plot vector fields and approximate solutions for a given linear
# system matrix
plot_vf <- function(e_vals) {
  # Gradient function
  Fn <- function(x, y, e_vals) {
    if (is.complex(e_vals)) {
      H <- 1 / sqrt(2) * cbind(c(1i, 1), c(1, 1i))
      Re(t(H %*% diag(e_vals) %*% t(Conj(H)) %*% rbind(x, y)))
    }
    else t(diag(e_vals) %*% rbind(x, y))
  }
  
  # Mesh for plot
  l <- 11
  m <- seq(-1, 1, l = l)
  y <- rep(m, l)
  x <- rep(m, each = l)
  
  # Time step
  dt <- 0.03
  
  # Arrow head length
  arwl <- 0.04
  
  # Gradient over mesh
  Fxy <- Fn(x, y, e_vals)
  
  # Plot vector field - gives warnings for points with zero gradient
  plot(x, y, type = 'n', axes = F)
  box()
  arrows(x, y, x + dt * Fxy[, 1], y + dt * Fxy[, 2], length = arwl)
  
  # Number of time steps
  T <- 100
  
  # Number of solutions
  n_s <- 10
  
  # Randomly initialize solutions
  x_s <- y_s <- matrix(nrow = T, ncol = n_s)
  x_s[1, ] <- sample(m, n_s)
  y_s[1, ] <- sample(m, n_s)
  
  # Approximate solutions with Euler's method
  for (t in 2:T) {
    Fxys <- Fn(x_s[t - 1, ], y_s[t - 1, ], e_vals)
    x_s[t, ] <- x_s[t - 1, ] + dt * Fxys[, 1]
    y_s[t, ] <- y_s[t - 1, ] + dt * Fxys[, 2]
  }
  
  # Plot solutions
  matlines(x_s, y_s, col = 'blue', lty = 1)
  arrows(x_s[T - 1, ], y_s[T - 1, ], x_s[T, ], y_s[T, ], length = arwl, 
         col = 'blue')
}

# Plot vector fields and approximate solutions for a given linear system matrix
par(mfrow = c(3, 3), mar = c(0, 0, 0, 0))

plot_vf(c(0, 0))
plot_vf(c(1, 0))
plot_vf(c(-1, 0))
plot_vf(c(1, 1))
plot_vf(c(-1, -1))
plot_vf(c(1, -1))
plot_vf(c(1, 2))
plot_vf(c(-1, -2))
plot_vf(c(1i, -1i))
plot_vf(c(1 + 1i, 1 - 1i))
plot_vf(c(-1 + 1i, -1 - 1i))

