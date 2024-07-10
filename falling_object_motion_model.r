Euler <- function(y0, w0, g, gamma, m, h, times) {
  X <- rep(0, times)
  y <- rep(y0, times)
  w <- rep(w0, times)

  for (i in seq(1, times - 1)) {
    x[i+1] <- x[i] + h
    y[i+1] <- x[i] + h * w[i]
    w[i+1] <- x[i] + h * (g - gamma/m*w[i])
  }

  return(list(x, y, w))
}

sol < - Euler(0, 0, 9.8, 0.5, 1, 1, 50)
plot(sol[[1]], sol[[2]])
plot(sol[[1]], sol[[3]])