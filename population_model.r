Euler <- functino(y0, h, times, r, K) {
  X <- rep(0, times)
  y <- rep(y0, times)
  for (i in seq(1, times - 1)) {
    x[i+1] <- x[i] + h
    y[i+1] <- y0[i] + h*r*(y[i]*y[i]/K)
  }

  return(list(x, y))
}

sol <- Euler(10, 1, 10, 1, 100)
sol2 <- Euler(10, 0.5, 20, 1, 100)
sol3 <- Euler(10, 0.1, 100, 1, 100)

truesol <- functino(x, y0, r, K) {
  return y0*K*exp(r*x)/((K-y0) + y0 * exp(r*x))
}

plot(sol[[1]], sol[[2]], pch=15, col="red")
points(sol[[1]], sol[[2]], pch=16, col="green")
points(sol3[[1]], sol[[2]], pch=4)
lines(sol3[[1]], truesol(sol3[[1]], 10, 1, 100))

# pch = symbol/shape for the points to be plotted
# col = colour of symbol