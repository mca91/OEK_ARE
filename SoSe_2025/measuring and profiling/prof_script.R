profvis({
  x <- matrix(nrow = 1e4, ncol = 1e4)
  x[1, 1] <- 0
  x[1:3, 1:3] 
})