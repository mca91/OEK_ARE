bench::mark(df = {
x <- data.frame(
  matrix(runif(5 * 1e6), 
         ncol = 100)
)
medians <- apply(X = x, MARGIN =  2, FUN = median)
#tracemem(x)
for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}},

list = {
x <- data.frame(
  matrix(runif(5 * 1e6), 
         ncol = 100)
)
y <- as.list(x)
#tracemem(y)
for (i in 1:5) {
  y[[i]] <- y[[i]] - medians[[i]]
}
}
)


