x <- data.frame(
  matrix(runif(5 * 1e6), 
         ncol = 100)
)

bench::mark(df = {
medians <- apply(X = x, MARGIN =  2, FUN = median)
#tracemem(x)
for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}},

list = {
medians <- apply(X = x, MARGIN =  2, FUN = median)
y <- as.list(x)
#tracemem(y)
for (i in 1:100) {
  y[[i]] <- y[[i]] - medians[[i]]
}
}
)


