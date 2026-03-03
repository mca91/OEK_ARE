install.packages('sloop')
library(sloop)

my_generic <- function(x){
  UseMethod('my_generic')
}

my_generic.default <- function(x){
  x
}

my_generic.my_class <- function(x){
  cat("Output for class 'my_class':\n", x)
}


y <- structure(x <- 1, class = 'my_class')

sloop::s3_dispatch(my_generic(x))

sloop::s3_dispatch(my_generic(y))

my_generic(x)

my_generic(y)



# prime 
prime <- function(x = integer()) {
  validate_prime(new_prime(x))
}

validate_prime <- function(x = list()) {
  sapply(
    unique(unlist(x)), function(z) {
      if(!all(z==2 || z %% 2:(z-1) > 0)) {
        stop('Input contains non-prime number(s)!', call. = F)
      }
    } 
  )
  x
}

new_prime <- function(x = integer()) {
  stopifnot(is.integer(x))
  x <- list(x)
  class(x) <- 'prime'
  x
}


# Subset of all prime numbers in 2,...,1e4
x <- 2:1e4
x <- x[sapply(unique(x), function(z) all(z %% 2:(z-1) > 0))]
# Assign 'prime' class
x <- prime(x)

#unlist(x)


plot.prime <- function(x = list(), coord, ...) {
  x <- unlist(x)
  if(missing(coord)) {
    d <- data.frame(x = x, y = x)
  } else if(coord == "polar") {
    d <- data.frame(x = cos(x * 180/pi) * x, y = sin(x * 180/pi) * x)
  } 
  plot(d, ...)
}

plot(x, coord = "polar", pch = 19, cex = 0.5, col = "red")

summary.prime <- function(x) {
  if(class(x)  == "prime") {
    x <- unlist(x)
    out <- list(
      name = quote(x),
      n = length(x),
      min = min(x),
      max = max(x)
    )
    class(out) <- "summary.prime"
    return(out)
  } else {
    message("Object not of class prime!")
  }
}

my_primes <- new_prime(c(3L, 5L, 7L))

summary(my_primes)

# 3-2
print.summary.prime <- function(x) {
  if(class(x)  == "summary.prime") {
    cat(
      "summary for", x$name, ":\n\n",
      x$n, "prime numbers between", x$min, "to", x$max
    )
  } else {
    message("Object not of class summary.prime!")
  }
}

# now with print method
summary(my_primes)



