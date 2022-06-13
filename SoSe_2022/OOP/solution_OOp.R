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




