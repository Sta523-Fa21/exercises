
# Exercise 1

## Part 1

# NA -> has type logical by default

c(1, NA+1L, "C") # Character

typeof( c(1L / 0, NA) ) # Double

c(1:3, 5) # Double

c(3L, NaN+1L) # Double

c(NA, TRUE) # Logical

## Part 2

#> Logical < Integer < Double < Character


# Exercise 2

f = function(x) {
  # Check small prime
  if (x > 10 || x < -10) {
    stop("Input too big")
  } else if (x %in% c(2, 3, 5, 7)) {
    cat("Input is prime!\n")
  } else if (x %% 2 == 0) {
    cat("Input is even!\n")
  } else if (x %% 2 == 1) {
    cat("Input is odd!\n")
  }
}

f(1)      # is odd
f(3)      # prime
f(8)      # even
f(-1)     # odd
f(-3)     # odd 
f(1:2)    # odd + warnings
f("0")    # error
f("3")    # error bigger
f("zero") # error 
