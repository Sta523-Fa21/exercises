## Exercise 1

z = 1
f = function(x, y, z) {
  z = x+y
  g = function(m = x, n = y) {
    m/z + n/z
  }
  z * g()
}

f(1, 2, x = 3) # => f(x=3, y=1, z=2)

# z value comes from the scope of function f, so z = 4
# function g has m = 3, n = 1, so 3/4 + 1/4 = 1
# z * g() = 4 * 1 = 4

## Exercise 2

primes = c( 2,  3,  5,  7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 
            43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)

x = c(3,4,12,19,23,51,61,63,78)

for(val in x) {
  not_prime = TRUE
  for(p in primes) {
    if (val == p) {
      not_prime = FALSE
      break # No need to continue loop if we find a match
    }
  }
  
  if (not_prime)
    print(val)
}