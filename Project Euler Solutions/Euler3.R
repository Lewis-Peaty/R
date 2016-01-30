#My solution to #https://projecteuler.net/problem=3

# vectorised for the lulz
is_prime <- function(x) {
  return(
    lapply(X=x,FUN=function(y) {
      if(!is.numeric(y) | y<2){
        return(c(-1)) # return -1 if not prime
      }
      if(y<9){ #lookup table for 2<=x<=8
        two_to_eight <- c(1,1,2,1,3,1,2)
        names(two_to_eight) <- as.character(2:8)    
        return(two_to_eight[as.character(y)][[1]])
      }
      #else
      pf <- c(2,seq(3,sqrt(y),2)) #possible factors for x>8 (skipping even numbers)
      f <-pf[y %% pf == 0]
      if(length(f) > 0) { return(f) } #return a list of factors
      else {return(1)} #return 1 if prime
    }
    )
  )
}

largest_prime_factor <- function(x) {
  factors <- unlist(is_prime(x)) 
  if(length(factors)==1){
    if(factors == c(1)) { return("Prime")}
    if(factors == c(-1)) { return("Not Prime")}
  }
  return(max(factors[unlist(is_prime(factors))==1]))
}


# also vectorised for the lulz
v_largest_prime_factor <- function(x) {
  return(lapply(X=x,FUN=largest_prime_factor))
}

# solution to the problem
largest_prime_factor(600851475143)



