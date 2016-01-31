# Solution to https://projecteuler.net/problem=4

# Helper functions
str_rev <- function(x) {
  paste(collapse = "",rev(strsplit(x,split="")[[1]]))
}

has_3digit_factor <- function(n) {
  
  x = trunc(sqrt(n))
  while(n/x<1000) {
    if( n %% x == 0) {return(TRUE)}
    x <- x-1
  }
  return(FALSE)
}

# Solution
a <- as.data.frame(as.character(100:998),stringsAsFactors = FALSE)
colnames(a)[1] <- "a"
a$rev_a <-(lapply(a$a,FUN=str_rev))
a$pals <- mapply(FUN=function(x,y) {paste(sep="",x,y)},a$a,a$rev_a)
a$has_factors <- (lapply(X = as.numeric(a$pals), FUN = has_3digit_factor)[TRUE])
# Answer
max(a$pals[a$has_factors==TRUE])
