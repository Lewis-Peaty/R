
ftbl <- function(x) {

 s <- "|"
 for(j in 1:ncol(x)){
  s <- c(s,x[1,j],"|")
 }  
 print(paste(collapse="",s))
 
 s <- "|"
 for(j in 1:ncol(x)){
   s <- c(s,rep("-",nchar(as.character(x[1,j]))),"|")
 }  
 print(paste(collapse="",s))
 
 for(i in 2:nrow(x)){
   s <- "|"
   for(j in 1:ncol(x)){
     s <- c(s,x[i,j],"|")
   }
   print(paste(collapse="",s))
 }
}

a <- diag(4)+20
ftbl(a)
