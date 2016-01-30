#My solution to https://projecteuler.net/problem=2

#intial conditions
f1 <- 1
f2 <- 2
f3 <- 0
s <- 2
#iterate over sequence
while(f2<=4000000){
 f3 <- f1 + f2
 f1 <- f2
 f2 <- f3
 
 #sum
 if(f3 %% 2 == 0){
   s <- s + f3
 }
}
#print solution
s