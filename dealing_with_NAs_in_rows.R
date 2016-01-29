########------------ Dealing With Presence of NAs in Row -------------########

test <- cbind(1:3,1:3,letters[1:3])
test[1,2] <- NA
test[2,3] <- NA
#Detect rows with NA using the any function
apply(test,2, FUN = function(x) {any(is.na(x))})
