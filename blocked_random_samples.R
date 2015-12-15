# This script demonstrates blocked random sampling.
# Original dataset was a table of streetlight metal poles with install dates.
# Samples are drawn from 3 install date blocks- the 70s, 80s, and 90s. 10 Samples randomly from each.


# load dataset
data <- read.csv("LSTD_All_Current_With_LatLong_T1.txt")

# parse DATE_INSTALLED string to an R date object for use with > and < comparators.
data$RInstall_Date <- as.Date(data$DATE_INSTALLED, '%d/%m/%Y')

# use the subset function to generate the decade blocks
seventies <- subset(data, RInstall_Date <  as.Date(c("1980-01-01")) & RInstall_Date >  as.Date(c("1970-01-01")),
                  select=c(PICK_ID,RInstall_Date)) 
eighties <- subset(data, RInstall_Date <  as.Date(c("1990-01-01")) & RInstall_Date >  as.Date(c("1980-01-01")),
                    select=c(PICK_ID,RInstall_Date)) 
nineties <- subset(data, RInstall_Date <  as.Date(c("2000-01-01")) & RInstall_Date >  as.Date(c("1990-01-01")),
                    select=c(PICK_ID,RInstall_Date)) 
# set random seed 
set.seed(2342545)

# generate a list of (10) random samples for each decade block
seventies <- seventies[sample(1:nrow(seventies), 10,
                          replace=FALSE),] 
eighties <- eighties[sample(1:nrow(eighties), 10,
                              replace=FALSE),] 
nineties <- nineties[sample(1:nrow(nineties), 10,
                              replace=FALSE),] 

# combine rows (like an Access SQL UNION query)
sample_set <- rbind(seventies,eighties,nineties)

# export!
write.csv(sample_set,file = "dslmp_samples.txt")

