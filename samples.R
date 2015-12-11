
# getting random samples
data <- read.csv("LSTD_All_Current_With_LatLong_T1.txt")

# reformat install date
data$RInstall_Date <- as.Date(data$DATE_INSTALLED, '%d/%m/%Y')

# using subset function
seventies <- subset(data, RInstall_Date <  as.Date(c("1980-01-01")) & RInstall_Date >  as.Date(c("1970-01-01")),
                  select=c(PICK_ID,RInstall_Date)) 
eighties <- subset(data, RInstall_Date <  as.Date(c("1990-01-01")) & RInstall_Date >  as.Date(c("1980-01-01")),
                    select=c(PICK_ID,RInstall_Date)) 
nineties <- subset(data, RInstall_Date <  as.Date(c("2000-01-01")) & RInstall_Date >  as.Date(c("1990-01-01")),
                    select=c(PICK_ID,RInstall_Date)) 
# set random seed
set.seed(2342545)

# random samples
seventies <- seventies[sample(1:nrow(seventies), 10,
                          replace=FALSE),] 
eighties <- eighties[sample(1:nrow(eighties), 20,
                              replace=FALSE),] 
nineties <- nineties[sample(1:nrow(nineties), 10,
                              replace=FALSE),] 

# combine
sample_set <- rbind(seventies,eighties,nineties)

# export!
write.csv(sample_set,file = "dslmp_samples.txt")

